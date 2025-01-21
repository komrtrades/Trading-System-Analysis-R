source("global.R")
source("Scrapers/get_tickers.R")
source("Scrapers/get_quotes.R")


generate_signals <- function(prices, y, z) {
  
  prices <- to.weekly(prices)
  names(prices) <- gsub("^.+\\.", "", names(prices))
  
  prices$return <- ROC(Cl(prices), n = 1, type = "discrete")
  prices$rsi <- RSI(Cl(prices), n = 2)
  prices$hh <- rollmaxr(Hi(prices), k = 52, fill = NA)
  prices$newhigh <- ifelse(Hi(prices) == prices$hh, 1, 0)
  
# Initialize last_newhigh to NA
prices$bars_since_newhigh <- NA
last_newhigh <- NA

for (i in 1:nrow(prices)) {
  if (!is.na(prices$newhigh[i]) && prices$newhigh[i] == 1) {
    last_newhigh <- i
    prices$bars_since_newhigh[i] <- 0
  } else {
    prices$bars_since_newhigh[i] <- if (!is.na(last_newhigh)) (i - last_newhigh) else NA
  }
}

  
  # Remove rows with NA values
  prices <- na.omit(prices)

  if (nrow(prices) == 0) {
    return(NULL)
  }

  # Define conditions for signals
  condition1 <- prices$rsi < y
  condition2 <- prices$bars_since_newhigh > 2 & prices$bars_since_newhigh <= z
  prices$signal <- condition1 & condition2
  
  # Create output dataframe with all necessary data
  df_signals <- data.frame(Date = as.POSIXct(index(prices)), coredata(prices))
  return(df_signals)
}

generate_trades <- function(df_signals, takeprofit, stoploss) {
  exitdate <- rep(NA, nrow(df_signals))
  exitprice <- rep(NA, nrow(df_signals))
  last_exit_date <- as.POSIXct("1970-01-01")

  for (i in which(df_signals$signal == 1)) {
    if (df_signals$Date[i] <= last_exit_date) {
      next
    }
    entry_date <- lead(df_signals$Date)[i]
    entry_price <- lead(df_signals$Open)[i]
    tp_price <- entry_price * (1 + takeprofit)
    sl_price <- entry_price * (1 - stoploss)
    
    if (i + 1 > nrow(df_signals)) {
      exitdate[i] <- NA
      exitprice[i] <- NA
      next
    }
    
    for (j in (i+1):nrow(df_signals)) {
      if (df_signals$High[j] >= tp_price) {
        exitdate[i] <- df_signals$Date[j]
        exitprice[i] <- tp_price
        last_exit_date <- exitdate[i]
        break
      } else if (df_signals$Low[j] <= sl_price) {
        exitdate[i] <- df_signals$Date[j]
        exitprice[i] <- sl_price
        last_exit_date <- exitdate[i]
        break
      }
    }
  }
  
  data.frame(
    entrydate = lead(df_signals$Date)[df_signals$signal == 1],
    entryprice = lead(df_signals$Open)[df_signals$signal == 1],
    exitdate = as.POSIXct(exitdate[df_signals$signal == 1], origin = "1970-01-01"),
    exitprice = exitprice[df_signals$signal == 1]
  )
}


generate_trades <- function(df_signals, takeprofit, stoploss) {
  exitdate <- rep(NA, nrow(df_signals))
  exitprice <- rep(NA, nrow(df_signals))
  last_exit_date <- as.POSIXct("1970-01-01")
  
  for (i in seq.int(nrow(df_signals))) {
    if (df_signals$signal[i] != 1 || df_signals$Date[i] <= last_exit_date) {
      next
    }
    entry_date <- lead(df_signals$Date)[i]
    entry_price <- lead(df_signals$Open)[i]
    tp_price <- entry_price * (1 + takeprofit)
    sl_price <- entry_price * (1 - stoploss)
    
    if (i + 1 > nrow(df_signals)) {
      next
    }
    
    for (j in (i + 1):nrow(df_signals)) {
      if (df_signals$High[j] >= tp_price) {
        exitdate[i] <- df_signals$Date[j]
        exitprice[i] <- tp_price
        last_exit_date <- exitdate[i]
        break
      } else if (df_signals$Low[j] <= sl_price) {
        exitdate[i] <- df_signals$Date[j]
        exitprice[i] <- sl_price
        last_exit_date <- exitdate[i]
        break
      }
    }
  }
  
  # Filter out rows where `last_exit_date` condition applies
  valid_indices <- !is.na(exitdate)
  
  data.frame(
    entrydate = lead(df_signals$Date)[df_signals$signal == 1 & valid_indices],
    entryprice = lead(df_signals$Open)[df_signals$signal == 1 & valid_indices],
    exitdate = as.POSIXct(exitdate[df_signals$signal == 1 & valid_indices], origin = "1970-01-01"),
    exitprice = exitprice[df_signals$signal == 1 & valid_indices]
  )
}



check_trade_status <- function(df_trades) {
  if (nrow(df_trades) == 0) {
    return("No Action")
  }
  last_trade <- tail(df_trades, 1)
  if (!is.na(last_trade$entrydate) && is.na(last_trade$exitdate)) {
    return("Open Trade")
  } else if (all(is.na(last_trade))) {
    if (nrow(df_trades) > 1) {
      second_last_trade <- tail(df_trades, 2)[1, ]
      if (!is.na(second_last_trade$entrydate) && !is.na(second_last_trade$entryprice) &&
          is.na(second_last_trade$exitdate) && is.na(second_last_trade$exitprice)) {
        return("Open Trade")
      }
    }
    return("New Signal")
  } else {
    return("No Action")
  }
}

check_trade_status <- function(df_signals, open_trades) {
  # Check if trades exist
  if (!is.na(open_trades$Date[1])) {
    if (open_trades$Date[1] == df_signals$Date[nrow(df_signals)]) {
      return("New Signal")
    } else {
      return("Open Trade")
    }
  } else {
    return("No Action")
  }
}


find_open_trades <- function(df_signals, df_trades) {
  # If no trades exist, return all signals with signal == 1
  if (nrow(df_trades) == 0) {
    return(subset(df_signals, signal == 1))
  }
  
  # Get the last exit date from trades
  last_exit_date <- max(df_trades$exitdate, na.rm = TRUE)
  
  # Identify signals after the last exit date
  open_signals <- subset(df_signals, signal == 1 & Date > last_exit_date)
  
  return(open_signals[1,])
}

auto_signal_check <- function(tickers, y, z, takeprofit, stoploss) {
  triggered_signals <- c()
  open_trades <- c()
  
  for (ticker in tickers) {
    print(ticker)
    prices <- tryCatch({
        yf_get_quotes(ticker)
    }, error = function(e) {
        message(paste("Error fetching quotes for ticker:", ticker, "Skipping..."))
        return(NULL)
    })
    
    if (is.null(prices)) {
        next
    }
    
    # Ensure there are enough rows for rollapply
    if (nrow(prices) < y) {
        message(paste("Not enough data for ticker:", ticker, "Skipping..."))
        next
    }
    
    df_signals <- tryCatch({
        generate_signals(prices, y, z)
    }, error = function(e) {
        message(paste("Error generating signals for ticker:", ticker, "Skipping..."))
        return(NULL)
    })
    
    if (is.null(df_signals)) {
        next
    }
    
    df_trades <- generate_trades(df_signals, takeprofit, stoploss)
    
    open_trade <- find_open_trades(df_signals, df_trades)
    trade_status <- check_trade_status(df_signals, open_trade)
    
    if (trade_status == "New Signal") {
        triggered_signals <- c(triggered_signals, ticker)
    } else if (trade_status == "Open Trade") {
        open_trades <- c(open_trades, ticker)
    }
  }  
  
  list(triggered_signals = triggered_signals, open_trades = open_trades)
}

tickers_mib <- yf_get_tickers_ita("idx_mib")
tickers_nq100 <- yf_get_tickers_nq100()
tickers_euronext100 <- yf_get_tickers_euronext100()

signals_nq <- auto_signal_check(tickers_nq100, 10, 8, 0.07, 0.15)
signals_mib <- auto_signal_check(tickers_mib, 8, 8, 0.08, 0.16)
signals_euro <- auto_signal_check(tickers_euronext100, 10, 8, 0.07, 0.15)


# send email
# dotenv::load_dot_env()
dotenv::load_dot_env(file = ".Renviron")
send.mail(from = Sys.getenv("USERNAME"),
          to = c("komrtrades@gmail.com"),
          subject = "Trading Signals",
          body = paste0("Signals for Nasdaq 100: \n\n", paste(signals_nq$triggered_signals, collapse = ", "), "\n\nOpen Trades: \n\n", paste(signals_nq$open_trades, collapse = ", "), "\n\n----------------\n\nSignals for MIB: \n\n", paste(signals_mib$triggered_signals, collapse = ", "), "\n\nOpen Trades: \n\n", paste(signals_mib$open_trades, collapse = ", "), "\n\n----------------\n\nSignals for Euronext 100: \n\n", paste(signals_euro$triggered_signals, collapse = ", "), "\n\nOpen Trades: \n\n", paste(signals_euro$open_trades, collapse = ", ")),
          smtp = list(host.name = "smtp.gmail.com", 
                      port = 465, 
                      user.name = Sys.getenv("USERNAME"), 
                      passwd = Sys.getenv("PASSWORD"), 
                      ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)

