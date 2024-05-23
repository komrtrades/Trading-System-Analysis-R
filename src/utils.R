# Function to load data
load_data <- function(file_path) {
  data <- read.csv2(file_path)
  return(data)
}

# Function to preprocess data
preprocess_data <- function(data) {
  data <- data %>% filter(exitSignal != 'Open')  # Remove open trades
  data$entrySignal <- as.factor(data$entrySignal)
  data$exitSignal <- as.factor(data$exitSignal)
  data$entryTime <- as.POSIXct(data$entryTime, format = "%d/%m/%Y %H:%M:%S")
  data$exitTime <- as.POSIXct(data$exitTime, format = "%d/%m/%Y %H:%M:%S")
  data$type <- as.factor(data$type)
  data$symbol <- as.factor(data$symbol)
  return(data)
}

# Function to compute additional metrics
compute_metrics <- function(data) {
  data <- data %>%
    mutate(duration = as.numeric(difftime(exitTime, entryTime, units = "weeks")),
           profitable = ifelse(profit>0,1,0)) %>%
    na.omit()  # Remove rows with NA values
  return(data)
}

# Function to compute equity lines
compute_equity_lines <- function(data) {
  equity_by_signal <- data %>% 
    group_by(entrySignal) %>% 
    arrange(entryTime) %>% 
    mutate(equity = cumsum(profit))
  
  overall_equity <- data %>% 
    arrange(entryTime) %>% 
    mutate(equity = cumsum(profit))
  
  return(list(equity_by_signal = equity_by_signal, overall_equity = overall_equity))
}

# Function to compute linearity metrics
compute_linearity_metrics <- function(equity_by_signal, overall_equity) {
  if (!is.null(equity_by_signal)) {
    linearity_metrics <- equity_by_signal %>% 
      group_by(entrySignal) %>% 
      do({
        model <- lm(equity ~ as.numeric(entryTime), data = .)
        predictions <- predict(model, newdata = .)
        mse <- mean((.$equity - predictions) ^ 2)
        rsquared <- summary(model)$r.squared
        data.frame(entrySignal = unique(.$entrySignal), mse = mse, rsquared = rsquared)
      })
  } else {
    linearity_metrics <- data.frame(entrySignal = character(), mse = numeric(), rsquared = numeric())
  }
  
  overall_model <- lm(equity ~ as.numeric(entryTime), data = overall_equity)
  overall_predictions <- predict(overall_model, newdata = overall_equity)
  overall_mse <- mean((overall_equity$equity - overall_predictions) ^ 2)
  overall_rsquared <- summary(overall_model)$r.squared
  linearity_metrics <- rbind(linearity_metrics, data.frame(entrySignal = "Portfolio", mse = overall_mse, rsquared = overall_rsquared))
  
  return(linearity_metrics)
}

# Function to plot equity lines
plot_equity_lines <- function(equity_by_signal, overall_equity) {
  plot <- ggplot() +
    geom_line(data = equity_by_signal, aes(x = entryTime, y = equity, color = entrySignal)) +
    geom_line(data = overall_equity, aes(x = entryTime, y = equity, color = "Portfolio")) +
    labs(title = "Equity Line by Entry Signal",
         x = "Entry Time",
         y = "Equity")
  return(plot)
}

# Function to plot boxplots
plot_boxplots <- function(data) {
  p1 <- ggplot(data, aes(x = entrySignal, y = profit)) + 
    geom_boxplot(na.rm = TRUE) + 
    labs(title = "Profit by Entry Signal", x = "Entry Signal", y = "Profit")
  
  p2 <- ggplot(data, aes(x = exitSignal, y = profit)) + 
    geom_boxplot(na.rm = TRUE) + 
    labs(title = "Profit by Exit Signal", x = "Exit Signal", y = "Profit")
  
  p3 <- ggplot(data, aes(x = entrySignal, y = duration)) + 
    geom_boxplot(na.rm = TRUE) + 
    labs(title = "Duration by Entry Signal", x = "Entry Signal", y = "Duration (weeks)")
  
  p4 <- ggplot(data, aes(x = exitSignal, y = duration)) + 
    geom_boxplot(na.rm = TRUE) + 
    labs(title = "Duration by Exit Signal", x = "Exit Signal", y = "Duration (weeks)")
  
  return(list(p1 = p1, p2 = p2, p3 = p3, p4 = p4))
}

# Function to plot all strategies equity lines
plot_all_strategies <- function(all_equity_lines) {
  all_equity <- bind_rows(all_equity_lines, .id = "Strategy")
  plot <- ggplot(all_equity, aes(x = entryTime, y = equity, color = Strategy)) +
    geom_line() +
    labs(title = "Equity Line by Strategy",
         x = "Entry Time",
         y = "Equity")
  return(plot)
}

# Function to compute key metrics
compute_key_metrics <- function(data, equity_lines, linearity_metrics) {
  net_profit <- sum(data$profit)
  max_cumulative_drawdown <- min(equity_lines$equity - cummax(equity_lines$equity))
  return_on_drawdown <- net_profit / abs(max_cumulative_drawdown)
  num_trades <- nrow(data)
  avg_trade <- mean(data$profit)
  percent_profitable <- mean(data$profit>0)
  start_date <- min(data$entryTime)
  end_date <- max(data$exitTime)
  mse <- linearity_metrics$mse
  rsquared <- linearity_metrics$rsquared
  
  metrics <- data.frame(
    net_profit = net_profit,
    max_cumulative_drawdown = max_cumulative_drawdown,
    return_on_drawdown = return_on_drawdown,
    num_trades = num_trades,
    avg_trade = avg_trade,
    percent_profitable = percent_profitable,
    start_date = start_date,
    end_date = end_date,
    mse = mse,
    rsquared = rsquared
  )
  return(metrics)
}
