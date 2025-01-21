yf_isin_to_ticker <- function(isin) {
    url <- paste0("https://query2.finance.yahoo.com/v1/finance/search?q=", isin)
    response <- GET(url)
    content <- content(response, as = "parsed")
    ticker <- content$quotes[[1]]$symbol[[1]]
    return(ticker)
}

yf_get_tickers_nq100 <- function() {
    url <- "https://www.slickcharts.com/nasdaq100"

    hh <- read_html(url) %>%
        html_nodes("table")

    htmlpage <- session(url, httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20"))

    nq100_tickers <- hh[[1]] %>%
        html_table() %>%
        select(Symbol) %>%
        filter(Symbol != "NA") %>%
        arrange(Symbol) %>%
        pull(Symbol)

    return(nq100_tickers)
}

# nq100_tickers <- yf_get_tickers_nq100()
# head(nq100_tickers)
# [1] "AAPL" "ABNB" "ADBE" "ADI"  "ADP"  "ADSK"


yf_get_tickers_ita <- function(idx = "") {

    available_indices <- c("idx_mib", "idx_cap", "idx_mid", "idx_small", "idx_star", "idx_brands", "idx_all", "idx_aim", "idx_global")

    if (idx != "" && !(idx %in% available_indices)) {
        stop(paste("Invalid index. Available indices are:", paste(available_indices, collapse = ", ")))
    }

    url <- "https://www.borsaitaliana.it/borsa/azioni/cerca-titolo/share-selector/search.html?lang=it#result"

    body_request <- list(
        page = "",
        indices = "ALL",
        sectors = "ALL",
        resultsPerPage = "100"
    )

    p <- 0
    df_stocks <- data.frame()
    while(TRUE) {
        p <- p + 1
        body_request$page <- p

        response <- POST(url, body = body_request, encode = "form")
        content <- content(response, as = "parsed")
        table_temp <- content %>% html_nodes("table.m-table") %>% html_table()
        table_temp <- table_temp[[2]]

        if (nrow(df_stocks) == 0) df_stocks <- table_temp else
        if (table_temp[nrow(table_temp), 1] == df_stocks[nrow(df_stocks), 1]) break
        else df_stocks <- rbind(df_stocks, table_temp)
    }

    df_stocks <- df_stocks %>% select(1:5,7:9,22:24)

    df_stocks <- df_stocks %>% 
    mutate(idx_mib = grepl("MIB", Indice),
              idx_cap = grepl("Capped", Indice),
              idx_mid = grepl("Mid", Indice),
              idx_small = grepl("Small", Indice),
              idx_star = grepl("STAR", Indice),
              idx_brands = grepl("Brands", Indice),
              idx_all = grepl("All-Share", Indice),
              idx_aim = grepl("Growth", `Mercato/Segmento`),
              idx_global = grepl("Global", `Mercato/Segmento`))

    # Capitalizzazione and Controvalore Giornaliero as numeric (remove dots and substitute commas with dots)
    df_stocks$Capitalizzazione <- as.numeric(gsub(",", ".", gsub("\\.", "", df_stocks$Capitalizzazione)))
    df_stocks$`Controvalore Giornaliero` <- as.numeric(gsub(",", ".", gsub("\\.", "", df_stocks$`Controvalore Giornaliero`)))


    if (idx == "") {
        tickers <- df_stocks %>% 
            mutate(ticker = paste0(`Codice Alfanumerico`, ".MI")) %>% 
            select(ticker) %>% 
            pull()
    } else {
        tickers <- df_stocks %>% 
            filter(!!sym(idx)) %>% 
            mutate(ticker = paste0(`Codice Alfanumerico`, ".MI")) %>% 
            select(ticker) %>% 
            pull()
    }

    return(tickers)
}


yf_get_tickers_euronext100 <- function() {
    url <- "https://live.euronext.com/en/ajax/getIndexComposition/FR0003502079-XPAR"

    euronext_isin <- tryCatch({
        x <- read_html(url) %>%
            html_nodes("table") %>%
            html_table() %>%
            .[[1]]
    }, error = function(e) {
        message("Error extracting table: ", e)
        return(NULL)
    })

    if (is.null(euronext_isin) || !is.data.frame(euronext_isin)) {
        stop("Failed to extract ISIN data from the table.")
    }

    euronext_tickers <- unname(sapply(euronext_isin$ISIN, yf_isin_to_ticker))
    return(euronext_tickers)
}

# ita_tickers <- yf_get_tickers_ita("idx_mib")
# head(ita_tickers)
# [1] "A2A.MI"  "AMP.MI"  "AZM.MI"  "BMED.MI" "BMPS.MI" "BAMI.MI"



# scrape_morningstar_data <- function(page = 1, pageSize = 10, currencyId = "GBP", universeIds = "E0EXG$XMIL") {
#   base_url <- "https://tools.morningstar.co.uk/api/rest.svc/klr5zyak8x/security/screener"

#   # Construct the query parameters
#   query_params <- list(
#     page = page,
#     pageSize = pageSize,
#     sortOrder = "Name%20asc",
#     outputType = "json",
#     version = 1,
#     languageId = "it-IT",
#     currencyId = currencyId,
#     universeIds = universeIds,
#     securityDataPoints = paste(
#       c(
#         "SecId", "LegalName", "Ticker", "Universe"
#       ),
#       collapse = "%7C"
#     ),
#     filters = "",
#     term = "",
#     subUniverseId = ""
#   )

#   # Make the GET request
#   response <- GET(paste(base_url, "?", paste(names(query_params), query_params, sep = "=", collapse = "&"), sep = ""))
  
#   # Check if the request was successful
#   if (http_status(response)$category != "Success") {
#     stop("Failed to retrieve data: ", http_status(response)$message)
#   }

#   # Parse the JSON response
#   data <- content(response, as = "text", encoding = "UTF-8")
#   parsed_data <- fromJSON(data, flatten = TRUE)

#   return(parsed_data)
# }

# ms_get_tickers <- function(exchange = "") {
#   # named vector of Universe IDs
# ms_exchange_ids <- c(
#     "E0EXG$XNAS", # NASDAQ
#     "E0EXG$XNYS", # NYSE
#     "E0EXG$XMIL", # FTSE MIB
#     "E0EXG$XETR", # XETRA
#     "E0EXG$XAMS", # Euronext Amsterdam
#     "E0EXG$XPAR", # Euronext Paris
#     "E0EXG$XBRU" # Euronext Brussels
# )
# names(ms_exchange_ids) <- c(
#     "NASDAQ",
#     "NYSE",
#     "MIL",
#     "XETRA",
#     "AMS",
#     "PAR",
#     "BRU"
# )

#     if (exchange == "" || !(exchange %in% names(ms_exchange_ids))) {
#         stop("Please provide an exchange name. Available exchanges are: ", paste(names(ms_exchange_ids), collapse = ", "))
#     }

#   universeIds <- universe_ids[exchange]
#   result <- scrape_morningstar_data(universeIds = universeIds)
#   total <- result$total
#   result <- scrape_morningstar_data(page = 1, pageSize = total, universeIds = universeIds)
#   return(result$rows)
# }

# # tickers_nyse <- ms_get_tickers("NYSE")
# # str(tickers_nyse)
# # 'data.frame':   2316 obs. of  4 variables:
# #  $ SecId    : chr  "0P0000000G" "0P0000000I" "0P0001NAQE" "0P0000000Y" ...
# #  $ LegalName: chr  "3D Systems Corp" "3M Co" "a.k.a. Brands Holding Corp" "A.O. Smith Corp" ...
# #  $ Ticker   : chr  "DDD" "MMM" "AKA" "AOS" ...
# #  $ Universe : chr  "E0EXG$XNYS" "E0EXG$XNYS" "E0EXG$XNYS" "E0EXG$XNYS" ...


