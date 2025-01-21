yf_get_quotes <- function(ticker, from = "2000-01-01", to = Sys.Date()) {
  prices <- getSymbols(ticker, from = from, to = to, auto.assign = FALSE)
  return(prices)
}


# ms_get_quotes <- function(secId, universeId, frequency = "daily", from = "1970-01-01") {

#     base_url <- "https://tools.morningstar.it/api/rest.svc/timeseries_ohlcv/jbyiq3rhyf"
    
#     # Construct the query parameters
#     query_params <- list(
#         currencyId = "EUR",
#         idtype = "Morningstar",
#         frequency = frequency,
#         startDate = from,
#         performanceType = "",
#         outputType = "COMPACTJSON",
#         id = paste0(secId,"]3]0]",universeId),
#         applyTrackRecordExtension = TRUE
#     )
    
#     # Make the GET request
#     response <- GET(paste(base_url, "?", paste(names(query_params), query_params, sep = "=", collapse = "&"), sep = ""))
    
#     # Check if the request was successful
#     if (http_status(response)$category != "Success") {
#         stop("Failed to retrieve data: ", http_status(response)$message)
#     }
    
#     # Parse the JSON response
#     data <- content(response, as = "parsed", encoding = "UTF-8")
    
#     # Convert the list to a dataframe
#     df_data <- do.call(rbind, lapply(data, function(x) {
#         data.frame(
#             timestamp = as.POSIXct(x[[1]] / 1000, origin = "1970-01-01", tz = "UTC"),
#             open = x[[2]],
#             high = x[[3]],
#             low = x[[4]],
#             close = x[[5]],
#             volume = x[[6]]
#         )
#     }))
    
#     return(df_data)
# }

# # eni_quotes <- ms_get_quotes(ms_tickers_mil$SecId[ms_tickers_mil$Ticker == "ENI"], ms_tickers_mil$Universe[ms_tickers_mil$Ticker == "ENI"])
# # tail(eni_quotes)
# # > tail(eni_quotes)
# #       timestamp   open   high    low  close   volume    
# # 7516 2025-01-10 13.750 14.030 13.704 13.744 17049554    
# # 7517 2025-01-13 13.840 13.926 13.810 13.872  9116516
# # 7518 2025-01-14 13.764 13.804 13.712 13.744 11347566    
# # 7519 2025-01-15 13.840 13.990 13.828 13.984 13160522    
# # 7520 2025-01-16 14.042 14.070 13.920 13.944  9221847    
# # 7521 2025-01-17 14.064 14.138 14.032 14.092 11902177 

