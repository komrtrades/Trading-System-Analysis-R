# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)

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
           profitable = profit > 0)
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
  linearity_metrics <- equity_by_signal %>% 
    group_by(entrySignal) %>% 
    do({
      model <- lm(equity ~ as.numeric(entryTime), data = .)
      predictions <- predict(model, newdata = .)
      mse <- mean((.$equity - predictions) ^ 2)
      rsquared <- summary(model)$r.squared
      data.frame(entrySignal = unique(.$entrySignal), mse = mse, rsquared = rsquared)
    })
  
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
    geom_boxplot() + 
    labs(title = "Profit by Entry Signal", x = "Entry Signal", y = "Profit")
  
  p2 <- ggplot(data, aes(x = exitSignal, y = profit)) + 
    geom_boxplot() + 
    labs(title = "Profit by Exit Signal", x = "Exit Signal", y = "Profit")
  
  p3 <- ggplot(data, aes(x = entrySignal, y = duration)) + 
    geom_boxplot() + 
    labs(title = "Duration by Entry Signal", x = "Entry Signal", y = "Duration (weeks)")
  
  p4 <- ggplot(data, aes(x = exitSignal, y = duration)) + 
    geom_boxplot() + 
    labs(title = "Duration by Exit Signal", x = "Exit Signal", y = "Duration (weeks)")
  
  return(list(p1 = p1, p2 = p2, p3 = p3, p4 = p4))
}

# Shiny UI
ui <- fluidPage(
  titlePanel("Portfolio Performance Report"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File", accept = ".csv")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Equity Line", plotOutput("equityPlot")),
        tabPanel("Profit and Duration Boxplots", 
                 plotOutput("profitEntryBoxplot"),
                 plotOutput("profitExitBoxplot"),
                 plotOutput("durationEntryBoxplot"),
                 plotOutput("durationExitBoxplot")),
        tabPanel("Linearity Metrics", tableOutput("linearityTable"))
      )
    )
  )
)

# Shiny Server
server <- function(input, output) {
  data <- reactive({
    req(input$file)
    load_data(input$file$datapath)
  })
  
  processed_data <- reactive({
    preprocess_data(data())
  })
  
  metrics_data <- reactive({
    compute_metrics(processed_data())
  })
  
  equity_lines <- reactive({
    compute_equity_lines(metrics_data())
  })
  
  linearity_metrics <- reactive({
    equity_data <- equity_lines()
    compute_linearity_metrics(equity_data$equity_by_signal, equity_data$overall_equity)
  })
  
  output$equityPlot <- renderPlot({
    equity_data <- equity_lines()
    plot_equity_lines(equity_data$equity_by_signal, equity_data$overall_equity)
  })
  
  boxplots <- reactive({
    plot_boxplots(metrics_data())
  })
  
  output$profitEntryBoxplot <- renderPlot({
    boxplots()$p1
  })
  
  output$profitExitBoxplot <- renderPlot({
    boxplots()$p2
  })
  
  output$durationEntryBoxplot <- renderPlot({
    boxplots()$p3
  })
  
  output$durationExitBoxplot <- renderPlot({
    boxplots()$p4
  })
  
  output$linearityTable <- renderTable({
    linearity_metrics()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)