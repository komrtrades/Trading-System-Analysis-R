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

# Shiny UI
ui <- fluidPage(
  titlePanel("Portfolio Performance Report"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Import",
                 fileInput("files", "Choose CSV Files", multiple = TRUE, accept = ".csv"),
                 actionButton("clear_all", "Clear All"),
                 uiOutput("file_list")),
        tabPanel("Equity Line",
                 selectInput("selected_file", "Select File", choices = NULL),
                 plotOutput("equityPlot")),
        tabPanel("Profit and Duration Boxplots",
                 selectInput("selected_file2", "Select File", choices = NULL),
                 plotOutput("profitEntryBoxplot"),
                 plotOutput("profitExitBoxplot"),
                 plotOutput("durationEntryBoxplot"),
                 plotOutput("durationExitBoxplot")),
        tabPanel("Linearity Metrics",
                 selectInput("selected_file3", "Select File", choices = NULL),
                 tableOutput("linearityTable")),
        tabPanel("Strategy Comparison",
                 plotOutput("allStrategiesPlot"))
      )
    ),
    mainPanel()
  )
)

# Shiny Server
server <- function(input, output, session) {
  # Reactive value to store uploaded files
  uploaded_files <- reactiveValues(data = list())
  
  observeEvent(input$files, {
    req(input$files)
    for (i in 1:nrow(input$files)) {
      file <- input$files[i,]
      file_data <- load_data(file$datapath)
      uploaded_files$data[[file$name]] <- preprocess_data(file_data)
    }
    updateSelectInput(session, "selected_file", choices = names(uploaded_files$data))
    updateSelectInput(session, "selected_file2", choices = names(uploaded_files$data))
    updateSelectInput(session, "selected_file3", choices = names(uploaded_files$data))
  })
  
  observeEvent(input$clear_all, {
    uploaded_files$data <- list()
    updateSelectInput(session, "selected_file", choices = NULL)
    updateSelectInput(session, "selected_file2", choices = NULL)
    updateSelectInput(session, "selected_file3", choices = NULL)
  })
  
  output$file_list <- renderUI({
    req(names(uploaded_files$data))
    tagList(
      lapply(names(uploaded_files$data), function(name) {
        actionButton(inputId = paste0("remove_", name), label = paste("Remove", name))
      })
    )
  })
  
  observe({
    lapply(names(uploaded_files$data), function(name) {
      observeEvent(input[[paste0("remove_", name)]], {
        uploaded_files$data[[name]] <- NULL
        uploaded_files$data <- uploaded_files$data[!sapply(uploaded_files$data, is.null)]
        updateSelectInput(session, "selected_file", choices = names(uploaded_files$data))
        updateSelectInput(session, "selected_file2", choices = names(uploaded_files$data))
        updateSelectInput(session, "selected_file3", choices = names(uploaded_files$data))
      })
    })
  })
  
  selected_data <- reactive({
    req(input$selected_file)
    uploaded_files$data[[input$selected_file]]
  })
  
  selected_data2 <- reactive({
    req(input$selected_file2)
    uploaded_files$data[[input$selected_file2]]
  })
  
  selected_data3 <- reactive({
    req(input$selected_file3)
    uploaded_files$data[[input$selected_file3]]
  })
  
  metrics_data <- reactive({
    compute_metrics(selected_data())
  })
  
  metrics_data2 <- reactive({
    compute_metrics(selected_data2())
  })
  
  metrics_data3 <- reactive({
    compute_metrics(selected_data3())
  })
  
  equity_lines <- reactive({
    compute_equity_lines(metrics_data())
  })
  
  equity_lines2 <- reactive({
    compute_equity_lines(metrics_data2())
  })
  
  equity_lines3 <- reactive({
    compute_equity_lines(metrics_data3())
  })
  
  linearity_metrics <- reactive({
    equity_data <- equity_lines3()
    compute_linearity_metrics(equity_data$equity_by_signal, equity_data$overall_equity)
  })
  
  all_equity_lines <- reactive({
    lapply(uploaded_files$data, function(data) {
      compute_equity_lines(compute_metrics(data))$overall_equity
    })
  })
  
  output$equityPlot <- renderPlot({
    equity_data <- equity_lines()
    plot_equity_lines(equity_data$equity_by_signal, equity_data$overall_equity)
  })
  
  boxplots <- reactive({
    plot_boxplots(metrics_data2())
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
  
  output$allStrategiesPlot <- renderPlot({
    plot_all_strategies(all_equity_lines())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
