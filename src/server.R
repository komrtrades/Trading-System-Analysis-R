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
        updateSelectInput(session, "selected_file2",

 choices = names(uploaded_files$data))
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
  
  all_metrics <- reactive({
    lapply(names(uploaded_files$data), function(strategy) {
      data <- uploaded_files$data[[strategy]]
      equity <- compute_equity_lines(compute_metrics(data))$overall_equity
      linearity <- compute_linearity_metrics(equity_by_signal = NULL, overall_equity = equity)
      metrics <- compute_key_metrics(data, equity, linearity)
      metrics <- cbind(strategy = strategy, metrics)
      return(metrics)
    }) %>% bind_rows()
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
  
  output$strategyComparisonTable <- renderTable({
    all_metrics()
  })
}
