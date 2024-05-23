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
                 plotOutput("allStrategiesPlot"),
                 tableOutput("strategyComparisonTable"))
      )
    ),
    mainPanel()
  )
)
