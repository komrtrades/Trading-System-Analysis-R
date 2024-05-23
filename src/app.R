# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)

source("src/utils.R")
source("src/ui.R")
source("src/server.R")

shinyApp(ui = ui, server = server)