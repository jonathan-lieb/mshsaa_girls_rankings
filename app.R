# Jonathan Lieb
# MSHSAA Girls Basketball Rankings and Predictions
# 1/29/2025


library("shiny")
library("tidyverse"); theme_set(theme_minimal())
library("DT")
library("gridExtra")
library("here")
library("shinydashboard")
library("arrow")
library("janitor")
library("lubridate")
library("scales")
library("reactable")
library("ggrepel")
library("glue")
library("tidymodels")
library("jsonlite")


# Load functions
miceadds::source.all("Functions/")


ui <- dashboardPage(
  header = dashboardHeader(title = "Lieb MSHSAA Girls Basketball"),
  sidebar = source("Components/sidebar.R", local = TRUE)$value,
  body = source("Components/body.R", local = TRUE)$value
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

}

# Run the application 
shinyApp(ui = ui, server = server)
