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
base <- read_parquet("Data/cleaned.parquet")
score_mod <- read_rds("Models/lm_reg.rds")
opp_score_mod <- read_rds("Models/lm_opp_reg.rds")
class_mod <- read_rds("Models/class_glm_model.rds")
drankings <- read_parquet("Data/drankings.parquet")
school_choices <- unique(drankings$school)

ui <- dashboardPage(
  header = dashboardHeader(title = "Lieb MSHSAA Girls Basketball"),
  sidebar = source("Components/sidebar.R", local = TRUE)$value,
  body = source("Components/body.R", local = TRUE)$value
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Home server
  output$home_top10 <- renderDT({
    get_top10(drankings)
  })
  
  # Rankings Server
  output$state_rankings <- renderDT({
    get_rankings(drankings, input$class_rank_in, input$year_rank_in, 
                 input$district_rank_in)})
  
  # Sim Server
  output$sim_donut_plot <- renderPlot({
    sim_game_donut(sim_single_game(base, score_mod, opp_score_mod, class_mod,
                                   input$sim_team1_in, input$sim_team2_in,
                                   max(base$season), input$sim_location_in))
  })
  
  output$sim_percentile_plot <- renderPlot({
    sim_game_percentiles(drankings |> filter(date == max(drankings$date)), 
                         input$sim_team1_in, input$sim_team2_in)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
