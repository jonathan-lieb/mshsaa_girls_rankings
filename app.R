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
# library("tidymodels")
library("jsonlite")

# Load functions
miceadds::source.all("Functions/")
base <- read_parquet("Data/base.parquet")
# score_mod <- read_rds("Models/lm_reg.rds")
# opp_score_mod <- read_rds("Models/lm_opp_reg.rds")
# class_mod <- read_rds("Models/class_glm_model.rds")
drankings <- read_parquet("Data/drankings.parquet")
upcoming <- read_parquet("Data/upcoming.parquet")
old_modeling <- read_parquet("Data/modeling.parquet")
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
  
  output$home_today <- renderDataTable({
    today <- upcoming |> filter(date == today())
    if(nrow(today) == 0)(today <- upcoming |> filter(date == min(date)))
    t <- today$school
    o <- today$opp
    s <- first(today$season)
    l <- today$h_a_n
    create_today_table(sim_single_game(base, #score_mod, opp_score_mod, class_mod,
                                       t, o, s, l))
  })
  
  # Rankings Server
  output$state_rankings <- renderDT({
    get_rankings(drankings, input$class_rank_in, input$year_rank_in, 
                 input$district_rank_in)})
  

  # Sim Server
  sim_game <- reactive({
    sim_single_game(base, # score_mod, opp_score_mod, class_mod, 
                    input$sim_team1_in, input$sim_team2_in, 
                    max(base$season), input$sim_location_in)
  })
  
  output$sim_donut_plot <- renderPlot(sim_game_donut(sim_game()))
  output$sim_points <- renderDT(sim_points_bars(sim_game()))
  
  output$sim_percentile_plot <- renderPlot({
    sim_game_percentiles(drankings |> filter(date == max(drankings$date)), 
                         input$sim_team1_in, input$sim_team2_in)
  })
  
  # Team Server
  output$team_dranking_sum <- renderDataTable({
    team_dranking_summary(drankings, input$school_team_in, input$year_team_in)
  })
  
  output$team_games <- renderDataTable({
    team_prior_games(old_modeling,# class_mod, score_mod, opp_score_mod, 
                     input$school_team_in, input$year_team_in)
  })
  
  output$team_future <- renderDataTable({
    team_future_preds(upcoming |> filter(school == input$school_team_in), 
                      base #class_mod, score_mod, opp_score_mod
                      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
