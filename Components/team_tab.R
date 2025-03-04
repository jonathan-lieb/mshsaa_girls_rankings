tabItem(tabName = "team",
        h1("Single Team History and Predictions", style = 
             "text-align: center;"),
        fluidRow(
          box(width = 6, 
              selectizeInput(inputId = "school_team_in",
                          label = "School",
                          # choices = NULL,
                          choices = school_choices,
                          selected = "Calvary Lutheran",
                          multiple = FALSE)),
          box(width = 6, 
              selectInput(inputId = "year_team_in",
                          label = "Season",
                          choices = c(2015:2025),
                          selected = 2025,
                          multiple = T))),
        fluidRow(
        conditionalPanel(
                condition = "input.year_team_in == 2025",
                fluidRow(
                        box(width=12,
                h3("Current Team Summary", style = "text-align: center;"),
                dataTableOutput(outputId = "team_dranking_sum"))))),
        fluidRow(
        conditionalPanel(
                condition = "!shinyjs.isEmpty(tableId = 'team_games')",
                fluidRow(
                        box(width = 12,
                            h2("Prior Games", style = "text-align: center;"),
                            p("Table showing the results of all prior games", 
                              style = "text-align: center;"),
                            dataTableOutput(outputId = "team_games")))
        ),
        ),
        conditionalPanel(
          condition = "input.year_team_in == 2025 && !shinyjs.isEmpty(tableId = 'team_future')",
          fluidRow(
          box(width = 12,
              h2("Future Games", style = "text-align: center;"),
              p("Table showing all predicted future outcomes", style = "text-align: center;"),
              dataTableOutput(outputId = "team_future"))),
        #   fluidRow(
        #     box(width = 12,
        #         h2("Current end of season projections"),
        #         dataTableOutput(outputId = "team_future_summary"))),
        #   
          )
)