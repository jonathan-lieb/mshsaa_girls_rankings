tabItem(tabName = "team",
        fluidRow(column(12, h1("Single Team History and Predictions"))),
        fluidRow(
          box(width = 6, 
              selectizeInput(inputId = "school_team_in",
                          label = "School",
                          choices = NULL,
                          # choices = school_choices,
                          # selected = "Calvary Lutheran",
                          multiple = FALSE)),
          box(width = 6, 
              selectInput(inputId = "year_team_in",
                          label = "Season",
                          choices = c(2015:2025),
                          selected = 2025,
                          multiple = T))),
        # fluidRow(
        # conditionalPanel(
        #         condition = "input.year_team_in == 2025",
        #         fluidRow(
        #                 box(width=12,
        #         h3("Current Team Summary"),
        #         dataTableOutput(outputId = "team_dranking_summary"))))),
        # fluidRow(
        # conditionalPanel(
        #         condition = "!shinyjs.isEmpty(tableId = 'team_games')",
        #         fluidRow(
        #                 box(width = 12,
        #                     h2("Prior Games"),
        #                     p("Table showing the results of all prior games"),
        #                     dataTableOutput(outputId = "team_games")))
        # ),
        # ),
        # conditionalPanel(
        #         condition = "!shinyjs.isEmpty(tableId = 'team_summary')",
        #         fluidRow(
        #                 box(width = 12,
        #                     h2("Summary of all prior games"),
        #                     dataTableOutput(outputId = "team_summary"))),
        # ),
        # conditionalPanel(
        #   condition = "input.year_team_in == 2025 && !shinyjs.isEmpty(tableId = 'team_future')",
        #   fluidRow(
        #   box(width = 12,
        #       h2("Future Games"),
        #       p("Table showing all predicted future outcomes"),
        #       dataTableOutput(outputId = "team_future"))),
        #   fluidRow(
        #     box(width = 12,
        #         h2("Current end of season projections"),
        #         dataTableOutput(outputId = "team_future_summary"))),
        #   
        #   )
)