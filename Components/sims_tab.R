tabItem(tabName = "sims",
        fluidRow(
          box(width = 12,
              h1("Simulate Game"),
              p("This lets you simulate any game between two teams found in the 
                database. The predictions are most accurate when the teams might
                reasonably play each other. If the teams are too many classes apart,
                you may be extrapolating. Also non-mshsaa teams are included in the
                list of teams, but predicting is less accurate with them."))),
        fluidRow(
          box(width = 6, 
              selectizeInput(inputId = "sim_team1_in",
                          label = "School",
                          choices = NULL,
                          # choices = school_choices,
                          # selected = "Calvary Lutheran",
                          multiple = FALSE)),
          box(width = 6,
              selectizeInput(inputId = "sim_team2_in",
                          label = "Opponent",
                          choices = NULL,
                          # choices = school_choices,
                          # selected = "Jamestown",
                          multiple = FALSE))),
        fluidRow(
          box(width = 6, 
              dateInput(inputId = "sim_date_in",
                        label = "Date",
                        value = today(),
                        min = today(),
                        max = today(),
                        format = "yyyy-mm-dd")),
          box(width = 6,
              selectInput(inputId = "sim_location_in",
                          label = "Location",
                          choices = c("Home" = "h",
                                      "Away" = "a",
                                      "Neutral" = "n"),
                          selected = "Home",
                          multiple = FALSE))),
        fluidRow(
          box(width = 4, 
              p("Graph showing win probabilities for each team"),
              # plotOutput(outputId = "sim_donut_plot")
              ),
          box(width = 8,
              p("Graph showing the percentile ranks for different statistics for
                the teams. The bars and colors show the ranks. The numbers on the
                right show the raw stats."),
              # plotOutput(outputId = "sim_percentile_plot"),
              ))
        
)