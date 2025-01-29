tabItem(tabName = "home",
        fluidRow(column(12, h1("Your Home for MSHSAA Girl's Basketball Projections"))),
        fluidRow(
          box(width = 12,
              h2("Current Top 10"),
              p("All rankings are based off of current projected win percentages 
                against other teams in the same class"),
              # dataTableOutput(outputId = "home_top10")
              )
        ),
        fluidRow(box(
          width = 12,
              h2("Upcoming Games"),
              h3("The 2024-25 season is under way!"),
              # h2(textOutput(outputId = "home_date_games")),
              # dataTableOutput(outputId = "home_today")
        )
        )
)