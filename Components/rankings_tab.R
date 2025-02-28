tabItem(tabName = "rankings",
        fluidRow(column(12, h1("Missouri Girls Basketball State Rankings",
                               style = "color: #747474; font-weight: bold;
                               text-align: center;"))),
        fluidRow(column(12, p("Get the rankings for any class, district, or season.
                              Note that some districts don't exist in some classes and
                              some classes don't exist in some seasons.",
                              style = "text-align: center;"))),
        fluidRow(
          box(width = 4,
              selectInput(inputId = "class_rank_in",
                          label = "Class",
                          choices = c(1:6),
                          selected = 1,
                          multiple = F)),
          box(width = 4, 
              selectInput(inputId = "year_rank_in",
                          label = "Season",
                          choices = c(2015:2025),
                          selected = 2025,
                          multiple = F)),
          box(width = 4,
              selectInput(inputId = "district_rank_in",
                          label = "District",
                          choices = c("All", 1:16),
                          selected = "All",
                          multiple = F))),
        fluidRow(
          box(width = 12,
              p("Table showing the rankings for each team in the selected class and
                district. The rankings are based on the projected win percentages
                for each of the teams if they played each of the other teams in the
                table."),
              DTOutput(outputId = "state_rankings")
              ))
)