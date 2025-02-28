# Jonathan Lieb
# 2/24/25

# This function is used to create a datatable with the top 10 teams
# for each class 

# requires
# library(dplyr)
# library(DT)
# library(shiny)

get_top10 <- function(drankings){
  drankings |> 
    filter(date == max(date), rank <= 10) |>
    select(school, class, rank) |> 
    pivot_wider(names_from = class, values_from = school) |> 
    datatable(class = paste0("cell-border compact"),
              container = htmltools::withTags(table(
                class = 'display',
                thead(
                  tr(
                    th(rowspan = 2, 'Rank', style = 'text-align: center;'),
                    th(colspan = 6, 'Class', style = 'text-align: center;')
                  ),
                  tr(lapply(c(
                    "1A", "2A", "3A", "4A", "5A", "6A"
                    ), function(col) {
                      th(col, style = 'border: 1px solid #ddd; text-align: center;')
                    })
                  )
                )
              )),
              rownames = FALSE,
              options = list(paging = F, scrollY = F,
                             scrollX = F, ordering = F, searching = F,
                             dom = 't',
                             initComplete = htmlwidgets::JS(
                               "function(settings, json) {",
                               paste0("$(this.api().table().container()).css({'font-size': '", "14px", "'});"),
                               "}"),
                             headerCallback = JS(
                               "function(thead, data, start, end, display) {",
                               sprintf("  $(thead).closest('thead').css({'background-color': '%s', 'color': '%s', 'border': '1px solid #ddd'});", 'lightgray', 'black'),
                               "  $(thead).find('th').css({'border': '1px solid #ddd'});",
                               "}"
                             ))) 
}
