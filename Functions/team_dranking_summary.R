# Jonathan Lieb
# 3/4/25

# Team Dranking summary
team_dranking_summary <- function(drankings, sc, sea){
  drankings |> 
    filter(season == sea, school == sc) |> 
    filter(date == max(date)) |>
    select(school, rank, wins, losses, ppg, papg, rpi, crpi) |> 
    datatable(class = paste0("cell-border compact"),
              container = htmltools::withTags(table(
                class = 'display',
                thead(
                  tr(lapply(c(
                    "School", "Class Rank", "Wins", "Losses", "PPG",
                    "PAPG", "RPI", "CRPI"
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
                             ))) |> 
    formatRound(c(5:6), 2) |> 
    formatRound(c(7:8), 3)
}