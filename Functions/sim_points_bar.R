# Jonathan Lieb
# 3/4/25

sim_points_bars <- function(matchup){
  matchup |> 
    select(school, pred_score, opp, pred_score_opp) |> 
    datatable(class = paste0("cell-border compact"),
              container = htmltools::withTags(table(
                class = 'display',
                thead(
                  tr(lapply(c(
                    "School", "Pred. Score", "Opponent", "Pred. Opponet Score"
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
    formatRound(c(2, 4), 2)
}