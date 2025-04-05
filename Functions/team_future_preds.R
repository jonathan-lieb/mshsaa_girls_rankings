# Jonathan Lieb
# 3/4/25

# This function takes sims all future games for the team 
team_future_preds <- function(future, base#, class_mod, score_mod, opp_score_mod
                              ){
  if(nrow(future) == 0){
    return(datatable(tibble()))
  }
  
  t <- future$school
  o <- future$opp
  s <- future$season
  locs <- future$h_a_n
  full <- bind_rows(base, future)
  
  sim_single_game(full, #score_mod, opp_score_mod, class_mod,
                  t, o, s, locs) |> 
    select(school, opp, wins, losses, wins_opp, losses_opp, pred_class, 
           pred_win, pred_score, pred_score_opp) |> 
    mutate(pred_class = ifelse(pred_class == "win", "Win", "Loss")) |>
    datatable(class = paste0("cell-border compact"),
              container = htmltools::withTags(table(
                class = 'display',
                thead(
                  tr(lapply(c(
                    "School", "Opp", "Wins", "Losses", "Opp Wins",
                    "Opp Losses", "Pred. Outcome", "Pred. W%", 
                    "Pred. Score", "Pred. Opp Score"
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
    formatStyle(
      7,
      target = 'row',
      backgroundColor = styleEqual(c("Win", "Loss"), c('lightgreen', 'coral'))
    ) |> 
    formatRound(c(8, 9, 10), 2)
  
}

# Example
# team_future_preds(upcoming |> filter(school == "St. Elizabeth"), base, class_mod, score_mod, opp_score_mod)

                  