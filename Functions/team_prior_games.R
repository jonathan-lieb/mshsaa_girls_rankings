# Jonathan Lieb
# 3/4/25

# This function predicts the scores for the old modeling data
team_prior_games <- function(old_modeling, class_mod, score_mod, opp_score_mod, sc, sea){
  old_modeling |> 
    filter(school == sc, season == sea) |> 
    weight_history() |> 
    add_ternary() |> 
    augment(class_mod, new_data = _) |>
    augment(score_mod, new_data = _) |>
    augment(opp_score_mod, new_data = _) |>
    rename(pred_score = `.pred...3`,
           pred_score_opp = `.pred...1`) |> 
    select(school, opp, score, opp_score, pred_score, pred_score_opp, .pred_w) |> 
    mutate(outcome = case_when(score > opp_score ~ "W",
                               score < opp_score ~ "L",
                               score == opp_score ~ "T")) |> 
    relocate(outcome, .after = opp) |> 
    datatable(class = paste0("cell-border compact"),
              container = htmltools::withTags(table(
                class = 'display',
                thead(
                  tr(lapply(c(
                    "School", "Opp", "Result", "Score", "Opp Score",
                    "Pred. Score", "Pred. Opp Score", "Pred. W%"
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
      3,
      target = 'row',
      backgroundColor = styleEqual(c("W", "L"), c('lightgreen', 'coral'))
    ) |> 
    formatRound(c(6, 7, 8), 2)
}