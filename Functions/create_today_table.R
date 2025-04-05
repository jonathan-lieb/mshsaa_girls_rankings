# Jonathan Lieb
#3/1/25

create_today_table <- function(pred_data){
  today <- pred_data |> 
    distinct(school, opp, .keep_all = T) |> 
    mutate(pred_win = pred_win *100) |> 
    mutate(across(c(ppg, papg, pred_win, ppg_opp, papg_opp), ~round(., 2))) |>
    mutate(across(c(crpi, crpi_opp), ~round(., 3))) |>
    select(School = school, Class = crpi, W = wins, L = losses, `PPG` = ppg,
           `PAPG` = papg,
           Opp = opp, `Opp. Class` = crpi_opp, `Opp. W` = wins_opp, `Opp. L` = losses_opp, `Opp. PPG` = ppg_opp,
           `Opp. PAPG` = papg_opp,
           `Pred. Outcome` = pred_class, `Pred. Win %` = pred_win, `Pred Score` = pred_score, `Pred Opp Score` = pred_score_opp)
  
  today_descriptions <- c(School = "Name of School", CRPI = "Class Rating Percentage Index of School",
                          W = "Wins", L = "Losses", `PPG` = "Points Per Game",
                          `PAPG` = "Points Allowed Per Game",
                          Opp = "Opponent", `Opp. CRPI` = "Class Rating Percentage Index of Opponent",
                          `Opp. W` = "Opponent Wins", `Opp. L` = "Opponent Losses",
                          `Opp. PPG` = "Opponent Points Per Game", `Opp. PAPG` = "Opponent Points Allowed Per Game",
                          `Pred. Outcome` = "Predicted Outcome", `Pred. Win %` = "Predicted Win %",
                          `Pred Score` = "Predicted Score", `Pred Opp Score` = "Predicted Opponent Score") |>
    unname()
  
  headers <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(lapply(colnames(today), function(col) {
        th(col, style = 'border: 1px solid #ddd; background-color: lightgray; color: black;
           text-align: center;')
      })
      )
    )
  ))
  
  datatable(today,
            class = paste0("cell-border compact"),
            container = headers,
            rownames = F,
            options = list(
              ordering = FALSE,
              headerCallback = JS(
                sprintf(
                  "function(thead, data, start, end, display) {
              var tooltips = %s;
              $(thead).find('th').each(function(index) {
                $(this).attr('title', tooltips[index]);
              });
            }",
                  jsonlite::toJSON(unname(today_descriptions), auto_unbox = TRUE)
                )
              )
            ),
  )|> 
    formatRound(15:16, 2) |> 
    formatStyle(names(today),
                valueColumns = "Pred. Outcome",
                backgroundColor = styleEqual(c("win", "lose"), c("lightgreen","coral")))
}
  