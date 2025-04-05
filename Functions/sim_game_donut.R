# Jonathan Lieb
# 2/28/25

sim_game_donut <- function(matchup){
  matchup |>
    select(school, opp, pred_win, pred_lose) |> 
    add_row(school = matchup[1,]$opp, opp = matchup[1,]$school, pred_win = matchup[1,]$pred_lose,
            pred_lose = matchup[1,]$pred_win) |> 
    mutate(row = row_number()) |>
    ggplot(aes(x = 0, y  = pred_win, fill = fct_inorder(school))) +
    geom_col() +
    labs(title = "Projected Win Probability")+
    scale_x_continuous(limits = c(-1,1)) +
    geom_label_repel(aes(x = 0, y = 1 - cumsum(pred_win) + pred_win * .5,
                         label = glue("{school}: {round(100 * pred_win, 2)}%")))+
    coord_polar(theta = "y", start = 2*pi, direction = 1) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}