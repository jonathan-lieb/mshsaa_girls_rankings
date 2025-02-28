# Jonathan Lieb
# 2/28/25

sim_game_percentiles <- function(drankings, s, o){
  drankings |> 
    select(school, rank, wins, losses, ppg, papg, rpi, crpi) |> 
    pivot_longer(cols = c(rank, wins, losses, ppg, papg, rpi, crpi), 
                 names_to = "stat", values_to = "value") |> 
    group_by(stat) |> 
    mutate(percent_rank = case_when(stat %in% c("wins", "rpi", "crpi", "ppg") ~ percent_rank(value) * 100,
                                    T ~ percent_rank(-value) * 100)) |> 
    ungroup() %>%
    filter(school %in% c(s, o)) |> # Keep only the two teams
    mutate(stat = case_when(stat == "rank" ~ "Class Rank",
                            stat == "rpi" ~ "Rating Percentage Index",
                            stat == "wins" ~ "Wins",
                            stat == "losses" ~ "Losses",
                            stat == "crpi" ~ "Class Rating Percentage Index",
                            stat == "ppg" ~ "Points Per Game",
                            stat == "papg" ~ "Points Allowed Per Game")) |> # Rename stat values
    ggplot(aes(stat, percent_rank, fill = percent_rank)) +
    geom_col() +
    geom_label(aes(stat, label = percent_rank %>% round(0), fill = percent_rank),
               fontface = "bold") + # hjust = -0.5
    geom_label(aes(stat, y = 110, label = value |> round(3)),
               fill = "white",
               fontface = "bold") + # hjust = -0.5
    #geom_text(aes(stat, 110, label = val)) +
    scale_fill_gradientn(
      limits = c(0, 100),
      colours = c("blue", "royalblue", "gray", "indianred", "red"),
      values = c(0, .25, .50, .75, 1.00)
    ) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 115)) +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 10),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none"
    ) +
    facet_wrap(~fct_inorder(school), ncol = 1) +
    labs(title = "School Percentile Ranks")+
    coord_flip() 
}