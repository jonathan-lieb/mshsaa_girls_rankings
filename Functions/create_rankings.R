# Jonathan Lieb
# 2/24/25

# This function creates rankings from a ranking frame with predictions

create_rankings <- function(ranking_preds){
  school_sums <- ranking_preds |> 
    group_by(school) |>
    summarize(n = n(),
              win_per = mean(.pred_class == "w"),
              avg_proj_wper = mean(.pred_w),
              avg_proj_score = mean(pred_score),
              avg_proj_opp_score = mean(pred_score_opp))
  opp_sums <- ranking_preds |>
    group_by(opp) |> 
    summarize(n = n(),
              win_per = mean(.pred_class == "l"),
              avg_proj_wper = mean(.pred_l),
              avg_proj_score = mean(pred_score_opp),
              avg_proj_opp_score = mean(pred_score)
              )
  full_sums <- full_join(school_sums, opp_sums, by = c("school" = "opp"), 
                         suffix = c("", "_opp")) |>
    mutate(across(everything(), ~ replace_na(.x, 0))) |> 
    group_by(school) |> 
    summarize(proj_wper = (win_per * n + win_per_opp * n_opp) / (n + n_opp),
           avg_proj_wper = (avg_proj_wper * n + avg_proj_wper_opp * n_opp) / (n + n_opp),
           avg_proj_score = (avg_proj_score * n + avg_proj_score_opp * n_opp)/
             (n + n_opp),
           avg_proj_score_opp = (avg_proj_opp_score * n + avg_proj_opp_score_opp * n_opp)/
             (n + n_opp)
           ) |> 
    arrange(desc(proj_wper), desc(avg_proj_wper))
    
}
