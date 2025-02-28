# Jonathan Lieb
# 2/25/25

# This function is used to get the rankings for a particular class or 
# district. 

get_rankings <- function(drankings, cl, season, districts){
  if(districts == "All")(districts <- 1:16)
  class_data <- drankings |> 
    filter(class == cl, district %in% districts, season == season) |>
    filter(date == max(date)) |>
    relocate(rank, .after = school) |>
    mutate(across(c(proj_wper, avg_proj_wper, ppg, papg), ~round(., 2))) |>
    select(school, rank, proj_wper, avg_proj_wper, wins, losses, ppg, papg, class, district) |> 
    rename(School = school, Rank = rank, `Proj. W %` = proj_wper,
           `Avg Proj. W %` = avg_proj_wper,
           W = wins, L = losses, PPG = ppg, PAPG = papg,
           Class = class, District = district
    )
  
  class_data_descriptions <- c(School = "The name of the school",
                               `Class Rank` = "The rank of the school in the class",
                               `Proj Win %` = "The current projected win percentage against all 
                         other teams in the same class",
                               `Avg Proj Win %` = "The average projected win percentage
                         against all other teams in the same class",
                               W = "The number of wins",
                               L = "The number of losses",
                               PPG = "Points Per Game",
                               PAPG = "Points Allowed Per Game",
                               Class = "Class for Women's Basketball",
                               District = "District for Women's Basketball") |> 
    unname()
  
  headers <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(lapply(colnames(class_data), function(col) {
        th(col, style = 'border: 1px solid #ddd; background-color: lightgray;
           color: black; text-align: center;')
      })
      )
    )
  ))
  
  datatable(class_data,
            class = paste0("cell-border compact"),
            container = headers,
            rownames = F,
            options = list(
              headerCallback = JS(
                sprintf(
                  "function(thead, data, start, end, display) {
              var tooltips = %s;
              $(thead).find('th').each(function(index) {
                $(this).attr('title', tooltips[index]);
              });
            }",
                  jsonlite::toJSON(unname(class_data_descriptions), auto_unbox = TRUE)
                )
              )
            ),
  )
  # }
}
