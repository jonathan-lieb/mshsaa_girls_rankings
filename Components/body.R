dashboardBody(
  tabItems(
    source("Components/home_tab.R", local = TRUE)$value,
    source("Components/rankings_tab.R", local = TRUE)$value,
    source("Components/sims_tab.R", local = TRUE)$value,
    source("Components/team_tab.R", local = TRUE)$value,
    source("Components/about_tab.R", local = TRUE)$value
  )
)