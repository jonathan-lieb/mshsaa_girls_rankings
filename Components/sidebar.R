dashboardSidebar(
  tags$head(tags$style(HTML('.logo {
                              background-color: #747474 !important;
                              }
                              .navbar {
                              background-color: #8F0000 !important;
                              }
                              '))),
  sidebarMenu(menuItem("Home", tabName = "home", icon = icon("house-fire")),
              menuItem("Rankings", tabName = "rankings", icon = icon("trophy")),
              menuItem("Sims", tabName = "sims", icon = icon("flask")),
              menuItem("Team Page", tabName = "team", icon = icon("basketball")),
              menuItem("About", tabName = "about", icon = icon("person")))
)

