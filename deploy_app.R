basic_file_path <- gsub("/", "\\\\", file.path(Sys.getenv("USERPROFILE"), "OneDrive",
                                               "Documents", "mshsaa_girls_rankings"))
setwd(basic_file_path)
# setwd("C:\\Users\\mac\\OneDrive\\Documents\\Sports Stats Analytics\\MSHSAA Boys Basketball\\mshsaa_app")
appfiles <- c("app.R", "Components", "Functions", "www",
              "Data/modeling.parquet", "Data/base.parquet",
              "Data/upcoming.parquet", "Data/drankings.parquet" #,
              # "Models/class_glm_model.rds", "Models/lm_opp_reg.rds",
              # "Models/lm_reg.rds"
              )
rsconnect::deployApp(appFiles = appfiles, launch.browser = FALSE)

