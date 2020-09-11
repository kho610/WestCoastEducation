# Packages
library("shiny")

# source() other files (app_ui.R and app_server.R)
source("app_ui.R")
source("app_server.R")

# Create a`shinyApp()
shinyApp(ui = ui, server = server)

# Link to Shiny app
# https://marshel4.shinyapps.io/project-mmarshel4/
