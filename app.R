# Load libraries and shared resources
source("global.R")

# Source UI and Server
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui, server)

