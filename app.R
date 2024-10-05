# app.R
#source("data.R")


# Source the necessary R scripts
source("global.R")
source("ui.R")
source("server.R")

# Run the Shiny application
shinyApp(ui = ui, server = server)
