library(shiny)

ui <- fluidPage("Hello")

server <- function(input, output, session) {}

shinyApp(
  ui, server,
  options = list(host = "0.0.0.0", port = as.numeric(Sys.getenv("PORT", 3838)))
)
