library(shiny)

ui <- fluidPage(
  numericInput('var', 'Sample Size', value = 25)
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)