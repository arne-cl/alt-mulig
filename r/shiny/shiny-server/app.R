# source: https://datawookie.dev/blog/2022/09/using-shiny-server-in-docker/

library(shiny)

ui <- fluidPage(
  titlePanel("Example Shiny Application")
)

server <- function(input, output) {
}

shinyApp(ui = ui, server = server)

