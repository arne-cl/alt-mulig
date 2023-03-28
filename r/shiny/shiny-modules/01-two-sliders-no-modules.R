library(shiny)

ui <- function() {
  fluidPage(
    sliderInput(
      inputId = "choice1",
      label = "choice 1",
      min = 1, max = 10, value = 5
    ),
    
    actionButton(
      inputId = "validate1",
      label = "Validate choice 1"
    ),

    sliderInput(
      inputId = "choice2",
      label = "choice 2",
      min = 1, max = 10, value = 5
    ),
    
    actionButton(
      inputId = "validate2",
      label = "Validate choice 2"
    )

  ) # END fluidPage()
} # END ui()

server <- function(input, output, session) {
  observeEvent(input$validate1, {
    print(input$choice1)
  })

  observeEvent(input$validate2, {
    print(input$choice2)
  })
} # END server()


shinyApp(ui = ui, server = server)
