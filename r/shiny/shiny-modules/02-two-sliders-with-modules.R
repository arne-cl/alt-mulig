library(shiny)

mod_choice_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(
      inputId = ns("choice"),
      label = "Choice",
      min = 1, max = 10, value = 5
    ),
    actionButton(
      inputId = ns("validate"),
      label = "Validate"
    )
  ) # END tagList()
}

mod_choice_server <- function(id) {
  # moduleServer() handles NS for input/output vars
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$validate, {
        print(input$choice)
      })
    }
  )
}

app_ui <- function() {
  fluidPage(
    mod_choice_ui(id = "choice_1"),
    mod_choice_ui(id = "choice_2")
  )
}

app_server <- function(input, output, session) {
  mod_choice_server(id = "choice_1")
  mod_choice_server(id = "choice_2")
}

shinyApp(ui = app_ui, server = app_server)
