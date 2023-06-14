library(shiny)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Bank loan"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for Immpobilienpreis
      sliderInput(inputId = "Preis", label = "Immpobilienpreis:",
                  min = 200000, max = 600000, value = 300000, step = 10000),
      
      # Input: Slider for Eigenkapital
      sliderInput(inputId = "Eigen", label = "Eigenkapital:",
                  min = 50000, max = 200000, value = 100000, step = 5000),
      
      # Input: Slider for Kreditdauer
      sliderInput(inputId = "n", label = "Kreditdauer (n):",
                  min = 8, max = 30, value = 20, step = 1),

      # Input: Slider for monatliche Tilgung
      sliderInput(inputId = "tilgung_monat", label = "Monatliche Tilgung:",
                  min = 900, max = 2000, value = 1100, step = 100),
      
      # Input: Slider for Sondertigung (jährlich)
      sliderInput(inputId = "tilgung_sonder", label = "Sonder-Tilgung:",
                  min = 0, max = 10000, value = 5000, step = 1000),
      
      # Input: Numeric for Zinssatz
      numericInput(inputId = "p", label = "Zinssatz (in %):", value = 0.77, step = 0.01)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table
      tabsetPanel(type = "tabs", 
                  tabPanel("Berechne Kreditsumme", 
                           tableOutput(outputId = "res_get_K")),
                  tabPanel("Berechne Kreditdauer", 
                           tableOutput(outputId = "res_get_n")),
                  tabPanel("Berechne Jährliche Annuität", 
                           tableOutput(outputId = "res_get_A")))
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  get_A <- function(q, n, K){
    A <- ( K * (q^n) * (q-1) ) / ( (q^n) - 1 )
    return(A)}
  
  get_n <- function(A, q, K){
    n <- (log(A/(-K*q + K + A))) / log(q)
    return(n)}
  
  get_K <- function(A, q, n){
    K <- (A * (q^n) - A) / ( (-q^n)  + (q^(n+1)) )
    return(K)}
  
  vals <- reactiveValues()
  observe({
    vals$gesamtkosten <- input$Preis + 0.15* input$Preis
    vals$kreditsumme <- vals$gesamtkosten - input$Eigen
    vals$tilgung <- (12*input$tilgung_monat) + input$tilgung_sonder
    vals$q <- 1 + input$p/100
  })
  
  results_table_K <- reactive({
    
    data.frame(Name = c("Immobilienpreis",
                        "Gesamtkosten (inkl. Gebühren)",
                        "Eigenkapital",
                        "Darlehensbetrag / Kreditsumme (K)",
                        "Berechnete Kreditsumme (K in Abh. von n, A)"),
               Value = c(round(input$Preis, 2),
                         round(vals$gesamtkosten, 2),
                         round(input$Eigen, 2),
                         round(vals$kreditsumme, 2),
                         round(get_K(A = vals$tilgung, q = vals$q, n = input$n), 2)))
  })
  
  results_table_n <- reactive({
    
    data.frame(Name = c("Immobilienpreis",
                        "Gesamtkosten (inkl. Gebühren)",
                        "Eigenkapital",
                        "Darlehensbetrag / Kreditsumme (K)",
                        "Berechnete Kreditdauer (n in Abh. von K, A)"),
               Value = c(round(input$Preis, 2),
                         round(vals$gesamtkosten, 2),
                         round(input$Eigen, 2),
                         round(vals$kreditsumme, 2),
                         round(get_n(A = vals$tilgung, q = vals$q, K = vals$kreditsumme), 2)))
  })
  
  results_table_A <- reactive({
    
    data.frame(Name = c("Immobilienpreis",
                        "Gesamtkosten (inkl. Gebühren)",
                        "Eigenkapital",
                        "Darlehensbetrag / Kreditsumme (K)",
                        "Berechnete Jährliche Annuität (A in Abh. von n, K)"),
               Value = c(round(input$Preis, 2),
                         round(vals$gesamtkosten, 2),
                         round(input$Eigen, 2),
                         round(vals$kreditsumme, 2),
                         round(get_A(q = vals$q, n = input$n, K = vals$kreditsumme), 2)))
  })
  
  output$res_get_K <- renderTable({ results_table_K() })
  output$res_get_n <- renderTable({ results_table_n() })
  output$res_get_A <- renderTable({ results_table_A() })
  
}

shinyApp(ui = ui, server = server)
