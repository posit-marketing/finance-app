library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)

cards <- list(card(
  card_header("Predicted interest rate",),
  textOutput(outputId = "predictionOutput")
))

foot <-
  tags$div(
    style = "background-color: #FFFFFF; padding: 0px; text-align: center; bottom: 0; width: 100%;",
    HTML(
      "Powered by <a href='https://posit.co'><img src='https://www.rstudio.com/assets/img/posit-logo-fullcolor-TM.svg' alt='Posit Logo' style='width:55px;'></a> | Integrated with <a href='https://www.databricks.com'><img src='https://cdn.cookielaw.org/logos/29b588c5-ce77-40e2-8f89-41c4fa03c155/bc546ffe-d1b7-43af-9c0b-9fcf4b9f6e58/1e538bec-8640-4ae9-a0ca-44240b0c1a20/databricks-logo.png' alt='Databricks Logo' style='width:85px;'></a>. For more details, see our <a href='https://posit.co/blog/databricks-and-posit-announce-new-integrations/' target='_blank'>blog post</a> announcing the partnership."
    )
  )

ui <- page_sidebar(
  title = "Interest rate prediction app",
  sidebar = sidebar(
    width = 275,
    p("Welcome!"),
    p("Predict interest rates based on loan applicant criteria."),
    selectInput(
      inputId = "select_term",
      label = "Select term of loan:",
      choices = c("36 months", "60 months"),
      selected = "All"
    ),
    numericInput(
      inputId = "input_installment_pct_inc",
      label = "Input percentage of monthly income the installment payment represents:",
      value = 0.07,
      min = 0,
      max = 1
    ),
    numericInput(
      inputId = "input_bc_open_to_buy",
      label = "Select open to buy on revolving bankcards:",
      value = 14467,
      min = 0,
      max = 500000
    ),
    numericInput(
      inputId = "input_installment",
      label = "Input installment payment:",
      value = 463,
      min = 30,
      max = 2000
    ),
    numericInput(
      inputId = "input_loan_to_income",
      label = "Input loan to income ratio:",
      value = 0.21,
      min = 0,
      max = 2
    )
  ),
  layout_columns(cards[[1]]),
  card_footer(foot)
)

server <- function(input, output, session) {
  
  board <- pins::board_rsconnect(server = Sys.getenv("CONNECT_SERVER"),
                                 key = Sys.getenv("CONNECT_API_KEY"),)
  
  predictions_df <- reactive({
    
    pred_tibble <-
      tibble(
        term = input$input_term,
        installment_pct_inc = input$input_installment_pct_inc,
        bc_open_to_buy = input$input_bc_open_to_buy,
        installment = input$input_installment,
        loan_to_income = input$loan_to_income
      )

    url <-
      "https://pub.palm.ptd.posit.it/content/50a4870a-92da-41ec-81cc-8240ae70c9f6/predict"
    
    endpoint <- vetiver_endpoint(url)
    
    predictions <-
      predict(
        endpoint,
        pred_tibble
        )
  
  })
  
}

shinyApp(ui, server)
