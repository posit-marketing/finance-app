library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(vetiver)
tidymodels::tidymodels_prefer(quiet = TRUE)

cards <- list(
  card(full_screen = TRUE,
       card_header(HTML("<br><br>Term of loan:")),
       card_body(
         selectInput(
           inputId = "select_term",
           choices = c("36 months", "60 months"),
           selected = "36 months",
           label = HTML("<br>Select 36 or 60 months:")
         )
       )), 
  card(full_screen = TRUE,
       card_header("Proportion of monthly income represented by the installment payment:"),
       card_body(
         numericInput(
           inputId = "input_installment_pct_inc",
           label = HTML("Input a number between<br>0 and 1:"),
           value = 0.07,
           min = 0,
           max = 1,
           step = 0.01
         )
       )),
  card(full_screen = TRUE,
       card_header(HTML("<br>Open to buy on revolving bankcards:")),
       card_body(
         numericInput(
           inputId = "input_bc_open_to_buy",
           label = HTML("Input a number between<br>0 and 500,000:"),
           value = 14467,
           min = 0,
           max = 500000,
           step = 1000
         )
       )),
  card(full_screen = TRUE,
       card_header(HTML("<br><br>Installment payment:")),
       card_body(
         numericInput(
           inputId = "input_installment",
           label = HTML("Input a number between<br>30 and 2000:"),
           value = 463,
           min = 30,
           max = 2000,
           step = 100
         )
       )),
  card(full_screen = TRUE,
       card_header(HTML("<br><br>Loan to income ratio:")),
       card_body(
         numericInput(
           inputId = "input_loan_to_income",
           label = HTML("Input a number between<br>0 and 2:"),
           value = 0.21,
           min = 0,
           max = 2,
           step = 0.1
         )
       ))
)

vbs <- list(
  value_box(
    title = "Predicted interest rate",
    value = textOutput("pred_int"),
    style = "background-color: #082D46!important; color: #FFFFFF!important",
    showcase = bsicons::bs_icon("bank", size = "0.75em"),
    showcase_layout = "top right",
    full_screen = FALSE,
    fill = TRUE,
    height = NULL
  )
)

foot <-
  tags$div(
    style = "background-color: #FFFFFF; padding: 0px; text-align: center; bottom: 0; width: 100%;",
    HTML(
      "Powered by <a href='https://posit.co'><img src='https://www.rstudio.com/assets/img/posit-logo-fullcolor-TM.svg' alt='Posit Logo' style='width:55px;'></a> | Integrated with <a href='https://www.databricks.com'><img src='https://cdn.cookielaw.org/logos/29b588c5-ce77-40e2-8f89-41c4fa03c155/bc546ffe-d1b7-43af-9c0b-9fcf4b9f6e58/1e538bec-8640-4ae9-a0ca-44240b0c1a20/databricks-logo.png' alt='Databricks Logo' style='width:85px;'></a>. For more details, see our <a href='https://posit.co/blog/databricks-and-posit-announce-new-integrations/' target='_blank'>blog post</a> announcing the partnership."
    )
  )

ui <- bslib::page(
  title = "Interest rate prediction app",
  layout_columns(width = 1/5,
                 height = 200,
                 cards[[2]], cards[[3]], cards[[4]], cards[[5]], cards[[1]]),
  layout_columns(vbs[[1]]),
  card_footer(foot)
)

server <- function(input, output, session) {
  
  board <- pins::board_connect(server = Sys.getenv("CONNECT_SERVER"),
                               key = Sys.getenv("CONNECT_API_KEY"))
  
  predictions_df <- reactive({
    
    req(input$select_term, input$input_installment_pct_inc, input$input_bc_open_to_buy, input$input_installment, input$input_loan_to_income)
    
    pred_tibble <-
      tibble(
        term = input$select_term,
        installment_pct_inc = input$input_installment_pct_inc,
        bc_open_to_buy = input$input_bc_open_to_buy,
        installment = input$input_installment,
        loan_to_income = input$input_loan_to_income
      )
    
    url <-
      "https://pub.palm.ptd.posit.it/content/50a4870a-92da-41ec-81cc-8240ae70c9f6/predict"
    
    endpoint <- vetiver_endpoint(url)
    
    apiKey = Sys.getenv("CONNECT_API_KEY")
    
    predictions <-
      predict(endpoint,
              pred_tibble,
              httr::add_headers(Authorization = paste("Key", apiKey)))
    
  })
  
  output$pred_int <- renderText({
    predictions_df()$.pred
  })
  
}

shinyApp(ui, server)