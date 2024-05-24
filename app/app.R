library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)
library(vetiver)
tidymodels::tidymodels_prefer(quiet = TRUE)

# Connect to deployed model to make predictions
server <- 
  Sys.getenv("CONNECT_SERVER")

api_key <- 
  Sys.getenv("CONNECT_API_KEY")

board <- 
  pins::board_connect(
    server = Sys.getenv("CONNECT_SERVER"),
    key = Sys.getenv("CONNECT_API_KEY")
  )

endpoint <- 
  vetiver_endpoint(
    "https://pub.demo.posit.team/public/lending-club-model-vetiver-api/predict"
  )

source("authenticate.R")
source("helpers.R")

rates <-
  tbl(con, dbplyr::in_catalog("hive_metastore", "default", "lendingclub")) |>
  mutate(int_rate = as.numeric(REPLACE(int_rate, "%", "")))

ranges <- 
  rates |> 
  summarize(min = min(int_rate, na.rm = TRUE), 
            max = max(int_rate, na.rm = TRUE)) |> 
  collect()

cards <- list(
  card(full_screen = TRUE,
       card_header(HTML("Term of loan")),
       card_body(
         selectInput(
           inputId = "term",
           choices = list("36 months" = 36, "60 months" = 60),
           selected = 36,
           label = "How soon would you like to pay off the loan?"
         )
       )), 
  card(full_screen = TRUE,
       card_header("Credit Utilization"),
       card_body(
         numericInput(
           inputId = "all_util",
           label = "What is the ratio of your current credit balances to your total credit limits for all sources?",
           value = 0,
           min = 0,
           max = 1,
           step = 0.05
         )
       )),
  card(full_screen = TRUE,
       card_header("Bank Card Utilization"),
       card_body(
         numericInput(
           inputId = "bc_util",
           label = "What is the ratio of your current credit balances to your total credit limits for only bank card sources?",
           value = 0,
           min = 0,
           max = 1,
           step = 0.05
         )
       )),
  card(full_screen = TRUE,
       card_header("Available Bank Card Credit"),
       card_body(
         numericInput(
           inputId = "bc_open_to_buy",
           label = "What is your current available credit across all bank cards?",
           value = 14467,
           min = 0,
           max = 500000,
           step = 1000
         )
       )),
  card(full_screen = TRUE,
       card_header("Heavily Withdrawn Bank Cards"),
       card_body(
         numericInput(
           inputId = "percent_bc_gt_75",
           label = "What percent of your bank cards currently have a balance that is greater than 75% of their limit?",
           value = 0,
           min = 0,
           max = 100,
           step = 1
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

plot <-
  card(full_screen = TRUE,
       card_header(HTML("Comparison to interest rates from the most recent 30 days")),
       card_body(
         plotOutput("plot")
       ))

ui <- bslib::page(
  title = "Predicted Interest Rate Calculator",
  layout_columns(
    layout_columns(cards[[1]], cards[[2]], cards[[3]], cards[[4]], cards[[5]],
                   width = 1/5,
                   height = 275),
    layout_columns(vbs[[1]], plot,
                   height = 350,
                   col_widths = c(3, 9)),
    col_widths = c(12, 12)
  ),
  card_footer(foot)
)

server <- function(input, output, session) {
  
  predictions_df <- reactive({
    
    req(input$term,
        input$all_util,
        input$bc_util,
        input$bc_open_to_buy,
        input$percent_bc_gt_75)
    
    pred_tibble <-
      tibble(term = input$term,
             all_util = input$all_util,
             bc_util = input$bc_util,
             bc_open_to_buy = input$bc_open_to_buy,
             percent_bc_gt_75 = input$percent_bc_gt_75)
    
    predict(endpoint, pred_tibble)
    
  })
  
  output$pred_int <- renderText({
    predictions_df()$.pred
  })
  
  rate_range <- reactive({
    rates |> 
      find_100_most_similar(input$term, 
                            input$all_util, 
                            input$bc_util, 
                            input$bc_open_to_buy, 
                            input$percent_bc_gt_75)
  })

  output$plot <-
    renderPlot({
      rate_range() |> 
        ggplot(aes(xmin = min_rate, xmax = max_rate, ymin = 1, ymax = 1.5)) +
        geom_rect(fill = "steelblue") +
        ylim(c(0, 2.5)) +
        xlim(c(ranges$min, ranges$max)) +
        theme_minimal()
    })
  
}

shinyApp(ui, server)