# install.packages("remotes")
# remotes::install_github("rstudio/bslib") # for version 0.7.1
# remotes::install_github("rstudio/shiny") # for version 1.8.1.9001
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

max_range <- 
  rates |> 
  pull(int_rate) |> 
  max(na.rm = TRUE)

cards <- list(
  card(
    card_body(
      selectInput(
        inputId = "term",
        choices = list("36 months" = 36, "60 months" = 60),
        selected = 36,
        label = tooltip(
          trigger = list("Term of loan", bs_icon("info-circle")),
          "How soon would you like to pay off the loan?"
        )
      )
    )
  ),
  
  card(
    card_body(
      numericInput(
        inputId = "all_util",
        value = 0,
        min = 0,
        max = 1,
        step = 0.05,
        label = tooltip(
          trigger = list("Credit Utilization", bs_icon("info-circle")),
          "What is the ratio of your current credit balances to your total credit limits for all sources?"
        )
      )
    )
  ),
  
  card(
    card_body(
      numericInput(
        inputId = "bc_util",
        value = 0,
        min = 0,
        max = 1,
        step = 0.05,
        label = tooltip(
          trigger = list("Bank Card Utilization", bs_icon("info-circle")),
          "What is the ratio of your current credit balances to your total credit limits for only bank card sources?"
        )
      )
    )
  ),
  
  card(
    card_body(
      numericInput(
        inputId = "bc_open_to_buy",
        value = 14467,
        min = 0,
        max = 500000,
        step = 1000,
        label = tooltip(
          trigger = list("Available Bank Card Credit", bs_icon("info-circle")),
          "What is your current available credit across all bank cards?"
        )
      )
    )
  ),
  
  card(
    card_body(
      numericInput(
        inputId = "percent_bc_gt_75",
        value = 0,
        min = 0,
        max = 100,
        step = 1,
        label = tooltip(
          trigger = list("Heavily Withdrawn Cards", bs_icon("info-circle")),
          "What percent of your bank cards currently have a balance that is greater than 75% of their limit?"
        )
      )
    )
  )
)

vbs <-
  value_box(
    title = "Predicted interest rate",
    value = textOutput("pred_int"),
    style = "background-color: #082D46!important; color: #FFFFFF!important",
    showcase = bsicons::bs_icon("bank", fill = '#4682b4 !important;'),
    showcase_layout = "top right"
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
       card_header(HTML("Applicants like you have received interest rates in this range")),
       card_body(
         plotOutput("plot")
       ))

ui <- bslib::page_navbar(
  title = "Predicted Interest Rate Calculator",
  underline = FALSE,
  theme = bs_theme(bootswatch = "flatly", success = "#4682b4"),
  bg = "#082D46",
  nav_panel(
    title = " ",
    layout_columns( 
      card(
        helpText("Please fill out the fields below. Then click Predict Rate."),
        layout_columns(cards[[1]], cards[[2]], cards[[3]], cards[[4]], cards[[5]],
                       width = 1/5,
                       height = 170),
        actionButton("predict", "Predict Rate", width = 200, 
                     style="color: #fff; background-color: #4682b4;")
      ),
      layout_columns(vbs, plot,
                     height = 500,
                     col_widths = c(3, 9)),
      col_widths = c(12, 12)
    ),
    card_footer(foot)
  )
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
    
  }) |> 
    bindEvent(input$predict)
  
  predicted_rate <-
    reactive(predictions_df()$.pred)
  
  output$pred_int <- renderText({
    predicted_rate()
  })
  
  rate_range <- reactive({
    rates |> 
      find_range_for_similar_applicants(input$term, 
                                        input$all_util, 
                                        input$bc_util, 
                                        input$bc_open_to_buy, 
                                        input$percent_bc_gt_75)
  })  |> 
  bindEvent(input$predict)

  output$plot <-
    renderPlot({
      min_rate <- rate_range() |> pull(min_rate)
      max_rate <- rate_range() |> pull(max_rate)
      
      rate_range() |> 
        ggplot(aes(xmin = min_rate, xmax = max_rate, ymin = 1, ymax = 1.5)) +
        geom_rect(fill = "#4682b4") +
        geom_segment(
          aes(x = predicted_rate(), xend = predicted_rate(), y = 0.9, yend = 1.6), 
          color = "#082D46",
          linewidth = 2) +
        ylim(c(0, 2.5)) +
        theme_minimal() +
        labs(y = "", x = "Interest Rate (%)") +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) +
        scale_x_continuous(limits = c(0, max_range),
                           breaks = seq(from = 0, to = max_range, by = 5)) +
        annotate("text", x = min_rate, y = 1.25, label = paste0(min_rate, " "), 
                 hjust = "right", color = "#4682b4", size = 7) +
        annotate("text", x = max_rate,  y = 1.25, label = paste0(" ", max_rate), 
                 hjust = "left", color = "#4682b4", size = 7)
    })
  
}

shinyApp(ui, server)