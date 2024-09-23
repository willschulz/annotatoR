library(shiny)
library(readr)
library(dplyr)
library(fs)
library(shinyjs)
library(shinyWidgets)
library(bslib)
library(shinycssloaders)
library(fontawesome)

csv_meets_condition <- function(file_path) {
  data <- read_csv(file_path)
  !("response" %in% colnames(data)) || any(is.na(data$response))
}

csv_files <- list.files("csvs", pattern = "\\.csv$", full.names = TRUE) %>%
  purrr::keep(csv_meets_condition) %>%
  .[order(as.numeric(gsub("_.*","",gsub(".*/", "", .))))]

ui <- fluidPage(
  theme = bs_theme(bootswatch = "cerulean"),  # Apply a modern Bootstrap theme
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f8f9fa;
        color: #343a40;
        text-align: center;
      }
      .well {
        background-color: #ffffff;
        border-radius: 10px;
        border-color: #ced4da;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        padding: 20px;
      }
      #displayText {
        color: #495057;
        text-align: left;
      }
      .mainpanel {
        margin: auto;
        width: 80%;
      }
      .btn-custom {
        padding: 10px 20px;
        font-size: 16px;
        border-radius: 5px;
      }
      #yesButton {
        background-color: #28a745;
        color: white;
      }
      #noButton {
        background-color: #dc3545;
        color: white;
      }
      #otherButton {
        background-color: #6c757d;
        color: white;
      }
    "))
  ),
  titlePanel("Instruction"),
  mainPanel(class = "mainpanel",
            h3(textOutput("currentCSV")),
            progressBar(id = "progress", display_pct = TRUE, value = 0, total = 100) %>% withSpinner(),
            br(),
            actionButton("noButton", "No", icon = icon("times-circle"), class = "btn-custom"),
            actionButton("yesButton", "Yes", icon = icon("check-circle"), class = "btn-custom"),
            br(),
            actionButton("otherButton", "Other", icon = icon("question-circle"), class = "btn-custom"),
            br(),
            actionButton("backButton", icon("arrow-left"), class = "btn-custom"),
            actionButton("nextButton", icon("arrow-right"), class = "btn-custom"),
            br(),
            br(),
            div(class="well", (htmlOutput("displayText")))
  )
)

server <- function(input, output, session) {
  # Function to read the selected CSV file
  read_selected_csv <- function(filepath) {
    data_file <- read_csv(filepath)
    if (!"response" %in% colnames(data_file)) {
      stop("CSV does not contain 'response' column.")
    }
    data_file
  }
  
  # (Your existing server logic here)
}

shinyApp(ui = ui, server = server)
