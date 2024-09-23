library(shiny)
library(readr)
library(dplyr)
library(fs)
library(shinyjs)
library(shinyWidgets)

#for icons
library(fontawesome)

csv_meets_condition <- function(file_path) {
  data <- read_csv(file_path)
  !("annotation_response" %in% colnames(data)) || any((data$annotation_response)==-1)
}

csv_files <- list.files("csvs", pattern = "\\.csv$", full.names = TRUE) %>%
  purrr::keep(csv_meets_condition) %>%
  .[order(as.numeric(gsub("_.*","",gsub(".*/", "", .))))]

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body {
        background-color: #dbdbdb;
        color: #453700;
        text-align: center;
      }
      #snippet {
        background-color: #f5f5f5;
        border-radius: 15px;
        border-color: #2e2e2e;
        box-shadow: 0 0 5px #4d4d4d;
        padding: 10px;
        font-size: 1.5em;
        min-height: 200px;
      }
      #snippet.flagged {
        background-color: #fcba03;
      }
      #displayText {
        color: #4d4d4d;
        text-align: left;
      }
      .mainpanel-container {
        display: flex;
        flex-direction: column;
        height: 100vh;
      }
      .mainpanel {
        flex-grow: 1;
        margin: auto;
        width: 100%;
      }
      #yesButton, #noButton, #otherButton, #flagButton {
        font-size: 2em;
        min-width: 175px;
      }
      #yesButton {
        background-color: #008000;  /* Green color */
        color: white;
      }
      #yesButton.clicked {
        box-shadow: 0 0 0 3px #dbdbdb, 0 0 0 6px #008000;
      }
      #noButton {
        background-color: #B22222;  /* FireBrick color */
        color: white;
      }
      #noButton.clicked {
        box-shadow: 0 0 0 3px #dbdbdb, 0 0 0 6px #B22222;
      }
      #otherButton {
        background-color: #828282;
        color: white;
      }
      #otherButton.clicked {
        box-shadow: 0 0 0 3px #dbdbdb, 0 0 0 6px #828282;
      }
      #flagButton {
        background-color: #fcba03;
        color: white;
        font-size: 1.5em;
        min-width: 100px;
      }

      #flagButton.flagged {
        box-shadow: 0 0 0 3px #dbdbdb, 0 0 0 6px #fcba03;
      }
      .progress-bar {
        background-color: #453700;
      }
      #progress_group {
        margin-top: auto;
      }
    "))
  ),
  #titlePanel(htmlOutput("displayInstruction")),
  div(class = "mainpanel-container",
      mainPanel(class = "mainpanel",
                h2(htmlOutput("displayInstruction")),
                div(id="snippet", (htmlOutput("displayText"))),
                br(),
                actionButton("yesButton", "Support", icon("check-circle"), width = "25%"),
                actionButton("noButton", "Oppose", icon = icon("times-circle"), width = "25%"),
                br(),
                actionButton("otherButton", "Other", icon = icon("question-circle"), width = "25%"),
                br()
      ),
      div(id = "progress_group",
          div(style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
              actionButton("backButton", "<"),
              actionButton("flagButton", "Flag", icon = icon("flag"), width = "10%"),
              actionButton("nextButton", ">")
          ),
          progressBar(id = "progress", display_pct = TRUE, value = 0, total = 100),
          p(textOutput("currentCSV")),
      )
  )
)


server <- function(input, output, session) {
  # Function to read the selected CSV file
  read_selected_csv <- function(filepath) {
    data_file <- read_csv(filepath)
    if (!"annotation_response" %in% colnames(data_file)) {
      data_file <- data_file %>% mutate(annotation_response = -1)
    }
    return(data_file)
  }
  
  values <- reactiveValues(index = min(which((read_selected_csv(csv_files[1])$annotation_response)==-1)), currentCSV = 1, data = read_selected_csv(csv_files[1]))
  
  output$currentCSV <- renderText({
    paste("Current File:", basename(csv_files[values$currentCSV]))
  })
  
  output$displayText <- renderText({
    req(values$data)
    return(values$data$annotation_html[values$index])
  })
  
  output$displayInstruction <- renderText({
    req(values$data)
    return(values$data$annotation_instruction[values$index])
  })
  
  observeEvent(input$yesButton, {
    req(values$data)
    if (values$index <= nrow(values$data)) {
      values$data[values$index, 'annotation_response'] <- 1
      values$index <- values$index + 1
      write_csv(values$data, csv_files[values$currentCSV])
    }
    if (values$index > nrow(values$data) && values$currentCSV < length(csv_files)) {
      values$currentCSV <- values$currentCSV + 1
      values$data <- read_selected_csv(csv_files[values$currentCSV])
      values$index <- min(which(is.na(values$data$annotation_response)))
      updateProgressBar(session = session, id = "progress", value = 0) # Reset progress bar
    }
  })
  
  observeEvent(input$noButton, {
    req(values$data)
    if (values$index <= nrow(values$data)) {
      values$data[values$index, 'annotation_response'] <- 0
      values$index <- values$index + 1
      write_csv(values$data, csv_files[values$currentCSV])
    }
    if (values$index > nrow(values$data) && values$currentCSV < length(csv_files)) {
      values$currentCSV <- values$currentCSV + 1
      values$data <- read_selected_csv(csv_files[values$currentCSV])
      values$index <- min(which(is.na(values$data$annotation_response)))
      updateProgressBar(session = session, id = "progress", value = 0) # Reset progress bar
    }
  })
  
  observeEvent(input$otherButton, {
    req(values$data)
    if (values$index <= nrow(values$data)) {
      values$data[values$index, 'annotation_response'] <- 99
      values$index <- values$index + 1
      write_csv(values$data, csv_files[values$currentCSV])
    }
    if (values$index > nrow(values$data) && values$currentCSV < length(csv_files)) {
      values$currentCSV <- values$currentCSV + 1
      values$data <- read_selected_csv(csv_files[values$currentCSV])
      values$index <- min(which(is.na(values$data$annotation_response)))
      updateProgressBar(session = session, id = "progress", value = 0) # Reset progress bar
    }
  })
  
  observeEvent(input$flagButton, {
    req(values$data)
    if (values$index <= nrow(values$data)) {
      if(values$data[values$index, 'annotation_flagged'] == 1) {
        values$data[values$index, 'annotation_flagged'] <- 0
      } else {
        values$data[values$index, 'annotation_flagged'] <- 1
      }
      write_csv(values$data, csv_files[values$currentCSV])
    }
  })
  
  observeEvent(input$nextButton, {
    req(values$data)
    if (values$index <= nrow(values$data)) {
      values$index <- values$index + 1
    }
    if (values$index > nrow(values$data) && values$currentCSV < length(csv_files)) {
      values$currentCSV <- values$currentCSV + 1
      values$data <- read_selected_csv(csv_files[values$currentCSV])
      values$index <- min(which(is.na(values$data$annotation_response)))
      updateProgressBar(session = session, id = "progress", value = 0) # Reset progress bar
    }
  })
  
  observeEvent(input$backButton, {
    req(values$data)
    if (values$index > 1) {
      values$index <- values$index - 1
    }
  })
  
  #update progress bar and styles
  observeEvent({
    list(
      values$index,
      input$flagButton
    )
  }, {
    message(values$data[values$index, 'annotation_flagged'] == 1)
    progress = updateProgressBar(session = session, id = "progress", value = values$index, total = nrow(values$data))
    if (values$data[values$index, 'annotation_flagged'] == 1) {
      shinyjs::addClass(id = "flagButton", class = "flagged")
      shinyjs::addClass(id = "snippet", class = "flagged")
    } else {
      shinyjs::removeClass(id = "flagButton", class = "flagged")
      shinyjs::removeClass(id = "snippet", class = "flagged")
    }
    if (values$data[values$index, 'annotation_response'] == 1) {
      shinyjs::addClass(id = "yesButton", class = "clicked")
    } else {
      shinyjs::removeClass(id = "yesButton", class = "clicked")
    }
    if (values$data[values$index, 'annotation_response'] == 0) {
      shinyjs::addClass(id = "noButton", class = "clicked")
    } else {
      shinyjs::removeClass(id = "noButton", class = "clicked")
    }
    if (values$data[values$index, 'annotation_response'] == 99) {
      shinyjs::addClass(id = "otherButton", class = "clicked")
    } else {
      shinyjs::removeClass(id = "otherButton", class = "clicked")
    }
  })
  
  runjs("
    $(document).keyup(function(e) {
      if(e.key === 'k') {
        $('#yesButton').click();
      } else if(e.key === 'd') {
        $('#noButton').click();
      } else if(e.key === 'o') {
        $('#otherButton').click();
      } else if(e.key === 'f') {
        $('#flagButton').click();
      } else if(e.key === 'ArrowRight') {
        $('#nextButton').click();
      } else if(e.key === 'ArrowLeft') {
        $('#backButton').click();
      }
    });
  ")
}


shinyApp(ui = ui, server = server)
