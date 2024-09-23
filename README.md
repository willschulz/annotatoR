# Text Annotation Shiny App

This Shiny application facilitates the manual annotation of texts stored in CSV files for purposes such as content analysis and training text classifiers. Users can load texts, annotate them, and save their annotations directly within the app.

## Features

- Load texts from CSV files in a designated directory.
- Manual annotation interface with support for categorical responses.
- Save annotations back to the CSV.

## Installation

Clone this repository and run the app locally using RStudio, or deploy it to a Shiny server.

```bash
git clone https://github.com/yourusername/yourrepositoryname.git
```


## Usage
Place your CSV files in the csvs folder.
Open the app in RStudio or on your Shiny Server.

### Keyboard Shortcuts
The app supports the following keyboard shortcuts for efficient annotation:

- K: Mark the current text as 'Yes'.
- D: Mark the current text as 'No'.
- O: Select 'Other' for the current text.
- F: Flag the current text for review.
- Right Arrow: Move to the next text.
- Left Arrow: Return to the previous text.

## Dependencies
- R
- Shiny
- readr
- dplyr
- fs
- shinyjs
- shinyWidgets

Make sure to install these packages from CRAN before running the app.
