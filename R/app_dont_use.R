library(shiny)
library(tidyverse)
library(scales)
library(glue)
library(reactable)
library(reactablefmtr)
library(colorspace)
# Data prep
d <- read_rds("../data/milwaukee_schools_2023.rda")
d <- d |> 
  arrange(school_name)

clean <- d |> 
  select(-c(school_year, dpi_true_id))

choices <- d |> 
  select(school_name) |> 
  pull() |> 
  unique()


# formatting helpers
rating_color <- function(r) {
  switch(r,
         "Significantly Exceeds Expectations" = "#63BE7B",
         "Exceeds Expectations" = "#97CE80",
         "Meets Expectations" = "#CDDD82",
         "Meets Few Expectations" = "#FEEB84",
         "Fails to Meet Expectations" = "#FBBD7B")
}

rating_column <- function(minWidth = 100, class = NULL, ...) {
  colDef(
    minWidth = minWidth,
    class = class,
    style = function(value) {
      list(background =rating_color(value))
    },
    ...
  )
}

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
          a#outputFile {
            background-color: #E57200;
            border-color: #E57200;
            color: #eef7ef;
            margin: 10pt;
          }
          a#outputFile:hover {
            background-color: #E5720095;
            border-color: #E5720095;
          }
          body {
            color: #003349;
            font-family: Arial;
          }
                    ")
    )
  ),
  fluidRow(
    column(5, offset = 0,
           selectizeInput(
             'school', 
             options = list(placeholder = 'Select one or more Milwaukee schools',
                            selected = ""),
             label = "School(s)", width = "100%",
             choices = choices,
             multiple = TRUE
           )
    ),
    column(1, offest = 0, uiOutput("download"))
  ),
  fluidRow(
    column(12, offset = 0,
           reactableOutput("table", width = "100%", height = "100%")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # TABLE ----------
  
  output$table <- renderReactable({
    
    if (is.null(input$school)) {
      c <- clean
    } else {
      c <- clean |> 
        filter(school_name %in% input$school)
    }
    
    c |> 
      reactable(columns = list(
        school_name = colDef(name = "School Name", 
                             minWidth = 200),
        accurate_agency_type = colDef(name = "Sector",
                                      minWidth = 150),
        grade_band = colDef(name = "Grade Band"),
        school_enrollment = colDef(name = "School Enrollment"),
        overall_rating = rating_column(name = "Overall Rating"),
        overall_score = colDef(name = "Overall Score", align = "center")
      ),
      pagination = FALSE
      )
    
    # t |> 
    #   mutate(district = factor(district, levels = c(choices, "Wisconsin"))) |> 
    #   arrange(district) |> 
    #   rename(DISTRICT = district) |> 
    #   reactable(columns = list(
    #     DISTRICT = colDef(minWidth = 50, align = "center"),
    #     AMOUNT = colDef(format = colFormat(prefix = "$", separators = TRUE, digits = 2),
    #                     minWidth = 75, align = "center"),
    #     `WISCONSIN ESSER CATEGORY` = colDef(minWidth = 125, 
    #                                         align = "center"),
    #     `IRG ESSER CATEGORY` = colDef(minWidth = 100, align = "center"),
    #     OBJECT = colDef(minWidth = 45, align = "center"),
    #     FUNCTION = colDef(minWidth = 55, align = "center"),
    #     LABEL = colDef(minWidth = 100, align = "center"),
    #     ITEM = colDef(minWidth = 100, align = "center"),
    #     DETAIL = colDef(minWidth = 75, align = "center")
    #   ),
    #   highlight = TRUE, searchable = FALSE, compact = TRUE, filterable = TRUE,
    #   style = list(fontSize = "1rem"), 
    #   defaultPageSize = 50, 
    #   paginationType = "jump")
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)