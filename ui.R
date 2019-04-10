##### DataFinder shiny app ###### 
# This shiny app will serve as a prototype design for an update to the
# datafinder on the IS UI. It should allow users to easily see how metadata
# filters affect composition of assay data available, and the composition of
# studies and participants available.


# Define UI 
fluidPage(
  shinyjs::useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # title ------------------------------
  titlePanel("Data Finder (v1.3.2)"),
  
  # Main -------------------------------
  sidebarLayout(
    
    # LHS (filters) --------------------
    sidebarPanel(
      
      # Choose ui based on ui options
      if (uiOption == 1) {
        source("helpers/ui1/ui1_ui.R")
        ui1()
      } else if (uiOption == 2) {
        tagList(
          h2("Filters"),
          p("Include data from..."),
          div(class = "dropdown",
              div(class = "btn-group", role = "group", style = "width:100%",
                  tags$button("Studies where", class = "btn btn-default", style="width:90%"),
                  tags$button(style="text-align:left;", 
                              HTML("&#9654;"), 
                              class = "btn btn-default dropdown-toggle", 
                              type = "button", 
                              "data-toggle"="dropdown", 
                              style = "width:10%"),
              div( class="dropdown-menu", 
                   style = "align:relative;left:100%;top:-5px",
                     # style = "background:#9E9AC8;",
                     uiOutput("studyFilters")
                   )
          ))

        )
        

      }
      
    ),
    
    # RHS (visualizations) -------------
    mainPanel(
      uiOutput("filterList"),
      tabsetPanel(
        tabPanel("Studies",
                 uiOutput("studyCards")
                 ),
        tabPanel("Plots",
                 textOutput("summaryText"),
                 div(plotOutput("genderBarplot", height = "100%"), class = "barplot"),
                 div(plotOutput("ageBarplot", height = "100%"), class = "barplot"),
                 div(plotOutput("raceBarplot", height = "100%"), class = "barplot"),
                 div(plotOutput("timepointHeatmap_study",  height = "200px")),
                 div(plotOutput("timepointHeatmap_sample", height = "200px")),
                 div(plotOutput("upsetPlot", height = "300px")),
                 div()
                 )
      )
      
      
    )
    
  )
)
