##### DataFinder shiny app ###### 
# This shiny app will serve as a prototype design for an update to the
# datafinder on the IS UI. It should allow users to easily see how metadata
# filters affect composition of assay data available, and the composition of
# studies and participants available.


# Define UI 
fluidPage(
  shinyjs::useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML("$(document).on('click', '.dropdown-menu', function (e) {
  e.stopPropagation();
});"))
  ),
  
  # title ------------------------------
  titlePanel("Data Finder (v1.4.0)"),
  
  # Main -------------------------------
  sidebarLayout(
    
    # LHS (filters) --------------------
    sidebarPanel(
      h2("Filters" ,
         div(style="position:absolute;right:10%;top:35px",
             actionButton("clear_input", "Clear All"))),
      p("Include data from... "),
      
      # Build query
      div(
        filterButton("Studies where", "study"),
        uiOutput("studyIndicators"),
        filterButton("Participants where", "subject"),
        uiOutput("subjectIndicators"),
        filterButton("Participants with data from", "sample"),
        uiOutput("sampleIndicators")
      )
        
        

    ),
    
    # RHS (visualizations) -------------
    mainPanel(
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
