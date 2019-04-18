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
  titlePanel("Data Finder (v1.4.3)"),
  
  # Main -------------------------------
  sidebarLayout(
    
    # LHS (filters) --------------------
    sidebarPanel(
      
      tags$button(class = "btn btn-default", 
                  "Save Participant Group"),
      div(style="margin-top:10px;",
        span("Filters", style="font-size:2.5em"),
        span(style="float:right;margin-top:10px;",
             actionButton("clear_input", "Clear All"))
        
      ),
      div(style="margin-bottom:10px;margin-top:10px;",
          span("Include data from... ")
          ),
      div(
        tags$input(type = "text", placeholder = "Search...", name = "search")
      ),
      # Build query
      div(
        .filterSelector("Studies where", "study"),
        uiOutput("studyIndicators"),
        .filterSelector("Participants where", "subject"),
        uiOutput("subjectIndicators"),
        .filterSelector("Participants with data from", "sample"),
        uiOutput("sampleIndicators")
      )
        
        

    ),
    
    # RHS (visualizations) -------------
    mainPanel(
      tabsetPanel(
        tabPanel("Participants",
                 textOutput("summaryText"),
                 div(plotOutput("genderBarplot", height = "100%"), class = "barplot"),
                 div(plotOutput("ageBarplot", height = "100%"), class = "barplot"),
                 div(plotOutput("raceBarplot", height = "100%"), class = "barplot"),
                 div(plotOutput("timepointHeatmap_study",  height = "200px")),
                 div(plotOutput("timepointHeatmap_sample", height = "200px")),
                 # div(plotOutput("upsetPlot", height = "300px")),
                 div()
                 ),
        tabPanel("Studies",
                 div(
                   style="float:right;",
                   p("Number of Samples"),
                   div( style = "",
                        studyCardLegend
                   )
                 ),
                 p(textOutput("studyCount", inline = TRUE), "studies"),
                 p("Timepoint-assay plots show which timepoints have assay data, where color ",
                   "corresponds to number of samples."),
                 uiOutput("studyCards")
        )
      )
      
      
    )
    
  )
)
