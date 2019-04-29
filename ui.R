##### DataFinder shiny app ###### 
# This shiny app will serve as a prototype design for an update to the
# datafinder on the IS UI. It should allow users to easily see how metadata
# filters affect composition of assay data available, and the composition of
# studies and participants available.

# NOTE:  Shiny uses bootstrap v3 https://getbootstrap.com/docs/3.3/components/

library(shinythemes)

# Define UI 
fluidPage(
  shinyjs::useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # Hack to prevent dropdowns from collapsing when you click on it
    tags$script(HTML("$(document).on('click', '.dropdown-menu', function (e) {
  e.stopPropagation();
});"))
  ),
  
  # title ------------------------------
  titlePanel("Data Finder (v2.1)"),
  
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
       
        tabPanel("d3",
                 d3Output(outputId = "d3Heatmap"),
                 verbatimTextOutput("d3Selection"))
      )
      
      
    )
    
  )
)
