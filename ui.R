##### DataFinder shiny app ###### 
# This shiny app will serve as a prototype design for an update to the
# datafinder on the IS UI. It should allow users to easily see how metadata
# filters affect composition of assay data available, and the composition of
# studies and participants available.


# Define UI 
fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # title ------------------------------
  titlePanel("Data Finder (0.1.0)"),
  
  # Main -------------------------------
  sidebarLayout(
    
    # LHS (filters) --------------------
    sidebarPanel(
      
      # Tabbed Filter selectors --------
      tabsetPanel(
        
        # Study ------------------------
        tabPanel("Study",
                 #inputs
                 uiOutput("studyFilters")
        ),
        
        # Subject ----------------------
        tabPanel("Subject",
                 # inputs
                 uiOutput("subjectFilters")
        ),
        
        # Sample -----------------------
        tabPanel("Sample",
                 # inputs
                 uiOutput("sampleFilters")
        )
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
