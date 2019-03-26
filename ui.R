##### DataFinder shiny app ###### 
# This shiny app will serve as a prototype design for an update to the
# datafinder on the IS UI. It should allow users to easily see how metadata
# filters affect composition of assay data available, and the composition of
# studies and participants available.


# Define UI 
fluidPage(
  # title ------------------------------
  titlePanel("Data Finder"),
  
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
                 textOutput("dim"),
                 textOutput("participants"),
                 textOutput("Studies"),
                 textOutput("Samples"),
                 textOutput("species"),
                 textOutput("gender"),
                 textOutput("assay"),
                 plotOutput("timepointHeatmap_study"),
                 plotOutput("timepointHeatmap_sample"),
                 plotOutput("upsetPlot"),
                 plotOutput("studyTypePlot"),
                 plotOutput("genderBarplot"),
                 plotOutput("assayBarplot")
                 )
      )
      
      
    )
    
  )
)
