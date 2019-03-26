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
                 checkboxGroupInput("species", "species", choices = c("Homo sapiens", "Mus musculus"),
                                    selected = c("Homo sapiens", "Mus musculus"))
        ),
        
        # Subject ----------------------
        tabPanel("Subject",
                 checkboxGroupInput("gender", "gender", choices = c("Male", "Female"), 
                                    selected = c("Male", "Female"))
                 # inputs
        ),
        
        # Sample -----------------------
        tabPanel("Sample",
                 # inputs
                 checkboxGroupInput("assay", "assay", choices = c("ELISA", "Flow Cytometry", "CyTOF", "HAI", "Gene Expression"),
                                    selected = c("ELISA", "Flow Cytometry", "CyTOF", "HAI", "Gene Expression"))
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
