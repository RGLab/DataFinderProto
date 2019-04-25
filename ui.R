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
  titlePanel("Data Finder (v2.2)"),
  
  # Main -------------------------------
  sidebarLayout(
    
    # LHS (filters) --------------------
    sidebarPanel(
      
      span("Participant Group", style = "font-size:1.5em"),
      actionButton("load", "Load", class = "btn btn-default", style = "float:right;"),
      actionButton("save", "Save", class = "btn btn-default", style = "float:right;"),
      tags$hr(),
      div(style="margin-top:10px;",
        span("Filters", style="font-size:1.5em"),
        span(style="float:right;margin-top:10px;",
             actionButton("clear_input", "Clear All"))
        
      ),
      div(style="margin-bottom:10px;margin-top:10px;",
          span("Include data from... ")
          ),
      div(
        tags$input(type = "text", placeholder = "Search... (placeholder)", name = "search")
      ),
      # Build query
      div(
        .filterSelector("Study Design", "study"),
        uiOutput("studyIndicators"),
        .filterSelector("Participant Characteristics", "subject"),
        uiOutput("subjectIndicators"),
        .filterSelector("Data Available", "sample"),
        uiOutput("sampleIndicators")
      ),
      tags$hr(),
      h2("Summary"),
      textOutput("summaryText")
        
        

    ),
    
    # RHS (visualizations) -------------
    mainPanel(
      tabsetPanel(
        tabPanel("Find",
                 
                 # Study Characteristics
                 h2("Select Study Characteristics"),
                 p("Study characteristics available based on current filters", tags$br(),
                   tags$em("Click on a barplot to see details or add a filter")),
                 div(plotOutput("speciesPlot", height = "100%"), class = "barplot"),
                 div(plotOutput("studyTypePlot", height = "100%"), class = "barplot"),
                 div(plotOutput("diseaseStudiedPlot", height = "100%"), class = "barplot"),
                 
                 # Participant Characteristics
                 h2("Select Participant Characteristics"),
                 p("Participant data available based on current filters", tags$br(),
                   tags$em("Click on a barplot to see details or add a filter")),
                 div(plotOutput("genderBarplot", height = "100%"), class = "barplot"),
                 div(plotOutput("ageBarplot", height = "100%"), class = "barplot"),
                 div(plotOutput("raceBarplot", height = "100%"), class = "barplot"),
                 
                 # Assay data
                 h2("Select Assay Data"),
                 p("Assay Data available by timepoint based on current filters", tags$br(),
                   tags$em("Click on a grid box to see details or add that assay-timepoint combination to filters filer")),
                 div(plotlyOutput(outputId = "interactiveHeatmap", height = "250px")),
                 
                 div()
                 ),
        
        
        tabPanel("Studies",
                 
                 # Study cards
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
                 uiOutput("studyCards"),
                 
                 div()
                 ),
        tabPanel("View",
                 h1("Clinical and Assay Data"),
                 p("placeholder")),
        tabPanel("Visualize",
                 h1("Data Explorer"),
                 p("placeholder")),
        tabPanel("Cluster",
                 h1("Dimension Reduction"),
                 p("placeholder")),
        tabPanel("Analyze",
                 h1("GE Anaylsis modules"),
                 p("placeholder")),
        tabPanel("Resources", 
                 h1("Links to tutorials etc"),
                 p("placeholder"))
        
        
      )
      
      
    )
    
  )
)
