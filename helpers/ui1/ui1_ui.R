# Tabbed Filter selectors --------
ui1 <- function() {
  tabsetPanel(
    
    # Study ------------------------
    tabPanel("Study",
             #inputs
             div(
               # style = "background:#9E9AC8;",
               uiOutput("studyFilters")
             )
    ),
    
    # Subject ----------------------
    tabPanel("Participant",
             # inputs
             div(
               # style = "background:#74C476;",
               uiOutput("subjectFilters")
             )
    ),
    
    # Sample -----------------------
    tabPanel("Sample",
             # inputs
             div(
               # style = "background:#FB6A4A;",
               uiOutput("sampleFilters")
             )
    )
  )
}