##### DataFinder shiny app ###### 
# This shiny app will serve as a prototype design for an update to the
# datafinder on the IS UI. It should allow users to easily see how metadata
# filters affect composition of assay data available, and the composition of
# studies and participants available.


# Define UI 
fluidPage(
  # title
  titlePanel("Data Finder"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Selector panel
      tabsetPanel(
        tabPanel("Study",
                 #inputs
                 
                 checkboxGroupInput("species", "species", choices = c("a", "b", "c")
                 )
        ),
        tabPanel("Subject",
                 checkboxGroupInput("gender", "gender", choices = c("a", "b", "c"))
                 # inputs
        ),
        tabPanel("Sample",
                 # inputs
                 checkboxGroupInput("assay", "assay", choices = c("a", "b", "c"))
        )
      )
    ),
    
    # visualization panel
    mainPanel(
      textOutput("textOutput")
    )
    
  )
)
