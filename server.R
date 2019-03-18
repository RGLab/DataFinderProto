##### DataFinder shiny app ###### 
# This shiny app will serve as a prototype design for an update to the
# datafinder on the IS UI. It should allow users to easily see how metadata
# filters affect composition of assay data available, and the composition of
# studies and participants available.

library(shiny)
library(ImmuneSpaceR)
library(Rlabkey)
library(data.table)

# Source helper files
helperFiles <- list.files("helpers")
for (file in helperFiles) {
  source(file.path("helpers", file))
} 


# Get the data
data <- getData()


function(input, output, session) {
  # Reactive filtered dataframe
  # Use filterData helper function
  
  # Plots for visualization panel
  # Use helper plotting functions
  output$textOutput <- renderText(
    paste0("species=", paste(input$species, collapse = ","), ";\n", 
           "gender=", paste(input$gender, collapse = ","), ";\n",
           "assay=", paste(input$assay, collapse = ","))
  )
}

