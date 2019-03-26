##### DataFinder shiny app ###### 
# This shiny app will serve as a prototype design for an update to the
# datafinder on the IS UI. It should allow users to easily see how metadata
# filters affect composition of assay data available, and the composition of
# studies and participants available.

library(shiny)
library(ImmuneSpaceR)
library(Rlabkey)
library(ggplot2)
library(data.table)
library(UpSetR)

# Source helper files
helperFiles <- list.files("helpers")
for (file in helperFiles) {
  source(file.path("helpers", file))
} 


# Get the data
# For testing use local data
data <- get(load("data/cube.RData"))
# data <- getData()


function(input, output, session) {
  
  # filter inputs for UI -----
  output$studyFilters <- renderUI({
    tagList(
      checkboxGroupInput(inputId = "species", 
                         label = "Species", 
                         choices = unique(data$species)),
      checkboxGroupInput(inputId = "condition", 
                         label = "Disease",
                         choices = unique(data$condition)),
      checkboxGroupInput(inputId = "exposure_material",
                         label = "Vaccine",
                         choices = unique(data$exposure_material)),
      checkboxGroupInput(inputId = "study_type",
                         label = "Study Type",
                         choices = unique(data$study_type)),
      div()
    )
  })
  output$subjectFilters <- renderUI({
    tagList(
      checkboxGroupInput(inputId = "gender",
                         label = "Gender",
                         choices = unique(data$gender)),
      checkboxGroupInput(inputId = "race",
                         label = "Race",
                         choices = unique(data$race)),
      checkboxGroupInput(inputId = "age",
                         label = "Age",
                         choices = unique(data$age)),
      div()
    )
  })
  output$sampleFilters <- renderUI({
    tagList(
      checkboxGroupInput(inputId = "assay",
                         label = "Assay",
                         choices = unique(data$assay)),
      checkboxGroupInput(inputId = "sample_type",
                         label = "Cell Type",
                         choices = unique(data$sample_type)),
      checkboxGroupInput("timepoint",
                         label = "Day of Study",
                         choices = unique(data$timepoint)),
      div()
    )
  })
  
  
  # Reactives ---------------------------
    # Filtered dataframe
  # NOTE:  If nothing is checked for checkboxGroupInput, 
  # the input value is NULL. Otherwise, it is a character vector. 
  reactiveData <- reactive({
    filterData(
      data,
      list(
        species = input$species,
        condition = input$condition,
        exposure_material = input$exposure_material,
        study_type = input$study_type,
        gender = input$gender,
        race = input$race,
        age = input$age,
        assay = input$assay,
        sample_type = input$sample_type,
        timepoint = input$timepoint
      )
    )
  })
  # Reactive filtered dataframe
  # Use filterData helper function
  
  
  # Study cards ----
  output$studyCards <- renderUI({
    studies <- unique(reactiveData()$study)
    tagList <- lapply(studies, createStudyCard, reactiveData(), output)
    tagList(tagList)
  })
  
  
  # Plots for visualization panel ----------
  # Use helper plotting functions
  output$dim <- renderText({
    paste(
      "dim(data) = ", paste0(dim(reactiveData()), collapse = ","))
    })
  
  output$species <- renderText({
    paste(
      "unique(data$species) = ", paste0(unique(reactiveData()$species), collapse = ",")
    )
  }) 
  
  output$gender <- renderText({
    paste(
      "unique(data$gender) = ", paste0(unique(reactiveData()$gender), collapse = ",")
    )
  })
  
  output$assay <- renderText({
    paste(
      "unique(data$assay) = ", paste0(unique(reactiveData()$assay), collapse = ",")
    )
  })
    
  output$participants <- renderText({
    paste(
      length(unique(reactiveData()$subjectid)),
      " subjects"
    )
  })
  
  output$Studies <- renderText({
    paste(
      length(unique(reactiveData()$study)),
      " studies"
    )
  })
  
  output$Samples <- renderText({
    paste(
      length(unique(reactiveData()$sampleid)),
      " samples"
    )
  })
  
  output$timepointHeatmap_sample <- renderPlot({
    timepointHeatmap_sample(reactiveData())
  })
  
  output$timepointHeatmap_study <- renderPlot({
    timepointHeatmap_study(reactiveData())
  })
  
  output$upsetPlot <- renderPlot({
    upsetPlot(reactiveData())
  })
  
  output$studyTypePlot <- renderPlot({
    studyTypePlot(reactiveData())
  })
  
  output$genderBarplot <- renderPlot({
    genderBarplot(reactiveData())
  })
  
  output$assayBarplot <- renderPlot({
    assayBarplot(reactiveData())
  })
}

