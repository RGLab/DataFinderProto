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
  # Helpers -----
  .createFilter <- function(id, label, rdata) {
    # Get choice names with reactive summary numberss
    choiceNames <- paste0(unique(data[[id]]), " (0)")
    names(choiceNames) <- unique(data[[id]])
    choices <- sapply(unique(rdata[[id]]), function(x)paste0(x, " (", nrow(rdata[rdata[[id]] == x]), ")"), USE.NAMES = TRUE)
    if (NA %in% names(choices)) choices <- choices[-which(is.na(names(choices)))]
    choiceNames[names(choices)] <- choices
    names(choiceNames) <- NULL
    
    # Create the input
    
    checkboxGroupInput(inputId = id,
                       label = label,
                       choiceValues = unique(data[[id]]),
                       choiceNames = choiceNames,
                       selected = input[[id]])
  }
  
  # filter inputs for UI -----
  output$studyFilters <- renderUI({
    print("rendering study filters")
    rdata <- reactiveData()
    
    tagList(
      .createFilter("species", "Species", rdata),
      .createFilter("condition", "Disease", rdata),
      .createFilter("exposure_material", "Vaccine", rdata),
      .createFilter("study_type", "Study Type", rdata),
      div()
    )
  })
  
  output$subjectFilters <- renderUI({
    print("rendering subject filters")
    rdata <- reactiveData()
    tagList(
      .createFilter("gender", "Gender", rdata),
      .createFilter("race","Race", rdata),
      .createFilter("age", "Age", rdata),
      div()
    )
  })
  output$sampleFilters <- renderUI({
    print("rendering sample filters")
    rdata <- reactiveData()
    tagList(
      .createFilter("assay", "Assay", rdata),
      .createFilter("sample_type", "Cell Type", rdata),
      .createFilter("timepoint", "Study Day", rdata),
      div()
    )
  })
  
  
  # Study cards ----
  output$studyCards <- renderUI({
    studies <- unique(reactiveData()$study)
    tagList <- lapply(studies, createStudyCard, reactiveData(), output)
    tagList(tagList)
  })
  
  
  # Plots for visualization panel ----------
  # Use helper plotting functions
  output$summaryText <- renderText({
    paste0(
      "Showing data from ",
      length(unique(reactiveData()$sampleid)),
      " samples from ",
      length(unique(reactiveData()$subjectid)),
      " subjects in ",
      length(unique(reactiveData()$study)),
      " studies."
    )
  })
  
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
  
  output$ageBarplot <- renderPlot({
    ageBarplot(reactiveData())
  })
  
  output$raceBarplot <- renderPlot({
    raceBarplot(reactiveData())
  })
  
  output$assayBarplot <- renderPlot({
    assayBarplot(reactiveData())
  })
}
