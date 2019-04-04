##### DataFinder shiny app ###### 
# This shiny app will serve as a prototype design for an update to the
# datafinder on the IS UI. It should allow users to easily see how metadata
# filters affect composition of assay data available, and the composition of
# studies and participants available.

library(shiny)
library(bsplus)
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
    print("filtering data")
    rdata <- filterData(
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

    # Update filter text
    mapply(c(
      "species",
      "condition",
      "exposure_material",
      "study_type",
      "gender",
      "race",
      "age",
      "assay",
      "sample_type",
      "timepoint"
    ),
    FUN = function(filter) {
      print(paste0("updating ", filter))
      # Get choice names with reactive summary numberss
      choiceNames <- paste0(unique(data[[filter]]), " (0)")
      names(choiceNames) <- unique(data[[filter]])
      choices <- sapply(unique(rdata[[filter]]), function(x)paste0(x, " (", nrow(rdata[rdata[[filter]] == x]), ")"), USE.NAMES = TRUE)
      if (NA %in% names(choices)) choices <- choices[-which(is.na(names(choices)))]
      choiceNames[names(choices)] <- choices
      names(choiceNames) <- NULL
      # Replace checkbox groups
      removeUI(paste0("#",filter,"_checkboxgroup"), immediate = TRUE)
      insertUI(paste0("#",filter,"_collapse"),
               "beforeEnd",
               div(id = paste0(filter,"_checkboxgroup"),
                   checkboxGroupInput(inputId = filter,
                                      label = NULL,
                                      choiceValues = unique(data[[filter]]),
                                      choiceNames = choiceNames,
                                      selected = input[[filter]])),
               immediate = TRUE)

    })
    return(rdata)
  })
  
  # Reactive filtered dataframe
  # Use filterData helper function
  # Helpers -----
  .createFilterButton <- function(id, label) {
    
  }
  .createFilter <- function(id, label, rdata) {

    return(
      tagList(
        bs_attach_collapse(
          bs_button(paste0("+ ", label),
                    button_type = "default",
                    style = "display:block;width:100%;text-align:left;"),
          id_collapse = paste0(id, "_collapse")
        ),
        bs_collapse(
          id = paste0(id, "_collapse"),
          content = div(
            id = paste0(id, "_checkboxgroup"),
            checkboxGroupInput(inputId = id,
                               label = NULL,
                               choiceValues = unique(data[[id]]),
                               choiceNames = unique(data[[id]])
            )
          )
        )
      )
    )
    
    
  }
  
  # filter inputs for UI -----
  output$studyFilters <- renderUI({
    tagList(
      HTML("<div>Only include studies with</div>"),
      .createFilter("species", "These species"),
      HTML("<div style='text-align:center;'>AND</div>"),
      .createFilter("study_type", "These data types"),
      HTML("<div style='text-align:center;'>Studying</div>"),
      .createFilter("condition", "These diseases"),
      HTML("<div style = 'text-align:center;'>AND</div>"),
      .createFilter("exposure_material", "These vaccines"),
      div()
    )
  })
  
  output$subjectFilters <- renderUI({
    tagList(
      .createFilter("gender", "Gender"),
      .createFilter("race","Race"),
      .createFilter("age", "Age"),
      div()
    )
  })
  output$sampleFilters <- renderUI({
    tagList(
      .createFilter("assay", "Assay"),
      .createFilter("sample_type", "Cell Type"),
      .createFilter("timepoint", "Study Day"),
      div()
    )
  })
  
  
  
  # 
  # # Study cards ----
  # output$studyCards <- renderUI({
  #   studies <- unique(reactiveData()$study)
  #   tagList <- lapply(studies, createStudyCard, reactiveData(), output)
  #   tagList(tagList)
  # })
  # 
  
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
