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

    # Update filter text ----
    mapply(
      filter = c("species", "condition","exposure_material","study_type","gender","race","age","assay","sample_type","timepoint"),
      filterClass = c(rep("study", 4), rep("subjectid", 3), rep("sampleid", 3)),
    FUN = function(filter, filterClass) {
      # Get choice names with reactive summary numbers
      # Make sure age and timepoint are in the correct order
      if (filter == "age") {
        choiceNames <- paste0(c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "> 70", "Unknown"), "(0)")
        names(choiceNames) <- c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "> 70", "Unknown")
        choiceValues <- names(choiceNames)
      } else if (filter == "timepoint") {
        choiceNames <- paste0( c("<0", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
                                 "13", "14", "15-27", "28", "29-55", "56", ">56", "Unknown", NA),
                               "(0)")
        names(choiceNames) <- c("<0", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
                               "13", "14", "15-27", "28", "29-55", "56", ">56", "Unknown", NA)
        choiceValues <- names(choiceNames)
      } else {
        choiceNames <- paste0(unique(data[[filter]]), " (0)")
        names(choiceNames) <- unique(data[[filter]])
        # order alphabetically
        choiceNames <- choiceNames[order(names(choiceNames), na.last = TRUE)]
        choiceValues <- names(choiceNames)
      }
      choices <- sapply(unique(rdata[[filter]]), function(x)paste0(x, " (", nrow(rdata[rdata[[filter]] == x, .(filterClass), filterClass]), ")"), USE.NAMES = TRUE)
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
                                      choiceValues = choiceValues,
                                      choiceNames = choiceNames,
                                      selected = input[[filter]])),
               immediate = TRUE)

    })
    return(rdata)
  })
  
  # Use filterData helper function
  # Helpers -----
  .createFilter <- function(id, label, rdata) {

    return(
      tagList(
        HTML(
          paste0(
            '<button class="btn btn-default filterbutton" data-toggle="collapse" data-target=#', id, '_collapse>', 
            label,
            '</button>'
          )
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
  filterDiv <- function(...){
    div(..., class = "filtertext")
  }
  
  # filter inputs for UI -----
  output$studyFilters <- renderUI({
    tagList(
      div("Only include studies with"),
      .createFilter("species", "These species"),
      filterDiv("AND"),
      .createFilter("study_type", "These data types"),
      filterDiv("AND"),
      .createFilter("condition", "These diseases"),
      filterDiv("AND"),
      .createFilter("exposure_material", "These vaccines"),
      div()
    )
  })
  
  output$subjectFilters <- renderUI({
    tagList(
      div("Only include participants where"),
      .createFilter("gender", "Gender is any of"),
      filterDiv("AND"),
      .createFilter("race","Race is any of"),
      filterDiv("AND"),
      .createFilter("age", "Age is any of"),
      div()
    )
  })
  output$sampleFilters <- renderUI({
    anyallDropdown <- "<select><option>any of</option><option>all of</option></select>"
    tagList(
      div("Only include participants with"),
      .createFilter("assay", paste0(anyallDropdown, "these assays")),
      filterDiv("AND"),
      .createFilter("sample_type", paste0(anyallDropdown, "these cell types")),
      filterDiv("AT"),
      .createFilter("timepoint", paste0(anyallDropdown, "these study days")),
      div()
    )
  })
  
  
  

  # Study cards ----
  output$studyCards <- renderUI({
    studies <- unique(reactiveData()$study)
    # Sort studies by number
    studies <- paste0("SDY", sort(as.numeric(gsub("SDY", "", studies))))
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
