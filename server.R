##### DataFinder shiny app ###### 
# This shiny app will serve as a prototype design for an update to the
# datafinder on the IS UI. It should allow users to easily see how metadata
# filters affect composition of assay data available, and the composition of
# studies and participants available.


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
    # Update summary numbers
      lapply(unique(data[[filter]]), 
             function (x) {
               # Get selector id
               id = paste0(filter, "_", tolower(gsub("\\s|[[:punct:]]", "_", x)))
               # Get summary number
               count <- paste0(" (", nrow(rdata[rdata[[filter]] == x, .(filterClass), filterClass]), ")")
               
               shinyjs::html(selector = paste0("#", id), 
                             html = count,
                             add = FALSE)
             })
    })
      
    return(rdata)
  })
  
  # Filter type based on UI option -----
  
  # uiOption1: tabbed filters 
  # Filters 
  
  
  # Use filterData helper function
  # Helpers -----
  .createFilter <- function(id, label, rdata) {
    
    # Get choice values and names in correct order
    if (id == "age") {
      choiceNames <- paste0(c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "> 70", "Unknown"))
      choiceValues <- choiceNames
    } else if (id == "timepoint") {
      choiceNames <- paste0( c("<0", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
                               "13", "14", "15-27", "28", "29-55", "56", ">56", "Unknown", NA))
      choiceValues <- choiceNames
    } else {
      choiceNames <- unique(data[[id]])
      # order alphabetically
      choiceNames <- choiceNames[order(choiceNames, na.last = TRUE)]
      choiceValues <- choiceNames
    }
    choiceNames <- lapply(choiceNames, 
                          function(x) {
                            HTML(paste0(x, "<span id=", paste0(id, "_", tolower(gsub("\\s|[[:punct:]]", "_", x))), "> (0)</span>"))
                          })
    
    
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
                               choiceValues = choiceValues,
                               choiceNames = choiceNames
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
        div(class = "filter-dropdown",
            .createFilter("species", "Species is"),
            filterDiv("AND"),
            .createFilter("study_type", "Study type is"),
            filterDiv("AND"),
            .createFilter("condition", "Disease studied is"),
            filterDiv("AND"),
            .createFilter("exposure_material", "Vaccine studied is"),
            div()
            )
        
      )
    })
    
    output$subjectFilters <- renderUI({
      tagList(
        div(class="filter-dropdown",
            .createFilter("gender", "Gender is any of"),
            filterDiv("AND"),
            .createFilter("race","Race is any of"),
            filterDiv("AND"),
            .createFilter("age", "Age is any of"),
            div()
            )
        
      )
    })
    output$sampleFilters <- renderUI({
      anyallDropdown <- "<select><option>any of</option><option>all of</option></select>"
      tagList(
        div(class="filter-dropdown",
            .createFilter("assay", paste0(anyallDropdown, "these assays")),
            filterDiv("AND"),
            .createFilter("sample_type", paste0(anyallDropdown, "these cell types")),
            filterDiv("AT"),
            .createFilter("timepoint", paste0(anyallDropdown, "these study days")),
            div()
            )
      )
    })
    
    # Clear filter button action -------
    observeEvent(input$clear_input, {
      checkboxIds <- c("species", "study_type", "condition", "exposure_material", "gender", 
                       "race", "age", "assay", "sample_type", "timepoint")
      lapply(checkboxIds, 
            function(id) {
              updateCheckboxGroupInput(session,
                               id,
                               selected = character(0))
            })
      
    })
    
    # Filter indicators -----
    output$studyIndicators <- renderUI({
      i <- reactiveValuesToList(input)
      studyList <- list()
      for (x in c("exposure_material", "study_type", "condition", "species")) {
        if (!is.null(i[[x]])) {
          if (length(i[[x]]) > 1) {
            studyList[[x]] <- div(class = "filterindicator study",
                                  span(
                                    x, "is", paste0(i[[x]], collapse = " OR ")
                                  ))
          } else {
            studyList[[x]] <- div(class = "filterindicator study", 
                                  span(
                                    x, "is", i[[x]]
                                  ))
          }
          
        }
      }
      studyList
    })
    
    output$subjectIndicators <- renderUI({
      i <- reactiveValuesToList(input)
      participantList <- list()
      for (x in c("gender", "race", "age")) {
        if (!is.null(i[[x]])) {
          if (length(i[[x]]) > 1) {
            participantList[[x]] <- div(class = "filterindicator participant", 
                                        span(
                                          x, "is", paste0(i[[x]], collapse = " OR ")
                                        ))
          } else {
            participantList[[x]] <- div(class = "filterindicator participant", 
                                        span(
                                          x, "is", i[[x]]
                                        ))
          }
          
        }
      }
      participantList
    })
    
    output$sampleIndicators <- renderUI({
      i <- reactiveValuesToList(input)
      sampleList <- list()
      for (x in c("assay", "sample_type", "timepoint")) {
        if (!is.null(i[[x]])) {
          if (length(i[[x]]) > 1) {
            sampleList[[x]] <- div(class = "filterindicator sample", 
                                   span(
                                     x, "is", paste0(i[[x]], collapse = " OR ")
                                   ))
          } else {
            sampleList[[x]] <- div(class = "filterindicator sample", 
                                   span(
                                     x, "is", i[[x]]
                                   ))
          }
          
        }
      }
      sampleList
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
