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
  
  
  # filter inputs for UI -----
  
    output$studyFilters <- renderUI({
      tagList(
        div(
            .createFilter("species", "Species is", data),
            filterDiv("AND"),
            .createFilter("study_type", "Study type is", data),
            filterDiv("AND"),
            .createFilter("condition", "Disease studied is", data),
            filterDiv("AND"),
            .createFilter("exposure_material", "Vaccine studied is", data),
            div()
            )
        
      )
    })
    
    output$subjectFilters <- renderUI({
      tagList(
        div(class="filter-dropdown",
            .createFilter("gender", "Gender is any of", data),
            filterDiv("AND"),
            .createFilter("race","Race is any of", data),
            filterDiv("AND"),
            .createFilter("age", "Age is any of", data),
            div()
            )
        
      )
    })
    output$sampleFilters <- renderUI({
      anyallDropdown <- function(id) {
        span(class = "form-group shiny-input-container", style = "width:6em;",
             tags$select(id = id,
                    tags$option(value = "OR", "any of"),
                    tags$option(value = "AND", "all of")))
      }
      tagList(
        div(class="filter-dropdown",
            .createFilter("assay", span(anyallDropdown("assay_operator"), "these assays"), data),
            filterDiv("AND"),
            .createFilter("sample_type", span(anyallDropdown("sample_type_operator"), "these cell types"), data),
            filterDiv("AT"),
            .createFilter("timepoint", span(anyallDropdown("timepoint_operator"), "these study days"), data),
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
    
    # Clear individual filter action -----
    
    
    # Filter indicators -----
    output$studyIndicators <- createSampleIndicators(session,
                                                     input,
                                                     options = c("exposure_material", "study_type", "condition", "species"), 
                                                     class = "study btn")
      
    
    output$subjectIndicators <- createSampleIndicators(session,
                                                       input,
                                                       options = c("gender", "race", "age"),
                                                       class = "participant btn")
      
    
    output$sampleIndicators <- createSampleIndicators(session,
                                                      input,
                                                      options = c("assay", "sample_type", "timepoint"),
                                                      class = "sample")
    
      
  
  
  # Study cards ----
  output$studyCards <- renderUI({
    studies <- unique(reactiveData()$study)
    # Sort studies by number
    studies <- paste0("SDY", sort(as.numeric(gsub("SDY", "", studies))))
    tagList <- lapply(studies, createStudyCard, reactiveData(), output)
    tagList(tagList)
  })
    # output$studyCardLegend <- renderUI({
    #   studyCardLegend(output)
    # })

  
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
    
    output$studyCount <- renderText(length(unique(reactiveData()$study)))
  
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
