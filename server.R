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
        study = input$study,
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
      ),
      list(
        assay = input$assay_operator,
        sample_type = input$sample_type_operator,
        timepoint = input$timepoint_operator
      )
    )

    # Update filter text ----
    mapply(
      filter = c("study", "species", "condition","exposure_material","study_type","gender","race","age","assay","sample_type","timepoint"),
      filterClass = c(rep("study", 5), rep("subjectid", 3), rep("sampleid", 3)),
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
  
  # Load/save buttons
  observeEvent(input$load, {
    showModal(modalDialog(
      title = "Load",
      "placeholder",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$save, {
    showModal(modalDialog(
      title = "Save",
      "placehoder",
      easyClose = TRUE
    ))
  })
  
  # filter inputs for UI -----
  
    output$studyFilters <- renderUI({
      tagList(
        div(.createFilter("study", "Study ID", data),
            .createFilter("species", "Species", data),
            # filterDiv("AND"),
            .createFilter("study_type", "Study type", data),
            # filterDiv("AND"),
            .createFilter("condition", "Disease studied", data),
            # filterDiv("AND"),
            .createFilter("exposure_material", "Vaccine studied", data),
            div()
            )
        
      )
    })
    
    output$subjectFilters <- renderUI({
      tagList(
        div(class="filter-dropdown",
            .createFilter("gender", "Gender", data),
            # filterDiv("AND"),
            .createFilter("race","Race", data),
            # filterDiv("AND"),
            .createFilter("age", "Age", data),
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
            tags$p(tags$em("Use the Assay heatmap in the \"Find\" tab for advanced filtering options.")),
            .createFilter("assay", span(anyallDropdown("assay_operator"), "these assays"), data),
            # filterDiv("AND"),
            .createFilter("sample_type", span(anyallDropdown("sample_type_operator"), "these cell types"), data),
            # filterDiv("AT"),
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
    output$studyIndicators <- createFilterIndicators(session,
                                                     input,
                                                     options = c("study", "exposure_material", "study_type", "condition", "species"), 
                                                     class = "study")
    # # Listeners
    # onclick("species_indicator",
    #         updateCheckboxGroupInput(session, "species", selected = character(0)))
    # onclick("condition_indicator",
    #         updateCheckboxGroupInput(session, "condition", selected = character(0)))
    # onclick("study_type_indicator",
    #         updateCheckboxGroupInput(session, "study_type", selected = character(0)))
    # onclick("exposure_material_indicator",
    #         updateCheckboxGroupInput(session, "exposure_material", selected = character(0)))
    
    output$subjectIndicators <- createFilterIndicators(session,
                                                       input,
                                                       options = c("gender", "race", "age"),
                                                       class = "participant")
    
      
    
    output$sampleIndicators <- createFilterIndicators(session,
                                                      input,
                                                      options = c("assay", "sample_type", "timepoint"),
                                                      class = "sample")
    
    # Listeners
    lapply(c("study", "species", "condition","exposure_material","study_type","gender","race","age","assay","sample_type","timepoint"),
          function(filter){
            onclick(paste0(filter, "_deletor"), 
                    updateCheckboxGroupInput(session, filter, selected = character(0)))
          })
      
  
  
  # Study cards ----
  output$studyCards <- renderUI({
    studies <- unique(reactiveData()$study)
    # Sort studies by number
    studies <- paste0("SDY", sort(as.numeric(gsub("SDY", "", studies))))
    tagList <- lapply(studies, createStudyCard, data = reactiveData(), origData = data, output = output, studyTitle = "Apoptosis and other immune biomarkers predict influenza vac...")
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
  output$studyHeatmapInfo <- renderText ({
    paste0("x=", input$studyHeatmapClick$x, "\ny=", input$studyHeatmapClick$y)
  })
  # div(plotOutput("timepointHeatmap_study",  height = "200px", click = "studyHeatmapClick")),
  # verbatimTextOutput("studyHeatmapInfo"),
  
  output$upsetPlot <- renderPlot({
    upsetPlot(reactiveData())
  })
  
  output$studyTypePlot <- renderPlot({
    studyTypePlot(reactiveData())
  })
  
  output$diseaseStudiedPlot <- renderPlot({
    diseaseStudiedPlot(reactiveData())
  })
  
  output$speciesPlot <- renderPlot({
    speciesPlot(reactiveData())
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
  
  output$interactiveHeatmap <- renderPlotly({
    d <- formatHeatmapData(reactiveData())
    plotlyHeatmap <- custom_timepointHeatmap(d, text = paste0("Number of Participants: ", count))
      htmlwidgets::onRender(plotlyHeatmap,
                            "//javascript
    // when hovering over an element, give it a thick, white border
    function(el, x) {
    el.on('plotly_click', function(d) {
    console.log(d.points[0]);
    });
    }


                            "
      )
  })
  
  output$selection <- renderPrint({
    s <- event_data("plotly_click")

    
    if (length(s) == 0) {
      "Click on a cell in the heatmap to display data"
    } else {
      tp <- gsub(" Days", "", s$x)
      as <- s$y
      rdata <- reactiveData()
      d <- formatHeatmapData(rdata)
      d <- d[timepoint == tp & assay == as]
      cat("You selected:\n\n")
      cat(paste0(d$count, " participants and ", length(d$studyList[[1]]), " studies:\n"))
      as.list(d$studyList[[1]])
    }
  })
}
