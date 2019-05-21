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
        timepoint = input$timepoint
      ),
      heatmapSelection$appliedFilters
      
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
  
  heatmapSelection <- reactiveValues(
    data = list(),
    selectedIds = list(),
    participants = character(0),
    selectionText = "",
    studies = character(0),
    appliedFilters = list()
  )
  
  
  # Buttons for all filters
  
  ## Clear all ##
  observeEvent(input$clear_input, {
    checkboxIds <- c("species", "study_type", "condition", "exposure_material", "gender", 
                     "race", "age", "assay", "sample_type", "timepoint")
    lapply(checkboxIds, 
           function(id) {
             updateCheckboxGroupInput(session,
                                      id,
                                      selected = character(0))
           })
    heatmapSelection$appliedFilters <- list()
    
  })
  
  ## Load/save buttons ##
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
  
  # Study Filter Tab -----
  
  ## Outputs ##
  
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
  
  output$studyIndicators <- createFilterIndicators(session,
                                                   input,
                                                   options = c("study", "exposure_material", "study_type", "condition", "species"), 
                                                   class = "study")
  
  output$studyTypePlot <- renderPlot({
    studyTypePlot(reactiveData())
  })
  
  output$diseaseStudiedPlot <- renderPlot({
    diseaseStudiedPlot(reactiveData())
  })
  
  output$speciesPlot <- renderPlot({
    speciesPlot(reactiveData())
  })
  
  # Participant Filter Tab -----
  
  ## Outputs ##
  
  
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
  
  output$subjectIndicators <- createFilterIndicators(session,
                                                     input,
                                                     options = c("gender", "race", "age"),
                                                     class = "participant")
  
  output$genderBarplot <- renderPlot({
    genderBarplot(reactiveData())
  })
  
  output$ageBarplot <- renderPlot({
    ageBarplot(reactiveData())
  })
  
  output$raceBarplot <- renderPlot({
    raceBarplot(reactiveData())
  })
  
  # Sample Filter Tab ----- 
  
  ## outputs ##
  
  output$interactiveHeatmap <- renderD3({
    d3Heatmap(reactiveData(),
              heatmapSelection$selectedIds)
  })
  
  output$selectedText <- renderText({
    if (length(heatmapSelection$data) > 0) {
      heatmapSelection$selectionText
    } else {
      "Click on a box in the heatmap to start building a filter. "
    }
  })
  
  output$sampleIndicators <- renderUI({
    createSampleFilterIndicators(session,
                                 heatmapSelection$appliedFilters)
  })
  
  output$cellTypeText <- renderText({
    paste0(input$sample_type, collapse = paste0(" OR "))
  })
  
  output$sampleTypeIndicators <- renderUI({
    lapply(input$sample_type, function(x) {
      
      id <- gsub(" ", "_", x)
      tl <- tagList(
        div(id = paste0(id, "_sample_type_indicator"),
            class = paste0("filter-indicator-text sample"),
            style = "width:80%;",
            x),
        tags$button(id = paste0(id, "_sample_type_deletor"),
                    class = "input-group-addon filterdeletor",
                    style = "width:20%;",
                    span(class = "glyphicon glyphicon-remove"))
      )
      
      onclick(paste0(id, "_sample_type_deletor"), 
              updateCheckboxGroupInput(session, "sample_type", selected = input$sample_type[- which(input$sample_type == x)]))
      
      tl 
      
    })
  })
  

  output$customText <- renderText({
    paste0(
      length(heatmapSelection$participants),
      " participants from ",
      length(heatmapSelection$studies),
      " studies"
    )
  })

  
  ## Listeners ##
  
  observeEvent(input$heatmap_value, {
    
    # Update Selection
    
    if (!is.null(input$heatmap_value)) {
      heatmap_value <- jsonlite::fromJSON(input$heatmap_value)
      if (heatmap_value$id %in% heatmapSelection$selectedIds) {
        heatmapSelection$data[[heatmap_value$id]] <- NULL
      } else {
        heatmapSelection$data[[heatmap_value$id]] <- heatmap_value$value
      }
      heatmapSelection$selectedIds <- names(heatmapSelection$data)
    }
    
    
  })
  
  # Get selection
  observeEvent(list(input$heatmap_value, input$assay_operator, input$sample_type), {
    # Get Selected studies and participants
    operator <- input$assay_operator
    if (length(heatmapSelection$data) > 0) {
    if (length(heatmapSelection$data) == 1) {
      heatmapSelection$participants <- unique(heatmapSelection$data[[1]]$participantList[1,])
      heatmapSelection$studies <- unique(heatmapSelection$data[[1]]$studyList)
    } else if (operator == "AND") {
      participantMatrixList <- lapply(heatmapSelection$data, "[[", "participantList")
      participantList <- lapply(participantMatrixList, function(x){
        if (!is.null(input$sample_type)) {
          x[1, x[2,] %in% input$sample_type]
        } else {
          x[1,]
        }
      })
      heatmapSelection$participants <- Reduce(intersect, participantList)
      
      studyList <- sapply(heatmapSelection$data, "[[", "studyList")
      heatmapSelection$studies <- Reduce(intersect, studyList)
    } else if (operator == "OR") {
      participantMatrixList <- lapply(heatmapSelection$data, "[[", "participantList")
      participantList <- lapply(participantMatrixList, function(x){
        if (!is.null(input$sample_type)) {
          x[1, x[2,] %in% input$sample_type]
        } else {
          x[1,]
        }
      })
      heatmapSelection$participants <- Reduce(union, participantList)
      studyList <- sapply(heatmapSelection$data, "[[", "studyList")
      heatmapSelection$studies <- Reduce(union, studyList)
    }
    } else {
      heatmapSelection$participants <- character(0)
      heatmapSelection$studies <- character(0)
    }
    
    # Text
    selectedAssayTimepoints <- lapply(heatmapSelection$data, function(x) {
      paste0(x$assay, " at ", x$timepoint, " Days")
    })
    
    text <- paste0(selectedAssayTimepoints, collapse = paste0(" ", operator, " "))
    
    if (!is.null(input$sample_type)) {
      text <- paste0(
        text,
        " for ", 
        paste0(input$sample_type, collapse = " OR ")
      )
    }
    
    
    heatmapSelection$selectionText <- text
  })
  
  
  observeEvent(input$"data-apply", {
    heatmapSelection$appliedFilters[[paste0("selection", (length(heatmapSelection$appliedFilters) + 1) )]] <- list(
      participants = heatmapSelection$participants,
      text = heatmapSelection$selectionText)
    
    
    heatmapSelection$data <- list()
    heatmapSelection$selectedIds <- list()
    heatmapSelection$participants <- character(0)
    heatmapSelection$selectionText <- ""
    heatmapSelection$studies <- character(0)
    updateCheckboxGroupInput(session,
                             "sample_type",
                             selected = character(0))
    
    lapply(names(heatmapSelection$appliedFilters),
           function(filter){
             onclick(paste0(filter, "_deletor"),
                     heatmapSelection$appliedFilters[[filter]] <- NULL)
           })
    
  })
  
    
    # Listeners -----
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
    
    
  # Not Currently In Use -----
#     
#     
#     
#     output$sampleFilters <- renderUI({
#       anyallDropdown <- function(id) {
#         span(class = "form-group shiny-input-container", style = "width:6em;",
#              tags$select(id = id,
#                          tags$option(value = "OR", "any of"),
#                          tags$option(value = "AND", "all of")))
#       }
#       tagList(
#         div(class="filter-dropdown",
#             tags$p(tags$em("Use the Assay heatmap in the \"Find\" tab for advanced filtering options.")),
#             .createFilter("assay", span(anyallDropdown("assay_operator"), "these assays"), data),
#             # filterDiv("AND"),
#             .createFilter("sample_type", span(anyallDropdown("sample_type_operator"), "these cell types"), data),
#             # filterDiv("AT"),
#             .createFilter("timepoint", span(anyallDropdown("timepoint_operator"), "these study days"), data),
#             div()
#         )
#       )
#     })
#     
#     
#     
#     # Filter indicators -----
#     
#     # # Listeners
#     # onclick("species_indicator",
#     #         updateCheckboxGroupInput(session, "species", selected = character(0)))
#     # onclick("condition_indicator",
#     #         updateCheckboxGroupInput(session, "condition", selected = character(0)))
#     # onclick("study_type_indicator",
#     #         updateCheckboxGroupInput(session, "study_type", selected = character(0)))
#     # onclick("exposure_material_indicator",
#     #         updateCheckboxGroupInput(session, "exposure_material", selected = character(0)))
#     
#     
#     
#     
#     
#   output$summaryText <- renderText({
#     # paste0(
#     #   "Showing data from ",
#     #   length(unique(reactiveData()$sampleid)),
#     #   " samples from ",
#     #   length(unique(reactiveData()$subjectid)),
#     #   " subjects in ",
#     #   length(unique(reactiveData()$study)),
#     #   " studies."
#     # )
#     paste0(
#       length(unique(reactiveData()$subjectid)),
#       " participants from ",
#       length(unique(reactiveData()$study)),
#       " studies"
#     )
#   })
#     
#     output$studyCount <- renderText(length(unique(reactiveData()$study)))
#   
#   output$dim <- renderText({
#     paste(
#       "dim(data) = ", paste0(dim(reactiveData()), collapse = ","))
#     })
#   
#   output$species <- renderText({
#     paste(
#       "unique(data$species) = ", paste0(unique(reactiveData()$species), collapse = ",")
#     )
#   }) 
#   
#   output$gender <- renderText({
#     paste(
#       "unique(data$gender) = ", paste0(unique(reactiveData()$gender), collapse = ",")
#     )
#   })
#   
#   output$assay <- renderText({
#     paste(
#       "unique(data$assay) = ", paste0(unique(reactiveData()$assay), collapse = ",")
#     )
#   })
#     
#   output$participants <- renderText({
#     paste(
#       length(unique(reactiveData()$subjectid)),
#       " subjects"
#     )
#   })
#   
#   output$Studies <- renderText({
#     paste(
#       length(unique(reactiveData()$study)),
#       " studies"
#     )
#   })
#   
#   output$Samples <- renderText({
#     paste(
#       length(unique(reactiveData()$sampleid)),
#       " samples"
#     )
#   })
#   
#   output$timepointHeatmap_sample <- renderPlot({
#     timepointHeatmap_sample(reactiveData())
#   })
#   
#   output$timepointHeatmap_study <- renderPlot({
#     timepointHeatmap_study(reactiveData())
#   })
#   output$studyHeatmapInfo <- renderText ({
#     paste0("x=", input$studyHeatmapClick$x, "\ny=", input$studyHeatmapClick$y)
#   })
#   # div(plotOutput("timepointHeatmap_study",  height = "200px", click = "studyHeatmapClick")),
#   # verbatimTextOutput("studyHeatmapInfo"),
#   
#   output$upsetPlot <- renderPlot({
#     upsetPlot(reactiveData())
#   })
#   
#   
#   
#   
#   output$assayBarplot <- renderPlot({
#     assayBarplot(reactiveData())
#   })
#   
#   # output$interactiveHeatmap <- renderPlotly({
#   #   d <- formatHeatmapData(reactiveData())
#   #   plotlyHeatmap <- custom_timepointHeatmap(d, text = paste0("Number of Participants: ", count))
#   #     htmlwidgets::onRender(plotlyHeatmap,
#   #                           "//javascript
#   #   // when hovering over an element, give it a thick, white border
#   #   function(el, x) {
#   #   el.on('plotly_click', function(d) {
#   #   console.log(d.points[0]);
#   #   });
#   #   }
#   # 
#   # 
#   #                           "
#   #     )
#   # })
#   
#   output$selection <- renderPrint({
#     s <- event_data("plotly_click")
# 
# 
#     if (length(s) == 0) {
#       "Click on a cell in the heatmap to display data"
#     } else {
#       tp <- gsub(" Days", "", s$x)
#       as <- s$y
#       rdata <- reactiveData()
#       d <- formatHeatmapData(rdata)
#       d <- d[timepoint == tp & assay == as]
#       cat("You selected:\n\n")
#       cat(paste0(d$count, " participants and ", length(d$studyList[[1]]), " studies:\n"))
#       as.list(d$studyList[[1]])
#     }
#   })
# 
}
