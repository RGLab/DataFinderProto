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

ui1Outputs <- function(input, output) {
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
  
  # Filter indicators -----
  output$filterList <- renderUI({
    i <- reactiveValuesToList(input)
    studyList <- list()
    participantList <- list()
    sampleList <- list()
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
    
    tagList(
      div(
        span("Showing studies where: "),
        studyList
      ),
      div(
        span("Showing participants where: "),
        participantList
      ),
      div(
        span("Showing samples where: "),
        sampleList
      )
    )
  })
}