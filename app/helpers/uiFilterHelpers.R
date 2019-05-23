.createFilterDropdown <- function(filter = "sample_type",
                      label = "Cell Type",
                      indicator = "sampleTypeIndicators",
                      data) {
  div(class = "dropdown",
      div(class = "btn-group filterselector", role = "group", style = "width:10em; ",
          # tags$button(label, class = "btn btn-default", style="width:8em", type = "button"),
          tags$button(style="text-align:left;", 
                      span(label),
                      span(HTML("&#9660;"), stlye = "float:right"), 
                      class = "btn btn-default dropdown-toggle", 
                      type = "button", 
                      "data-toggle"="dropdown", 
                      style = "width:auto"),
          div( class="dropdown-menu filter-dropdown", style = "width:10em;",
               
               checkboxGroupInput(
                 inputId = filter,
                 label = NULL,
                 choices = unique(data[[filter]])[order( unique(data[[filter]]), na.last = TRUE)])),
          div(class = "input-group filter-indicator", style = "width: 10em;",
              uiOutput(indicator))))
}

.createSingleIndicators <- function(filter = "sample_type",
                        class = "sample",
                        input,
                        session) {
  lapply(input[[filter]], function(x) {
    
    id <- gsub(" ", "_", x)
    tl <- tagList(
      div(id = paste0(id, "_", filter, "_indicator"),
          class = paste0("filter-indicator-text ", class),
          style = "width:80%;",
          x),
      tags$button(id = paste0(id, "_", filter, "_deletor"),
                  class = "input-group-addon filterdeletor",
                  style = "width:20%;",
                  span(class = "glyphicon glyphicon-remove"))
    )
    
    onclick(paste0(id, "_", filter, "_deletor"), 
            updateCheckboxGroupInput(session, filter, selected = input[[filter]][- which(input[[filter]] == x)]))
    
    tl 
    
  })
}


# This creates the "filter indicators
createFilterIndicators <- function(session,
                                   input, 
                                   options, 
                                   class) {
  # Create ui elements
  renderUI({
    i <- input
    indicatorList <- list()
    for (x in options) {
      operator <- ifelse(paste0(x, "_operator") %in% names(i), i[[paste0(x, "_operator")]], "OR")
      if (!is.null(i[[x]])) {
        if (length(i[[x]]) > 1) {
          indicatorText <- paste0(x, 
                                  " is \"",
                                  paste0(i[[x]], 
                                         collapse = paste0("\" ", operator, " \"")),
                                  "\"") 
        } else {
          indicatorText <- paste0(x, " is \"", i[[x]], "\"")
        }
        
        # <div class="input-group filter-indicator">
        #   <div id = "species_indicator" class="filterindicator study">study_type is "longitudinal"</div>
        #   <button id = "species_deletor" class="input-group-addon filterdeletor">
        #     <span class = "glyphicon glyphicon-remove"></span>
        #   </button>
        # </div>
        
        indicatorList[[x]] <- div(class = "input-group filter-indicator",
                                  div(id = paste0(x, "_indicator"),
                                      class = paste0("filter-indicator-text ", class),
                                      indicatorText
                                  ),
                                  tags$button(id = paste0(x, "_deletor"),
                                              class = "input-group-addon filterdeletor",
                                              span(class = "glyphicon glyphicon-remove"))
        )
      }
      
      
      
    }
    if (length(indicatorList) == 0) {
      tags$em("No filters currently applied")
    } else {
      indicatorList
    }
  })
  
}




# This is the button that you click on to expand the dropright menu
.filterSelector <- function(buttonText, filterClass) {
  div(class = "dropdown",
      div(class = "btn-group filterselector", role = "group", style = "width:100%",
          tags$button(buttonText, class = "btn btn-default", style="width:90%", type = "button"),
          tags$button(style="text-align:left;", 
                      HTML("&#9654;"), 
                      class = "btn btn-default dropdown-toggle", 
                      type = "button", 
                      "data-toggle"="dropdown", 
                      style = "width:10%"),
          div( class="dropdown-menu filter-dropdown", 
               uiOutput(paste0(filterClass, "Filters"))
          )
      )
  )
}

# This creates the collapsible checkbox groups
.createFilter <- function(id, label, data) {
  
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
      div(class = "collapse",
          id = paste0(id, "_collapse"),
          div(
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


createSampleFilterIndicators <- function(session,
                                        appliedFilters) {
  # Create ui elements
    indicatorList <- list()
    for (filter in names(appliedFilters)) {
      indicatorText <- appliedFilters[[filter]]$text
        
        # <div class="input-group filter-indicator">
        #   <div id = "species_indicator" class="filterindicator study">study_type is "longitudinal"</div>
        #   <button id = "species_deletor" class="input-group-addon filterdeletor">
        #     <span class = "glyphicon glyphicon-remove"></span>
        #   </button>
        # </div>
        
        indicatorList[[filter]] <- div(class = "input-group filter-indicator",
                                  div(id = paste0(filter, "_indicator"),
                                      class = paste0("filter-indicator-text ", "sample"),
                                      indicatorText
                                  ),
                                  tags$button(id = paste0(filter, "_deletor"),
                                              class = "input-group-addon filterdeletor",
                                              span(class = "glyphicon glyphicon-remove"))
        )
      }
      
    if (length(indicatorList) == 0) {
      tags$em("No filters currently applied")
    } else {
      indicatorList
    }
}

# This is just a wrapper for styling other text within the filter dropdown menu
filterDiv <- function(...){
  div(..., class = "filtertext")
}