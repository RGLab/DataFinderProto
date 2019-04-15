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