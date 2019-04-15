filterButton <- function(buttonText, filterClass) {
  div(class = "dropdown",
      div(class = "btn-group", role = "group", style = "width:100%",
          tags$button(buttonText, class = "btn btn-default", style="width:90%", type = "button"),
          tags$button(style="text-align:left;", 
                      HTML("&#9654;"), 
                      class = "btn btn-default dropdown-toggle", 
                      type = "button", 
                      "data-toggle"="dropdown", 
                      style = "width:10%"),
          div( class="dropdown-menu", 
               style = "left:100%;top:0%",
               uiOutput(paste0(filterClass, "Filters"))
          )
      )
  )
}
