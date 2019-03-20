# This is a helper function which filters the ISdataframe based on 
# user input

filterData <- function(data,
                       filters = list()) {
  
  # adv-r.had.co.nz/Computing-on-the-language.html
  # http://adv-r.had.co.nz/Expressions.html
  
  # helper
  .createExprText = function(filterName, filterValue) {
    
    
    if (!is.null(filterValue)) {
      filter <- paste0(eval(filterName),  " %in% ", paste0(deparse(eval(filterValue)), collapse = ""))
    } else {
      filter <- "TRUE"
    }
    
    return(substitute(filter))
  }
  
  fullFilterText = character(0)
  for (filter in names(filters)) {
    if (length(fullFilterText) == 0)  {
      fullFilterText <- .createExprText(filter, filters[[filter]])
    } else {
      fullFilterText = paste0(fullFilterText, " & ", .createExprText(filter, filters[[filter]]))
    }
  }
  
  filteredData <- data[eval(parse(text = fullFilterText))]
  
}
