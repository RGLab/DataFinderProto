# GetData -----
# This function will pull the relevant data from the labkey server and 
# get it into the correct format. This dataset will be static within an
# instance of the app and filters will be applied to it to create the 
# "reactive" version of the data for plotting. 
getData <- function(baseUrl = "https://www.immunespace.org", 
                    folderPath = "/Studies/") {
  
  # pull in all relevant tables (immport.dim*)
  
  # Tables to pull from:
  # demographic
  dimDemographic <- data.table(labkey.selectRows(baseUrl,
                                                 folderPath,
                                                 schemaName = "immport",
                                                 queryName = "dimdemographic",
                                                 colNameOpt = "fieldname"))
  # sample
  dimSample <- data.table(labkey.selectRows(baseUrl,
                                            folderPath,
                                            schemaName = "immport", 
                                            queryName = "dimsample",
                                            colNameOpt = "fieldname"))
  setnames(dimSample, "type", "sample_type")
  
  # sampleassay
  dimSampleassay <- data.table(labkey.selectRows(baseUrl,
                                                 folderPath,
                                                 schemaName = "immport", 
                                                 queryName = "dimsampleassay",
                                                 colNameOpt = "fieldname"))
  
  # study
  dimStudy <- data.table(labkey.selectRows(baseUrl,
                                           folderPath,
                                           schemaName = "immport", 
                                           queryName = "dimstudy",
                                           colNameOpt = "fieldname"))
  setnames(dimStudy, c("type", "sortorder"), c("study_type", "study_sortorder"))
  
  # studycondition -- come back to this later...
  # TODO: There are some studies (eg SDY201, SDY614) which have multiple conditions,
  # and therefore have multiple rows in this table. Determine the best method for dealing with that 
  dimStudycondition <- data.table(labkey.selectRows(baseUrl ,
                                         folderPath,
                                         schemaName = "immport", 
                                         queryName = "dimstudycondition",
                                         colNameOpt = "fieldname"))
  
  
  cube <- merge(dimDemographic, dimSample, by = "subjectid", all = TRUE)
  cube <- merge(cube, dimSampleassay, by = "sampleid", all = TRUE)
  cube <- merge(cube, dimStudy, by = "study", all = TRUE)
  # allow.cartesian = TRUE means that for studies with multiple rows in
  # dimStudycondition (ie multiple conditions for one study) each row will be
  # duplicated so that eg for a study with two conditions there will be two rows
  # where everything is the same except study condition
  cube <- merge(cube, dimStudycondition, by = "study", all = TRUE, allow.cartesian = TRUE)
  
  # Filter to only include studies on ImmuneSpace...
  studies <- labkey.selectRows(
    baseUrl=baseUrl,
    folderPath="/home",
    schemaName="lists",
    queryName="Studies",
    viewName="",
    colSort="id",
    colFilter=NULL,
    containerFilter=NULL
  )
  
  cube <- cube[study %in% studies$Name]
  
  # > names(cube)
  # [1] "study"               "sampleid"            "subjectid"           "ageinyears"          "species"            
  # [6] "gender"              "race"                "age"                 "exposure_material"   "exposure_process"   
  # [11] "timepoint"           "timepoint_sortorder" "sample_type"         "assay"               "program"            
  # [16] "study_type"          "study_sortorder"    
  
  return(cube)
  
}

# FilterData -----
# This is a helper function which filters the ISdataframe based on 
# user input

filterData <- function(data,
                       filters = list(),
                       heatmapFilters = NULL) {
  if (length(heatmapFilters) > 1) {
    participantList <- sapply(heatmapFilters, "[[", "participants")
    participants <- Reduce(intersect, participantList)
  } else if (length(heatmapFilters) == 1) {
    participants <- heatmapFilters[[1]]$participants
  } else {
    participants <- NULL
  }
  filters$subjectid <- participants
  
  # adv-r.had.co.nz/Computing-on-the-language.html
  # http://adv-r.had.co.nz/Expressions.html
  
  # helpers -----
  .createExprText = function(filterName, filterValue, operator = NULL) {
    # Creates a text string like
    # [1] "any(c(\"0-10\", \"11-20\") %in% age)"
    
    operator <- .operatorToFunction(operator)
    
    # sample-level filters
    if (!is.null(filterValue)) {
      # NOTE:  Need to collapse deparse statement because it splits it into multiple lines
      # when it is very long
      filter <- paste0(operator, "(", paste0(deparse(eval(filterValue)), collapse = "") , " %in% ", filterName, ")")
    } else {
      filter <- "TRUE"
    } 
    
    return(substitute(filter))
  }
  
  .operatorToFunction <- function(operator) {
    if(length(operator) == 0) return("any")
    if(operator == "OR") return("any")
    if(operator == "AND") return("all")
    return(operator)
  }
  
  # Main -----
  # Everything will be grouped by subjectid
  # Want a filter like: 
  # data[,
  #      if (all(c("0", "7") %in% timepoint) &
  #          any(c("Gene Expression", "HAI") %in% assay) &
  #          any(c("0-10", "11-20") %in% age)) {
  #        .SD
  #      },
  #      subjectid]
  
  # Paste all filters together
  filterList <- mapply(.createExprText, 
                       filterName = names(filters),
                       filterValue = filters,
                       # operator = operators[names(filters)],
                       SIMPLIFY = FALSE)
  filterText <- paste0(filterList, collapse = " & ")
  
  # Create full expression
  fullFilterText <- paste0("if (", filterText, ") .SD")
  filteredData <- data[, eval(parse(text = fullFilterText)), by = "subjectid"]
  return(filteredData)
  
}
