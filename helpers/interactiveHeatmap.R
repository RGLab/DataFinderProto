plotlyHeatmap <- function(data) {

  d <- formatHeatmapData(data)
  
  
  
  plotlyHeatmap <- custom_timepointHeatmap(d, text = paste0("Number of Participants: ", count))
#   htmlwidgets::onRender(plotlyHeatmap,
#                         "//javascript
# // when hovering over an element, give it a thick, white border
# function(el, x) {
# el.on('plotly_click', function(d) {
# console.log(d);
# console.log(self);
# console.log(x);
# d.points[0].data.fillcolor = 'black';
# });
# }
# 
# 
#                         "
#   )
}

# ---------------------------------------------------------------

formatHeatmapData <- function(data) {
  
  # Remove samples with no assay data
  td <- data[!is.na(assay) & timepoint != "Unknown"]
  timepoints_xaxis <- c("<0", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
                        "13", "14", "15-27", "28", "29-55", "56", ">56")
  # Make sure combinations with zero studies have a row, with count = 0.
  df <- expand.grid(assay = unique(td$assay), timepoint = timepoints_xaxis, stringsAsFactors = FALSE)
  d <- td[, .(studyList = list(unique(study)), 
              participantList = list(unique(subjectid)),
              participantCount = length(unique(subjectid))),
              by = c("timepoint", "assay")]

  
  setDT(d)
  setkey(d, timepoint, assay)
  setDT(df)
  setkey(df, timepoint, assay)
  
  hmdata <-  merge(df, d, all.x = TRUE, by = c("timepoint", "assay"))
  hmdata[, participantCount := ifelse(is.na(participantCount), 0, participantCount)]
  return(hmdata)
  
}

custom_timepointHeatmap <- function(d,
                                    breaks = c(0, 10, 50, 100, 500, 1000, Inf),
                                    legendLabels = NULL,
                                    legendName = "Number of\nSamples",
                                    colorScheme = "Greens",
                                    abbreviateAssayNames = FALSE,
                                    ...) {
  timepoints_xaxis <- c("<0", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
                        "13", "14", "15-27", "28", "29-55", "56", ">56")
  
  
  
  # Make labels for legend
  if (is.null(legendLabels)) {
    
    legendLabels <- character(length(breaks))
    for (i in seq_along(legendLabels)) {
      if (i == 1) {
        legendLabels[i] <- paste0("  ", "0")
      } else if (i == length(breaks)) {
        legendLabels[i] <- paste0(" >",breaks[i-1])
      } else {
        legendLabels[i] <- paste0(breaks[i-1] + 1, "-", breaks[i])
      }
      
    }
    
  }
  
  # Set color
  d$colorIndex <- as.character(.bincode(d$count, breaks = breaks, include.lowest = TRUE) + 1)
  d$colorIndex <- ifelse(d$count == 0, "1", d$colorIndex)
  
  # get assay labels, filtered by present assays
  assays <- unique(d$assay)[order(unique(d$assay), decreasing = TRUE)]
  
  
  # Create dfs with various 
  ###
  participantCount <- 
    # Creates a matrix with one column for each timepoint
    # and one row for each assay, where values are the count at 
    # that timepoint and assay
    vapply(timepoints_xaxis, function(tp) {
      vapply(assays, function(as) {
        d[timepoint == tp & assay == as, count]
      }, FUN.VALUE = 1)
    }, FUN.VALUE = rep(1, length(assays)))
    
  studies <- 
    # Creates a matrix with one column for each timepoint
    # and one row for each assay, where values are a vector
    # of study ids for that timepoint and assay
    vapply(timepoints_xaxis, function(tp) {
      lapply(assays, function(as) {
        unlist(d[timepoint == tp & assay == as, studyList])
        })
    }, FUN.VALUE = rep(list(c("SDY1", "SDY2")), length(assays)))
  
  colorIndex <- 
    vapply(timepoints_xaxis, function(tp) {
      vapply(assays, function(as) {
        as.numeric(d[timepoint == tp & assay == as, colorIndex])
      }, FUN.VALUE = 1)
    }, FUN.VALUE = rep(1, length(assays)))
  
  text <- 
    vapply(timepoints_xaxis, function(tp) {
      vapply(assays, function(as) {
        paste0(
          "<em>", as, " at Day ", tp, "</em><br>",
          "Number of Participants: ", d[timepoint == tp & assay == as, count], "<br>",
          "<em>Studies:</em> <br>", 
          paste0(unlist(d[timepoint == tp & assay == as, studyList]), collapse = "<br>")
               )
      }, FUN.VALUE = "text")
    }, FUN.VALUE = rep("text", length(assays)))
  
  customdata <- 
    vapply(timepoints_xaxis, function(tp) {
      vapply(assays, function(as) {
        jsonlite::toJSON(list(participantCount = d[timepoint == tp & assay == as, count],
             studies = unlist(d[timepoint == tp & assay == as, studyList])))
      }, FUN.VALUE = "text")
    }, FUN.VALUE = rep("text", length(assays)))
  rownames(customdata) <- assays
  
  ids <-     
    vapply(timepoints_xaxis, function(tp) {
      vapply(assays, function(as) {
        gsub("<|>|\\s", "", paste0(as, "_", tp))
      }, FUN.VALUE = "")
    }, FUN.VALUE = rep("", length(assays)))
    
  
  plot_ly(z = colorIndex, 
          # Plotly tries to coerce strings into numbers. Add "days" to force it to stay character
          x = paste0(timepoints_xaxis, " Days"), 
          y = assays,
          type = "heatmap",
          colors = c("#FFFFFF", "#238B45"),
          text = text,
          hoverinfo = 'text',
          xgap = 2,
          ygap = 2,
          showlegend = FALSE,
          showscale = FALSE,
          key = I(customdata)
          ) 
  
  # Custom data seems to be a bust :(
  
}















