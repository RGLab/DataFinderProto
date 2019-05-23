d3Heatmap <- function(data,
                      selectedParticipants,
                      selected) {
  hmdata <- formatHeatmapData(data,
                              selectedParticipants)
  
  # Get data in format for d3
  timepoints_xaxis <- c("<0", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
                        "13", "14", "15-27", "28", "29-55", "56", ">56")
  assays <- c("PCR", "Neutralizing Antibody", "MBAA", "HLA Typing", "HAI", "Gene Expression",
              "Flow Cytometry", "ELISPOT", "ELISA", "CyTOF")
  assayCodes <- 1:length(assays)
  names(assayCodes) <- assays
  
  timepointCodes <- letters[1:length(timepoints_xaxis)]
  names(timepointCodes) <- timepoints_xaxis
  
  # Add ids to hmdata
  hmdata$id <- apply(hmdata, 1, function(x) {
    paste0(assayCodes[x$assay], timepointCodes[x$timepoint])
  })
  
  
  d <- hmdata
  j <- jsonlite::toJSON(d)
  
  
  r2d3(j, script = "d3/heatmap.js", 
       options = list(breaks = c(1, 10, 50, 100, 500, 1000),
                      colors = c("#FFFFFF", RColorBrewer::brewer.pal(7, "Greens")),
                      xaxis = timepoints_xaxis,
                      yaxis = assays,
                      selected = selected))
}
