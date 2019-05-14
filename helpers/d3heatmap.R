d3Heatmap <- function(data) {
  hmdata <- formatHeatmapData(data)
  
  # Get data in format for d3
  timepoints_xaxis <- c("<0", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
                        "13", "14", "15-27", "28", "29-55", "56", ">56")
  assays <- unique(hmdata$assay)[order(unique(hmdata$assay), decreasing = TRUE)]
  
  
  d <- hmdata
  j <- jsonlite::toJSON(d)
 
  
  r2d3(j, script = "d3/heatmap.js", 
       options = list(breaks = c(10, 50, 100, 500, 1000),
                      colors = c("#FFFFFF", RColorBrewer::brewer.pal(6, "Greens")),
                      xaxis = timepoints_xaxis,
                      yaxis = assays)
       )
}