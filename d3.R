library(r2d3)
library(data.table)
r2d3(data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20), script = "d3/barplot.js")


data <- get(load("data/cube.RData"))
setDT(data)
source("helpers/interactiveHeatmap.R")
hmdata <- formatHeatmapData(data)

# Get data in format for d3
timepoints_xaxis <- c("<0", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
                      "13", "14", "15-27", "28", "29-55", "56", ">56")
assays <- unique(hmdata$assay)[order(unique(hmdata$assay), decreasing = TRUE)]

# Get colors
colors = c("#FFFFFF", RColorBrewer::brewer.pal(6, "Greens"))
hmdata$colorIndex <- .bincode(hmdata$count, breaks = c(0, 10, 50, 100, 500, 1000, Inf), include.lowest = TRUE) + 1
hmdata$colorIndex <- ifelse(hmdata$count == 0, 1, hmdata$colorIndex)
hmdata$color <- colors[hmdata$colorIndex]

d <- hmdata
d3data <- 
  vapply(timepoints_xaxis, function(tp) {
    lapply(assays, function(as) {
      list(participantCount = d[timepoint == tp & assay == as, count],
           studies = unlist(d[timepoint == tp & assay == as, studyList]),
           color = d[timepoint == tp & assay == as, color])
    })
  }, FUN.VALUE = rep(list(list()), length(assays)))

r2d3(data = d3data, script = "d3/heatmap.js")
