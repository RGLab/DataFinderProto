createStudyCard <- function(studyName, data, output) {
  
  studyData <- data[study == studyName]
  participantCount <- length(unique(studyData$subjectid))
  exposureMaterial <- paste0(unique(studyData$exposure_material), collapse = ", ")
  condition <- paste0(unique(studyData$condition), collapse = ", ")
  sampleType <- paste0(unique(studyData$sample_type), collapse = ", ")
  assays <- paste0(unique(studyData$assay), collapse = ", ")
  assayCount <- length(unique(studyData$assay))
  # adjust height based on assayCount
  height = paste0(30 + assayCount * 15, "px")
  # browser()
  
  # For studies with assay data, create timepoint-assay heatmap
  if (sum(!is.na(studyData$assay)) > 0) {
    output[[paste0("plot_", studyName)]] <- renderCachedPlot({
      timepointHeatmap_sample_small(studyData,
                                    breaks = c(0, 5, 10, 50, Inf))+
        theme(legend.position = "none",
              axis.title.x = element_blank())},
      cacheKeyExpr = list(studyName, "timepointHeatmap_sample_small"))
    outputPlot <- plotOutput(paste0("plot_", studyName), height = height)
  } else {
    outputPlot <- NULL
  }
  
  
  div(
    style = paste0(
      "border: 2px solid #bcbcbc;",
      "padding: 5px;",
      "margin: 10px;",
      "border-radius: 10px;",
      "width: 300px;",
      "display:inline-block;"
    ),
    span(studyName, style = "font-size:1.2em;"),
    span("Helen Miller", style = "float:right;font-size:1.2em"),  # Study PI
    tags$hr(style="margin-top:5px;margin-bottom:10px;"),
    p(em(participantCount, " participants")),
    p(paste0("Exposure material: ", exposureMaterial)),
    p(paste0("Condition: ", condition)),
    p(paste0("Sample Type: ", sampleType)),
    p(paste0("Assays: ", assays)),
    p(outputPlot),
    div()
  )
}

# Study card html legend
colorPalette <- c("white", RColorBrewer::brewer.pal(4, "Greens"))
breaks <- c(0, 5, 10, 50, Inf)
legendLabels <- character(length(breaks))
for (i in seq_along(legendLabels)) {
  if (i == 1) {
    legendLabels[i] <- "0"
  } else if (i == length(breaks)) {
    legendLabels[i] <- paste0(">",breaks[i-1])
  } else {
    legendLabels[i] <- paste0(breaks[i-1] + 1, "-", breaks[i])
  }
  
}

studyCardLegend <- 
  # html legend for study cards
  tags$table(
    tagList(
      mapply(function(color, label) {
        tags$tr(
          tags$td(div(style=paste0("width:20px; height:20px; background-color:",color,"; border: solid 1px #999; padding:5px;"))),
          tags$td(HTML(label), style = "font-size:.8em;")
        )
      },
      color = colorPalette,
      label = legendLabels,
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
      )
    )
  )
