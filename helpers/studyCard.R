createStudyCard <- function(studyName, data, output) {
  
  studyData <- data[study == studyName]
  participantCount <- length(unique(studyData$subjectid))
  exposureMaterial <- paste0(unique(studyData$exposure_material), collapse = ", ")
  condition <- paste0(unique(studyData$condition), collapse = ", ")
  sampleType <- paste0(unique(studyData$sample_type), collapse = ", ")
  assays <- paste0(unique(studyData$assay), collapse = ", ")
  # browser()
  output[[paste0("plot_", studyName)]] <- renderCachedPlot({
    timepointHeatmap_sample_small(studyData,
                                  breaks = c(0, 5, 10, 50, Inf))},
    cacheKeyExpr = list(studyName, "timepointHeatmap_sample_small"))
  
  div(
    style = paste0(
      "border: 2px solid #bcbcbc;",
      "padding: 5px;",
      "margin: 10px;",
      "border-radius: 10px;",
      "width: 300px;",
      "display:inline-block;"
    ),
    span(h3(studyName)), em(participantCount, " participants"),
    p(paste0("Exposure material: ", exposureMaterial)),
    p(paste0("Condition: ", condition)),
    p(paste0("Sample Type: ", sampleType)),
    p(paste0("Assays: ", assays)),
    plotOutput(paste0("plot_", studyName), height = "200px"),
    div()
  )
}