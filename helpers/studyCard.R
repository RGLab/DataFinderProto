createStudyCard <- function(studyName, data, origData, output, studyTitle = "Title of the Study") {
  
  maxParticipants <- length(unique(origData[study == studyName]$subjectid))
  
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
    tags$input(type = "checkbox", name = paste0(studyName, "_check"), value = paste0(studyName, "_check")),
    span(studyName, style = "font-size:1.2em;"),
    span("Helen Miller", style = "float:right;font-size:1.2em"),  # Study PI
    tags$hr(style="margin-top:5px;margin-bottom:10px;"),
    div(studyTitle, tags$a(">>", style = "cursor:pointer;"), style = "text-align:left;font-weight:bold;font-size:1.1em;margin-bottom:10px;heigth:2.5em;text-overvlow:elipsis;"),
    div(class = "progress", style = "margin-bottom:0px;height:10px;",
        div(class = "progress-bar progress-bar-success", 
            role = "progressbar", 
            "aria-valuenow" = participantCount,
            "aria-valuemin"="0",
            "aria-valuemax"=as.character(maxParticipants),
            style = paste0("width:", participantCount/maxParticipants*100, "%"))),
    p(tags$em(paste0(participantCount, " of ", maxParticipants, " participants selected."))),
    div(
      p(span("Exposure material: ", style = "font-weight:bold;"), exposureMaterial, class = "card-text"),
      p(span("Condition: ", style = "font-weight:bold;"), condition, class = "card-text"),
      p(span("Sample Type: ", style = "font-weight:bold;"), sampleType, class = "card-text"),
      p(span("Assays: ", style = "font-weight:bold;"), assays, class = "card-text"),
      style = "font-size:0.9em;"
    ),
    outputPlot,
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
