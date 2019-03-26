createStudyCard <- function(studyName, data) {
  
  studyData <- data[study == studyName]
  participantCount <- length(unique(studyData$subjectid))
  exposureMaterial <- unique(studyData$exposure_material)
  condition <- paste0(unique(studyData$condition), collapse = ", ")
  sampleType <- paste0(unique(studyData$sample_type), collapse = ", ")
  assays <- paste0(unique(studyData$assay), collapse = ", ")
  
  
  div(
    style = paste0(
      "border: 2px solid #bcbcbc;",
      "padding: 5px;",
      "margin: 10px;",
      "border-radius: 10px;",
      "width: 250px;",
      "display:inline-block;"
    ),
    span(h3(studyName)), em(participantCount, " participants"),
    p(paste0("Exposure material: ", exposureMaterial)),
    p(paste0("Condition: ", condition)),
    p(paste0("Sample Type: ", sampleType)),
    p(paste0("Assays: ", assays)),
    div()
  )
}