studyTypePlot <- function(data) {
  # Transform data so one row per study
  td <- data[, .(study_type = unique(study_type)), by = "study"]
  
  # plot data
  ggplot(td, aes(study_type)) +
    geom_bar() +
    xlab("Study Type") +
    ylab("Number of Studies") +
    theme_IS()
}

genderBarplot <- function(data) {
  # Transform data so one row per participant
  td <- data[, .(gender = unique(gender)), by = "subjectid"]
  
  # plot data
  ggplot(td, aes(gender)) +
    geom_bar() + 
    xlab("Gender") +
    ylab("Number of participants") +
    theme_IS()
  
}


assayBarplot <- function(data) {
  # Transform data so one row per sample/assay combo
  td <- data[, c(placeholder = NA), by = c("sampleid", "assay")]
  
  # plot data 
  ggplot(td, aes(assay)) + 
    geom_bar() +
    xlab("Assay") + 
    ylab("Number of samples") +
    theme_IS() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}


timepointHeatmap_study <- function(data) {
  # transform data into matrix with rows for assay, columns for timepoint
  timepoints_xaxis <- c("<0", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
                  "13", "14", "15-27", "28", "29-55", "56", ">56")
  # Get relevant data
  # By sample
  td <- data[, .N, c("timepoint", "assay", "study")]
  td1 <- td[, .(N = sum(N)), c("timepoint", "assay")]
  # By study
  td <-td[, .N, c("timepoint", "assay")]
  
  # Remove samples with no assay data
  td <- td[!is.na(assay) & timepoint != "Unknown"]
  # Set color
  td$colorIndex <- as.character(.bincode(td$N, breaks = c(0, 1, 2, 3, 4, 5, 10, Inf)) + 1)
  td$color <- RColorBrewer::brewer.pal(7, "Greens")[td$colorIndex]
  
  ggplot(td, aes(timepoint, assay)) + 
    geom_tile(aes(fill = colorIndex)) +
    scale_x_discrete(limits = timepoints_xaxis) +
    theme(panel.background = element_rect(fill = RColorBrewer::brewer.pal(8, "Greens")[1]),
          panel.border = element_rect(linetype = 1, fill = "transparent"),
          legend.key = element_rect(color= "gray50"),
          panel.grid = element_blank()) +
    xlab("Week") +
    coord_equal() +
    scale_fill_manual(values =  c(RColorBrewer::brewer.pal(8, "Greens")),
                      limits = as.character(1:8),
                      labels = c("0", "1", "2", "3", "4", "5", "6-10", "11+"),
                      name = "Number of\nStudies") 
    
  
}
timepointHeatmap_sample <- function(data) {
  # transform data into matrix with rows for assay, columns for timepoint
  timepoints_xaxis <- c("<0", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
                        "13", "14", "15-27", "28", "29-55", "56", ">56")
  # Get relevant data
  # By sample
  td <- data[, .N, c("timepoint", "assay", "study")]
  td1 <- td[, .(N = sum(N)), c("timepoint", "assay")]
  
  # Remove samples with no assay data
  td <- td[!is.na(assay) & timepoint != "Unknown"]
  # Set color
  td$colorIndex <- as.character(.bincode(td$N, breaks = c(0, 5, 10, 50, 100, 500, 1000, Inf)) + 1)
  td$color <- RColorBrewer::brewer.pal(7, "Greens")[td$colorIndex]
  
  ggplot(td, aes(timepoint, assay)) + 
    geom_tile(aes(fill = colorIndex)) +
    scale_x_discrete(limits = timepoints_xaxis) +
    theme(panel.background = element_rect(fill = RColorBrewer::brewer.pal(8, "Greens")[1]),
          panel.border = element_rect(linetype = 1, fill = "transparent"),
          legend.key = element_rect(color= "gray50"),
          panel.grid = element_blank()) +
    xlab("Week") +
    coord_equal() +
    scale_fill_manual(values =  c(RColorBrewer::brewer.pal(8, "Greens")),
                      limits = as.character(1:8),
                      labels = c("0", "1-5", "6-10", "11-50", "51-100", "101-500", "501-1000", ">1000"),
                      name = "Number of\nSamples") 
  
  
}

upsetPlot <- function(data) {
  td <- data[, .N, c("study", "assay")]
  td <- td[!is.na(assay)]
  
  b <- logical(length(unique(td$study)))
  assays <- data.frame(rep(list(b), length(unique(td$assay)) + 1))
  names(assays) <- c("identifier", unique(td$assay))
  rownames(assays) <- unique(td$study)
  assays$identifier <- rownames(assays)
  
  for (studyName in unique(td$study)) {
    for (assay in names(assays)[-1]) {
      studySubset <- td[td$study == studyName, ]
      assays[studyName, assay] <- as.numeric(assay %in% studySubset$assay)
    }
  }
  
  upset(assays)
  
}
