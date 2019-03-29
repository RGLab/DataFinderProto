# Themes --------------

custom_barplot_theme <- list(
  theme_minimal(base_size = 12) +
    theme(axis.title = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(vjust = 1, hjust = 1, angle = 45),
          axis.ticks.x = element_line()),
  scale_y_continuous(expand = expand_scale(mult= c(0, .05)))
)



# Barplots ------------

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
    ggtitle("Gender") +
    custom_barplot_theme
}

ageBarplot <- function(data) {
  # Transform data so one row per participant
  td <- data[, .(age = unique(age)), by = "subjectid"]
  age_order <- c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "> 70", "Unknown")
  
  # plot data
  ggplot(td, aes(age)) +
    geom_bar() + 
    ggtitle("Age") +
    scale_x_discrete(limits = age_order) +
    custom_barplot_theme
}

raceBarplot <- function(data) {
  # Transform data so one row per participant
  td <- data[, .(race = unique(race)), by = "subjectid"]
  td$race <- ifelse(td$race %in% c("Not Specified", "Not_Specified", "Unknown"), "Unknown", td$race)
  raceLabels <- c(
    "American Indian or Alaska Native" = "American Indian or\nAlaska Native",
    "Asian" = "Asian",
    "Black or African American" = "Black or\nAfrican American",
    "Native Hawaiian or Other Pacific Islander" = "Hawaiian or\nPacific Islander", 
    "White" = "White",
    "Other" = "Other",
    "Unknown" = "Unknown")
  td$race <- raceLabels[td$race]
  
  # plot data
  ggplot(td, aes(race)) +
    geom_bar() + 
    ggtitle("Race") +
    custom_barplot_theme +
    scale_x_discrete(limits = raceLabels)
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

# timepoint heatmaps --------

timepointHeatmap <- function(td,
                             breaks = c(0, 10, 50, 100, 500, 1000, Inf),
                             legendLabels = NULL,
                             legendName = "Number of\nSamples",
                             colorScheme = "Greens",
                             abbreviateAssayNames = FALSE) {
  
  timepoints_xaxis <- c("<0", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
                        "13", "14", "15-27", "28", "29-55", "56", ">56")
  
  # Make sure combinations with zero studies have a row, with count = 0.
  d <- expand.grid(assay = unique(td$assay), timepoint = timepoints_xaxis)
  d$count <- unlist(apply(d, 1, function(row){
      tdrow <- which(td$assay == row[1] & td$timepoint == row[2])
      ifelse(length(tdrow) == 0, 0, td[tdrow, "N"])
    }))
  
  # Make map for assay labels
  if (abbreviateAssayNames) {
    
    assayLabels <- c(
      "HLA Typing" = "HLA",
      "ELISPOT" = "ELISPOT", 
      "HAI" = "HAI",
      "Gene Expression" = "GE",
      "CyTOF" = "CyTOF",
      "Flow Cytometry" = "Flow",
      "ELISA" = "ELISA", 
      "MBAA" = "MBAA",
      "Neutralizing Antibody" = "nAb",
      "PCR" = "PCR"
    )
  } else {
    assayLabels <- c(
      "HLA Typing" = "HLA Typing",
      "ELISPOT" = "ELISPOT", 
      "HAI" = "HAI",
      "Gene Expression" = "Gene Expression",
      "CyTOF" = "CyTOF",
      "Flow Cytometry" = "Flow Cytometry",
      "ELISA" = "ELISA", 
      "MBAA" = "MBAA",
      "Neutralizing Antibody" = "Neutalizing Antibody",
      "PCR" = "PCR"
    )
  }
  
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
  assays <- unique(td$assay)[order(unique(td$assay), decreasing = TRUE)]
  
  ggplot(d, aes(timepoint, assay)) + 
    geom_tile(aes(fill = colorIndex)) +
    scale_x_discrete(limits = timepoints_xaxis) +
    scale_y_discrete(limits = assays, labels = assayLabels[assays]) +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(linetype = 1, fill = "transparent"),
          legend.key = element_rect(color= "gray50"),
          panel.grid = element_blank(),
          axis.title = element_blank()) +
    xlab("Study Day") +
    scale_fill_manual(values =  c("white", RColorBrewer::brewer.pal(length(breaks)-1, colorScheme)),
                      limits = as.character(seq_along(legendLabels)),
                      labels = legendLabels,
                      name = legendName) 
}

timepointHeatmap_sample <- function(data) {
  # transform data into matrix with rows for assay, columns for timepoint
  # Get relevant data
  # By sample
  td <- data[, .N, c("timepoint", "assay")]
  
  # Remove samples with no assay data
  td <- td[!is.na(assay) & timepoint != "Unknown"]
  
  timepointHeatmap(td)
  
}

timepointHeatmap_study <- function(data) {
  # Get relevant data
  # By sample
  td <- data[, .N, c("timepoint", "assay", "study")]
  td <-td[, .N, c("timepoint", "assay")]
  
  # Remove samples with no assay data
  td <- td[!is.na(assay) & timepoint != "Unknown"]
  
  timepointHeatmap(td, 
                   breaks = c(0, 1, 2, 3, 4, 5, 10, Inf),
                   legendLabels = c("0", "1", "2", "3", "4", "5", "6-10", "11+"),
                   legendName = "Number of\nStudies",
                   colorScheme = "Purples") +
    theme(axis.title = element_text(),
          axis.title.y = element_blank())
}

timepointHeatmap_sample_small <- function(data,
                                          breaks = c(0, 5, 10, 50, Inf)) {
  
  # Get relevant data
  # By sample
  td <- data[, .N, c("timepoint", "assay", "study")]
  
  # Remove samples with no assay data
  td <- td[!is.na(assay) & timepoint != "Unknown"]

  timepointHeatmap(td, 
                   breaks = breaks,
                   abbreviateAssayNames = TRUE) +
    coord_equal() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.text = element_text(angle = 45, hjust = 0.2, vjust = .5),
          axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.title = element_text(color = "black"),
          axis.title.y = element_blank()) +
    guides(fill = guide_legend(label.position = "bottom"))
  
}


# Other ------------

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
