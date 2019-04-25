
if (FALSE) {
  
  timepointHeatmap_study <- function(data) {
    # Get relevant data
    # By sample
    td <- data[, .N, c("timepoint", "assay", "study")]
    td <-td[, .N, c("timepoint", "assay")]
    
    # Remove samples with no assay data
    td <- td[!is.na(assay) & timepoint != "Unknown"]
    
    timepointHeatmap(td, 
                     breaks = c(0, 1, 2, 5, 10, Inf),
                     legendLabels = c("0", "1", "2", "3-5", "6-10", "11+"),
                     legendName = "Number of\nStudies",
                     colorScheme = "Purples") +
      theme(axis.title = element_text(),
            axis.title.y = element_blank())
  }
  
  
  
  timepoints_xaxis <- c("<0", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
                        "13", "14", "15-27", "28", "29-55", "56", ">56")
  
  # Make sure combinations with zero studies have a row, with count = 0.
  d <- expand.grid(assay = unique(td$assay), timepoint = timepoints_xaxis, stringsAsFactors = FALSE)
  d$count <- unlist(apply(d, 1, function(row){
    tdrow <- which(td$assay == row[1] & td$timepoint == row[2])
    ifelse(length(tdrow) == 0, 0, td[tdrow, "N"])
  }))
  
  # Turn into matrix
  mx <- tidyr::spread(d, timepoint, as.numeric(count) )
  mx$assay[is.na(mx$assay)] <- "NA"
  rownames(mx) <- mx$assay
  mx <- mx[,-1]
  d3heatmap(mx, Rowv = FALSE, Colv = FALSE, colors = "Greens")
  
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
}
