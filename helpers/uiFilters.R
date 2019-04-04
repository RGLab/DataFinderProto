# study filters ------
createStudyFilters <- function(data) {
  tagList(
    checkboxGroupInput(inputId = "species", 
                       label = "Species", 
                       choices = unique(data$species)),
    checkboxGroupInput(inputId = "condition", 
                       label = "Disease",
                       choices = unique(data$condition)),
    checkboxGroupInput(inputId = "exposure_material",
                       label = "Vaccine",
                       choices = unique(data$exposure_material)),
    checkboxGroupInput(inputId = "study_type",
                       label = "Study Type",
                       choices = unique(data$study_type)),
    div()
  )
}

# subject filters ------
createSubjectFilters <- function(data) {
  tagList(
    checkboxGroupInput(inputId = "gender",
                       label = "Gender",
                       choices = unique(data$gender)),
    checkboxGroupInput(inputId = "race",
                       label = "Race",
                       choices = unique(data$race)),
    checkboxGroupInput(inputId = "age",
                       label = "Age",
                       choices = unique(data$age)),
    div()
  )
  
}

# sample filters -----
createSampleFilters <- function(data) {
  tagList(
    checkboxGroupInput(inputId = "assay",
                       label = "Assay",
                       choices = unique(data$assay)),
    checkboxGroupInput(inputId = "sample_type",
                       label = "Cell Type",
                       choices = unique(data$sample_type)),
    checkboxGroupInput("timepoint",
                       label = "Day of Study",
                       choices = unique(data$timepoint)),
    div()
  )
  
}