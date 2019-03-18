# GetData.R
# This function will pull the relevant data from the labkey server and 
# get it into the correct format. This dataset will be static within an
# instance of the app and filters will be applied to it to create the 
# "reactive" version of the data for plotting. 
getData <- function(baseUrl = "https://www.immunespace.org", 
                    folderPath = "/Studies/") {
  
  # pull in all relevant tables (immport.dim*)
  
  # Tables to pull from:
  # demographic
  dimDemographic <- data.table(labkey.selectRows(baseUrl,
                                                 folderPath,
                                                 schemaName = "immport",
                                                 queryName = "dimdemographic",
                                                 colNameOpt = "fieldname"))
  # sample
  dimSample <- data.table(labkey.selectRows(baseUrl,
                                            folderPath,
                                            schemaName = "immport", 
                                            queryName = "dimsample",
                                            colNameOpt = "fieldname"))
  dimSample[, sample_type := type]
  dimSample[, type := NULL]
  
  # sampleassay
  dimSampleassay <- data.table(labkey.selectRows(baseUrl,
                                                 folderPath,
                                                 schemaName = "immport", 
                                                 queryName = "dimsampleassay",
                                                 colNameOpt = "fieldname"))
  
  # study
  dimStudy <- data.table(labkey.selectRows(baseUrl,
                                           folderPath,
                                           schemaName = "immport", 
                                           queryName = "dimstudy",
                                           colNameOpt = "fieldname"))
  dimStudy[, c("study_type", "study_sortorder") := list(type, sortorder)]
  dimStudy[, c("type", "sortorder") := c(NULL, NULL)]
  
  
  # studycondition -- come back to this later...
  # TODO: Some studies show up multiple times in this 
  # table. Figure out why and how to deal with that
  dimStudycondition <- data.table(labkey.selectRows(baseUrl = "https://test.immunespace.org",
                                         folderPath = "/Studies",
                                         schemaName = "immport", 
                                         queryName = "dimstudycondition",
                                         colNameOpt = "fieldname"))
  
  
  cube <- merge(dimDemographic, dimSample, by = "subjectid", all = TRUE)
  cube <- merge(cube, dimSampleassay, by = "sampleid", all = TRUE)
  cube <- merge(cube, dimStudy, by = "study", all = TRUE)
  # cube <- merge(cube, dimStudycondition, by = "study", all = TRUE)
  
  # > names(cube)
  # [1] "study"               "sampleid"            "subjectid"           "ageinyears"          "species"            
  # [6] "gender"              "race"                "age"                 "exposure_material"   "exposure_process"   
  # [11] "timepoint"           "timepoint_sortorder" "sample_type"         "assay"               "program"            
  # [16] "study_type"          "study_sortorder"    
  
  return(cube)
  
}
