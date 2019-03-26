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
  setnames(dimSample, "type", "sample_type")
  
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
  setnames(dimStudy, c("type", "sortorder"), c("study_type", "study_sortorder"))
  
  # studycondition -- come back to this later...
  # TODO: There are some studies (eg SDY201, SDY614) which have multiple conditions,
  # and therefore have multiple rows in this table. Determine the best method for dealing with that 
  dimStudycondition <- data.table(labkey.selectRows(baseUrl ,
                                         folderPath,
                                         schemaName = "immport", 
                                         queryName = "dimstudycondition",
                                         colNameOpt = "fieldname"))
  
  
  cube <- merge(dimDemographic, dimSample, by = "subjectid", all = TRUE)
  cube <- merge(cube, dimSampleassay, by = "sampleid", all = TRUE)
  cube <- merge(cube, dimStudy, by = "study", all = TRUE)
  # allow.cartesian = TRUE means that for studies with multiple rows in
  # dimStudycondition (ie multiple conditions for one study) each row will be
  # duplicated so that eg for a study with two conditions there will be two rows
  # where everything is the same except study condition
  cube <- merge(cube, dimStudycondition, by = "study", all = TRUE, allow.cartesian = TRUE)
  
  # Filter to only include studies on ImmuneSpace...
  studies <- labkey.selectRows(
    baseUrl=baseUrl,
    folderPath="/home",
    schemaName="lists",
    queryName="Studies",
    viewName="",
    colSort="id",
    colFilter=NULL,
    containerFilter=NULL
  )
  
  cube <- cube[study %in% studies$Name]
  
  # > names(cube)
  # [1] "study"               "sampleid"            "subjectid"           "ageinyears"          "species"            
  # [6] "gender"              "race"                "age"                 "exposure_material"   "exposure_process"   
  # [11] "timepoint"           "timepoint_sortorder" "sample_type"         "assay"               "program"            
  # [16] "study_type"          "study_sortorder"    
  
  return(cube)
  
}
