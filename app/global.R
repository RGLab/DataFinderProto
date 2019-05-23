##### DataFinder shiny app ###### 
# This shiny app will serve as a prototype design for an update to the
# datafinder on the IS UI. It should allow users to easily see how metadata
# filters affect composition of assay data available, and the composition of
# studies and participants available.

# Configurables
# uiOption <- 2

# Load libraries
library(shiny)
library(shinyjs)
# library(bsplus)
# library(ImmuneSpaceR)
# library(Rlabkey)
library(ggplot2)
library(data.table)
# library(UpSetR)
# library(plotly)
library(r2d3)

# Set up cache
# Be sure to wipe cache if plotting function changes
shinyOptions(cache = diskCache("plot-cache"))

# Source helper files
helperFiles <- list.files("helpers")
for (file in helperFiles) {
  source(file.path("helpers", file))
} 


# Get the data
# For testing use local data
data <- get(load("data/cube.RData"))
# data <- getData()
