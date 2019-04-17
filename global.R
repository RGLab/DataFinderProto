# Configurables
uiOption <- 2

# Load libraries
library(shiny)
library(shinyjs)
# library(bsplus)
library(ImmuneSpaceR)
library(Rlabkey)
library(ggplot2)
library(data.table)
# library(UpSetR)

# Set up cache
# Be sure to wipe cache if plotting function changes
shinyOptions(cache = diskCache("~/shiny-cache/DataFinderProto"))

# Source helper files
helperFiles <- list.files("helpers")
for (file in helperFiles) {
  source(file.path("helpers", file))
} 


# Get the data
# For testing use local data
data <- get(load("data/cube.RData"))
# data <- getData()
