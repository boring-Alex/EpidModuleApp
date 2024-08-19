library(dplyr)
library(knitr)
library(DT)
library(foreign)
library(digest)
library(bslib)
library(plotly)

source("Methods/supplementCalc.R", chdir = TRUE)
source("Methods/dbfLoader.R", chdir = TRUE)
source("Methods/dataLoader.R", chdir = TRUE)
source("Methods/orgCodeSelector.R", chdir = TRUE)
source("Methods/orglistLoader.R", chdir = TRUE)
source("Methods/uiReportRender.R", chdir = TRUE)
source("Methods/tabUiGenerator.R", chdir = TRUE)

loadFiles<-function(pathToFolder){
  for(file in list.files(pathToFolder,
                         full.names = TRUE, recursive = TRUE)){
    if(grepl("\\.R", file, ignore.case = TRUE)){
      source(file)
    }
  }
}
## Methods Loader Section
loadFiles("Methods")
## End Methods Loader Section

## Modules Loader Section
loadFiles("Modules/UIs")
loadFiles("Modules/Servers")
## End Modules Loader Section

## Graphics loader
loadFiles("CustomUi")
##End Graphics loader
