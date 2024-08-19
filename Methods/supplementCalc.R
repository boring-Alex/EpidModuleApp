CalcPercent<-function(partNum, allNum){
  if(allNum == 0){
    return(0)
  }else{
    return(round(partNum*100/allNum, digits = 1))
  }
}

CalcK<-function(thisSpec, allSpec){
  if(allSpec == 0){
    return(1)
  }else{
    return(round(1-(thisSpec/allSpec),digits = 2))
  }
}

renderMdReport<-function(dataInput, reportDir){
  fName = paste(dataInput@WardName, "c", dataInput@StartTime, "по", dataInput@EndTime, ".html")
  Year = format(dataInput@EndTime,format = "%Y")
  Month = format(dataInput@EndTime, forma = "%m")
  Day = format(dataInput@EndTime, format = "%d")
  pathToFolder = file.path(reportDir, Year, Month, Day)
  dir.create(pathToFolder, recursive = TRUE)
  rmarkdown::render("report.Rmd", params = list(
    dataReport = dataInput
  ),
  output_file = fName, 
  output_dir = pathToFolder,
  output_format = "html_document"
  )
}