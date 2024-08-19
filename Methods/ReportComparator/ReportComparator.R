getPrimeReport<-function(dataReport, myFunc, reportNum){
  output<-myFunc(dataReport[[reportNum]]@SelectedData %>% filter(SPEC_DATE >= dataReport[[reportNum]]@StartTime &
                                                                    SPEC_DATE <= dataReport[[reportNum]]@EndTime))
  return(output)
}

CompareReports<-function(dataReportList, prepFunc){
  outputReport<-getPrimeReport(dataReportList, prepFunc, 1)
  if(!is.null(outputReport)){
    dataColNum<-ncol(outputReport)-1
    currNames<-colnames(outputReport)[-1]
    bindCol<-colnames(outputReport)[1]
    nonEmpty<-1
  }
  colNamesArr<-c("", "Отчёт 1")
  repNum = length(unlist(dataReportList))
  if(repNum>1){
    for(i in 2:repNum){
      if(is.null(outputReport)){
        outputReport = getPrimeReport(dataReportList, prepFunc, i)
        if(!is.null(outputReport)){
          dataColNum<-ncol(outputReport)-1
          currNames<-colnames(outputReport)[-1]
          bindCol<-colnames(outputReport)[1]
          nonEmpty<-1
        }
      }else{
        tempTab <- getPrimeReport(dataReportList, prepFunc, i)
        if(!is.null(tempTab)){
          colNamesArr<-c(colNamesArr, paste0("Отчёт ", i))
          outputReport<-merge(outputReport, tempTab, by.x = bindCol, by.y = bindCol, all.x = TRUE, all.y = TRUE)
          nonEmpty<-nonEmpty + 1
        }
      }
    }
  }
  if(is.null(outputReport)){
    return(NULL)
  }
  if(dataColNum > 1){
    colNamesArr = ""
    for(i in 1:nonEmpty){
      repName = paste0("Отчёт ",i, ":")
      colNamesArr<-c(colNamesArr, paste(repName, currNames))
    }
  }
  colnames(outputReport)<-colNamesArr
  outputReport[is.na(outputReport)]<-""
  return(outputReport)
}

