loadDbf<-function(filePath, dateColName, dateStart, dateEnd, completeCol){
  temp<-read.dbf(filePath, as.is = TRUE)
  temp<-temp[!is.na(temp[,dateColName]),]
  temp<-temp[temp[,completeCol] %in% c("1", "2"),]
  temp<-temp[temp[,dateColName] >= dateStart,]
  temp<-temp[temp[,dateColName] <= dateEnd,]
  return(changeEncoding(temp))
}

loadFileGroup<-function(filesNames,
                        fileDirectory,
                        dateColumn,
                        start,
                        end,
                        completeColName){
  allFiles<-unlist(list.files(fileDirectory, recursive = TRUE, full.names = TRUE))
  selectedFiles<-character()
  for(f in filesNames){
    for(p in allFiles){
      if(grepl(f,p, ignore.case = TRUE)){
        selectedFiles<-c(selectedFiles, p)
        break;
      }
    }
  }
  fullData<-data.frame()
  for(i in 1:length(selectedFiles)){
    tmp = loadDbf(selectedFiles[i],
                  dateColumn,
                  start,
                  end,
                  completeColName)
    if(i==1){
      fullData = tmp
    }else{
      fullData = bind_rows(fullData, tmp)
    }
  }
  return(fullData)
}

changeEncoding<-function(dataFrame){
  for(i in 1:ncol(dataFrame)){
    dataFrame[,i]<-iconv(dataFrame[,i], from = "CP866", to = "UTF-8")
  }
  return(dataFrame)
}

getFileNames<-function(directory, extension, filePattren){
  files<-character()
  for(i in 1:length(extension)){
    files<-c(files,unlist(list.files(directory, pattern = extension[i], recursive = TRUE, ignore.case = TRUE)))
  }
  outputFiles<-character()
  for(i in 1:length(files)){
    for(j in 1:length(filePattren)){
      if(grepl(filePattren[j],files[i], ignore.case = TRUE)){
        fNamePath = unlist(strsplit(files[i],"/"))
        fName = fNamePath[length(fNamePath)]
        outputFiles<-c(outputFiles, fName)
        break
      }
    }
  }
  return(outputFiles)
}