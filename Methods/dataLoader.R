getLastMonth<-function(day, month, year){
  lastDayLastMonth <- as.Date(paste0(year,"-",month,"-", 1), format = "%Y-%m-%d") - 1
  lDay <- as.numeric(format(lastDayLastMonth, format = "%d"))
  if(day > lDay){
    return(as.Date(paste0(cYear,"-",cMon - 1,"-", lDay), format = "%Y-%m-%d"))
  }
  return(as.Date(paste0(cYear,"-",cMon - 1,"-", day), format = "%Y-%m-%d"))
}
getYearsPattern<-function(StartDate, EndDate){
  firstYear<-as.numeric(format(StartDate, format = "%y"))
  lastYear<-as.numeric(format(EndDate, format = "%y"))
  return(c(firstYear:lastYear))
}
currDat<-Sys.Date()
cMon<-as.numeric(format(currDat, format = "%m"))
cDay<-as.numeric(format(currDat, format = "%d"))
cYear<-as.numeric(format(currDat, format = "%Y"))
startDat<-getLastMonth(day = cDay, month = cMon, year = cYear)
wards<-read.table("/home/aleksandr/Эпидемиологическое приложение/EpidModuleApp/Data/Wards.txt", sep = "\t", header = TRUE)
repoDir<-"Reports/"