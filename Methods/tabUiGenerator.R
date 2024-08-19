setClass("ReportData",
         representation(
          WardName = "character",
          StartTime = "Date",
          EndTime = "Date",
          SelectedData = "data.frame"
         ))

setGeneric("LoadWardData", function(dataInput, wardsList, startD, endD, wardName){
  standardGeneric("LoadWardData")
})

setMethod("LoadWardData",
          signature("data.frame",
                    "character",
                    "Date",
                    "Date",
                    "character"),
          function(dataInput,
                   wardsList,
                   startD,
                   endD,
                   wardName){
  outputList<-list()
  for(i in 1:length(wardsList)){
    tempData <- dataInput %>% filter(DEPARTMENT == wardsList[i])
    if(nrow(tempData)>0){
      element<-new("ReportData")
      element@WardName = wardName[i]
      element@StartTime = startD
      element@EndTime = endD
      element@SelectedData = tempData
      outputList<-c(outputList, element)
    }
  }
  return(outputList)
})

setGeneric("GetReportsNames", function(reportList){
  standardGeneric("GetReportsNames")
})

setMethod("GetReportsNames", signature("list"), function(reportList){
  output<-character()
  for(i in 1:length(unlist(reportList))){
    output<-c(output, reportList[[i]]@WardName)
  }
  return(output)
})

setGeneric("BuildPreview", function(dataReport){
  standardGeneric("BuildPreview")
})

setMethod("BuildPreview", signature("ReportData"), function(dataReport){
  if(nrow(dataReport@SelectedData) == 0){
    return(renderUI(renderText("Нет данных")))
  }
  ui<-renderUI({
    tagList(
      h2(renderText(GetHeader(dataReport@WardName, dataReport@StartTime, dataReport@EndTime))),
      h3(renderText({"\nТаблица 1. Сводка результатов"})),
      renderTable(GetSummaryTab(dataReport@SelectedData)),
      h3(renderText("\nВыделенные микроорганизмы")),
      h4(renderText("\nI.	Энтерококки")),
      h4(renderText("Таблица 2. Сводная информация по группе «энтерококки»")),
      renderTable(GetSummary(dataReport@SelectedData)$Enterococcus),
      h4(renderText("Таблица 3. Видовая структура Enterococcus sp.")),
      renderTable(GetEnterococcusSpecTable(dataReport@SelectedData)),
      h4(renderText("\nII.	Стафилококки")),
      h4(renderText("Таблица 4. Сводная информация по группе «стафилококки»")),
      renderTable(GetSummary(dataReport@SelectedData)$Staph),
      h4(renderText("Таблица 5. Видовая структура Staphylococcus sp.")),
      renderTable(GetStaphSpecTable(dataReport@SelectedData)),
      h4(renderText("\nIII.	Энтеробактерии")),
      h4(renderText("Таблица 6. Сводная информация по группе «энтеробактерии»")),
      renderTable(GetSummary(dataReport@SelectedData)$EBC),
      h4(renderText("Таблица 7. Видовая структура Энтеробактерий")),
      renderTable(GetEbcSpecTable(dataReport@SelectedData)),
      h4(renderText("\nIV.	Неферментирующие грамотрицательные бактерии")),
      h4(renderText("Таблица 8. Сводная информация по группе «НГОБ»")),
      renderTable(GetSummary(dataReport@SelectedData)$NFR),
      h4(renderText("Таблица 9. Видовая структура НГОБ")),
      renderTable(GetNfrSpecTable(dataReport@SelectedData)),
      h4(renderText("\nV.	грибы рода Candida")),
      h4(renderText("Таблица 10. Сводная информация по группе «Candida»")),
      renderTable(GetSummary(dataReport@SelectedData)$Cand),
      h4(renderText("Таблица 11. Видовая структура Candida sp.")),
      renderTable(GetCandSpecTable(dataReport@SelectedData)),
    )
  })
  return(ui)
})

setGeneric("BuildMarkDown", function(dataReport){
  standardGeneric("BuildMarkDown")
})

setMethod("BuildMarkDown", signature("ReportData"), function(dataReport){
  
})

BuildReport<-function(inputData){
  list(Preview = BuildPreview(inputData),
       Markdown = BuildMarkDown(inputData)
       )
}