ReportComparatorServer<-function(myTheme, id = "ReportComparator"){
  moduleServer(
    id, function(input, output, session){
      ns<-NS(id)
      selectedGrp<-reactiveVal(NULL)
      observeEvent(input$addToCompare,{
        table<-selectedGrp()
        grpNew<-new("ComparatorGroup",
                    Name = input$wardsChoose,
                    Code = wards[wards$Name == input$wardsChoose, 3][1],
                    StartDate = input$startDate,
                    EndDate = input$endDate)
        if(is.null(table) || nrow(table) == 0){
          table<-data.frame(Name = grpNew@Name,
                            Code = grpNew@Code,
                            StartDate = grpNew@StartDate,
                            EndDate = grpNew@EndDate)
        }else{
          tmp<-data.frame(Name = grpNew@Name,
                          Code = grpNew@Code,
                          StartDate = grpNew@StartDate,
                          EndDate = grpNew@EndDate)
          table<-rbind(table, tmp)
        }
        selectedGrp(table)
      })
      observe({
        output$selectedGroups <- renderPreview(id, selectedGrp())
        if(!is.null(selectedGrp())){
          tab<-selectedGrp()
          colnames(tab)<-c("Название отделения"," Код WHONET","С","По")
          output$previewTab <- renderDT(tab, selection = 'single')
        }
      })
      observeEvent(input$deleteSelected,{
        selectedRec<-input$previewTab_rows_selected
        temp<-selectedGrp()
        temp<-temp[-selectedRec,]
        selectedGrp(temp)
      })
      observeEvent(input$Compare, {
        compGroups<-selectedGrp()
        datesArr<-c(compGroups[,3],compGroups[,4])
        minDate<-min(datesArr)
        maxDate<-max(datesArr)
        years<-getYearsPattern(minDate, maxDate)
        withProgress(
          message = 'Обработка данных...', value = 0,{
            n = 6
            choosedItems = compGroups[,2]
            incProgress(1/n, detail = "Отбираем отделения...")
            names<-compGroups[,1]
            incProgress(1/n, detail = "Обновляем список файлов...")
            allFiles<-getFileNames("/home/aleksandr/Эпидемиологическое приложение/Data", c("\\.011", "\\.dgr", "\\.dis"), getYearsPattern(minDate, maxDate))
            incProgress(1/n, detail = "Загружаем файлы...")
            allData<-loadFileGroup(allFiles, "/home/aleksandr/Эпидемиологическое приложение/Data","SPEC_DATE", minDate, maxDate, "X_COMLETE")
            incProgress(1/n, detail = "Создаем отчёты...")
            reportData<-list()
            for(i in 1:nrow(compGroups)){
              tList<-LoadWardData(allData, choosedItems[i], compGroups[i,3], compGroups[i,4], names[i])
              reportData<-c(reportData,tList)
            }
            if(length(unlist(reportData)) == 0){
              output$reportsOutput<-renderUI(h4(renderText("Ничего не найдено")))
              return()
            }
            incProgress(1/n, detail = "Генерируем пользовательский интерфейс...")
            output$reportsOutput<-renderUI(
              tagList(
                accordion(
                  accordion_panel(
                    "Общие данные",
                    tableOutput(ns("summTab"))
                  ),
                  accordion_panel(
                    "Общая таблица: энтерококки",
                    tableOutput(ns("EnterococcusTab"))
                  ),
                  accordion_panel(
                    "Виды: энтерококки",
                    tableOutput(ns("EnterococSpec"))
                  ),
                  accordion_panel(
                    "Общая таблица: стафилококки",
                    tableOutput(ns("StaphTab"))
                  ),
                  accordion_panel(
                    "Виды: стафилококки",
                    tableOutput(ns("StaphSpec"))
                  ),
                  accordion_panel(
                    "Общая таблица: энтеробактерии",
                    tableOutput(ns("EBCTab"))
                  ),
                  accordion_panel(
                    "Виды: энтеробактерии",
                    tableOutput(ns("EBCSpec"))
                  ),
                  accordion_panel(
                    "Общая таблица: неферментеры",
                    tableOutput(ns("NfrTab"))
                  ),
                  accordion_panel(
                    "Виды: неферментеры",
                    tableOutput(ns("NfrSpec"))
                  ),
                  accordion_panel(
                    "Общая таблица: Candida",
                    tableOutput(ns("CandidaTab"))
                  ),
                  accordion_panel(
                    "Виды: Candida",
                    tableOutput(ns("CandidaSpec"))
                  )
                )
              )
            )
            incProgress(1/n, detail = "Заполняем данные...")
            output$summTab<-renderTable(CompareReports(reportData, GetSummaryTab))
            output$EnterococcusTab<-renderTable(CompareReports(reportData, EntSummaryTab))
            output$EnterococSpec<-renderTable(CompareReports(reportData, GetEnterococcusSpecTable))
            output$StaphTab<-renderTable(CompareReports(reportData, StaphSummaryTab))
            output$StaphSpec<-renderTable(CompareReports(reportData, GetStaphSpecTable))
            output$EBCTab<-renderTable(CompareReports(reportData, EbcSummaryTab))
            output$EBCSpec<-renderTable(CompareReports(reportData, GetEbcSpecTable))
            output$NfrTab<-renderTable(CompareReports(reportData, NFRSummaryTab))
            output$NfrSpec<-renderTable(CompareReports(reportData, GetNfrSpecTable))
            output$CandidaTab<-renderTable(CompareReports(reportData, CandidaSummaryTab))
            output$CandidaSpec<-renderTable(CompareReports(reportData, GetCandSpecTable))
          }
        )
      })
    }
  )
}