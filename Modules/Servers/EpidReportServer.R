EpidReportServer<-function(myTheme, id = "EpidReport"){
  moduleServer(
    id,
    function(input, output, session){
      ns<-NS(id)
      allFiles<-reactiveVal(NULL)
      allData<-reactiveVal(NULL)
      reportData<-reactiveVal(NULL)
      observeEvent(input$selectAll,{
        if(length(input$wardsChoose) == length(wards$Name)){
          updateActionButton(session, "selectAll", label = "Выбрать все")
          updateCheckboxGroupInput(session, "wardsChoose",
                                   choiceNames = wards$Name,
                                   choiceValues = wards$Code,
                                   selected = NULL)
        }else{
          updateActionButton(session, "selectAll", label = "Снять все")
          updateCheckboxGroupInput(session, "wardsChoose",
                                   selected = wards$Code)
        }
      })
      observeEvent(input$createReports,{
        withProgress(
          message = 'Обработка данных...', value = 0,{
          n = 6
          choosedItems = input$wardsChoose
          incProgress(1/n, detail = "Отбираем отделения...")
          names<-wards[wards$Code %in% choosedItems, "Name"]
          incProgress(1/n, detail = "Обновляем список файлов...")
          allFiles(getFileNames("/home/aleksandr/Эпидемиологическое приложение/Data", c("\\.011", "\\.dgr", "\\.dis"), getYearsPattern(input$startDate, input$endDate)))
          incProgress(1/n, detail = "Загружаем файлы...")
          allData(loadFileGroup(allFiles(),"/home/aleksandr/Эпидемиологическое приложение/Data","SPEC_DATE", input$startDate, input$endDate, "X_COMLETE"))
          incProgress(1/n, detail = "Создаем отчёты...")
          reportData(LoadWardData(allData(),choosedItems, input$startDate, input$endDate, names))
          if(length(unlist(reportData())) == 0){
            output$selectUI<-renderUI({h4(renderText("Ничего не найдено..."))})
            return()
          }
          incProgress(1/n, detail = "Генерируем кнопки...")
          nonEmpty<-GetReportsNames(reportData())
          output$selectUI<-renderUI({
            selectInput(
              ns("selectReport"), "Выберите сгенерированный отчёт", choices = nonEmpty, selected = nonEmpty[1])
          })
          output$exportButton<-renderUI({
            tagList(
              checkboxInput(ns("exportAll"),"Экспортировать все сразу", value = FALSE),
              actionButton(ns("exportReport"), "Экспортировать отчёт(ы)")
            )
          })
          incProgress(1/n, detail = "Готово...")
        })
        selectedWard<-nonEmpty[1]
        withProgress(message = 'Подготовка отчета...', value = 0,{
          n = length(unlist(reportData()))
          for(i in 1:n){
            incProgress(1/n, detail = paste("отчёт",i))
            currReport = reportData()[[i]]
            if(currReport@WardName == selectedWard){
              output$reportPreview = renderUI({
                BuildReport(currReport)$Preview
              })
              break
            }
          }
        })
      })
      observeEvent(input$exportReport,{
        if(input$exportAll){
          withProgress(message = 'Генерация отчетов...', value = 0,{
            n = length(unlist(reportData()))
            for(i in 1:n){
              renderMdReport(reportData()[[i]], repoDir)
              incProgress(1/n, detail = paste("Выполнено", i))
            }
          })
          
        }else{
          withProgress(message = 'Генерация отчетов...', value = 0,{
            n = length(unlist(reportData()))
            for(i in 1:n){
              if(reportData()[[i]]@WardName == input$selectReport){
                renderMdReport(reportData()[[i]], repoDir)
                incProgress(1/n, detail = paste("Выполнено", i))
                break
              }
            }
          })
        }
        utils::browseURL(repoDir)
      })
      observeEvent(input$selectReport,{
        selectedWard<-input$selectReport
        for(i in 1:length(unlist(reportData()))){
          currReport = reportData()[[i]]
          if(currReport@WardName == selectedWard){
            output$reportPreview = renderUI({
              BuildReport(currReport)$Preview
            })
            break
          }
        }
      }, ignoreInit = TRUE)
    }
  )
}