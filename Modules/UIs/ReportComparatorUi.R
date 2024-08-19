ReportComparatorUi<-function(myTheme, id = "ReportComparator"){
  ns<-NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        dateInput(ns("startDate"),
                  "Дата начала отбора",
                  value = startDat),
        dateInput(ns("endDate"),
                  "Дата окончания отбора",
                  value = currDat),
        selectInput(ns("wardsChoose"),
                    "Отделение",
                    choices = wards$Name),
        actionButton(ns("addToCompare"), "Добавить к сравнению")
      ),
      mainPanel(
        bslib::accordion(
          bslib::accordion_panel(
            "Выбранные группы для сравнения",
            uiOutput(ns("selectedGroups"))
          ),
          uiOutput(ns("reportsOutput"))
        )
      )
    )
  )
}

renderPreview<-function(id, inputData){
  ns<-NS(id)
  if(is.null(inputData)){
    ui<-renderUI(h4(renderText("Ничего не выбрано")))
    return(ui)
  }
  ui<-tagList(
      DTOutput(ns("previewTab")),
      flowLayout(
        actionButton(ns("deleteSelected"), "Удалить выбранное"),
        actionButton(ns("Compare"), "Начать сравнение")
      )
    )
  return(renderUI(ui))
}