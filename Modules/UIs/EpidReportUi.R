EpidReportUi<-function(myTheme, id = "EpidReport"){
  ns<-NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        accordion(
          accordion_panel(
            "Выбор дат",
            dateInput(ns("startDate"),
                      "Дата начала отбора",
                      value = startDat),
            dateInput(ns("endDate"),
                      "Дата окончания отбора",
                      value = currDat)
          ),
          accordion_panel(
            "Отделения",
            actionButton(ns("selectAll"), "Выбрать все"),
            checkboxGroupInput(ns("wardsChoose"),
                               "Выберите отделения",
                               choiceNames = wards$Name,
                               choiceValues = wards$Code)
          )
        ),
        actionButton(ns("createReports"), "Создать отчёт(ы)")
      ),
      mainPanel(
        uiOutput(ns("selectUI")),
        uiOutput(ns("exportButton")),
        uiOutput(ns("reportPreview"))
      )
    )
  )
}