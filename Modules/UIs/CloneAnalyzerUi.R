CloneAnalyzerUi<-function(myTheme, id = "CloneAnalyzer"){
  ns<-NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        accordion(
          accordion_panel(
            "Даты отбора",
            dateInput(ns("startDate"),
                      "Дата начала отбора",
                      value = startDat),
            dateInput(ns("endDate"),
                      "Дата окончания отбора",
                      value = currDat)
          ),
          accordion_panel(
            "Выбор микроорганизмов",
            selectInput(
              ns("SelectionType"),
              "Выбирать как",
              choices = c("Вид", "Род")
            ),
            selectInput(ns("Organism"),
                        "Микроорганизм (группа)",
                        choices = c("Staphylococcus aureus"))
          ),
          accordion_panel(
            "Настройка графиков",
            selectInput(ns("plotType"),
                        "Тип графика",
                        choices = c("Точечная диаграмма", "Иерархический кластерный анализ")),
            selectInput(ns("distanceType"),
                        "Тип расчёта дистанции",
                        choices = c("Евклидова",
                                    "Максимальная",
                                    "Манхеттеновская",
                                    "Минковского",
                                    "Канберра",
                                    "Двоичная")),
            selectInput(ns("clusterMethod"),
                        "Метод кластеризации",
                        choices = c("Уорда","Одиночный","Полный","Метод средних","Маккуити","Медианный","Метод центроидов"))
          )
        ),
        PrimaryButton(ns("createReport"), "Создать графики")
      ),
      mainPanel(
        plotlyOutput(ns("CreatedPlot")),
        DTOutput("Selected")
      )
    )
  )
}