library(shiny)
source("headers.R")
mainTheme<-bslib::bs_theme(
  bootswatch = "minty"
)

ui <- shinyUI(
  fluidPage(
    navbarPage(
      collapsible = TRUE,
      title="Эпидемиологические отчёты",
      theme = mainTheme,
      tabPanel("Клиническая микробиология",
               EpidReportUi(mainTheme)),
      tabPanel("Сравнение клинических отчётов",
               ReportComparatorUi(mainTheme)),
      tabPanel("Анализ клональной структуры",
               CloneAnalyzerUi(mainTheme))
    ),
    hr(),
    print("Developed by A. Stepanov")
  )
)

server <- function(input, output, session) {
  EpidReportServer(mainTheme)
  ReportComparatorServer(mainTheme)
  CloneAnalyzerServer(mainTheme)
}

shinyApp(ui = ui, server = server)