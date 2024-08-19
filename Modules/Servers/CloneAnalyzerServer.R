CloneAnalyzerServer<-function(myTheme, id = "CloneAnalyzer"){
  moduleServer(id,function(input, output, session){
    ns<-NS(id)
    orgSelectorType<-reactiveVal("Вид")
    organismsGroup<-reactiveVal(OrgList %>% filter(SCT_TEXT != "") %>% distinct(SCT_TEXT))
    graphType<-reactiveVal(data.frame(Name = c("Точечная диаграмма", "Иерархический кластерный анализ"),
                                      Value = c("test1", "test2")))
    distType<-reactiveVal(data.frame(Name = c("Евклидова",
                                              "Максимальная",
                                              "Манхеттеновская",
                                              "Минковского",
                                              "Канберра",
                                              "Двоичная"),
                                     Value = c("euclidean",
                                               "maximum",
                                               "minkowski",
                                               "manhattan",
                                               "canberra",
                                               "binary")))
    clustMethod<-reactiveVal(data.frame(Name = c("Уорда","Одиночный","Полный","Метод средних","Маккуити","Медианный","Метод центроидов"),
                                        Values = c("ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")))
    observe({
      if(input$SelectionType == orgSelectorType()){
        organismsGroup(OrgList %>% filter(SCT_TEXT != "") %>% distinct(SCT_TEXT))
      }else{
        organismsGroup(OrgList %>% filter(GENUS != "") %>% distinct(GENUS))
      }
      updateSelectInput(session,"Organism", choices = organismsGroup())
    })
    
  })
}