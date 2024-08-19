GetSummary<-function(summaryData){
  list(Staph = StaphSummaryTab(summaryData),
       Enterococcus = EntSummaryTab(summaryData),
       EBC = EbcSummaryTab(summaryData),
       NFR = NFRSummaryTab(summaryData),
       Cand = CandidaSummaryTab(summaryData)
       )
}

GetHeader<-function(wardName, startD, endD){
  outputText<-paste("Результаты микробиологического мониторинга в отделении", wardName,
                    "за период с",startD,"по", endD)
  return(outputText)
}

GetSummaryTab<-function(selectedData){
  col1Names<-c("01. Количество пациентов, получивших результаты микробиологического исследования",
               "02. Количество проведенных исследований",
               "03.    из них нет роста",
               "04. Группы возбудителей",
               "05.    ESKAPE",
               "06.    Метициллинрезистентные стафилококки",
               "07.    Ванкомицинрезистентные стафилококки",
               "08.    Ванкомицин-резистентные энтерококки",
               "09.    БЛРС-продуценты",
               "10.    Карбапенем-устойчивые штаммы")
  patientsNum<-nrow(selectedData %>% distinct(LAST_NAME, PATIENT_ID))
  specNum<-nrow(selectedData %>% distinct(SPEC_DATE, SPEC_TYPE, SPEC_NUM, LAST_NAME, PATIENT_ID))
  noGrouth<-nrow(selectedData %>% filter(ORGANISM == "xxx") %>% distinct(SPEC_DATE, SPEC_TYPE, SPEC_NUM))
  blankRow<-""
  eskapeOrgCodes<-getOrgCodes(c("Enterococcus faecium",
                              "Staphylococcus aureus",
                              "Klebsiella pneumoniae",
                              "Acinetobacter baumannii",
                              "Pseudomonas aeruginosa",
                              "Enterobacter"))
  ESKAPE<-(selectedData %>% filter(ORGANISM %in% eskapeOrgCodes) %>% count())[1,1]
  MRSA<-(selectedData %>% filter(ORGANISM == "sau") %>% filter(FOX_ND30 < 23) %>% count())[1,1]
  MRSA = MRSA + ((selectedData %>% filter(ORGANISM == "sau") %>% filter(FOX_EM >= 8) %>% count()))[1,1]
  VRSA<-(selectedData %>% filter(ORGANISM == "sau") %>% filter(VAN_EM >= 4) %>% count())[1,1]
  enterocccus<-getOrgCodes("Enterococcus")
  VRE<-(selectedData %>% filter(ORGANISM %in% enterocccus) %>% filter(VAN_EM>=4) %>% count())[1,1]
  esblProd<-(selectedData %>% filter(ESBL_PROD == "+") %>% count())[1,1]
  carbaProd<-GetOrgGroupCodes("EBC")
  carbaOrg<-(selectedData %>% filter(ORGANISM %in% carbaProd) %>% filter(IPM_ND10 < 22) %>% count())[1,1]
  carbaOrg<-carbaOrg + ((selectedData %>% filter(ORGANISM %in% carbaProd) %>% filter(MEM_ED10 < 22) %>% count()))[1,1]
  carbaOrg<-carbaOrg + ((selectedData %>% filter(ORGANISM %in% carbaProd) %>% filter(MEM_EM > 2) %>% count()))[1,1]
  carbaOrg<-carbaOrg + ((selectedData %>% filter(ORGANISM %in% carbaProd) %>% filter(IPM_EE > 2) %>% count()))[1,1]
  col2Vals<-c(patientsNum, specNum, noGrouth, blankRow, ESKAPE, MRSA, VRSA, VRE, esblProd, carbaOrg)
  outputDf<-data.frame(`Имя параметра` = col1Names, `Значение` = col2Vals)
  return(outputDf)
}

StaphSummaryTab<-function(inputData){
  temData<- inputData %>% filter(ORGANISM %in% GetOrgGenusCodes("STA"))
  col1Names<-c("Всего выделено изолятов",
               "Метициллин устойчивых, абс (%)",
               "Ванкомицин-устойчивых, абс (%)",
               "Коэффициент видового разнообразия (K)")
  allIsol<-nrow(temData)
  mrsaCount<-(temData %>% filter(FOX_ND30 < 23) %>% count() + temData %>% filter(FOX_EM >= 8) %>% count())
  mrsaString <- paste0(mrsaCount, " (",CalcPercent(mrsaCount, allIsol),")")
  vrsaCount<-(temData %>% filter(VAN_EM >= 4) %>% count())[1,1]
  vrsaString <- paste0(vrsaCount, " (",CalcPercent(vrsaCount, allIsol),")")
  GrowthCount<- inputData %>% filter(ORGANISM != "xxx")
  coeffK<-CalcK(nrow(temData),nrow(GrowthCount))
  col2Vals<-c(allIsol, mrsaString, vrsaString, coeffK)
  return(data.frame(`Параметр` = col1Names, `Значение` = col2Vals))
}

EntSummaryTab<-function(inputData){
  temData<- inputData %>% filter(ORGANISM %in% GetOrgGenusCodes("ENT"))
  col1Names<-c("Всего выделено изолятов",
               "Ванкомицин-устойчивых, абс (%)",
               "Коэффициент видового разнообразия (K)")
  allIsol<-nrow(temData)
  vrsaCount<-(temData %>% filter(VAN_EM >= 4) %>% count())[1,1]
  vrsaString <- paste0(vrsaCount, " (",CalcPercent(vrsaCount,allIsol),")")
  GrowthCount<- inputData %>% filter(ORGANISM != "xxx")
  coeffK<-CalcK(nrow(temData),nrow(GrowthCount))
  col2Vals<-c(allIsol, vrsaString, coeffK)
  return(data.frame(`Параметр` = col1Names, `Значение` = col2Vals))
}

EbcSummaryTab<-function(inputData){
  temData<- inputData %>% filter(ORGANISM %in% GetOrgGroupCodes("EBC"))
  col1Names<-c("Всего выделено изолятов",
               "БЛРС, абс (%)",
               "карбапенем-устойчивых, абс (%)",
               "Коэффициент видового разнообразия (K)")
  allIsol<-nrow(temData)
  esblCount<-(temData %>% filter(ESBL_PROD == "+") %>% count())[1,1]
  EsblString <- paste0(esblCount, " (",CalcPercent(esblCount, allIsol),")")
  mblCount <- (temData %>% filter(MEM_EM >= 4) %>% count())[1,1] +
    (temData %>% filter(MEM_ED10 < 22) %>% count())[1,1] +
    (temData %>% filter(IPM_ND10 < 22) %>% count())[1,1] +
    (temData %>% filter(IPM_EE > 2) %>% count())[1,1]
  mblString<-paste0(mblCount, " (",CalcPercent(mblCount, allIsol),")")
  GrowthCount<- inputData %>% filter(ORGANISM != "xxx")
  coeffK<-CalcK(nrow(temData),nrow(GrowthCount))
  col2Vals<-c(allIsol, EsblString, mblString, coeffK)
  return(data.frame(`Параметр` = col1Names, `Значение` = col2Vals))
}

NFRSummaryTab<-function(inputData){
  temData<- inputData %>% filter(ORGANISM %in% GetOrgGroupCodes("NFR"))
  col1Names<-c("Всего выделено изолятов",
               "карбапенем-устойчивых, абс (%)",
               "Коэффициент видового разнообразия (K)")
  allIsol<-nrow(temData)
  mblCount <- (temData %>% filter(MEM_EM >= 4) %>% count())[1,1] +
    (temData %>% filter(MEM_ED10 < 22) %>% count())[1,1] +
    (temData %>% filter(IPM_ND10 < 22) %>% count())[1,1] +
    (temData %>% filter(IPM_EE > 2) %>% count())[1,1]
  mblString<-paste0(mblCount, " (",CalcPercent(mblCount,allIsol),")")
  GrowthCount<- inputData %>% filter(ORGANISM != "xxx")
  coeffK<-CalcK(nrow(temData),nrow(GrowthCount))
  col2Vals<-c(allIsol, mblString, coeffK)
  return(data.frame(`Параметр` = col1Names, `Значение` = col2Vals))
}

CandidaSummaryTab<-function(inputData){
  temData<- inputData %>% filter(ORGANISM %in% GetOrgGroupCodes("FUNGUS"))
  col1Names<-c("Всего выделено изолятов",
               "Устойчивых к флуконазолу, абс (%)",
               "Устойчивых к вориконазолу, абс (%)",
               "Коэффициент видового разнообразия (K)")
  allIsol<-nrow(temData)
  fcnCount <- (temData %>% filter(X_1_ED40 <= 20) %>% count())[1,1] +
    (temData %>% filter(FLU_EM > 2) %>% count())[1,1]
  fcnString<-paste0(fcnCount, " (",CalcPercent(fcnCount,allIsol),")")
  vorCount <- (temData %>% filter(VOR_ND1 <= 17) %>% count())[1,1] +
    (temData %>% filter(VOR_EM >= 0.25) %>% count())[1,1]
  vorString<-paste0(vorCount, " (",CalcPercent(vorCount,allIsol),")")
  GrowthCount<- inputData %>% filter(ORGANISM != "xxx")
  coeffK<-CalcK(nrow(temData),nrow(GrowthCount))
  col2Vals<-c(allIsol, fcnString, vorString, coeffK)
  return(data.frame(`Параметр` = col1Names, `Значение` = col2Vals))
}

getSpecTable<-function(inputData, groupName, as.Genus = TRUE){
  orgCodes = character()
  if(as.Genus){
    orgCodes = GetOrgGenusCodes(groupName)
  }else{
    orgCodes = GetOrgGroupCodes(groupName)
  }
  temData <- inputData %>% filter(ORGANISM %in% orgCodes)
  return(temData)
}

GetStaphSpecTable<-function(loadedData){
  prepData <- getSpecTable(loadedData, "STA", TRUE)
  countOrgs <- nrow(prepData)
  if(countOrgs == 0){
    return(NULL)
  }
  allSpecCounted <- prepData %>% count(ORGANISM)
  colnames(allSpecCounted)<-c("ORGANISM", "Всего")
  mrsaCounted <- prepData %>% filter(FOX_ND30 < 23 | FOX_EM>4) %>% count(ORGANISM)
  colnames(mrsaCounted)<-c("ORGANISM", "MRSA")
  vrsaCounted <- prepData %>% filter(VAN_EM > 4) %>% count(ORGANISM)
  colnames(vrsaCounted)<-c("ORGANISM", "VRSA")
  kVals <- lapply(allSpecCounted$`Всего`, function(x, y){
    result<-character()
    for(i in 1:length(unlist(x))){
      result<-c(result, CalcK(x[[i]],y))
    }
    return(result)
  }, countOrgs)
  resultTable<-merge(allSpecCounted, mrsaCounted, by.x = "ORGANISM", by.y = "ORGANISM", all.x = TRUE)
  resultTable<-merge(resultTable, vrsaCounted, by.x = "ORGANISM", by.y = "ORGANISM", all.x = TRUE)
  resultTable$`Коэффициент разнообразия` = unlist(kVals)
  resultTable<-merge(resultTable, OrgList[,c("ORG", "SCT_TEXT")], by.x = "ORGANISM", by.y = "ORG")
  resultTable<-resultTable[,c(6,2,3,4,5)]
  colnames(resultTable)[1]<-"Вид"
  resultTable[is.na(resultTable)]<-0
  return(resultTable)
}

GetEnterococcusSpecTable<-function(loadedData){
  prepData <- loadedData %>% filter(ORGANISM %in% GetOrgGenusCodes("ENT"))
  countOrgs <- nrow(prepData)
  if(countOrgs == 0){
    return(NULL)
  }
  allSpecCounted <- prepData %>% count(ORGANISM)
  colnames(allSpecCounted)<-c("ORGANISM", "Всего")
  vrsaCounted <- prepData %>% filter(VAN_EM > 4) %>% count(ORGANISM)
  colnames(vrsaCounted)<-c("ORGANISM", "VRSA")
  kVals <- lapply(allSpecCounted$`Всего`, function(x, y){
    result<-character()
    for(i in 1:length(unlist(x))){
      result<-c(result, CalcK(x[[i]],y))
    }
    return(result)
  }, countOrgs)
  resultTable<-merge(allSpecCounted, vrsaCounted, by.x = "ORGANISM", by.y = "ORGANISM", all.x = TRUE)
  resultTable$`Коэффициент разнообразия` = unlist(kVals)
  resultTable<-merge(resultTable, OrgList[,c("ORG", "SCT_TEXT")], by.x = "ORGANISM", by.y = "ORG")
  resultTable<-resultTable[,c(5,2,3,4)]
  colnames(resultTable)[1]<-"Вид"
  resultTable[is.na(resultTable)]<-0
  return(resultTable)
}

GetEbcSpecTable<-function(loadedData){
  prepData <- getSpecTable(loadedData, "EBC", FALSE)
  countOrgs <- nrow(prepData)
  if(countOrgs == 0){
    return(NULL)
  }
  allSpecCounted <- prepData %>% count(ORGANISM)
  colnames(allSpecCounted)<-c("ORGANISM", "Всего")
  esblCounted <- prepData %>% filter(ESBL_PROD == "+") %>% count(ORGANISM)
  colnames(esblCounted)<-c("ORGANISM", "ESBL")
  mblCounted <- prepData %>% filter(MEM_ED10 < 22 | MEM_ED10 < 22 | IPM_EE > 2 | MEM_EM > 2) %>% count(ORGANISM)
  colnames(mblCounted)<-c("ORGANISM", "MBL")
  kVals <- lapply(allSpecCounted$`Всего`, function(x, y){
    result<-character()
    for(i in 1:length(unlist(x))){
      result<-c(result, CalcK(x[[i]],y))
    }
    return(result)
  }, countOrgs)
  resultTable<-merge(allSpecCounted, esblCounted, by.x = "ORGANISM", by.y = "ORGANISM", all.x = TRUE)
  resultTable<-merge(resultTable, mblCounted, by.x = "ORGANISM", by.y = "ORGANISM", all.x = TRUE)
  resultTable$`Коэффициент разнообразия` = unlist(kVals)
  resultTable<-merge(resultTable, OrgList[,c("ORG", "SCT_TEXT")], by.x = "ORGANISM", by.y = "ORG")
  resultTable<-resultTable[,c(6,2,3,4,5)]
  colnames(resultTable)[1]<-"Вид"
  resultTable[is.na(resultTable)]<-0
  return(resultTable)
}

GetNfrSpecTable<-function(loadedData){
  prepData <- loadedData %>% filter(ORGANISM %in% GetOrgGroupCodes("NFR"))
  countOrgs <- nrow(prepData)
  if(countOrgs == 0){
    return(NULL)
  }
  allSpecCounted <- prepData %>% count(ORGANISM)
  colnames(allSpecCounted)<-c("ORGANISM", "Всего")
  mblCounted <- prepData %>% filter(MEM_ED10 < 22 | MEM_ED10 < 22 | IPM_EE > 2 | MEM_EM > 2) %>% count(ORGANISM)
  colnames(mblCounted)<-c("ORGANISM", "MBL")
  kVals <- lapply(allSpecCounted$`Всего`, function(x, y){
    result<-character()
    for(i in 1:length(unlist(x))){
      result<-c(result, CalcK(x[[i]],y))
    }
    return(result)
  }, countOrgs)
  resultTable<-merge(allSpecCounted, mblCounted, by.x = "ORGANISM", by.y = "ORGANISM", all.x = TRUE)
  resultTable$`Коэффициент разнообразия` = unlist(kVals)
  resultTable<-merge(resultTable, OrgList[,c("ORG", "SCT_TEXT")], by.x = "ORGANISM", by.y = "ORG")
  resultTable<-resultTable[,c(5,2,3,4)]
  colnames(resultTable)[1]<-"Вид"
  resultTable[is.na(resultTable)]<-0
  return(resultTable)
}

GetCandSpecTable<-function(loadedData){
  prepData <- getSpecTable(loadedData, "FUNGUS", FALSE)
  countOrgs <- nrow(prepData)
  if(countOrgs == 0){
    return(NULL)
  }
  allSpecCounted <- prepData %>% count(ORGANISM)
  colnames(allSpecCounted)<-c("ORGANISM", "Всего")
  fcnRCounted <- prepData %>% filter(X_1_ED40 <= 20 | FLU_EM > 2) %>% count(ORGANISM)
  colnames(fcnRCounted)<-c("ORGANISM", "Флуко-R")
  vorRCounted <- prepData %>% filter(VOR_ND1 <= 17 | VOR_EM >= 0.25) %>% count(ORGANISM)
  colnames(vorRCounted)<-c("ORGANISM", "Ворико-R")
  kVals <- lapply(allSpecCounted$`Всего`, function(x, y){
    result<-character()
    for(i in 1:length(unlist(x))){
      result<-c(result, CalcK(x[[i]],y))
    }
    return(result)
  }, countOrgs)
  resultTable<-merge(allSpecCounted, fcnRCounted, by.x = "ORGANISM", by.y = "ORGANISM", all.x = TRUE)
  resultTable<-merge(resultTable, vorRCounted, by.x = "ORGANISM", by.y = "ORGANISM", all.x = TRUE)
  resultTable$`Коэффициент разнообразия` = unlist(kVals)
  resultTable<-merge(resultTable, OrgList[,c("ORG", "SCT_TEXT")], by.x = "ORGANISM", by.y = "ORG")
  resultTable<-resultTable[,c(6,2,3,4,5)]
  colnames(resultTable)[1]<-"Вид"
  resultTable[is.na(resultTable)]<-0
  return(resultTable)
}