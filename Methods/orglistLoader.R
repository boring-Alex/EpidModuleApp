OrgList<-read.table("/home/aleksandr/Эпидемиологическое приложение/EpidModuleApp/Data/ORGLIST.txt",
                    sep="\t", header = TRUE)
OrgList<-na.omit(OrgList)
OrgList<-OrgList %>% distinct(ORG, .keep_all = TRUE)