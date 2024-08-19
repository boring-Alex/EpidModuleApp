getOrgCodes<-function(orgNames){
  selectedOrgs<-character()
  for(i in 1:nrow(OrgList)){
    for(j in 1:unlist(length(orgNames))){
      if(grepl(tolower(orgNames[[j]]),tolower(OrgList[i,3]))){
        selectedOrgs<-c(selectedOrgs,OrgList[i,1])
        break;
      }
    }
  }
  return(selectedOrgs)
}

GetOrgGroupCodes<-function(orgGroup){
  selectOrgs<-OrgList %>% filter(ORG_GROUP %in% orgGroup)
  return(selectOrgs$ORG)
}

GetOrgGenusCodes<-function(orgGroup){
  selectOrgs<-OrgList %>% filter(GENUS_CODE %in% orgGroup)
  return(selectOrgs$ORG)
}

GetOrgNames<-function(orgCodes){
  selectedOrgs<-(OrgList %>% filter(ORG %in% orgCodes))$SCT_TEXT
  return(selectedOrgs)
}