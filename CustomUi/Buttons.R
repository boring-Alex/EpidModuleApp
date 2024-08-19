getButton<-function(bttnId, bttnName, bttnClass){
  ui<-actionButton(inputId = bttnId,
                 label = bttnName,
                 class = bttnClass)
  return(ui)
}

PrimaryButton<-function(buttonId, buttonName){
  bttn<-getButton(buttonId, buttonName, "btn btn-primary")
}

SecondaryButton<-function(buttonId, buttonName){
  bttn<-getButton(buttonId, buttonName, "btn btn-secondary")
}

SuccessButton<-function(buttonId, buttonName){
  bttn<-getButton(buttonId, buttonName, "btn btn-success")
}

InfoButton<-function(buttonId, buttonName){
  bttn<-getButton(buttonId, buttonName, "btn btn-info")
}

WarningButton<-function(buttonId, buttonName){
  bttn<-getButton(buttonId, buttonName, "btn btn-warning")
}

DangerButton<-function(buttonId, buttonName){
  bttn<-getButton(buttonId, buttonName, "btn btn-danger")
}

DarkButton<-function(buttonId, buttonName){
  bttn<-getButton(buttonId, buttonName, "btn btn-dark")
}

LightButton<-function(buttonId, buttonName){
  bttn<-getButton(buttonId, buttonName, "btn btn-light")
}

LinkButton<-function(buttonId, buttonName){
  bttn<-getButton(buttonId, buttonName, "btn btn-link")
}