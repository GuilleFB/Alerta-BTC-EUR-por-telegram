rm(list=ls())
library(httr)
library(jsonlite)
library(rvest)
library(telegram.bot)
library(telegram)

pausa<-function(x){
  for (i in 1:x)
  {
    date_time<-Sys.time()
    while((as.numeric(Sys.time()) - as.numeric(date_time))<1){}
  }
}

bot = Bot(token = bot_token('nombre bot'))
updates = bot$getUpdates()
if (length(updates)==0){
  chat_id='id del chat' # numero sin comillas
} else {
  chat_id = updates[[1L]]$from_chat_id()  
}

PriceBTCEURurl="https://api.coinbase.com/v2/prices/BTC-EUR/spot"

# Alertas ###
Alertas=c(33000,32000,31000,30000,29000,28000,27000,26000,25000,24000,23000,22000,21000,20000,19000,18000,17000) # Tantas alertas como quiera.
Alertas=sort(Alertas , decreasing=T) # Configurado para que este en orden descendente y no haya problemas con las alertas.
print(Alertas)

repeat {
  t1=Sys.time()
  #Comprueba si hay conexion a internet mediante un error de pagina web  
  status <- tryCatch(GET(PriceBTCEURurl),error = function(e) e)
  # Si da el error espera 5 segundo y lo vuelve a intentar volviendo al principio del bucle (next)  
  if (inherits(status,  "error")){
    print("Intentando reconectar")
    pausa(5)
    next
  } else {
    
    Price=as.numeric(fromJSON(content(GET(PriceBTCEURurl), type ="text", encoding = "UTF-8"))$data$amount)
    
    for (i in 1:(length(Alertas)-1)) {
      if (Price<=Alertas[i] & Price>=Alertas[i+1]){
          texto=paste(Price, " EUR, *ALERTA!!!!* Alerta eliminada",
                    sep = "")
          print(texto)
          bot$sendMessage(chat_id, text = texto, parse_mode = "Markdown")
          Alertas=Alertas[-i]
      }
    }
    
    Price=as.numeric(fromJSON(content(GET(PriceBTCEURurl), type ="text", encoding = "UTF-8"))$data$amount)
    
    #print(paste(Price, "EUR"))
    t2=Sys.time()-t1
    pausa(2-round(t2))
    
  }
}
