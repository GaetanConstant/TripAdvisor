# load libraries
library(RCurl)
library(XML)
library(xml2)
library(lubridate)
library(ggplot2)
library(pryr)
library(rvest)
library(xlsx)
library("zoo")
source("AuxiliaryDownloadFunctions.R")


options(stringsAsFactors = FALSE, silent=TRUE)


#podatki za mesta in države
worldcities = read.csv(file = "worldcities.csv", header = TRUE, stringsAsFactors = FALSE)

# podatki o hotelih
datah = read.csv(file = "Hoteli.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)

hotelid = gsub(" ", "", paste(datah$Regija,datah$Lokacija, datah$Hotel, sep = ""), fixed = TRUE)
datah = cbind(datah, hotelid)

hotelsid = as.list(datah$hotelid)




# hotels=c('KempinskiPalacePortoroz') hotels=as.list(hotels)
# ----------------------------------------------------------------------------------------------------------

# pick hotel for which review data is to be extracted choices: jwmarriott,hamptoninn,conrad

for (stevec in 104:length(hotelsid)) 
  {
  
  #stevec=1
  
  pickhotel = hotelsid[[stevec]]
  
  print("Hotel")
  print(pickhotel)
  
  
  ## Kreiranje linkov
  urllink = createLinks(datah[datah$hotelid == pickhotel, 1])
  
  if (length(urllink)==0)
  { next 
  }
  
  
  ##Pobiranje podatkov
  dfrating=as.data.frame(NULL)
  
  dfrating.l = as.list(rep(NA, length(length(urllink))))
  
  
  for (i in 1:(length(urllink))) {
    
    ##if (1) {break}
    print("Korak...")
    print(i)
    dfrating.l[[i]] = try(getTAdata(urllink[i], worldcities))
    if (length(dfrating.l[[i]])==0) {break}
    
  }
  
  
  dfrating = try(do.call(rbind, dfrating.l))
  names(dfrating)
  
  
  
  if (length(dfrating) > 0) {
    
    # removing NA
    dfrating = dfrating[!is.na(dfrating$id), ]
    
    dfrating=cbind(dfrating,datah[datah$hotelid==pickhotel,c(2,3,4,5)])
    head(dfrating)
    
    # save to Rdataset
    filenm = paste("dfrating_", pickhotel, ".Rda", sep = "")
    save(dfrating, file = filenm)
    
  }
  
  try(mem_change(rm("dfrating")))
  try(mem_change(rm("dfrating.l")))
  try(gc())
  
} 


#Vse skupaj dam v eno datoteko
datoteke<- list.files( pattern="*.Rda", full.names=FALSE)
datoteke<-gsub(".Rda","",datoteke)
try(rm(podatki_s), silent = TRUE)
try(rm(podatki), silent = TRUE)


podatki_s=lapply(datoteke,function(x) {
  filenm=paste(as.character(x),".Rda",sep="")
  
  if (file.exists(filenm))
  {
    print(x)
    load(filenm)
    return(dfrating)
    
  }
})

##celotne podatke izvozim v Excelovo datoteko
podatki=as.data.frame(try(do.call(rbind,podatki_s)))
# write.xlsx(x = podatki, file = "TA.xlsx",
#            sheetName = "Podatki", row.names = FALSE, append=FALSE)

write.table(podatki, file = "TA.txt", append = FALSE, quote = TRUE, sep = "|")

