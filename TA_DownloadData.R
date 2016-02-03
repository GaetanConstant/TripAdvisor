# load libraries
library(RCurl)
library(XML)
library(xml2)
library(ggplot2)
library(pryr)
library(rvest)
library(xlsx)
library("zoo")
source("AuxiliaryDownloadFunctions.R")
source("mainFunctionForScrapping.R")


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

for (stevec in 1:length(hotelsid)) 
  {
  
  #stevec=1
  
  pickhotel = hotelsid[[stevec]]
  
  print("Hotel")
  print(pickhotel)
  
  
  ## Kreiranje linkov
  urllink = createLinks(datah[datah$hotelid == pickhotel, 1])
  
  stkorakov=length(urllink)
  
  a <- 1:stkorakov
  b <- a[seq(1, length(a), 20)]
  b<-b[-1]
  korakSave<-unique(c(b,length(a)))
  
  if (is.element(i,korakSave))
  {}
  
  if (stkorakov==0)
  { next 
  }
  
  
  ##Pobiranje podatkov
  dfrating=as.data.frame(NULL)
  
  dfrating.l = as.list(rep(NA, length(length(urllink))))
  
  
  for (i in 1:(length(urllink))) {
    
    #i=121
    ##if (1) {break}
    print("Korak...")
    print(i)
    dfrating.l[[i]] = try(getTAdata(urllink[i]))
    if (length(dfrating.l[[i]])==0) {break} 
    
    ###Zaradi varnosti pri velikem številu korakov, snemam vsak 20 korak
    if (stkorakov>50)
    {
      
      if (is.element(i,korakSave))
      {
        
        dfrating = try(do.call(rbind, dfrating.l))
        
        dfrating = dfrating[!is.na(dfrating$id), ]
        
        dfrating=cbind(dfrating,datah[datah$hotelid==pickhotel,c(2,3,4,5)])
        
        print("Shranjevanje.....")
        print(i)
        
        filenm = paste("dfrating_", pickhotel, "_",i,".Rda", sep = "")
        save(dfrating, file = filenm)
        
        try(mem_change(rm("dfrating")))
        try(mem_change(rm("dfrating.l")))
        try(gc())
        
        
        dfrating=as.data.frame(NULL)
        
        dfrating.l = as.list(rep(NA, length(length(urllink))))
        
      }
      
      
    }
    
    
    
  }
  
  

  
  
  if (length(dfrating)>0 && stkorakov<=50 ) {
    
    dfrating = try(do.call(rbind, dfrating.l))
    names(dfrating)
    
    
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


######################Konec downloada##########################################
################################################################################
#################################################################################



#Vse skupaj dam v eno datoteko
datoteke<- list.files( pattern="*.Rda", full.names=FALSE)
datoteke<-gsub(".Rda","",datoteke)
try(rm(podatki_s), silent = TRUE)
try(rm(podatki), silent = TRUE)
podatki_s=lapply(datoteke,function(x) {
  filenm=paste(as.character(x),".Rda",sep="")
  
  if (file.exists(filenm))
  {
    print(filenm)
    load(filenm)
    return(dfrating)
    
  }
})
podatki=as.data.frame(try(do.call(rbind,podatki_s)))


#Predobdelam podatke
  ObdelajPodatke(podatki)
  names(podatki)

write.table(podatki, file = "TA.txt", append = FALSE, quote = TRUE, sep = ";", row.names = FALSE)
