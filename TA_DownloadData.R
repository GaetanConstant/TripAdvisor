# load libraries
library(RCurl)
library(XML)
library(xml2)
library(ggplot2)
library(pryr)
library(plyr)
library(rvest)
library(xlsx)
library("zoo")
source("AuxiliaryDownloadFunctions.R")
source("mainFunctionForScrapping.R")


options(stringsAsFactors = FALSE, silent=TRUE)


#podatki za mesta in države
#worldcities = read.csv(file = "./lookups/worldcities.csv", header = TRUE, stringsAsFactors = FALSE)

#podatki za čustveno analizo
positive=scan("./lookups/positive-words.txt",what="character",comment.char=";")

negative=scan("./lookups/negative-words.txt",what="character",comment.char=";")

# podatki o hotelih
datah = read.csv(file = "./lookups/Hoteli.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
AFINN = read.csv(file = "./lookups/AFINN.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)

hotelid = gsub(" ", "", paste(datah$Regija,datah$Lokacija, datah$Hotel, sep = ""), fixed = TRUE)
datah = cbind(datah, hotelid)

hotelsid = as.list(datah$hotelid)




# hotels=c('KempinskiPalacePortoroz') hotels=as.list(hotels)
# ----------------------------------------------------------------------------------------------------------

# pick hotel for which review data is to be extracted choices: jwmarriott,hamptoninn,conrad

for (stevec in 21:length(hotelsid)) 
  {
  
  #stevec=5
  
  pickhotel = hotelsid[[stevec]]
  
  print("Hotel")
  print(pickhotel)
  
  
  ## Kreiranje linkov
  urllink = createLinks(datah[datah$hotelid == pickhotel, 1])
  
  stkorakov=length(urllink)

  a <- 1:stkorakov
  
  if (stkorakov>100)
  {b <- a[seq(1, length(a), 100)]} else
  {b <- a[seq(1, length(a), stkorakov)]}
  
  b<-b[-1]
  korakSave<-unique(c(b,length(a)))
  

  
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
    
    
    ###Zaradi varnosti pri velikem številu korakov, snemam vsak 100 korak
    if (stkorakov>120)
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
  
  

  
  
  if (stkorakov<=120 ) {
    
    
    
    dfrating = try(do.call(rbind, dfrating.l))
    names(dfrating)
    
    
    # removing NA
    dfrating = dfrating[!is.na(dfrating$id), ]
    
    dfrating=cbind(dfrating,datah[datah$hotelid==pickhotel,c(2,3,4,5)])
    head(dfrating)
    
    print("Shranjevanje.....")
    
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
datoteke<- list.files(path="./data", pattern="*.Rda", full.names=FALSE)
datoteke<-gsub(".Rda","",datoteke)
try(rm(podatki_s), silent = TRUE)
try(rm(podatki), silent = TRUE)
podatki_s=lapply(datoteke,function(x) {
  filenm=paste("./data/",as.character(x),".Rda",sep="")
  
  if (file.exists(filenm))
  {
    print(filenm)
    load(filenm)
    return(dfrating)
    
  }
})
podatki=as.data.frame(try(do.call(rbind,podatki_s)))

nrow(podatki)

##Obdržim samo unikatne vrstice
podatki<-unique(podatki)

nrow(podatki)

#Izvržem prazne fullrev
podatki<-podatki[!is.na(podatki$fullrev),]

nrow(podatki)
podatki<-head(podatki,10)


#Predobdelam podatke
  results<-ObdelajPodatke(podatki)
  names(results)

write.table(results, file = "./outputs/TA.txt", append = FALSE, quote = TRUE, sep = ";", row.names = FALSE)
