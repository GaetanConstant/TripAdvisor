# load libraries
library(RCurl)
library(XML)
library(xml2)
library(lubridate)
library(ggplot2)
library(pryr)
library(rvest)
library("zoo")
source("AuxiliaryDownloadFunctions.R")


options(stringsAsFactors = FALSE, silent=TRUE)


#podatki za mesta in države
worldcities = read.csv(file = "worldcities.csv", header = TRUE, stringsAsFactors = FALSE)

# podatki o hotelih
datah = read.csv(file = "Hoteli.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)

hotelid = gsub(" ", "", paste(datah$Regija, datah$Hotel, sep = ""), fixed = TRUE)
datah = cbind(datah, hotelid)

hotelsid = as.list(datah$hotelid)




# hotels=c('KempinskiPalacePortoroz') hotels=as.list(hotels)
# ----------------------------------------------------------------------------------------------------------

# pick hotel for which review data is to be extracted choices: jwmarriott,hamptoninn,conrad

for (stevec in 1:length(hotelsid)) 
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
    
    ##if (i>1) {break}
    print(i)
    dfrating.l[[i]] = try(getTAdata(urllink[i], worldcities))
    if (length(dfrating.l[[i]])==0) {break}
    
  }
  
  
  dfrating = try(do.call(rbind, dfrating.l))
  
  
  
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
