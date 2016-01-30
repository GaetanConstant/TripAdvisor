library(rvest)
library(XML)
library("zoo")
source("AuxiliaryDownloadFunctions.R")
 
  worldcities=read.csv(file="worldcities.csv",header = TRUE, stringsAsFactors = FALSE)
  
  head(worldcities)
  
  url="http://www.tripadvisor.com/Hotel_Review-g274873-d456147-Reviews-Grand_Hotel_Union-Ljubljana_Upper_Carniola_Region.html#REVIEWS"
  
  reviews <- url %>%
    read_html() %>%
    html_nodes("#REVIEWS .innerBubble")
  
  id <- gsub("rn","",reviews %>%
    html_node(".quote a") %>%
    html_attr("id"))
  
  quote <- reviews %>%
    html_node(".quote span") %>%
    html_text()
  
  rating <- reviews %>%
    html_node(".rating .rating_s_fill") %>%
    html_attr("alt") %>%
    gsub(" of 5 stars", "", .) %>%
    as.integer()
  
 
  #Novi datumi
  date1 <- try(reviews %>%
                      html_node(".relativeDate") %>%
                      html_attr("title"))
  
  date1 <-try(as.Date((monthR(gsub("\n","",date1))),"%B %d, %Y"))
  
  if (class(date1)!="Date") {
    date1<-NA }
  
  date1 <- date1[!is.na(date1)]
  
  
  
  #Za nazaj datumi
  date2 <- try(reviews %>%
                 html_node(".ratingDate") %>%
                 html_attr("title"))
  
  date2 <-try(as.Date((monthR(gsub("\n","",date2))),"%B %d, %Y"))
  
  if (class(date2)!="Date") {
       date2<-NA}
  
  date2 <- date2[!is.na(date2)]
  
  
  date3 <- try(gsub("Reviewed ","",reviews %>%
    html_node(".ratingDate") %>%
    html_text()))
  
  date3 <-try(as.Date((monthR(gsub("\n","",date3))),"%B %d, %Y"))
  
  
  if (class(date3)!="Date") {
    date3<-NA}
  
  date3 <- date3[!is.na(date3)]
  
  #Združim datume
        if (identical(date1,date2))
        {
          date=date1
            } else {
           date<- as.Date(c(date1,date2, date3))
             }
 
  

  fullrev<-sapply(as.list(id),getFullRev)
  
  
  #meta podatki recenzentov
  
  member_info <- url %>%
                   read_html() %>%
                     html_nodes("#REVIEWS .col1of2")
    
   
  #Lokacija
  rlocation <- gsub("\n","",member_info %>%
                      html_nodes(".location") %>% 
                      html_text()) 
  
  
  #število vseh recenzij
  rAll <- gsub("\\D","",member_info %>%
                   html_nodes(".reviewerBadge") %>% 
                   html_text(), perl = TRUE) %>%
                      as.integer()
  
  
  #Moram it eno pa po eno, ker ni nujno,d a imajo vsi naslednje metapodatke
  
  ids=as.list(id)
  
  rHot=c("")
  rHel=c("")
  rLevel=c("")
  
  if (length(ids)>0)
  {  
  
  for (i in 1:length(ids))
     {
      
    
    #Značka
            rlev <- try(member_info[[i]] %>%
                               html_nodes(".levelBadge") %>% 
                                      html_attr("class"))
            
            if (length(rlev)>0)
            {
              
              rlev <- gsub("levelBadge badge lvl_","",rlev)
              
              rlev=try(as.integer(rlev))
            }
            else
            {
              rlev=0
            }
            rLevel=c(rLevel,rlev)
            rLevel=rLevel [!rLevel %in% c("")]
    
    
   
    #Število hotelskih recenzij
             rHotels <- try(member_info[[i]] %>%
                               html_node(".contributionReviewBadge") %>% 
                                       html_text())
  
                         if (length(rHotels)>0)
                                   {
  
                                         rHotels <- gsub("\n","",rHotels)
  
                                         rHotels=ifelse(grepl("hotel", rHotels), as.integer(gsub("\\D","",rHotels, perl=TRUE)), 0)
                                         }
                          else
                            {
                              rHotels=0
                            }
            rHot=c(rHot,rHotels)
            rHot=rHot [!rHot %in% c("")]
            
            
            
      #Helpful votes
            rHelp <- try(member_info[[i]] %>%
                                  html_node(".helpfulVotesBadge") %>% 
                                  html_text())
            
           
            
            if (length(rHelp)>0)
            {
              rHelp=gsub("\n","",rHelp)
              
              rHelp=ifelse(grepl("helpful", rHelp), as.integer(gsub("\\D","",rHelp, perl=TRUE)), 0)
            }
            else
            {
              rHelp=0
            }
            rHel=c(rHel,rHelp)
            rHel=rHel [!rHel %in% c("")]
  
     }
  }
  
  
 
  tmpDF<-data.frame(id, quote, rating, date, fullrev, rlocation, rLevel, rAll, rHot, rHel,  stringsAsFactors = FALSE)
  tmpDF[,"drzava"]<-NA
  
  #poiščemo državo
  
  for (f in 1:nrow(tmpDF))
  {
    
  
    print(f)
    #f=3
    
    lokacija<-as.list(unlist(strsplit(tmpDF[f,6], "[,]")))
     
     if (length(lokacija)>0)
        {
    
              for (j in 1:length(lokacija))
      
               {
    
                      city.index<- match(lokacija[[j]], worldcities[,7])
                      
                      if (is.na(city.index))
                        { city.index<-0}
    
                      drzava<-ifelse(city.index>0 ,worldcities[city.index,1 ],"Uknown")
                      
                      if (drzava!="uknown") 
                        {
                        tmpDF[f,c("drzava")]<-drzava 
                           break
                        }
   
              }
    
          }
         else 
         {
           tmpDF[f,c("drzava")]<-"uknown" 
         }
    
  }

  tmpDF %>%View()
  
  rm("tmpDF")
  rm("worldcities")
  

 
  
  
  
 