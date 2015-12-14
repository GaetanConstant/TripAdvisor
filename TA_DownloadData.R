#
# Scrape some hotel review data from Trip Advisor
#

# set working directory

# load libraries
library(RCurl)
library(XML)
library(lubridate)
library(ggplot2)
library(pryr)
library(rvest)


options(stringsAsFactors=FALSE)

#
# Web page is parsed based on url
# Search for a hotel gives results spanning several pages
# the function below extracts information for a given page
#

monthR=function(x)
  
{x=try(gsub("January","Januar",x))
x=try(gsub("February","Februar",x))
x=try(gsub("March","Marec",x))
x=try(gsub("May","Maj",x))
x=try(gsub("June","Junij",x))
x=try(gsub("July","Julij",x))
x=try(gsub("August","Avgust",x))
x=try(gsub("October","Oktober",x))


return(x)
}

getOnePage=function(urllink){
  
  #urllink="http://www.tripadvisor.com/Hotel_Review-g295375-d1907209-Reviews-or80-Solaris_Hotel_Andrija-Sibenik_Sibenik_Knin_County_Dalmatia.html#REVIEWS"
  
  # get html page content
  doc=htmlTreeParse(urllink,useInternalNodes=TRUE)
  
  ## get node sets
  # review id
  ns_id=getNodeSet(doc,"//div[@id='REVIEWS']/div[@class='reviewSelector  ' or @class='reviewSelector   track_back']") 
  #Reviever location"
  
  # top quote for a review
  ns_topquote=getNodeSet(doc,"//div[@id='REVIEWS']//span[@class='noQuotes']") 
  # get partial entry for review that shows in the page
  ns_partialentry=getNodeSet(doc,"//div[@class='col2of2']//p[@class='partial_entry'][1]")
  # date of rating
  ns_ratingdt=getNodeSet(doc,"//div[@class='col2of2']//span[@class='ratingDate relativeDate' or @class='ratingDate']")
  # rating (number of stars)
  ns_rating=getNodeSet(doc,"//div[@class='col2of2']//span[@class='rate sprite-rating_s rating_s']/img[@alt]")
  
  # get actual values extracted from node sets
  # review id
  id=sapply(ns_id,function(x) gsub("review_","",xmlAttrs(x)["id"]))
  
  
  
  # top quote for the review
  topquote=sapply(ns_topquote,function(x) xmlValue(x))
  
  #Odstranim grafične elemente
  topquote=gsub("[^[:graph:]]", " ",topquote) 
  
  # rating date (couple of formats seem to be used and hence a and b below)
  ratingdta=sapply(ns_ratingdt,function(x) xmlAttrs(x)["title"])
  ratingdtb=sapply(ns_ratingdt,function(x) xmlValue(x))
  # rating (number of stars)
  rating=sapply(ns_rating,function(x) xmlAttrs(x)["alt"])
  # partial entry for review
  partialentry=sapply(ns_partialentry,function(x) xmlValue(x))
  
  # get rating date in date format
  ratingdt.pick=ratingdta
  ratingdt.pick[is.na(ratingdta)]=ratingdtb[is.na(ratingdta)]
  tmpDate=gsub("Reviewed ","",ratingdt.pick)
  ratingdt=as.Date((monthR(gsub("\n","",tmpDate))),"%B %d, %Y")
  
  
  dfr <- data.frame(location=character(),
                    Title=character(),
                    AllReviews=numeric(), 
                    HotelReviews=numeric(),
                    Contribution=numeric(),
                    stringsAsFactors=FALSE) 
  
  
  
  ids=as.list(id)
  
  if (length(topquote)>0)
  {
    
    #going through ids
    for (m in 1:length(topquote))
    {
      
      
      idr=ids[[m]]
      
      idlink=paste("//div[@id='review_",idr,"']",sep="")
      
      #preverim, ali je že slučajno google translate. Če je, grem ven
      ns_google=getNodeSet(doc,paste(idlink,"//div[@class='col1of2']//div[contains(@class, 'googleTranslation')]",sep=""))
      
      #Reviwers location
      ns_rlocation=getNodeSet(doc,paste(idlink,"//div[@class='col1of2']//div[@class='location']",sep=""))
      
      
      #Reviwers title
      ns_rtitle=getNodeSet(doc,paste(idlink,"//div[@class='col1of2']//div[contains(@class, 'levelBadge')]",sep=""))
      
      
      if (length(ns_rtitle)>0) {
        ns_level=ns_rtitle %>% html_attr("class")}
      
      #AllReviews
      ns_rAll=getNodeSet(doc,paste(idlink,"//div[@class='col1of2']//div[contains(@class,'reviewerBadge')]",sep=""))
      
      
      #Hotel reviews
      ns_rH=getNodeSet(doc,paste(idlink,"//div[@class='col1of2']//div[contains(@class,'contributionReviewBadge')]",sep=""))
      
      
      
      #Contribution helpfulVotesBadge badge no_cpu
      ns_rC=getNodeSet(doc,paste(idlink,"//div[@class='col1of2']//div[contains(@class,'helpfulVotesBadge')]",sep=""))
      
      
      #location of reviewer    
      if (length(ns_rlocation)==0)
      {rlocation="Unknown"} else
      {    
        rlocation=sapply(ns_rlocation,function(x) xmlValue(x))
        rlocation=gsub("\n","",rlocation)
      }
      
      #Še enkrat preverim rlocation
      if (nchar(rlocation)<1)
      {
        rlocation="Uknown"
      }
      
      #Reviwer title  
      if (length(ns_rtitle)==0)
      {
        rtitle="Unknown"} else
        {
          
          
          rtitle=sapply(ns_rtitle,function(x) xmlValue(x))
          rtitle=gsub("\nLevel","",rtitle)
          rtitle=gsub(" ","",rtitle)
          
          if (length(ns_level)>0)
          {
            rlevel=gsub("levelBadge badge lvl_","",ns_level)
            rtitle=paste("Level ",rlevel," ",rtitle,sep="")}
          
          
        }  
      
      #All reviews ns_rAll
      
      if (length(ns_rAll)==0)
      {rAll=0} else
      {
        rAll=gsub(" review","",sapply(ns_rAll,function(x) xmlValue(x)), fixed=TRUE)
        rAll=as.numeric(gsub("s","",rAll,fixed=TRUE))   
      }
      
      #Hotel reviws
      
      if (length(ns_rH)==0)
      {rH=0} else
      {
        rH=gsub(" review","",sapply(ns_rH,function(x) xmlValue(x)), fixed=TRUE)
        rH=gsub(" hotel","",rH,fixed=TRUE)
        rH=as.numeric(gsub("s","",rH,fixed=TRUE))
      }  
      
      
      #Contribution
      if (length(ns_rC)==0)
      {rC=0} else
      {
        rC=gsub(" helpful vote","",sapply(ns_rC,function(x) xmlValue(x)), fixed=TRUE)
        rC=as.numeric(gsub("s","",rC,fixed=TRUE))
        
      }  
      
      
      dfr=rbind(dfr,data.frame(rlocation,rtitle,rAll,rH,rC))
      
    }
    
  } 
  if ((length(topquote)==0) || (!length(topquote)==length(id)) || (length(ratingdt)==0))
  {dfrating=as.data.frame(NULL)}
  
  else
  {
    # put all the fields in a dataframe
    dfrating=data.frame(id=id,topquote=topquote,ratingdt=ratingdt,rating=rating,partialentry=partialentry)
    dfrating=cbind(dfrating,dfr)
    dfrating$ratingnum=as.numeric(substr(dfrating$rating,1,1),1,1)
    
  }
  
  return(dfrating)
  
}

# function to extract full review given review id and full review urllink
getfullrev=function(urllink,id){
  
  # get html content of page containing full review
  docrev=htmlTreeParse(urllink,useInternalNodes=TRUE)
  # extract node set containing full review
  revid=paste("review_",id,sep="")
  qry=paste("//p[@id='",revid,"']",sep="")
  ns_fullrev=getNodeSet(docrev,eval(qry))
  # get full review content
  return(xmlValue(ns_fullrev[[1]]))  
  
}


#--- This part is manual and based on looking up the search url after typing search term in tripadvisor site------
# main url link for different hotels
# 3 hotels considered here
#    1. J W Marriott, Indianapolis
#    2. Hampton Inn Indianapolis Northwest -100
#    3. Conrad Indianapolis
#

# podatki o hotelih
datah=read.csv(file="Hoteli.csv",sep=";",header=TRUE,stringsAsFactors=FALSE)

hotelid=gsub(" ","",paste(datah$Lokacija,datah$Hotel,sep=""), fixed=TRUE)
datah=cbind(datah,hotelid)

hotelsid=as.list(datah$hotelid)


# hotels=c("KempinskiPalacePortoroz")
# hotels=as.list(hotels)
#----------------------------------------------------------------------------------------------------------

# pick hotel for which review data is to be extracted
# choices: jwmarriott,hamptoninn,conrad

for (stevec in 22:length(hotelsid))
{  
  
  
  #stevec=22
  
  pickhotel=hotelsid[[stevec]]
  
  print("Hotel")
  print(pickhotel)
  # get list of urllinks corresponding to different pages
  
  
  ## Sestavim ULRMAIN TER MOREPG
  urlmainlist=datah[datah$hotelid==pickhotel,1]
  morepglist=seq(10,datah[datah$hotelid==pickhotel,3],10)
  
  # url link for first search page
  urllinkmain=urlmainlist
  # counter for additional pages
  morepg=as.numeric(morepglist)
  
  urllinkpre=paste(strsplit(urllinkmain,"Reviews-")[[1]][1],"Reviews",sep="")
  urllinkpost=strsplit(urllinkmain,"Reviews-")[[1]][2]
  end="#REVIEWS"
  urllink=rep(NA,length(morepg)+1)
  
  urllink[1]=urllinkmain
  for(i in 1:length(morepg)){
    reviews=paste(urllinkpre,"-or",morepg[i],"-",urllinkpost,sep="")
    urllink[i+1]=paste(reviews,end,sep="")
  }
  
  head(urllink)
  
  urllink
  
  # get summary content with rating
  
  # note: there are few format that are not captured in scraping
  # that is reflected in some reviews for Conrad not being extracted
  
  dfrating.l=as.list(rep(NA,length(morepg)+1))
  
  for(i in 1:(length(morepg)+1)){
    #print(i)
    dfrating.l[[i]]=try(getOnePage(urllink[i]))
    ##print(i)
    #print(dfrating.l[[i]])
  }
  dfrating=try(do.call(rbind,dfrating.l))
  
  # removing NA
  dfrating=dfrating[!is.na(dfrating$id),]
  
  if (length(dfrating)>0)
  {
    
    dfrating=cbind(dfrating,datah[datah$hotelid==pickhotel,c(2,4,5,6,7)])
    
    
    # get url list that contain full reviews
    if (grepl("Hotel_Review",urllinkmain))
    {
      
      urllinkpre.rev=gsub("Hotel_Review","ShowUserReviews",strsplit(urllinkmain,"Reviews-")[[1]][1])
    } else if (grepl("Attraction_Review",urllinkmain)) {
      
      urllinkpre.rev=gsub("Attraction_Review","ShowUserReviews",strsplit(urllinkmain,"Reviews-")[[1]][1]) }
    else {
      urllinkpre.rev=gsub("Restaurant_Review","ShowUserReviews",strsplit(urllinkmain,"Reviews-")[[1]][1])
    }
    
    urlrev=paste(urllinkpre.rev,"rXX-",urllinkpost,sep="")
    
    dfrating$id2=gsub("rn","",dfrating$id)
    urlfullrevlist=sapply(dfrating$id2,function(x) gsub("XX",x,urlrev))
    head(urlfullrevlist)
    
    
    
    # Get the full review content
    fullrev=rep(NA,nrow(dfrating))
    for(i in 1:nrow(dfrating)){
      fullrev[i]=try(getfullrev(urlfullrevlist[i],dfrating$id2[i]))
      if(i %% 20 == 0) print(i)
    }
    
    
    head(fullrev)
    
    dfrating$fullrev=gsub("\n","",fullrev)
    
    # save to Rdataset
    filenm=paste("dfrating_",pickhotel,".Rda",sep="")
    save(dfrating,file=filenm)
    
    
  }
  
  try(mem_change(rm("dfrating")))
  try(mem_change(rm("dfrating.l")))
  try(gc())
  
}