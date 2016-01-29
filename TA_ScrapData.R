library(rvest)
library(XML)
source(AuxiliaryDownloadFunctions.R)


  
  url="http://www.tripadvisor.com/Attraction_Review-g274873-d298666-Reviews-Ljubljana_Old_Town-Ljubljana_Upper_Carniola_Region.html#REVIEWS"
  
  
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
  
  date <- reviews %>%
    html_node(".relativeDate") %>%
    html_attr("title") 

  date <-as.Date((monthR(gsub("\n","",date))),"%B %d, %Y")
  
  

  fullrev<-sapply(as.list(id),getFullRev)
  

  

  
  
  
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