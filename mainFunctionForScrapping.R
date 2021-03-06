getTAdata<-function(url)
{
  
  
  reviews <- url %>% read_html() %>% html_nodes("#REVIEWS .innerBubble")
  
  
  id <- gsub("rn", "", reviews %>% html_node(".quote a") %>% html_attr("id"))
  
  if (length(id)>0)
  {
    
    
    
    quote <- reviews %>% html_node(".quote span") %>% html_text()
    
    rating <- reviews %>% html_node(".rating .rating_s_fill") %>% html_attr("alt") %>% gsub(" of 5 stars", "", 
                                                                                            .) %>% as.integer()
    
    
    # Novi datumi
    date1 <- try(reviews %>% html_node(".relativeDate") %>% html_attr("title"), silent = TRUE)
    
    date1 <- try(as.Date((monthR(gsub("\n", "", date1))), "%B %d, %Y"), silent = TRUE)
    
    if (class(date1) != "Date") {
      date1 <- NA
    }
    
    date1 <- date1[!is.na(date1)]
    
    
    
    # Za nazaj datumi
    date2 <- try(reviews %>% html_node(".ratingDate") %>% html_attr("title"), silent = TRUE)
    
    date2 <- try(as.Date((monthR(gsub("\n", "", date2))), "%B %d, %Y"), silent = TRUE)
    
    if (class(date2) != "Date") {
      date2 <- NA
    }
    
    date2 <- date2[!is.na(date2)]
    
    
    date3 <- try(gsub("Reviewed ", "", reviews %>% html_node(".ratingDate") %>% html_text()), silent = TRUE)
    
    date3 <- try(as.Date((monthR(gsub("\n", "", date3))), "%B %d, %Y"), silent = TRUE)
    
    
    if (class(date3) != "Date") {
      date3 <- NA
    }
    
    date3 <- date3[!is.na(date3)]
    
    # Združim datume
    if (identical(date1, date2)) {
      date = date1
    } else {
      date <- as.Date(c(date1, date2, date3))
    }
    
    
    
    fullrev <- sapply(as.list(id), getFullRev)
    
    
    # meta podatki recenzentov
    
    member_info <- url %>% read_html() %>% html_nodes("#REVIEWS .col1of2")
    
    
    
    
    # Moram it eno pa po eno, ker ni nujno,d a imajo vsi naslednje metapodatke
    
    ids = as.list(id)
    
    rHot = c("")
    rHel = c("")
    rLevel = c("")
    rAttr = c("")
    rPoint=-99
    rStarost=c("")
    rOstalo=c("")
    rGender=c("")
    rFood=c("")
    rTags=c("")
    rlocation=c("")
    rAllReviewes=c("")
    
    if (length(ids) > 0) {
      
      for (i in 1:length(ids)) {
        
        
        
        # Member info
        ruse <- try(member_info[[i]] %>% html_nodes(".username") %>% html_text())
        if (!grepl(" ",ruse))
        {
          
          urlM<-gsub("\n","",paste("http://www.tripadvisor.com/members/",ruse,sep=""))
          member<-try(urlM %>% read_html()%>% html_nodes("#MODULES_MEMBER_CENTER"), silent = TRUE)
        } else
          
        {
          member=""
        }  
        #MODULES_MEMBER_CENTER
        
        
        #Točk
        
        
        points<-try(member%>% html_nodes(".memberPointInfo .points")%>% html_text(), silent = TRUE)
        
        points=as.integer(gsub("\\D","",points))
        
        points=ifelse(points==0, NA, points)
        
        rPoint=c(rPoint, points)
        
        rPoint=rPoint[!rPoint==-99]
        
        
        #Član od
        rStar<-try(member%>% html_nodes(".ageSince .since")%>% html_text(), silent = TRUE)
        
        if (length(rStar) > 0) {
          
          rStar <- gsub("\n", "", rStar)
          
          rStar = ifelse(grepl("Since", rStar), rStar, 
                         NA)
        } else {
          rStar = NA
        }
        rStarost = c(rStarost, rStar)
        rStarost = rStarost[!rStarost %in% c("")]
        
        
        #Spol - dodal sem še ostalo, bom kasneje sfilitriral
        rOst<-try(member%>% html_nodes(".ageSince")%>% html_text(), silent = TRUE)
        
        if (length(rOst) > 0 && class(rOst)=="character") {
          
          rOst <- gsub("\n", "", rOst)
          
          #rOst = ifelse(grepl("female", rOst), "Female", 
          #           ifelse(grepl("male", rOst),"Male",NA))
        } else {
          rOst = NA
        }
        rGender = c(rGender, rOst)
        rGender = rGender[!rGender %in% c("")]
        
        #Tagi
        rT<-try(member%>% html_nodes(".tagBlock .tagBubble")%>% html_text(), silent = TRUE)
        
        if (length(rT) > 0 && (class(rT)!="try-error")) {
          
          rT <- gsub(" ", "", rT)
          rT=paste(rT , collapse =", ")
          
        } else {
          rT = NA
        }
        
        rTags = c(rTags, rT)
        rTags = rTags[!rTags %in% c("")]
        
        # Značka
        rlev <- try(member_info[[i]] %>% html_nodes(".levelBadge") %>% html_attr("class"), silent = TRUE)
        
        if (length(rlev) > 0 && class(rlev)=="character")  {
          
          rlev <- gsub("levelBadge badge lvl_", "", rlev)
          
          rlev = try(as.integer(rlev), silent = TRUE)
        } else {
          rlev = NA
        }
        rLevel = c(rLevel, rlev)
        rLevel = rLevel[!rLevel %in% c("")]
        
        
        # Lokacija
        rloc <- gsub("\n", "", member_info[[i]] %>% html_nodes(".location") %>% html_text())
        
        if (length(rloc)==0){
          rloc = NA
        }
        if (length(rloc)==1 & nchar(rloc)==0) {
          rloc = gsub("",NA,rloc)
        }
        rlocation = c(rlocation, rloc)
        rlocation = rlocation[!rlocation %in% c("")]
        
        
        # število vseh recenzij
        rAll <- gsub("\\D", "", member_info[[i]] %>% html_nodes(".reviewerBadge") %>% html_text(), perl = TRUE) %>% as.integer()
        
        if (length(rAll)==0){
          rAll = NA
        }
        rAllReviewes = c(rAllReviewes, rAll)
        rAllReviewes = rAllReviewes[!rAllReviewes %in% c("")]
        
        # Število hotelskih recenzij
        rHotels <- try(member_info[[i]] %>% html_node(".contributionReviewBadge") %>% html_text(), silent = TRUE)
        
        if (length(rHotels) > 0 && class(rHotels)=="character")  {
          
          rHotels <- gsub("\n", "", rHotels)
          
          rHotels = ifelse(grepl("hotel", rHotels), as.integer(gsub("\\D", "", rHotels, perl = TRUE)), 
                           NA)
        } else {
          rHotels = NA
        }
        rHot = c(rHot, rHotels)
        rHot = rHot[!rHot %in% c("")]
        
        
        # Število recenzij atrakcij
        rAttraction<- try(member_info[[i]] %>% html_node(".contributionReviewBadge") %>% html_text(), silent = TRUE)
        
        if (length(rAttraction) > 0 && class(rAttraction)=="character") {
          
          rAttraction <- gsub("\n", "", rAttraction)
          
          rAttraction = ifelse(grepl("attraction", rAttraction), as.integer(gsub("\\D", "", rAttraction, perl = TRUE)), 
                               NA)
        } else {
          rAttraction = NA
        }
        rAttr = c(rAttr, rAttraction)
        rAttr = rAttr[!rAttr %in% c("")]
        
        
        # Število recenzij restavracij
        rFod<- try(member_info[[i]] %>% html_node(".contributionReviewBadge") %>% html_text(), silent = TRUE)
        
        if (length(rFod) > 0 && class(rFod)=="character") {
          
          rFod <- gsub("\n", "", rFod)
          
          rFod = ifelse(grepl("restaurant", rFod), as.integer(gsub("\\D", "", rFod, perl = TRUE)), 
                        NA)
        } else {
          rFod = NA
        }
        rFood = c(rFood, rFod)
        rFood = rFood[!rFood %in% c("")]
        
        
        
        
        # Helpful votes
        rHelp <- try(member_info[[i]] %>% html_node(".helpfulVotesBadge") %>% html_text(), silent = TRUE)
        
        
        
        if (length(rHelp) > 0 && class(rHelp)=="character") {
          rHelp = gsub("\n", "", rHelp)
          
          rHelp = ifelse(grepl("helpful", rHelp), as.integer(gsub("\\D", "", rHelp, perl = TRUE)), NA)
        } else {
          rHelp = NA
        }
        rHel = c(rHel, rHelp)
        rHel = rHel[!rHel %in% c("")]
        
      }
    }
    
    rAllReviewes=as.numeric(rAllReviewes)
    rHot=as.numeric(rHot)
    rAttr=as.numeric(rAttr)
    rFood=as.numeric(rFood)
    rHel=as.numeric(rHel)
    rLevel=as.numeric(rLevel)
    
    
    tmpDF <- data.frame(id, quote, rating, date, fullrev, rlocation, rLevel, rAllReviewes, rHot,rAttr,rFood, rHel,rStarost,rGender,rPoint, rTags, stringsAsFactors = FALSE)
    tmpDF[, "drzava"] <- NA
    
    colnames(tmpDF) <- c("id","quote","rating","date","fullrev","location","contributor_level","All_reviews","Hotel_reviews",
                         "Attraction_reviews","Restaurant_reviews","Helpful_reviews","Member_since","Gender","TA_points",
                         "Tags","State")
    
    # poiščemo državo
    
    #   for (f in 1:nrow(tmpDF)) {
    #     
    #     
    #     #print(f)
    #     # f=3
    #     
    #     lokacija <- as.list(unlist(strsplit(tmpDF[f, 6], "[,]")))
    #     
    #     if (length(lokacija) > 0) {
    #       
    #       for (j in 1:length(lokacija)) 
    #       {
    #         
    #         city.index <- match(lokacija[[j]], worldcities[, 7])
    #         
    #         if (is.na(city.index)) {
    #           city.index <- 0
    #         }
    #         
    #         drzava <- ifelse(city.index > 0, worldcities[city.index, 1], "Uknown")
    #        
    #          #če je namibija, je poblem, zato, dam v string
    #         if (is.na(drzava))
    #         {drzava<-"NA"}
    #         
    #         if (drzava != "uknown") {
    #           tmpDF[f, c("State")] <- drzava
    #           break
    #         }
    #         
    #       }
    
    #     } else {
    #       tmpDF[f, c("State")] <- "uknown"
    #     }
    
    # }
    
  }
  if (class(tmpDF)!="data.frame")
  {
    tmpDF=as.data.frame(NULL)
  } 
  
  
  return(tmpDF) 
  
  
}