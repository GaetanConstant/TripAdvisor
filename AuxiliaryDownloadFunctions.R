# Funkcija za pridobitev datuma

monthR = function(x) 
{
    x = try(gsub("January", "Januar", x))
    x = try(gsub("February", "Februar", x))
    x = try(gsub("March", "Marec", x))
    x = try(gsub("May", "Maj", x))
    x = try(gsub("June", "Junij", x))
    x = try(gsub("July", "Julij", x))
    x = try(gsub("August", "Avgust", x))
    x = try(gsub("October", "Oktober", x))
    
    
    return(x)
}

# Funkcija za pridobitev celotnega reviewa

getfullrev = function(url, id) {
    
    # get html content of page containing full review
    docrev = htmlTreeParse(urllink, useInternalNodes = TRUE)
    # extract node set containing full review
    revid = paste("review_", id, sep = "")
    qry = paste("//p[@id='", revid, "']", sep = "")
    ns_fullrev = getNodeSet(docrev, qry)
    # get full review content
    return(xmlValue(ns_fullrev[[1]]))
    
}


# Priprava URLja
urlPrepare <- function(url, id) {
    
    urllinkpost = strsplit(url, "Reviews-")[[1]][2]
    
    if (grepl("Hotel_Review", url)) {
        
        urllinkpre.rev = gsub("Hotel_Review", "ShowUserReviews", strsplit(url, "Reviews-")[[1]][1])
    } else if (grepl("Attraction_Review", url)) {
        
        urllinkpre.rev = gsub("Attraction_Review", "ShowUserReviews", strsplit(url, "Reviews-")[[1]][1])
    } else {
        urllinkpre.rev = gsub("Restaurant_Review", "ShowUserReviews", strsplit(url, "Reviews-")[[1]][1])
    }
    
    urlrev = paste(urllinkpre.rev, "rXX-", urllinkpost, sep = "")
    
    urlfullrevlist = gsub("XX", id, urlrev)
    
    return(urlfullrevlist)
    
}

# Potegni dol celotni review
getFullRev <- function(id) {
    
    
    
    urlfull <- urlPrepare(url, id)
    
    

    
    # get html content of page containing full review
    doc = htmlTreeParse(urlfull, useInternalNodes = TRUE)
    # extract node set containing full review
    revid = paste("review_", id, sep = "")
    qry = paste("//p[@id='", revid, "']", sep = "")
    ns_fullrev = getNodeSet(doc, qry)
    
    if (length(ns_fullrev)>0)
    {      r=xmlValue(ns_fullrev[[1]])
    
           r=(gsub("\n", "", r))
    
    } else {

      r=NA
      
    }
    
    return (r)

    
}


createLinks <- function(url)
  
{
  pages <- url %>% read_html() %>% html_nodes("#REVIEWS .pageNumbers")
  
  lastPage <-max(as.integer(pages %>% html_nodes("a" ) %>% html_attr("data-page-number")))
  
  urlmainlist = url
  morepglist = seq(10, lastPage*10, 10)
  
  # url link for first search page
  urllinkmain = urlmainlist
  # counter for additional pages
  morepg = as.numeric(morepglist)
  
  urllinkpre = paste(strsplit(urllinkmain, "Reviews-")[[1]][1], "Reviews", sep = "")
  urllinkpost = strsplit(urllinkmain, "Reviews-")[[1]][2]
  end = "#REVIEWS"
  urllink = rep(NA, length(morepg) + 1)
  
  urllink[1] = urllinkmain
  for (i in 1:length(morepg)) {
    reviews = paste(urllinkpre, "-or", morepg[i], "-", urllinkpost, sep = "")
    urllink[i + 1] = paste(reviews, end, sep = "")
  }
  
  return(urllink)
}

#Funkcija za obdelavo datumov

monthF<-function(x)
{
  if (grepl("Jan",x)) {
    r="1"
  } else if (grepl("Feb",x)) {
    r="2"
  } else if (grepl("Mar",x)) {
    r="3"
  } else if (grepl("Apr",x)) {
    r="4"
  } else if (grepl("May",x)) {
    r="5"
  } else if (grepl("Jun",x)) {
    r="6"
  } else if (grepl("Jul",x)) {
    r="7"
  } else if (grepl("Aug",x)) {
    r="8"
  } else if (grepl("Sep",x)) {
    r="9"
  } else if (grepl("Oct",x)) {
    r="10"
  } else if (grepl("Nov",x)) {
    r="11"
  } else if (grepl("Dec",x)) {
    r="12"
  } else
    r=NA
  
  return(r)     
}

ObdelajStarost<-function(x)
{
  
  m <- regexpr("[0-9][0-9]-[0-9][0-9]", x, perl=TRUE)
  r=ifelse((is.na(m) | m<0),NA,regmatches(x, m))
  
  return(r)
  
}

ObdelajDatume<-function(x)
{
  
  m1 <- regexpr("[0-9]...", x , perl=TRUE)
  leto<-regmatches(x, m1)
  mesec<-monthF(x)
  r<-ifelse(is.na(mesec),NA,paste(leto,"-",mesec,"-1",sep=""))
  
  return(r)
  
}


ObdelajPodatke <- function (podatki) {
  #naredim par izračunov
  ##najprej generiram nove stolpce
  tagi<-podatki[!is.na(podatki$Tags),c("Tags")]
  tagi<-unlist(strsplit(tagi,","))
  tagi<-as.list(unique(str_trim(tagi, side=c("both"))))
  
  for (i in 1:length(tagi))
  {
    ime<-tagi[[i]]
    stevilo<-NA
    podatki<-cbind(podatki,stevilo)
    names(podatki)[ncol(podatki)]<-ime
    
  }
  
  
  
  
  #starost in datumi
  podatki[,c("Age_of_reviewer")]<-NA
  podatki[,c("Reviews")]<-NA
  
  podatki[is.na(podatki$Hotel_reviews),c("Hotel_reviews")]<-0
  podatki[is.na(podatki$Attraction_reviews),c("Attraction_reviews")]<-0
  podatki[is.na(podatki$Restaurant_reviews),c("Restaurant_reviews")]<-0
  
  
  sentimentAll=list()

  
  for (i in 1:nrow(podatki))
  {
    podatki[i,c("Age_of_reviewer")]<-ObdelajStarost(podatki[i,c("Gender")])
    podatki[i,c("Member_since")]<-ObdelajDatume(podatki[i,c("Member_since")])
    
    #####################Obdelava tagov #####################################
    tagiVrstica<-podatki[i,c("Tags")]
    tagiVrstica<-unlist(strsplit(tagiVrstica,","))
    tagiVrstica<-as.list(unique(str_trim(tagiVrstica, side=c("both"))))
    
    
    for (j in 1:length(tagi))
    {
      
      if (length(intersect(unlist(tagi),unlist(tagiVrstica)))>0)
      {
         if (length(intersect(tagi[[j]],unlist(tagiVrstica)))>0)
         {
           podatki[i,tagi[[j]]]<-1
           
         } else {
           
           podatki[i,tagi[[j]]]<-0
           
         }
        
      } else {
        
        podatki[i,tagi[[j]]]<-NA
      }

      
    }
    
    #######################################################################################
    
    nrev<-max(podatki[i,c("Hotel_reviews")],podatki[i,c("Attraction_reviews")],podatki[i,c("Restaurant_reviews")], na.rm = TRUE)
    
    nrev<-ifelse(nrev==-Inf,0,nrev)
    
    podatki[i,c("Reviews")]<-nrev
    
    ############################Obdelava sentimenta Liu#######################################
    
    sentimentAll[[i]]<-getSentiment(podatki[i,c("fullrev")])
    
    
    ############################Obdelava sentimenta AFINN#######################################
    
    print("Obdelava vrstice...")
    print(i)
  }
  

  podatki<-cbind(podatki,do.call(rbind,sentimentAll))
  
  podatki[,c("Gender")]<-ifelse(grepl("female", podatki[,c("Gender")]), "Female", ifelse(grepl("male", podatki[,c("Gender")]),"Male",NA))
  
  podatki[,c("Hotel_reviews")]<-NULL
  podatki[,c("Attraction_reviews")]<-NULL
  podatki[,c("Restaurant_reviews")]<-NULL
  podatki[,c("Tags")]<-NULL
  podatki[,c("Tip")]<-NULL
  stolpci<-ncol(podatki) 
  colnames(podatki)[stolpci-1]<-"Liu"
  colnames(podatki)[stolpci]<-"St_besed"
  
  return(podatki)
  
}

getSentiment=function(text)
{
  
  
  
  
  #Sentiment analiza
  ## Score sentiment for each Twitt
  results=score.sentiment(text,positive,negative)
  
  
  # print ("Povzetek sentimen analize")
  
  s=summary(results$score)
  nWords=results$nwords
  # numberSentences=length(results$text)
  
  #print(results$text)
  
  #    print( class(s))
  # 
  #    print (s)
  
  # Dodam rezultate v tabelo
  
  #r3=cbind.data.frame(s[1], s[4], s[6],numberSentences)
  
  #names(r3)=c("Min","Mean","Max","St. stavkov")
  
  return(data.frame(s[4],nWords))
  
}

score.sentiment = function(sentences, pos.words, neg.words, .progress="none")
{     
  require(stringr)   
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us   
  
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply: 
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) 
  {  
    # clean up sentences with Rs regex-driven global substitute, gsub():	 
    
    sentence = gsub("[[:punct:]]","", sentence)	 
    sentence = gsub("[[:cntrl:]]","" , sentence)	 
    sentence = gsub("\\d+","" , sentence)	 
    
    # and convert to lower case:	 
    
    sentence = tolower(sentence)	 
    
    # split into words. str_split is in the stringr package	 
    
    word.list = str_split(sentence, "\\s+")	
    
    # sometimes a list() is one level of hierarchy too much	
    
    words = unlist(word.list)	
    
    nWords=length(words)
    # compare our words to the dictionaries of positive & negative terms	 
    
    pos.matches = match(words, pos.words) 
    neg.matches = match(words, neg.words)	 
    
    # match() returns the position of the matched term or NA	 
    # we just want a TRUE/FALSE:	 
    
    pos.matches = !is.na(pos.matches)	 
    neg.matches = !is.na(neg.matches)	 
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():	 
    
    score = sum(pos.matches) - sum(neg.matches)
    words.df=data.frame(score,nWords)
    return(words.df)	 
    
  }, pos.words, neg.words, .progress=.progress )	 
  
  scores.df = data.frame(score=scores$score, text=sentences,nwords=scores$nWords)	 
  
  return(scores.df)
}
