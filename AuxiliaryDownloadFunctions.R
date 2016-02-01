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
    
    
    fullrev = xmlValue(ns_fullrev[[1]])
    
    return(gsub("\n", "", fullrev))
    
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
  
  #starost in datumi
  podatki[,c("Age_of_reviewer")]<<-NA
  podatki[,c("Reviews")]<<-NA
  
  
  for (i in 1:nrow(podatki))
  {
    podatki[i,c("Age_of_reviewer")]<<-ObdelajStarost(podatki[i,c("Gender")])
    podatki[i,c("Member_since")]<<-ObdelajDatume(podatki[i,c("Member_since")])
    
    nrev<<-max(podatki[i,c("Hotel_reviews")],podatki[i,c("Attraction_reviews")],podatki[i,c("Restaurant_reviews")], na.rm = TRUE)
    
    nrev<<-ifelse(nrev==-Inf,0,nrev)
    
    podatki[i,c("Reviews")]<<-nrev
  }
  
  podatki[,c("Gender")]<<-ifelse(grepl("female", podatki[,c("Gender")]), "Female", ifelse(grepl("male", podatki[,c("Gender")]),"Male",NA))
  
  podatki[,c("Hotel_reviews")]<<-NULL
  podatki[,c("Attraction_reviews")]<<-NULL
  podatki[,c("Restaurant_reviews")]<<-NULL
  podatki[,c("Tip")]<<-NULL
}