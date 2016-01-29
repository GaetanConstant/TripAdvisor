#Funkcija za pridobitev datuma

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

#Funkcija za pridobitev celotnega reviewa

getfullrev=function(url,id){
  
  # get html content of page containing full review
  docrev=htmlTreeParse(urllink,useInternalNodes=TRUE)
  # extract node set containing full review
  revid=paste("review_",id,sep="")
  qry=paste("//p[@id='",revid,"']",sep="")
  ns_fullrev=getNodeSet(docrev,qry)
  # get full review content
  return(xmlValue(ns_fullrev[[1]]))  
  
}


#Priprava URLja
urlPrepare<-function(url,id)
{
  
  urllinkpost=strsplit(url,"Reviews-")[[1]][2] 
  
  if (grepl("Hotel_Review",url))
  {
    
    urllinkpre.rev=gsub("Hotel_Review","ShowUserReviews",strsplit(url,"Reviews-")[[1]][1])
  } else if (grepl("Attraction_Review",url)) {
    
    urllinkpre.rev=gsub("Attraction_Review","ShowUserReviews",strsplit(url,"Reviews-")[[1]][1]) }
  else {
    urllinkpre.rev=gsub("Restaurant_Review","ShowUserReviews",strsplit(url,"Reviews-")[[1]][1])
  }
  
  urlrev=paste(urllinkpre.rev,"rXX-",urllinkpost,sep="")
  
  urlfullrevlist=gsub("XX",id,urlrev)
  
  return(urlfullrevlist)
  
}

#Potegni dol celotni review
getFullRev<-function(id){
  
  
  
  urlfull<-urlPrepare(url,id)
  
  
  # get html content of page containing full review
  doc=htmlTreeParse(urlfull,useInternalNodes=TRUE)
  # extract node set containing full review
  revid=paste("review_",id,sep="")
  qry=paste("//p[@id='",revid,"']",sep="")
  ns_fullrev=getNodeSet(doc,qry)
  
  
  fullrev=xmlValue(ns_fullrev[[1]])
  
  return(gsub("\n","",fullrev))
  
}