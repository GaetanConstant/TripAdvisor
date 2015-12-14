require(plyr) 
library(dplyr)
library(lubridate)
library(ggplot2)
library(tm)
library(scales)
library(topicmodels)
library("knitr")
library("xlsx")
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(xtable)
library(lda)
library(reshape)
library(graph) 
library(Rgraphviz)
library(data.table)

#Preberem za ?ustveno analizo

positive=scan("positive-words.txt",what="character",comment.char=";")

negative=scan("negative-words.txt",what="character",comment.char=";")

### Ratings over time graph

getrev.bymon=function(hotel){
  
  # get review data
  dfrating=dfrating.l[[hotel]]$dfrating
  
  # create a month label
  dfrating$yrmon=floor_date(dfrating$ratingdt,"month")
  
  # create sequence of months
  yrmon=na.omit(unique(dfrating$yrmon))
  yrmonseq=seq(min(yrmon),max(yrmon),by="months")
  
  # yr-month and rating combinations
  yrmon.rating=expand.grid(yrmon=yrmonseq,ratingnum=c(1,2,3,4,5))
  
  # get % of reviews for each rating by month
  dfrating.bymon=dfrating%>%group_by(yrmon,ratingnum)%>%summarize(count=n())
  dfrating.bymon.agg=dfrating.bymon%>%group_by(yrmon)%>%summarize(countfull=sum(count))
  dfrating.bymon=merge(dfrating.bymon,dfrating.bymon.agg,c("yrmon"))
  dfrating.bymon$pctrating=dfrating.bymon$count/dfrating.bymon$countfull
  
  
  dfrating.bymon=merge(yrmon.rating,dfrating.bymon,by=c("yrmon","ratingnum"),all.x=TRUE)
  dfrating.bymon$pctrating[is.na(dfrating.bymon$pctrating)]=0
  dfrating.bymon$count[is.na(dfrating.bymon$count)]=0
  dfrating.bymon$countfull[is.na(dfrating.bymon$countfull)]=0
  
  dfrating.bymon$hotel=hotel
  
  # get number of reviews by month
  dfrating.bymon.revs=dfrating.bymon%>%group_by(yrmon)%>%summarize(numrevs=sum(count))
  dfrating.bymon.revs$hotel=hotel
  
  return(list(dfrating.bymon=dfrating.bymon,dfrating.bymon.revs=dfrating.bymon.revs))
  
}

# Explore top level quotes for each rating

# function to get document-term matrix from hotel review data for a given hotel
getDTM=function(dftxt){
  
  # code adapted from http://www.rdatamining.com/examples/text-mining
  
  txtcorpus=Corpus(VectorSource(dftxt))
  #inspect(txtcorpus[1:5])
  
  
  
  txtcorpus.cl=tm_map(txtcorpus, content_transformer(tolower))
  txtcorpus.cl=tm_map(txtcorpus.cl,removePunctuation)
  txtcorpus.cl=tm_map(txtcorpus.cl,removeNumbers)
  
  mystopwords=c(stopwords("english"),"hotel","staff","room","rooms","indianapolis","marriott","conference",
                "convention","indy","downtown","hampton","stay","stayed","inn","conrad")
  txtcorpus.cl=tm_map(txtcorpus.cl,removeWords,mystopwords)
  #dictCorpus=txtcorpus.cl
  #txtcorpus.cl=tm_map(txtcorpus.cl,stemDocument)
  #txtcorpus.cl=tm_map(txtcorpus.cl,stemCompletion,dictionary=dictCorpus)
  
  dtm=DocumentTermMatrix(txtcorpus.cl)
  
  dtm.m=as.matrix(dtm)
  
  return(dtm.m)
}

getTopTerms=function(hotel){
  
  # get review data
  dfrating=dfrating.l[[hotel]]$dfrating
  
  minrating=min(dfrating$ratingnum)
  maxrating=max(dfrating$ratingnum)
  tfreq.l=as.list(rep(NA,maxrating-minrating+1))
  
  # of frequent words to retain
  numterms=10
  
  rating=maxrating+1
  
  for(i in 1:(maxrating-minrating+1)){
    
    rating=rating-1
    #sprintf("Processing data for %s stars",rating)
    
    dftxt=dfrating$topquote[dfrating$ratingnum==rating]
    
    dtm.m=getDTM(dftxt)
    
    tfreq=colSums(dtm.m)
    tfreq.l[[i]]=names(sort(tfreq,decreasing=TRUE)[1:numterms])
  }
  
  topTerms=do.call(cbind,tfreq.l)
  colnames(topTerms)=paste(seq(maxrating,minrating)," star")
  
  return(list(dtm.m=dtm.m,topTerms=topTerms))
  
}

# Further investigation of high star rating using full reviews for a hotel

# functiont to cluster words from term document matrix of full reviews for a hotel

getClust=function(hotel){
  
  # load hotel full review data
  dfrating=dfrating.l[[hotel]]$dfrating
  
  dftxt=dfrating$fullrev[dfrating$ratingnum>=4]
  dtm.m=getDTM(dftxt)
  
  # clustering of words to detect themes/topics
  
  set.seed(1234)
  txtclust=kmeans(t(dtm.m),5)
  
  # size of clusters
  txtclust$size
  
  # within and total sum of squares
  txtclust$totss
  txtclust$withinss
  
  # get list of frequent terms in each cluster
  clustTerms=as.list(rep(NA,5))
  
  termlist=colnames(dtm.m)
  for(i in 1:5){
    termlist.filt=termlist[txtclust$cluster == i]
    tfreq=colSums(dtm.m)
    tfreq.filt=sort(tfreq[termlist.filt],decreasing=TRUE)
    clustTerms[[i]]=names(tfreq.filt[1:10])  
  }
  clust.topic=do.call(cbind,clustTerms)
  clust.topic[is.na(clust.topic)]=""
  colnames(clust.topic)=c("cluster 1","cluster 2","cluster 3","cluster 4","cluster 5")
  
  # print list of frequent terms in each cluster
  return(list(txtclust=txtclust,clust.topic=clust.topic))
  
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

fLDA1=function(ocena=TRUE, mesta=FALSE, kljucne=TRUE,custva=FALSE)
{
  
  if (ocena==TRUE && mesta==FALSE)
  {
    
    oc=as.list(unique(dtAll[,3]))
    
    for (i in 1:length(oc))
    {
      
      mark=oc[[i]]
      
      print("Ocena")  
      print(mark)
      
      besedilo=dtAll[dtAll[,3]==mark,4]
      
      if (length(besedilo)>0 && kljucne==TRUE)
      {    
        try(fLDA(besedilo))  
      }
      
      
      if (length(besedilo)>0 && custva==TRUE)
      {   
        
        text=dtAll[dtAll[,3]==mark,]
        custva=ggplot(text, aes(x=Emocije)) + geom_bar(fill="#FF9999")+
          geom_text(stat="bin", aes(
            label=..count.. 
          ))+
          ggtitle("Distribucija ?ustev ")+  scale_fill_brewer(palette="Spectral") 
        
        print(custva)
        
        emotion(text)
      }
      
    }
    
    
  }
  
  if (mesta==TRUE && ocena==FALSE)
  {
    
    
    
    
    ms=as.list(unique(dtAll[,5]))
    
    for (i in 1:length(ms))
      
    {  
      
      mesto=as.vector(ms[[i]])
      
      print("Mesto")  
      print(mesto)
      
      besedilo=dtAll[dtAll[,5]==mesto,]
      
      if (length(besedilo)>0 && kljucne==TRUE)
      {    
        try(fLDA(besedilo))   
      }
      
      if (length(besedilo)>0 && custva==TRUE)
      {   
        
        text=dtAll[dtAll[,5]==mesto,]
        custva=ggplot(text, aes(x=Emocije)) + geom_bar(fill="#FF9999")+
          geom_text(stat="bin", aes(
            label=..count.. 
          ))+
          ggtitle("Distribucija ?ustev ")+  scale_fill_brewer(palette="Spectral") 
        
        print(custva)
        
        emotion(text)
      }
      
    }
    
  }
  
  if (mesta==TRUE && ocena==TRUE )
  {
    
    oc=as.list(unique(dtAll[,3]))
    
    ms=as.list(unique(dtAll[,5]))
    
    for (i in 1:length(ms))
      
    {  
      
      mesto=as.vector(ms[[i]])
      
      print("Mesto")  
      print(mesto)
      
      for (j in 1:length(oc))
        
      {
        
        
        mark=oc[[j]]
        
        print("Ocena")  
        
        print(mark)  
        
        besedilo=dtAll[dtAll[,5]==mesto && dtAll[,3]==mark,4]
        
        
        if (length(besedilo)>0 && kljucne==TRUE)
        {    
          try(fLDA(besedilo))  
        }
        
        if (length(besedilo)>0 && custva==TRUE)
        {   
          
          text=dtAll[dtAll[,5]==mesto && dtAll[,3]==mark,]
          custva=ggplot(text, aes(x=Emocije)) + geom_bar(fill="#FF9999")+
            geom_text(stat="bin", aes(
              label=..count.. 
            ))+
            ggtitle("Distribucija ?ustev ")+  scale_fill_brewer(palette="Spectral") 
          
          print(custva)
          
          emotion(text)
        }
        
      }
      
      
      
    }
    
  }
  
  
  
}


fLDA= function(a)
{
  
  besedilo=tekst
  
  myCorpus <- Corpus(VectorSource(besedilo))
  
  # build a corpus, which is a collection of text documents
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  
  # remove punctuation
  myCorpus <- tm_map(myCorpus, removePunctuation)
  
  # remove numbers
  myCorpus <- tm_map(myCorpus, removeNumbers)
  
  
  
  
  # remove stopwords
  # keep "r" by removing it from stopwords
  
  stop="english"
  
  
  # Grem ?e ?ez angle?ki stop words
  
  customStop <- c(stopwords("english"),"askfs","hotel","spindelegger","directpr","greekpmlive","cdm","also","will")
  
  customStop=c(customStop,"can","see","first","fragam","must","reynders","begov","didier","uccle","rtbf","rtl")
  
  customStop=c(customStop,"yes","just","ready","glader","danish","one","katainen","tuomioja","stub")
  
  customStop=c(customStop,"video","direct","francois","holland","confpr","mali","must","greek","taoiseach")
  
  customStop=c(customStop,"irish","enda","bruton","kenny","now","new","say","clock","rutte","dutch","van")
  
  customStop=c(customStop,"ploumen","T?naiste","Fran?ois","tpniinisto")
  
  doc <- tm_map(myCorpus, removeWords, customStop)
  
  # # Stemming documents
  # doc.copy <- myCorpus
  
  
  #  myCorpus=try(tm_map(myCorpus, stemDocument))
  
  #  doc <- try(tm_map(myCorpus, stemCompletion, dictionary=doc.copy))
  
  doc <- tm_map(doc, stripWhitespace)
  
  tdm <- TermDocumentMatrix(doc, control = list(wordLengths = c(4, Inf)))
  
  freq.terms <- findFreqTerms(tdm, lowfreq = 5)
  
  
  
  print("Najpogostej?ih 20 besed")
  
  term.freq <- rowSums(as.matrix(tdm)) 
  term.freq <- subset(term.freq, term.freq >= 5)
  
  term.freq=sort(term.freq, decreasing=T)[1:20]
  
  
  
  
  df <- data.frame(term = names(term.freq), freq = term.freq) 
  
  print(df)
  
  graf=ggplot(df, aes(x = term, y = freq)) + 
    geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip() 
  
  print(graf)
  
  print("Korelacije najpogostej?ih besed ve?je od 0.99999")
  plot(tdm, term = names(term.freq), corThreshold = 0.99999, weighting = T) 
  
  dtm <- DocumentTermMatrix(doc, control = list(wordLengths =c(3,Inf), weighting =weightTf))
  
  rowTotals <- apply(dtm , 1, sum) #Find the sum of words in   each Document
  
  dtm.new   <- dtm[rowTotals> 0, ] 
  
  
  k <- 3
  
  lda <- LDA(dtm.new, k ) # find 8 topics 
  term <- terms(lda, 7) # first 
  
  print("Prvih 7 besed vsake teme")
  
  print(term)
  
  
  gammaDF <- as.data.frame(lda@gamma) 
  names(gammaDF) <- c(1:k)
  
  gammaDF
  
  #   print("Teme in dokumenti")
  #   
  toptopics <- as.data.frame(cbind(document = row.names  (gammaDF), 
                                   topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
  #   
  #   # inspect...
  #   print(toptopics)
  #   
  
  
  topic1=toptopics[!is.na(toptopics$topic),]
  
  #print(topic1)
  
  print("Frekvence tem")
  
  counts=table(topic1$topic)
  
  print(counts)
  
  
  
  
  
  
  #   
}

getSentimentBayes=function(text)
{
  
  
  ###Priprava teksta
  
  # remove punctuation
  some_txt = gsub("[[:punct:]]", "", text)
  # remove numbers
  some_txt = gsub("[[:digit:]]", "", some_txt)
  # remove html links
  some_txt = gsub("http\\w+", "", some_txt)
  # remove unnecessary spaces
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  
  # define "tolower error handling" function 
  try.error = function(x)
  {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  # lower case using try.error with sapply 
  some_txt = sapply(some_txt, try.error)
  
  # remove NAs in some_txt
  some_txt = some_txt[!is.na(some_txt)]
  names(some_txt) = NULL
  
  
  ## Dejanska klasifikacija
  
  # classify emotion
  class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
  # get emotion best fit
  emotion = class_emo[,7]
  # substitute NA's by "unknown"
  emotion[is.na(emotion)] = "unknown"
  
  # classify polarity
  class_pol = classify_polarity(some_txt, algorithm="bayes")
  # get polarity best fit
  polarity = class_pol[,4]
  
  
  sent_df = data.frame(text=some_txt, emotion=emotion,
                       polarity=polarity, stringsAsFactors=FALSE)
  
  # sort data frame
  sent_df = within(sent_df,
                   emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
  
  return(sent_df)
  
}

emotion=function(besedilo)
{
  # separating text by emotion
  emos = levels(factor(besedilo$Emocije))
  nemo = length(emos)
  emo.docs = rep("", nemo)
  
  
  for (i in 1:nemo)
  {
    tmp = besedilo[besedilo$Emocije == emos[i],]
    emo.docs[i] = paste(tmp, collapse=" ")
  }
  
  # remove stopwords
  emo.docs = removeWords(emo.docs, stopwords("english"))
  # create corpus
  corpus = Corpus(VectorSource(emo.docs))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  colnames(tdm) = emos
  
  # comparison word cloud
  oblak=comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                         scale = c(3,.5), random.order = FALSE, title.size = 1.5)
  
  print(oblak)
}