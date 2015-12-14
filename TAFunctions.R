# sentiment analysis
#funkcija mora obvezno prejeti tekstovni DF, ki mora imeti na prvem mestu ID, na drugem pa text fff

doSentiment<-function(lexDF,textDF,join=FALSE)
{
  dtm.control<-list(
    tolower           = TRUE,
    removePunctuation = TRUE,
    removeNumbers     = TRUE,
    stopwords         = c(stopwords("english"),"hotel"),
    stemming          = FALSE,
    wordLengths       = c(3,"inf")   
    
  )
  
  noteSentiment<-mat.or.vec(nrow(textDF),2)
  
  colnames(noteSentiment)<- c("text","SentimentTotal")
  
  word.frequency                <- vector("list",nrow(textDF))
  word.frequency.sentiment     <- vector("list",nrow(textDF))
  
  for (f in 1:nrow(textDF))
  {
    noteSentiment[f,1] <- textDF[f,1]
    
    
    #Create corpus
    corp<-Corpus(VectorSource(textDF[f,1]))
    
    
    #Create DTM
    dtm <- DocumentTermMatrix(corp, control=dtm.control)
    
    
    #Find frequencis of words
    TermFreq <- data.frame(findFreqTerms(dtm), dtm$v)
    
    # Store in list for ruture use
    word.frequency[[f]] <- data.frame(findFreqTerms(dtm), dtm$v)
    
    colnames(word.frequency[[f]]) <- c("Word","Frequency")
    
    
    # Find global sentiment of text
    sentwords       <- intersect(findFreqTerms(dtm), lexDF[,1])
    sentwords.index <- match(sentwords, lexDF[,1])
    
    sentimentDF=lexDF[sentwords.index, ]
    colnames(sentimentDF) <- c("Word","sentIndex")
    
    word.frequency.sentiment[[f]] <- merge(word.frequency[[f]],sentimentDF,by="Word")
    
    if(length(sentwords.index)>0) #if there are any sentiment words
    {
      
      sentwords.score     <- lexDF[sentwords.index, 2]
      sentwords.freq      <- TermFreq[match(sentwords, TermFreq[,1]), 2]
      noteSentiment[f, 2] <- sum(sentwords.score*sentwords.freq)
    }
    else {
      noteSentiment[f, 2] <- 0
      
    }
    
    
  }
  
  if (join==FALSE)
  {
    
    wordsSentiment=try(do.call(rbind,word.frequency.sentiment))
    
    wST=data.table(wordsSentiment)
    
    setkey(wST, Word,sentIndex)
    
    resultsDF<-as.data.frame(wST[, sum(Frequency, na.rm = TRUE),by = list(Word,sentIndex)])
    
    colnames(resultsDF) <- c("word","sentiment","frequency")
    
    
    ###Povzetek sentiment analize
    
    freq=as.data.frame(table(noteSentiment[,2]))
    
    colnames(freq) <- c("scores","freq")
    
    print("Tabela frekven?ne porazdelitve sentiment analize")
    
    print(freq)
    
    print("Histogram sentiment analize")
    
    hist(noteSentiment[,2])
    
    print ("Povzetek sentiment analize")
    
    print (summary(noteSentiment[,2]))
    
    ###Najpogostej?e besede po sentimentu
    
    print("Najpogostej?e besede po sentimentu")
    
    sentimentID=as.list(sort(unique(resultsDF$sentiment)))
    
    sapply(sentimentID,function(x) {
      
      print("Vrednost sentimenta besede")
      print(x)
      
      
      freq.sorted <- resultsDF[resultsDF[,2]==x,]
      
      print(head(freq.sorted[order(-freq.sorted$frequency),],6))
      
    })
    
    ##Najpogostej?e negativne besede
    print("Najpogostejse negativne besede v celoti")
    freq.sorted <- resultsDF[resultsDF[,2]<0,]
    
    print(head(freq.sorted[order(-freq.sorted$frequency),],6))
    
    
    
    ##Najpogostej?e pozitivne besede
    print("Najpogostejse pozitivne besede v celoti")
    freq.sorted <- resultsDF[resultsDF[,2]>0,]
    
    print(head(freq.sorted[order(-freq.sorted$frequency),],6))
    
  }
  
  return (noteSentiment)
}

###New LDA
doLDA<-function(nTopic,textDF)
{
  
  dtm.control<-list(
    tolower           = TRUE,
    removePunctuation = TRUE,
    removeNumbers     = TRUE,
    stopwords         = c(stopwords("english"),"hotel"),
    stemming          = TRUE,
    wordLengths       = c(3,"inf"),
    weightinh         = weightTf
    
    
  )
  
  #Create corpus
  corp<-Corpus(VectorSource(textDF[,1]))
  
  
  #Create DTM
  dtm <- DocumentTermMatrix(corp, control=dtm.control)
  
  burnin <- 500
  
  iter <- 5000
  
  keep <- 30
  
  mods <- LDA(dtm, nTopic,
              method ="Gibbs",
              control = list (burnin = burnin,
                              iter = iter,
                              keep = keep))
  
  term <- terms(mods, 7) # first 
  #   
  print("Prvih 7 besed vsake teme")
  #   
  print(term)
  
  
  
  #   reviews <- as.vector(textDF[['text']])
  #   
  #   stop_words <- stopwords("English")
  #   
  #   # pre-processing:
  #   reviews <- gsub("'", "", reviews)  # remove apostrophes
  #   reviews <- gsub("[[:punct:]]", " ", reviews)  # replace punctuation with space
  #   reviews <- gsub("[[:cntrl:]]", " ", reviews)  # replace control characters with space
  #   reviews <- gsub("^[[:space:]]+", "", reviews) # remove whitespace at beginning of documents
  #   reviews <- gsub("[[:space:]]+$", "", reviews) # remove whitespace at end of documents
  #   reviews <- tolower(reviews)  # force to lowercase
  #   
  #   # tokenize on space and output as a list:
  #   doc.list <- strsplit(reviews, "[[:space:]]+")
  #   
  #   # compute the table of terms:
  #   term.table <- table(unlist(doc.list))
  #   term.table <- sort(term.table, decreasing = TRUE)
  #   
  #   # remove terms that are stop words or occur fewer than 5 times:
  #   del <- names(term.table) %in% stop_words | term.table < 5 
  #   term.table <- term.table[!del]
  #   vocab <- names(term.table)
  #   
  #   # now put the documents into the format required by the lda package:
  #   get.terms <- function(x) {
  #     index <- match(x, vocab)
  #     index <- index[!is.na(index)]
  #     rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
  #   }
  #   documents <- lapply(doc.list, get.terms)
  #   
  #   D <- length(documents)  # number of documents (2,000)
  #   
  #   W <- length(vocab)  # number of terms in the vocab (14,568)
  #   
  #   doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
  #   
  #   N <- sum(doc.length)  # total number of tokens in the data 
  #   
  #   term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]
  #   
  #   
  #   K <- nTopic  #?tevilo tem
  #   G <- 4000
  #   alpha <- 0.02
  #   eta <- 0.02
  #   
  #   # Fit the model:
  #   fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
  #                                      num.iterations = G, alpha = alpha, 
  #                                      eta = eta, initial = NULL, burnin = 0,
  #                                      compute.log.likelihood = TRUE)
  #   
  #   term <- terms(fit, 7) # first 
  #   
  #   print("Prvih 7 besed vsake teme")
  #   
  #   print(term)
  #   
  # #   theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
  # #   
  # #   phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
  # #   
  #   
  # #   HotelReviews <- list(
  # #                        phi = phi,
  # #                        theta = theta,
  # #                        doc.length = doc.length,
  # #                        vocab = vocab,
  # #                        term.frequency = term.frequency)
  # #   
  # #   
  # #   
  # #   json <- createJSON(
  # #                      phi = HotelReviews$phi, 
  # #                      theta = HotelReviews$theta, 
  # #                      doc.length = HotelReviews$doc.length, 
  # #                      vocab = HotelReviews$vocab, 
  # #                      term.frequency = HotelReviews$term.frequency)
  # #   
  # #   
  # #   serVis(json, out.dir = 'vis', open.browser = TRUE)
  #   
  
}

doSyu<-function(textDF)
{
  textDF <- as.vector(textDF[['text']])
  
  sents <- get_sentiment(textDF, method="nrc")
  
  plot(
    sents, 
    type="h", 
    main="Trajektorija sentimenta po dokumentih", 
    xlab = "Dokumenti", 
    ylab= "Emotional Valence"
  )
  
  nrc_data <-get_nrc_sentiment(textDF)
  
  nrcDF=as.data.frame(sort(colSums(prop.table(nrc_data[, 1:8]))*100))
  
  
  colnames(nrcDF)[1] <- c("Percentage")
  
  print("Emocije v recenzijah")
  
  pander::pandoc.table(nrcDF)
  
  barplot(
    sort(colSums(prop.table(nrc_data[, 1:8]))*100), 
    horiz = TRUE, 
    cex.names = 0.7, 
    las = 1, 
    main = "Emocije v reviews z nrc metodo", xlab="Percentage"
  )
  
  
  print("Povzetek sentiment analize z nrc metodo")
  
  print(summary(sents))
  
  
  
}

doFrequentWords<-function(text,xcorr=0.2)
  
{  
  
  dtm.control<-list(
    tolower           = TRUE,
    removePunctuation = TRUE,
    removeNumbers     = TRUE,
    stopwords         = c(stopwords("english"),"one","just","can","also","hotel"),
    stemming          = FALSE,
    wordLengths       = c(4,"inf")   
    
  )
  
  corp<-Corpus(VectorSource(text))
  
  
  #Create TDM
  
  tdm <- TermDocumentMatrix(corp, control = dtm.control)
  
  removeSparseTerms(tdm, 0.2)
  
  freq.terms <- findFreqTerms(tdm, lowfreq = 5)
  
  
  print("Najpogostejsih 20 besed")
  
  term.freq <- rowSums(as.matrix(tdm)) 
  term.freq <- subset(term.freq, term.freq >= 5)
  
  term.freq=sort(term.freq, decreasing=T)[1:20]
  
  
  df <- data.frame(term = names(term.freq), freq = term.freq) 
  
  print(df)
  
  graf=ggplot(df, aes(x = term, y = freq)) + 
    geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip() 
  
  print(graf)
  
  print(paste("Korelacije najpogostejsih besed večjih od ",xcorr,sep=""))
  plot(tdm, term = names(term.freq), corThreshold = xcorr, weighting = T) 
  
  rm("tdm")
  
  try(mem_change(rm("tdm")))
  try(gc())
  
}