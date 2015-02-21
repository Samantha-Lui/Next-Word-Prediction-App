library(data.table)
options( java.parameters = "-Xmx50g" )
library(tm)
library(qdap)
library(openNLP)
library(RWeka)


###
## Load all documents from the local repo US_English.
###
en_US_path <- file.path ( "." , "en_US" )
en_US.corpus <- Corpus(DirSource(en_US_path, encoding="UTF-8"),
                       readerControl=list(language="eng", reader=readPlain))
## Check the lengths of each corpus.
lengths <- sapply(1:3,function(i) length(en_US.corpus[[i]]$content));lengths



### 
## Create the training corpus which consists of 80% of each document.
###
en_US.corpus[[1]]$content <- 
        en_US.corpus[[1]]$content[1:ceiling(.8*length(en_US.corpus[[1]]$content))]
en_US.corpus[[2]]$content <- 
        en_US.corpus[[2]]$content[1:ceiling(.8*length(en_US.corpus[[2]]$content))]
en_US.corpus[[3]]$content <- 
        en_US.corpus[[3]]$content[1:ceiling(.8*length(en_US.corpus[[3]]$content))]
lengths <- sapply(1:3,function(i) length(en_US.corpus[[i]]$content));lengths
## Combine the three corpora.
text <- c(en_US.corpus[[1]]$content, en_US.corpus[[2]]$content, en_US.corpus[[3]]$content)
rm(en_US.corpus)





### 
## Clean the corpus.
###
corpus <- VCorpus(VectorSource(text)); 
## Remove the text since it has becomes redundant and call the garbage collector.
rm(text); gc(); 
save(corpus, file="rawCorpus.RData")
## Convert all character into utf8 encoding.
corpus <- tm_map(corpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
## Convert all character into lower cases.
corpus <- tm_map(corpus, content_transformer(tolower))
## Replace the specified symbols by a white space.
corpus <- tm_map(corpus, 
                 content_transformer(function(x, pattern) gsub(pattern, " ", x)), 
                 "/|@|\\|~|_|\\*|#|%|\\^|&")
## Eliminate the extra white spaces.
corpus <- tm_map(corpus, stripWhitespace)
## Remove the number in the corpus.
corpus <- tm_map(corpus, removeNumbers)
## Break the corpus down into a vector having a single document as an entry.
corpus <- as.vector(unlist(sapply(corpus, '[',"content")))
save(corpus, file ="corpus.RData")



load("corpus.RData")
###
## Tokenize the corpus into 1-, 2-, 3- and 4-grams and tabulates the results 
## accordingly.
##
## The corpus is broken down into subcorpora during the process due to its size.
##
## The n-gram table is updated on each iteration to minimize the memory space 
## occupied.
###
# Tokenization for unigram(1-gram).
unigram <- data.table()
n <- 5
k <- ceiling(length(corpus)/n)
for(i in 1:n){
        start <- (i-1)*k+1
        end <- i*k
        if(i==n)
                end <- length(corpus)
        tempTokens <- NGramTokenizer(corpus[start:end], Weka_control(min=1,max=1))
        tempTable <- data.table(word=tempTokens, freq=c(1))
        unigram <- rbind(unigram, tempTable)
        setkey(unigram, word)
        unigram <- unigram[, sum(freq), by=word]
        setnames(unigram, names(unigram), c("word","freq"))
}
## Check for NA and empty entries.
which(unigram$word==""); which(unigram$word==NA)
## Remove the empty entry in the unigram model
unigram <- unigram[-which(unigram$word==""), ]
save(unigram, file="Grand_Unigram.RData")
rm(tempTokens, tempTable, unigram); gc()

## Tokenization for bigram(2-gram).
bigram <- data.table()
n <- 10
k <- ceiling(length(corpus)/n)
for(i in 1:n){
        start <- (i-1)*k+1
        end <- i*k
        if(i==n)
                end <- length(corpus)
        tempTokens <- NGramTokenizer(corpus[start:end], Weka_control(min=2,max=2))
        tempTable <- data.table(word=tempTokens, freq=c(1))
        bigram <- rbind(bigram, tempTable)
        setkey(bigram, word)
        bigram <- bigram[, sum(freq), by=word]
        setnames(bigram, names(bigram), c("word","freq"))
}
## Check for NA and empty entries.
which(bigram$word==""); which(bigram$word==NA)
save(bigram, file="Grand_Bigram.RData")
rm(tempTokens, tempTable, bigram); gc()

## Tokenization for trigram(3-gram).
trigram <- data.table()
n <- 20
k <- ceiling(length(corpus)/n)
for(i in 1:n){
 print(i)
        start <- (i-1)*k+1
        end <- i*k
        if(i==n)
                end <- length(corpus)
        tempTokens <- NGramTokenizer(corpus[start:end], Weka_control(min=3,max=3))
        tempTable <- data.table(word=tempTokens, freq=c(1))
        trigram <- rbind(trigram, tempTable)
        setkey(trigram, word)
        trigram <- trigram[, sum(freq), by=word]
        setnames(trigram, names(trigram), c("word","freq"))
}
## Check for NA and empty entries.
which(trigram$word==""); which(trigram$word==NA)
save(trigram, file="Grand_Trigram.RData")
rm(tempTokens, tempTable, trigram); gc()

## Tokenization for quadgram(4-gram).
quadgram <- data.table()
n <- 25
k <- ceiling(length(corpus)/n)
for(i in 1:n){
  print(i)
        start <- (i-1)*k+1
        end <- i*k
        if(i==n)
                end <- length(corpus)
        tempTokens <- NGramTokenizer(corpus[start:end], Weka_control(min=4,max=4))
        tempTable <- data.table(word=tempTokens, freq=c(1))
        quadgram <- rbind(quadgram, tempTable)
        setkey(quadgram, word)
        quadgram <- quadgram[, sum(freq), by=word]
        setnames(quadgram, names(quadgram), c("word","freq"))
}
## Check for NA and empty entries.
which(quadgram$word==""); which(quadgram$word==NA)
save(quadgram, file="Grand_Quadgram.RData")
rm(tempTokens, tempTable, quadgram, corpus); gc()