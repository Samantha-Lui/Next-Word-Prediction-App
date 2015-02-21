library(data.table)
library(stringr)


## Source the helper functions.
source("Helpers.R")


############################  SECTION 1 ########################################
###
## Find the total number of unique words covering 97% of the training corpus
## and replace the other 31% with the special character "UNK" for the unigram.
###
load("Grand_Unigram.RData")
unigram <- unigram[order(-unigram$freq),]
cover <- coverage(unigram$freq, .97); cover
unigram[cover,] .
threshold <- max(which(unigram$freq>=unigram$freq[cover]))
## Replace the other 3% with the special character "UNK".
unigram$word[(threshold+1):length(unigram$word)] <- "UNK"
## Update the unigram table.
unigram <- getTotalFreq(unigram$word, unigram$freq)
setnames(unigram, names(unigram), c("w1","c"))
save(unigram, file="Trimmed_Unigram.RData")
## Create a dictionary from words in the unigram
## which will be used as a reference for the words
## in the higher-order N-grams.
dict <- unigram$w1
save(dict, file="Dictionary.RData")
rm(unigram); gc()



###
## Replace the consistuent words in the bigrams that are not covered in the 
## 97% of the training corpus with the special character 'UNK' and update the 
## the bigram table.
###
load("Grand_Bigram.RData")
## Break the bigrams into the consistuent words and store them as separate 
## entities.
bigram.w1 <- c()
bigram.w2 <- c()
n <- 50
k <- as.integer(length(bigram$word)/n)
for(j in 1:n)
{
  print(j)
  start <- (j-1)*k + 1
  end <- j*k
  if(j==n)    end <- length(bigram$word)
  bi <- bigram$word[start:end]
  bg <- sapply(1:length(bi), function(i) strsplit(bi[i]," "))
  bigram.w1 <- c(bigram.w1, sapply(1:length(bg), function(i) bg[[i]][1]))
  bigram.w2 <- c(bigram.w2, sapply(1:length(bg), function(i) bg[[i]][2]))
}
rm(bi, bg)
bigram <- bigram[,word:=NULL,]
## Replace the words not covered in the 97th percentile of the training
## corpus with the special character "UNK".
bigram.w1[which(!bigram.w1 %in% dict)] <- "UNK"
bigram.w2[which(!bigram.w2 %in% dict)] <- "UNK"
bigram$word <- paste(bigram.w1, bigram.w2); rm(bigram.w1, bigram.w2)
bigram <- getTotalFreq(bigram$word, bigram$freq)
setnames(bigram, names(bigram), c("word","c"))
bigram.w1 <- c()
bigram.w2 <- c()
n <- 50
k <- as.integer(length(bigram$word)/n)
for(j in 1:n)
{
  print(j)
  start <- (j-1)*k + 1
  end <- j*k
  if(j==n)    end <- length(bigram$word)
  bi <- bigram$word[start:end]
  bg <- sapply(1:length(bi), function(i) strsplit(bi[i]," "))
  bigram.w1 <- c(bigram.w1, sapply(1:length(bg), function(i) bg[[i]][1]))
  bigram.w2 <- c(bigram.w2, sapply(1:length(bg), function(i) bg[[i]][2]))
}
rm(bi, bg)
bigram <- bigram[,word:=NULL,]
bigram$w1 <- bigram.w1; bigram$w2 <- bigram.w2
rm(bigram.w1, bigram.w2); gc()
save(bigram, file="Trimmed_Bigram.RData")
rm(bigram); gc()


###
## Replace the consistuent words in the trigrams that are not covered in the 
## 97% of the training corpus with the special character 'UNK' and updates the 
## the trigram table.
###
load("Grand_Trigram.RData")
## Breaks the trigrams into the consistuent words and stores them as separate 
## entities.
trigram.w1 <- c()
trigram.w2 <- c()
trigram.w3 <- c()
n <- 100
k <- as.integer(length(trigram$word)/n)
for(j in 1:n)
{
  print(j)
  start <- (j-1)*k + 1
  end <- j*k
  if(j==n)    end <- length(trigram$word)
  tri <- trigram$word[start:end]
  tg <- sapply(1:length(tri), function(i) strsplit(tri[i]," "))
  trigram.w1 <- c(trigram.w1, sapply(1:length(tg), function(i) tg[[i]][1]))
  trigram.w2 <- c(trigram.w2, sapply(1:length(tg), function(i) tg[[i]][2]))
  trigram.w3 <- c(trigram.w3, sapply(1:length(tg), function(i) tg[[i]][3]))
}
rm(tri, tg)
trigram <- trigram[,word:=NULL,]
trigram.w1[which(!trigram.w1 %in% dict)] <- "UNK"
trigram.w2[which(!trigram.w2 %in% dict)] <- "UNK"
trigram.w3[which(!trigram.w3 %in% dict)] <- "UNK"
trigram$word <- paste(trigram.w1, trigram.w2, trigram.w3)
rm(trigram.w1, trigram.w2, trigram.w3)
trigram <- getTotalFreq(trigram$word, trigram$freq)
setnames(trigram, names(trigram), c("word","c"))
trigram.w1 <- c()
trigram.w2 <- c()
trigram.w3 <- c()
n <- 100
k <- as.integer(length(trigram$word)/n)
for(j in 1:n)
{
  print(j)
  start <- (j-1)*k + 1
  end <- j*k
  if(j==n)    end <- length(trigram$word)
  tri <- trigram$word[start:end]
  tg <- sapply(1:length(tri), function(i) strsplit(tri[i]," "))
  trigram.w1 <- c(trigram.w1, sapply(1:length(tg), function(i) tg[[i]][1]))
  trigram.w2 <- c(trigram.w2, sapply(1:length(tg), function(i) tg[[i]][2]))
  trigram.w3 <- c(trigram.w3, sapply(1:length(tg), function(i) tg[[i]][3]))
}
rm(tri, tg)
trigram <- trigram[,word:=NULL,]
trigram$w1 <- trigram.w1; trigram$w2 <- trigram.w2; trigram$w3 <- trigram.w3
rm(trigram.w1, trigram.w2, trigram.w3)
save(trigram, file="Trimmed_Trigram.RData")
rm(trigram); gc()



###
## Replace the consistuent words in the quadgrams that are not covered in the 
## 97% of the training corpus with the special character 'UNK' and update the 
## the quadgram table.
###
load("Grand_Quadgram.RData")
## Breaks the trigrams into the consistuent words and stores them as separate 
## entities.
quadgram.w1 <- c()
quadgram.w2 <- c()
quadgram.w3 <- c()
quadgram.w4 <- c()
n <- 250
k <- as.integer(length(quadgram$word)/n)
for(j in 1:n)
{
  print(j)
  start <- (j-1)*k + 1
  end <- j*k
  if(j==n)    end <- length(quadgram$word)
  quad <- quadgram$word[start:end]
  qg <- sapply(1:length(quad), function(i) strsplit(quad[i]," "))
  quadgram.w1 <- c(quadgram.w1, sapply(1:length(qg), function(i) qg[[i]][1]))
  quadgram.w2 <- c(quadgram.w2, sapply(1:length(qg), function(i) qg[[i]][2]))
  quadgram.w3 <- c(quadgram.w3, sapply(1:length(qg), function(i) qg[[i]][3]))
  quadgram.w4 <- c(quadgram.w4, sapply(1:length(qg), function(i) qg[[i]][4]))
}
rm(quad, qg)
quadgram <- quadgram[,word:=NULL,]
quadgram.w1[which(!quadgram.w1 %in% dict)] <- "UNK"
quadgram.w2[which(!quadgram.w2 %in% dict)] <- "UNK"
quadgram.w3[which(!quadgram.w3 %in% dict)] <- "UNK"
quadgram.w4[which(!quadgram.w4 %in% dict)] <- "UNK"
quadgram$word <- paste(quadgram.w1, quadgram.w2, quadgram.w3, quadgram.w4)
rm(quadgram.w1, quadgram.w2, quadgram.w3, quadgram.w4)
quadgram <- getTotalFreq(quadgram$word, quadgram$freq)
setnames(quadgram, names(quadgram), c("word","c"))
quadgram.w1 <- c()
quadgram.w2 <- c()
quadgram.w3 <- c()
quadgram.w4 <- c()
n <- 250
k <- as.integer(length(quadgram$word)/n)
for(j in 1:n)
{
  print(j)
  start <- (j-1)*k + 1
  end <- j*k
  if(j==n)    end <- length(quadgram$word)
  quad <- quadgram$word[start:end]
  qg <- sapply(1:length(quad), function(i) strsplit(quad[i]," "))
  quadgram.w1 <- c(quadgram.w1, sapply(1:length(qg), function(i) qg[[i]][1]))
  quadgram.w2 <- c(quadgram.w2, sapply(1:length(qg), function(i) qg[[i]][2]))
  quadgram.w3 <- c(quadgram.w3, sapply(1:length(qg), function(i) qg[[i]][3]))
  quadgram.w4 <- c(quadgram.w4, sapply(1:length(qg), function(i) qg[[i]][4]))
}
rm(quad,qg)
quadgram <- quadgram[,word:=NULL,]
quadgram$w1 <- quadgram.w1
quadgram$w2 <- quadgram.w2
quadgram$w3 <- quadgram.w3
quadgram$w4 <- quadgram.w4
rm(quadgram.w1, quadgram.w2, quadgram.w3, quadgram.w4)
save(quadgram, file="Trimmed_Quadgram.RData")
rm(quadgram); gc()
################################################################################




############################  SECTION 2 ########################################
###
## Compute the probability of the unigrams in accordance with the interpolated 
## modified Kneser-Ney smoothing on the unigram model.
###
load("Trimmed_Unigram.RData")
load("Trimmed_Bigram.RData") 

P0 <- 1/nrow(unigram)

N_w1_hist.table <- getCounts(bigram$w2)
setnames(N_w1_hist.table, names(N_w1_hist.table), c("w1", "N_w1_hist"))
setkey(N_w1_hist.table, w1)
zero_w1_hist <- which(! unigram$w1 %in% N_w1_hist.table$w1)
zero_w1_hist.table <- unigram[zero_w1_hist, ]
zero_w1_hist.table <- cbind(zero_w1_hist.table, N_w1_hist=c(0))
unigram <- unigram[-zero_w1_hist,]
setkey(unigram, w1)
unigram <- merge(unigram, N_w1_hist.table)
unigram <- rbind(unigram, zero_w1_hist.table)
rm(N_w1_hist.table, zero_w1_hist.table); gc()

total_N_w1_hist <- sum(unigram$N_w1_hist)

n <- 10
k <- ceiling(nrow(unigram)/n)
prob <- c()
for(i in 1:n)
{
        start <- (i-1)*k+1
        end <- i*k
        if(i==n)
                end <- nrow(unigram)
        sect <- unigram[start:end,]
        ans <- sect$N_w1_hist/total_N_w1_hist
        prob <- c(prob, ans)
}
unigram$prob <- prob
rm(sect, prob)

unigram <- unigram[, N_w1_hist:=NULL, ]
unigram <- unigram[, c:=NULL, ]
gc()

save(unigram, file="Computed_Unigram.RData")
################################################################################





############################  SECTION 3 ######################################
###
## Compute the probability of the bigrams in accordance with the interpolated 
## modified Kneser-Ney smoothing on the bigram model.
###
P1 <- data.table(w1=unigram$w1, p_lower=unigram$prob, key="w1")
rm(unigram)
load("Trimmed_bigram.RData")
setkey(bigram, w1)
zero_P1 <- which(! bigram$w1 %in% P1$w1) # integer(0)
bigram <- merge(bigram, P1, by="w1")
rm(P1)

D_c <- getDVals(bigram$c)
bigram <- bigram[order(-bigram$c),]
D_c.table <- mergeDVals(bigram$c, D_c[1], D_c[2], D_c[3])
setnames(D_c.table, names(D_c.table), c("c", "D_c"))
bigram <- bigram[, c:=NULL, ]
bigram <- cbind(bigram,D_c.table)
rm(D_c.table); gc()

c_w1_ext.table <- getTotalFreq(bigram$w1, bigram$c)
setnames(c_w1_ext.table, names(c_w1_ext.table), c("w1","c_w1_ext"))
setkey(c_w1_ext.table, w1)
bigram <- merge(c_w1_ext.table, bigram, by="w1")
rm(c_w1_ext.table); gc()

N1 <- which(bigram$c==1)
N1.table <- getCounts(bigram$w1[N1])
setnames(N1.table, names(N1.table), c("w1", "N1"))
zeroN1 <- data.table(w1=unique(setdiff(bigram$w1, N1.table$w1)), N1=c(0))
N1.table <- rbind(N1.table, zeroN1)
setkey(N1.table, w1)
bigram <- merge(N1.table, bigram, by="w1")
rm(N1.table, zeroN1); gc()

N2 <- which(bigram$c==2)
N2.table <- getCounts(bigram$w1[N2])
setnames(N2.table, names(N2.table), c("w1", "N2"))
zeroN2 <- data.table(w1=unique(setdiff(bigram$w1, N2.table$w1)), N2=c(0))
N2.table <- rbind(N2.table, zeroN2)
setkey(N2.table, w1)
bigram <- merge(N2.table, bigram, by="w1")
rm(N2.table, zeroN2); gc()

N3 <- which(bigram$c>=3)
N3.table <- getCounts(bigram$w1[N3])
setnames(N3.table, names(N3.table), c("w1", "N3"))
zeroN3 <- data.table(w1=unique(setdiff(bigram$w1, N3.table$w1)), N3=c(0))
N3.table <- rbind(N3.table, zeroN3)
setkey(N3.table, w1)
bigram <- merge(N3.table, bigram, by="w1")
rm(N3.table, zeroN3); gc()

n <- 250
k <- ceiling(nrow(bigram)/n)
prob <- c()
for(i in 1:n)
{
  start <- (i-1)*k+1
  end <- i*k
  if(i==n)
    end <- nrow(bigram)
  sect <- bigram[start:end,]
  ans <- (sect$c-sect$D_c)/sect$c_w1_ext + 
    (D_c[1]*sect$N1+D_c[2]*sect$N2+D_c[3]*sect$N3)*sect$p_lower/sect$c_w1_ext
  prob <- c(prob, ans)
}
bigram$prob <- prob
rm(sect, prob)

bigram <- bigram[, N3:=NULL, ]
bigram <- bigram[, N2:=NULL, ]
bigram <- bigram[, N1:=NULL, ]
bigram <- bigram[, c_w1_ext:=NULL, ]
bigram <- bigram[, p_lower:=NULL, ]
bigram <- bigram[, D_c:=NULL, ]
bigram <- bigram[, c:=NULL, ]
gc()

save(bigram, file="Computed_bigram.RData")
################################################################################





############################  SECTION 4 ########################################
###
## Compute the probability of the trigrams in accordance with the interpolated 
## modified Kneser-Ney smoothing on the trigram model.
###
P2 <- data.table(w23=paste(bigram$w1, bigram$w2), p_lower=bigram$prob, key="w23")
rm(bigram)
load("Trimmed_Trigram.RData") # 24153401        5
trigram$w23 <- paste(trigram$w2, trigram$w3)
setkey(trigram, w23)
zero_P2 <- which(! trigram$w23 %in% P2$w23) 
trigram <- merge(trigram, P2, by="w23")
trigram <- trigram[, w23:=NULL, ]
rm(P2)

D_c <- getDVals(trigram$c)
trigram <- trigram[order(-trigram$c),]
D_c.table <- mergeDVals(trigram$c, D_c[1], D_c[2], D_c[3])
setnames(D_c.table, names(D_c.table), c("c", "D_c"))
trigram <- trigram[, c:=NULL, ]
trigram <- cbind(trigram,D_c.table)
rm(D_c.table); gc()

trigram$w12 <- paste(trigram$w1, trigram$w2)
c_w12_ext.table <- getTotalFreq(trigram$w12, trigram$c)
setnames(c_w12_ext.table, names(c_w12_ext.table), c("w12","c_w12_ext"))
setkey(c_w12_ext.table, w12)
trigram <- merge(c_w12_ext.table, trigram, by="w12")
rm(c_w12_ext.table); gc()

N1 <- which(trigram$c==1)
N1.table <- getCounts(trigram$w12[N1])
setnames(N1.table, names(N1.table), c("w12", "N1"))
zeroN1 <- data.table(w12=unique(setdiff(trigram$w12, N1.table$w12)), N1=c(0))
N1.table <- rbind(N1.table, zeroN1)
setkey(N1.table, w12)
trigram <- merge(N1.table, trigram, by="w12")
rm(N1.table, zeroN1); gc()

N2 <- which(trigram$c==2)
N2.table <- getCounts(trigram$w12[N2])
setnames(N2.table, names(N2.table), c("w12", "N2"))
zeroN2 <- data.table(w12=unique(setdiff(trigram$w12, N2.table$w12)), N2=c(0))
N2.table <- rbind(N2.table, zeroN2)
setkey(N2.table, w12)
trigram <- merge(N2.table, trigram, by="w12")
rm(N2.table, zeroN2); gc()

N3 <- which(trigram$c>=3)
N3.table <- getCounts(trigram$w12[N3])
setnames(N3.table, names(N3.table), c("w12", "N3"))
zeroN3 <- data.table(w12=unique(setdiff(trigram$w12, N3.table$w12)), N3=c(0))
N3.table <- rbind(N3.table, zeroN3)
setkey(N3.table, w12)
trigram <- merge(N3.table, trigram, by="w12")
rm(N3.table, zeroN3); gc()

n <- 250
k <- ceiling(nrow(trigram)/n)
prob <- c()
for(i in 1:n)
{
  start <- (i-1)*k+1
  end <- i*k
  if(i==n)
    end <- nrow(trigram)
  sect <- trigram[start:end,]
  ans <- (sect$c-sect$D_c)/sect$c_w12_ext + 
    (D_c[1]*sect$N1+D_c[2]*sect$N2+D_c[3]*sect$N3)*sect$p_lower/sect$c_w12_ext
  prob <- c(prob, ans)
}
trigram$prob <- prob
rm(sect, prob)

trigram <- trigram[, w12:=NULL, ]
trigram <- trigram[, N3:=NULL, ]
trigram <- trigram[, N2:=NULL, ]
trigram <- trigram[, N1:=NULL, ]
trigram <- trigram[, c_w12_ext:=NULL, ]
trigram <- trigram[, p_lower:=NULL, ]
trigram <- trigram[, D_c:=NULL, ]
trigram <- trigram[, c:=NULL, ]
gc()

save(trigram, file="Computed_Trigram.RData")
################################################################################





############################  SECTION 5 ########################################
###
## Compute the probability of the trigrams in accordance with the interpolated 
## modified Kneser-Ney smoothing on the trigram model.
###
P3 <- data.table(w234=paste(trigram$w1, trigram$w2, trigram$w3), 
                 p_lower=trigram$prob, key="w234")
rm(trigram)  
load("Trimmed_Quadgram.RData")  
quadgram$w234 <- paste(quadgram$w2, quadgram$w3, quadgram$w4)
setkey(quadgram, w234)
zero_P3 <- which(! quadgram$w234 %in% P3$w234) 
zero_P3.table <- quadgram[zero_P3]
zero_P3.table <- cbind(zero_P3.table, p_lower=c(0))
quadgram <- quadgram[-zero_P3,]
quadgram <- merge(quadgram, P3, by="w234")
quadgram <- rbind(quadgram, zero_P3.table)
quadgram <- quadgram[, w234:=NULL, ]
rm(P3, zero_P3.table)

D_c <- getDVals(quadgram$c)
quadgram <- quadgram[order(-quadgram$c),]
D_c.table <- mergeDVals(quadgram$c, D_c[1], D_c[2], D_c[3])
setnames(D_c.table, names(D_c.table), c("c", "D_c"))
quadgram <- quadgram[, c:=NULL, ]
quadgram <- cbind(quadgram,D_c.table)
rm(D_c.table); gc()

quadgram$w123 <- paste(quadgram$w1, quadgram$w2, quadgram$w3)
c_w123_ext.table <- getTotalFreq(quadgram$w123, quadgram$c)
setnames(c_w123_ext.table, names(c_w123_ext.table), c("w123","c_w123_ext"))
setkey(c_w123_ext.table, w123)
setkey(quadgram, w123)
quadgram <- merge(c_w123_ext.table, quadgram, by="w123")
rm(c_w123_ext.table); gc()

N1 <- which(quadgram$c==1)
N1.table <- getCounts(quadgram$w123[N1])
setnames(N1.table, names(N1.table), c("w123", "N1"))
zeroN1 <- data.table(w123=unique(setdiff(quadgram$w123, N1.table$w123)), N1=c(0))
N1.table <- rbind(N1.table, zeroN1)
setkey(N1.table, w123)
quadgram <- merge(N1.table, quadgram, by="w123")
rm(N1.table, zeroN1); gc()

N2 <- which(quadgram$c==2)
N2.table <- getCounts(quadgram$w123[N2])
setnames(N2.table, names(N2.table), c("w123", "N2"))
zeroN2 <- data.table(w123=unique(setdiff(quadgram$w123, N2.table$w123)), N2=c(0))
N2.table <- rbind(N2.table, zeroN2)
setkey(N2.table, w123)
quadgram <- merge(N2.table, quadgram, by="w123")
rm(N2.table, zeroN2); gc()

N3 <- which(quadgram$c>=3)
N3.table <- getCounts(quadgram$w123[N3])
setnames(N3.table, names(N3.table), c("w123", "N3"))
zeroN3 <- data.table(w123=unique(setdiff(quadgram$w123, N3.table$w123)), N3=c(0))
N3.table <- rbind(N3.table, zeroN3)
setkey(N3.table, w123)
quadgram <- merge(N3.table, quadgram, by="w123")
rm(N3.table, zeroN3); gc()

n <- 500
k <- ceiling(nrow(quadgram)/n)
prob <- c()
for(i in 1:n)
{
  start <- (i-1)*k+1
  end <- i*k
  if(i==n)
    end <- nrow(quadgram)
  sect <- quadgram[start:end,]
  ans <- (sect$c-sect$D_c)/sect$c_w123_ext + 
    (D_c[1]*sect$N1+D_c[2]*sect$N2+D_c[3]*sect$N3)*sect$p_lower/sect$c_w123_ext
  prob <- c(prob, ans)
}
quadgram$prob <- prob
rm(sect, prob)

quadgram <- quadgram[, w123:=NULL, ]
quadgram <- quadgram[, N3:=NULL, ]
quadgram <- quadgram[, N2:=NULL, ]
quadgram <- quadgram[, N1:=NULL, ]
quadgram <- quadgram[, c_w123_ext:=NULL, ]
quadgram <- quadgram[, p_lower:=NULL, ]
quadgram <- quadgram[, D_c:=NULL, ]
quadgram <- quadgram[, c:=NULL, ]
gc()

save(quadgram, file="Computed_Quadgram.RData")
################################################################################





############################  SECTION 6 ########################################
###
## Select the (up to) 4 most probable words folllowing a given string w123, and
## then encode the words according to the dictionary and stores as the final result.
## Singletons in the result are rejected.
###
## Create the dictionary which consists of the words and the associated codes.
load("Dictionary.RData")
## Eliminate entries in the dictionary that contain a non-alphabetical character.
dict <- dict[-which(!grepl("^-?[a-zA-Z][a-zA-Z]*(-[a-zA-Z]+)*$", dict))]
## Assign a, id number for each word in the dictionary.
dict <- data.table(word=dict, wid=1:length(dict))
save(dict, file="Grand_Dictionary.RData")
write.csv(dict, "Grand_Dictionary.csv", row.names=FALSE)

## Selects the (up to) 4 most probable words folllowing a given string w123.
load("Computed_Quadgram.RData")
quadgram <- data.table(w1=quadgram$w1, w2=quadgram$w2, w3=quadgram$w3, w4=quadgram$w4, prob=quadgram$prob)

## Eliminate 4-grams that contain a non-alphabetical character.
quadgram <- quadgram[-which(!grepl("^-?[a-zA-Z][a-zA-Z]*(-[a-zA-Z]+)*$", quadgram$w1)),]
quadgram <- quadgram[-which(!grepl("^-?[a-zA-Z][a-zA-Z]*(-[a-zA-Z]+)*$", quadgram$w2)),]
quadgram <- quadgram[-which(!grepl("^-?[a-zA-Z][a-zA-Z]*(-[a-zA-Z]+)*$", quadgram$w3)),]
quadgram <- quadgram[-which(!grepl("^-?[a-zA-Z][a-zA-Z]*(-[a-zA-Z]+)*$", quadgram$w4)),]

## Eliminates 4-grams which give "UNK" as the prediction.
quadgram <- quadgram[-which(quadgram$w4=="UNK"),]

quadgram$w123 <- paste(quadgram$w1, quadgram$w2, quadgram$w3)
## Order the 4-grams in descending order of their probabilities.
setorder(quadgram, w123, -prob)
setkey(quadgram, w123)
## Select the top 4 predictions for each category.
quadgram$row <- 1:nrow(quadgram)
tops <- data.table()
numSuggestions <- 4
for(i in 1:numSuggestions)
{
  temp <- quadgram[, .SD[1], by=w123]
  temp <- cbind(temp, rank=c(numSuggestions-i+1))
  tops <- rbind(tops, temp)
  quadgram <- quadgram[-which(quadgram$row %in% temp$row), ]
}

## Discard the singletons.
singletons <- which(getCounts(tops$w123)$count==1)
singletons <- tops$w123[singletons]
tops <- tops[-which(tops$w123 %in% singletons), ]
quadgram <- tops
rm(tops, temp); gc()
quadgram <- quadgram[, w123:=NULL, ]
quadgram <- quadgram[, prob:=NULL, ]
quadgram <- quadgram[, row:=NULL, ]

## Replace the word id number from the dictionary for the words.
setnames(dict, names(dict), c("w1","wid1"))
setkey(dict,w1)
setkey(quadgram,w1)
quadgram <- merge(quadgram, dict, by="w1")
quadgram <- quadgram[, w1:=NULL, ]

setnames(dict, names(dict), c("w2","wid2"))
setkey(dict,w2)
setkey(quadgram,w2)
quadgram <- merge(quadgram, dict, by="w2")
quadgram <- quadgram[, w2:=NULL, ]

setnames(dict, names(dict), c("w3","wid3"))
setkey(dict,w3)
setkey(quadgram,w3)
quadgram <- merge(quadgram, dict, by="w3")
quadgram <- quadgram[, w3:=NULL, ]

setnames(dict, names(dict), c("w4","wid4"))
setkey(dict,w4)
setkey(quadgram,w4)
quadgram <- merge(quadgram, dict, by="w4")
quadgram <- quadgram[, w4:=NULL, ]

## Saves the final result.
save(quadgram, file="result_Quadgram.RData")
write.csv(quadgram, "result_Quadgram.csv", row.names=FALSE)
rm(quadgram); gc()
################################################################################





############################  SECTION 7 ########################################
###
## Select the (up to) 4 most probable words folllowing a given string w12, and
## then encode the words according to the dictionary and stores as the final result.
## Singletons in the result are rejected.
## See the previous section for comments on similar calculations.
###
load("Computed_Trigram.RData")
trigram <- data.table(w1=trigram$w1, w2=trigram$w2, w3=trigram$w3, prob=trigram$prob)

trigram <- trigram[-which(!grepl("^-?[a-zA-Z][a-zA-Z]*(-[a-zA-Z]+)*$", trigram$w1)),]
trigram <- trigram[-which(!grepl("^-?[a-zA-Z][a-zA-Z]*(-[a-zA-Z]+)*$", trigram$w2)),]
trigram <- trigram[-which(!grepl("^-?[a-zA-Z][a-zA-Z]*(-[a-zA-Z]+)*$", trigram$w3)),]

trigram <- trigram[-which(trigram$w3=="UNK"),]

trigram$w12 <- paste(trigram$w1,trigram$w2 )
setorder(trigram, w12, -prob)
setkey(trigram, w12)
trigram$row <- 1:nrow(trigram)
tops <- data.table()
numSuggestions <- 4
for(i in 1:numSuggestions)
{
  temp <- trigram[, .SD[1], by=w12]
  temp <- cbind(temp, rank=c(numSuggestions-i+1))
  tops <- rbind(tops, temp)
  trigram <- trigram[-which(trigram$row %in% temp$row), ]
}

singletons <- which(getCounts(tops$w12)$count==1)
singletons <- tops$w12[singletons]
tops <- tops[-which(tops$w12 %in% singletons), ]
trigram <- tops
rm(tops, temp); gc()
trigram <- trigram[, w12:=NULL, ]
trigram <- trigram[, c:=NULL, ]
trigram <- trigram[, prob:=NULL, ]
trigram <- trigram[, row:=NULL, ]

setnames(dict, names(dict), c("w1","wid1"))
setkey(dict,w1)
setkey(trigram,w1)
trigram <- merge(trigram, dict, by="w1")
trigram <- trigram[, w1:=NULL, ]

setnames(dict, names(dict), c("w2","wid2"))
setkey(dict,w2)
setkey(trigram,w2)
trigram <- merge(trigram, dict, by="w2")
trigram <- trigram[, w2:=NULL, ]

setnames(dict, names(dict), c("w3","wid3"))
setkey(dict,w3)
setkey(trigram,w3)
trigram <- merge(trigram, dict, by="w3")
trigram <- trigram[, w3:=NULL, ]

save(trigram, file="result_Trigram.RData")
write.csv(trigram, "result_Trigram1.csv", row.names=FALSE)
rm(trigram); gc()
################################################################################





############################  SECTION 8 ########################################
###
## Select the (up to) 4 most probable words folllowing a given string w1, and
## then encode the words according to the dictionary and stores as the final result.
## Singletons in the result are rejected.
## See the Section 6 for comments on similar calculations.
###
load("Computed_Bigram.RData")
bigram <- data.table(w1=bigram$w1, w2=bigram$w2, prob=bigram$prob)

bigram <- bigram[-which(!grepl("^-?[a-zA-Z][a-zA-Z]*(-[a-zA-Z]+)*$", bigram$w1)),]
bigram <- bigram[-which(!grepl("^-?[a-zA-Z][a-zA-Z]*(-[a-zA-Z]+)*$", bigram$w2)),]

bigram <- bigram[-which(bigram$w2=="UNK"),]

setorder(bigram, w1, -prob)
setkey(bigram, w1)
bigram$row <- 1:nrow(bigram)
tops <- data.table()
numSuggestions <- 4
for(i in 1:numSuggestions)
{
  temp <- bigram[, .SD[1], by=w1]
  temp <- cbind(temp, rank=c(numSuggestions-i+1))
  tops <- rbind(tops, temp)
  bigram <- bigram[-which(bigram$row %in% temp$row), ]
}

singletons <- which(getCounts(tops$w1)$count==1)
singletons <- tops$w1[singletons]
tops <- tops[-which(tops$w1 %in% singletons), ]
bigram <- tops
rm(tops, temp); gc()
bigram <- bigram[, c:=NULL, ]
bigram <- bigram[, prob:=NULL, ]
bigram <- bigram[, row:=NULL, ]

setnames(dict, names(dict), c("w1", "wid1"))
setkey(dict, w1)
setkey(bigram, w1)
bigram <- merge(bigram, dict, by="w1")
bigram <- bigram[, w1:=NULL, ]

setnames(dict, names(dict), c("w2","wid2"))
setkey(dict,w2)
setkey(bigram,w2)
bigram <- merge(bigram, dict, by="w2")
bigram <- bigram[, w2:=NULL, ]

save(bigram, file="result_Bigram.RData")
write.csv(bigram, "result_Bigram1.csv", row.names=FALSE)
rm(bigram); gc()
################################################################################





############################  SECTION 9 ########################################
###
## Select the 5 most probable unigram in case there is no result for the
## input.
###
load("Computed_Unigram.RData")
unigram <- unigram[order(-unigram$prob),]
unigram$w1[1:6] ## "and" "UNK" "in"  "the" "i"   "to" 
rm(unigram); gc()

