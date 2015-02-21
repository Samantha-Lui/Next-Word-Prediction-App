library(data.table)

## Set up for the parallel processing.
library(doSNOW);library(foreach)
cl <- makeCluster(8, type = "SOCK")
registerDoSNOW(cl)

###
## Parse the bigram models into a tree structure w.r.t. a 
## level-order traversal.
###
load("result_Bigram.RData")
bigram <- bigram[order(bigram$wid1,bigram$rank), ]
w1 <- unique(bigram$wid1)
firstPos <- c(1)
lastPos <- c(sum(bigram$wid1==w1[1]))
for(i in 2:length(w1)){
        print(i)
        firstPos <- c(firstPos, lastPos[i-1]+1)
        lastPos <- c(lastPos, firstPos[i]+sum(bigram$wid1==w1[i])-1)
}
tw1 <- data.frame(w=w1, first=firstPos, last=lastPos); tw1

bigram.tree <- foreach(i=1:nrow(tw1), .combine=c) %dopar% {
        tree <- c()
        sect <- bigram[tw1$first[i]:tw1$last[i], ]
        tree <- c(tree, tw1$w[i], -1)
        tree <- c(tree, sect$wid2)
}
## Convert data from numeric to integer, the native type of the content data.
bigram.tree <- as.integer(bigram.tree)
save(bigram.tree, file="bigram.tree.RData")
rm(bigram, bigram.tree, tw1)





###
## Parse the trigram models into a tree structure w.r.t. a 
## level-order traversal.
###
load("result_Trigram.RData")
trigram <- trigram[order(trigram$wid1,trigram$wid2,trigram$rank), ]
w1 <- unique(trigram$wid1)
firstPos <- c(1)
lastPos <- c(sum(trigram$wid1==w1[1]))
for(i in 2:length(w1)){
  print(i)
  firstPos <- c(firstPos, lastPos[i-1]+1)
  lastPos <- c(lastPos, firstPos[i]+sum(trigram$wid1==w1[i])-1)
}
tw1 <- data.frame(w=w1, first=firstPos, last=lastPos)
rm(w1, firstPos, lastPos)

trigram.tree <- foreach(i=1:nrow(tw1), .combine=c) %dopar% {
        tree <- c()
        sect <- trigram[tw1$first[i]:tw1$last[i], ]
        tree <- c(tree, tw1$w[i], -1)
        w2 <- unique(sect$wid2)
        tree <- c(tree, w2, -2)
        w3 <- c()
        for(j in 1:length(w2)){
                w3 <- c(w3, sect$wid3[sect$wid2==w2[j]], -3)
        }
        tree <- c(tree, w3)
        tree
}
trigram.tree <- as.integer(trigram.tree)
save(trigram.tree, file="trigram.tree.RData")





###
## Parss the quadgram models into a tree structure w.r.t. a 
## level-order traversal.
###
load("result_Quadgram.RData")
quadgram <- quadgram[order(quadgram$wid1,quadgram$wid2,quadgram$wid3,quadgram$rank), ]
w1 <- unique(quadgram$wid1)
firstPos <- c(1)
lastPos <- c(sum(quadgram$wid1==w1[1]))
for(i in 2:length(w1)){
        firstPos <- c(firstPos, lastPos[i-1]+1)
        lastPos <- c(lastPos, firstPos[i]+sum(quadgram$wid1==w1[i])-1)
}
tw1 <- data.frame(w=w1, first=firstPos, last=lastPos);
rm(w1, firstPos, lastPos)

quadgram.tree <- foreach(i=1:nrow(tw1), .combine=c) %dopar%{
        tree <- c()
        sect <- quadgram[tw1$first[i]:tw1$last[i], ]
        tree <- c(tree, tw1$w[i], -1)
        w2 <- unique(sect$wid2)
        tree <- c(tree, w2, -2)
        w3 <- c()
        w4 <- c()
        for(j in 1:length(w2)){
                sect2 <- sect[sect$wid2==w2[j],]
                w3Temp <- unique(sect2$wid3)
                w3 <- c(w3, w3Temp, -3)
                for(k in 1:length(w3Temp)){
                        w4 <- c(w4, sect2$wid4[sect2$wid3==w3Temp[k]], -4) 
                }
                w4 <- c(w4, -5)
        }
        tree <- c(tree, w3)
        tree <- c(tree, w4)
        tree
}
quadgram.tree <- as.integer(quadgram.tree)
save(quadgram.tree, file="quadgram.tree.RData")
rm(quadgram.tree, quadgram, tw1)


stopCluster(cl)