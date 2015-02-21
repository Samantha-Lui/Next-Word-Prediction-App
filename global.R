###
## global.R provides the data to be shared across the servers.
###

## The dictionary containing links between the words and word id numbers.
load("Grand_Dictionary.RData")

## Predictions from the bigram model.
load("bigram.tree.RData")
flags <- which(bigram.tree==-1)
bw1 <- data.frame(w=bigram.tree[flags-c(1)], loc=flags-c(1))

## Predictions from the trigram model.
load("trigram.tree.RData")
flags <- which(trigram.tree==-1)
tw1 <- data.frame(w=trigram.tree[flags-c(1)], loc=flags-c(1))

## Predictions from the quadgram model.
load("quadgram.tree.RData")
flags <- which(quadgram.tree==-1)
qw1 <- data.frame(w=quadgram.tree[flags-c(1)], loc=flags-c(1))

## A list of profanities to be filtered.
profanity <- "(fuck)|(shit)|(piss)|(cunt)|(pussy)|(pussies)|(dick)|(cock)|(asshole)|(fag)|(nigga)|(bitch)"