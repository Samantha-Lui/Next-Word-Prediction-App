library(stringr)

###
## cleanText() trims and formats the user's input for the prediction.
## 
## @param text a String input by the user for the prediction.
## @return A String in the appropriate format for the prediction.
###
cleanText <- function(text){
        ## Remove the specified symbols if exist.
        result <- gsub("/|@|\\|~|_|\\*|#|%|\\^|&|\\+|`|=|\\{|\\}\\[|\\]", " ", text)
        result <- gsub("[!:;\'\"?\\.,]", " ", result) ## Remove all punctuations.
        result <- gsub("[[:digit:]]", " ", result) ## Remove all digits.
        result <- gsub("^ *", "", result) ## Remove leading space.
        result <- gsub(" *$", "", result) ## Remove trailing spaces.
        result <- gsub(" +", " ", result) ## Remove extra spaces.
        result <- tolower(result) ## Convert the text into lower case.
        ## Convert the String into a vector of consistuent words.
        words <- unlist(strsplit(result," "))
        rm(result)
        words
}

###
## goUnigram() returns the four most probable answers when 
## there is no match to the input from other higher-order 
## N-gram models.
##
## @return A data frame containing the answers and their respective 
## odds with 4 indicating most likely and 1 least.
###
goUnigram <- function(){
        word <- c("I", "the", "in", "and")
        freq <- 1:4
        data.frame(word=word, freq=freq)
}

###
## getWid() returns the word id number in the dicitionary for
## the specified word.
##
## @param w The specified word.
## @return the word id number in the dictionary.
###
getWid <- function(w){
        if(!w %in% dict$word)
                return(1)
        return(dict$wid[which(dict$word == w)])
}

###
## getWord() returns the word in the dicitionary for
## the specified word id number.
##
## @param w The specified word id number.
## @return the word in the dictionary.
###
getWord <- function(wid){
        w <- as.character(dict$word[which(dict$wid == wid)])

        ## The following modifications are for cosmetic purposes.
        if(w == "i")
                w <- "I"

        if(w == "don")
                w <- "don\'t"
        if(w == "doesn")
                w <- "doesn\'t"
        if(w == "couldn")
                w <- "couldn\'t"
        if(w == "wouldn")
                w <- "wouldn\'t"
        if(w == "shouldn")
                w <- "couldn\'t"
        if(w == "mustn")
                w <- "mustn\'t"
        
        if(w == "t")
                w <- "\'t"
        if(w == "s")
                w <- "\'s"
        if(w == "m")
                w <- "\'m"
        if(w == "re")
                w <- "\'re"
        if(w == "ve")
                w <- "\'ve"
        if(w == "d")
                w <- "\'d"
        if(w == "ll")
                w <- "\'ll"
        
        w
}

###
## goBigram() looks up the predictions for the specified phrase by the bigram 
## model. The function returns the top four predictions if it finds a match and 
## backs up to the unigram model otherwise.
###
goBigram <- function(w1){
        wid1 <- getWid(w1)
        
        if(wid1 %in% bw1$w){
                ans <- c()
                wpos <- which(bw1$w == wid1)
                index <- bw1$loc[wpos]
                if(wpos==nrow(bw1))
                        ans <- bigram.tree[(index+2):length(bigram.tree)]
                else{
                        index2 <- bw1$loc[wpos+1]
                        ans <- bigram.tree[(index+2):(index2-1)]
                        
                }
                word <- c()
                for(i in 1:length(ans))
                        word <- c(word, getWord(ans[i]))
                return(data.frame(word=word, freq=1:length(word)))
        }
        goUnigram()        
}


###
## goTrigram() looks up the predictions for the specified phrase by the trigram 
## model. The function returns the top four predictions if it finds a match and 
## backs up to the bigram model otherwise.
###
goTrigram <- function(w1,w2){
        wid1 <- getWid(w1)
        if(wid1 %in% tw1$w){
                wpos1 <- which(tw1$w == wid1)
                index <- tw1$loc[wpos1]
                sect <- c()
                if(wpos1 == nrow(tw1))
                        sect <- trigram.tree[(index+2):length(trigram.tree)]
                else{
                        index2 <- tw1$loc[wpos1+1]
                        sect <- trigram.tree[(index+2):(index2-1)]
                }
                wpos2 <- which(sect==-2)
                w2.tree <- sect[1:(wpos2-1)]
                wid2 <- getWid(w2)
                if(wid2 %in% w2.tree){
                        ans <- c()
                w3 <- sect[(wpos2+1):length(sect)]
                        rm(sect)
                        index3 <- which(w3==-3)
                        k <- which(w2.tree==wid2)
                        if(k==1)
                                ans <- w3[1:(index3[1]-1)]
                        else
                                ans <- w3[(index3[k-1]+1):(index3[k]-1)]
                        word <- c()
                        for(i in 1:length(ans))
                                word <- c(word, getWord(ans[i]))
                        return(data.frame(word=word, freq=1:length(word)))        
                }
                goBigram(w2)
        }
        goBigram(w2)
}


###
## goQuadgram() looks up the predictions for the specified phrase by the quadgram 
## model. The function returns the top four predictions if it finds a match and 
## backs up to the trigram model otherwise.
###
goQuadgram <- function(w1,w2,w3){
        wid1 <- getWid(w1)
        if(wid1 %in% qw1$w){
                wpos1 <- which(qw1$w==wid1)
                index <- qw1$loc[wpos1]
                sect <- c()
                if(wpos1==nrow(qw1))
                        sect <- quadgram.tree[(index+2):length(quadgram.tree)]
                else{
                        index2 <- qw1$loc[wpos1+1]
                        sect <- quadgram.tree[(index+2):(index2-1)]
                }
                wid2 <- getWid(w2)
                p2 <- which(sect==-2)
                w2.tree <- sect[1:(p2-1)]
                sect <- sect[(p2+1):length(sect)]
                if(wid2 %in% w2.tree){
                        k <- which(w2.tree==wid2)
                        p3 <- which(sect==-3)
                        w3.tree <- c()
                        if(k==1)
                                w3.tree <- sect[1:(p3[k]-1)]
                        else
                                w3.tree <- sect[(p3[k-1]+1):(p3[k]-1)]
                        sect <- sect[(p3[length(p3)]+1):length(sect)]
                        wid3 <- getWid(w3)
                        if(wid3 %in% w3.tree){
                                p5 <- which(sect==-5)
                                if(k==1)
                                        sect <- sect[1:(p5[k]-1)]
                                else
                                        sect <- sect[(p5[k-1]+1):(p5[k]-1)]
                                m <- which(w3.tree==wid3)
                                p4 <- which(sect==-4)
                                ans <- c()
                                if(m==1)
                                        ans <- sect[1:(p4[m]-1)]
                                else
                                        ans <- sect[(p4[m-1]+1):(p4[m]-1)]
                                word <- c()
                                for(i in 1:length(ans))
                                        word <- c(word, getWord(ans[i]))
                                return(data.frame(word=word, freq=1:length(word))) 
                        }
                        goTrigram(w2,w3)
                }
                goTrigram(w2,w3)
        }
        goTrigram(w2,w3) 
}

###
## predict() is a wrapper function which invokes the cleanText() function to 
## convert the user's input into the appropriate format and then assigns an 
## N-gram model for the prediction according to the number of words in the input.
##
## predict() also filters the profanities from the result of a prediction once
## it is returned. See global.R for the list of profanities.
###
predict <- function(phrase)
{
        words <- cleanText(phrase)
        n <- length(words)
        predictions <- data.frame()
        if(n==0)
                predictions <- goUnigram()
        else{
                if(n==1)
                        predictions <- goBigram(words[n])
                else{
                        if(n==2)
                                predictions <- goTrigram(words[n-1], words[n]) 
                        else
                                predictions <- goQuadgram(words[n-2], words[n-1], words[n])    
                }
        }
        ## The profanities are replaced by "LLAMA".
        predictions$word <- gsub(profanity, "LLAMA", predictions$word)
        predictions[nrow(predictions):1, ]
}