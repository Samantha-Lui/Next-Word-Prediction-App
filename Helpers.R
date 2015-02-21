###
## coverage() calculates number of unique terms required to cover a certain 
## percentage of the total number of terms in the corpus.
##
## @param vals a decreasingly ordered integer vector.
## @param cover the coverage in decimal.
## @return: position of the cut-off term in the vals vector.
###
coverage <- function(vals, cover)
{
        total <- sum(vals)
        accum <- 0
        for(i in 1: length(vals))
        {
                accum <- accum + vals[i]/total
                if(accum>cover)
                        return(i)
        }
}

###
## getCounts() return a table of the input items and their respective counts.
##
## @param item a vector of items to be counted.
## @return a table of the input items and their respective counts.
###
getCounts <- function(item)
{
        CTable <- data.table(item=item, count=c(1), key="item")
        CTable <- CTable[, sum(count), by=item]
        setnames(CTable, names(CTable), c("item","count"))
}

###
## getTotalFreq() take a table consisted of (duplicated) items and their 
## frequencies and returns a table and the total frequencies by item.
##
## @param item a vector of items.
## @param freq a vector of frequencies corresponding the the items.
##
getTotalFreq <- function(item, freq)
{
        FTable <- data.table(item=item, freq=freq, key="item")
        FTable <- FTable[, sum(freq), by=item]
        setnames(FTable, names(FTable), c("item","totalFreq"))
}

###
## getDVals() returns the D values (d1,d2,d3) of the input frequency vector.
## 
## @para, freq a vector of frequencies.
## @return a vector of D values (d1,d2,d3) of the input frequency vector.
###
getDVals <- function(freq)
{
        DTable <- getCounts(freq)
        setnames(DTable, names(DTable), c("r","n_r"))
        n1 <- DTable$n_r[DTable$r==1]
        n2 <- DTable$n_r[DTable$r==2]
        n3 <- DTable$n_r[DTable$r==3]
        n4 <- DTable$n_r[DTable$r==4]
        rm(DTable)
        y <- n1/(n1+2*n2)
        d1 <- 1-2*y*n2/n1
        d2 <- 2-3*y*n3/n2
        d3 <- 3-4*y*n4/n3
        c(d1,d2,d3)
}

###
## mergeDVals() maps a descreasing-ordered frequency vector with the 
## appropriate D values.
##
## @param freq a vector of descreasing-ordered frequencies.
## @param d1 the d1 value.
## @param d2 the d2 value.
## @param d3 the d3 value.
## @return A data table with a frequency column and a D-value column whose 
## entries are corresponding to those in the frequency column.
###
mergeDVals <- function(freq, d1, d2, d3)
{
        lastThree <- max(which(freq>2))
        lastTwo <- max(which(freq>1))
        d <- c(rep(d3,lastThree), rep(d2,lastTwo-lastThree), rep(d1,length(freq)-lastTwo))
        data.table(freq=freq,d=d)
}

