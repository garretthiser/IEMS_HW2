#install and load plyr
install.packages("plyr")
library(plyr)
library(dplyr)
#install and load package arules
install.packages("arules")
library(arules)
#install and load arulesViz
install.packages("arulesViz")
library(arulesViz)
#install and load tidyverse
install.packages("tidyverse")
library(tidyverse)
#install and load readxml
install.packages("readxml")
library(readxl)
#install and load knitr
install.packages("knitr")
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
install.packages("lubridate")
library(lubridate)

skip = 0
chunksize = 100000
subdat = data.frame(0)
for (i in c(1:684)){
chunk <- read_csv("trnsact.csv", col_names = FALSE, n_max = chunksize, skip = skip)
chunk <- chunk[chunk$X2 %in% c(
8109,
8209,
8309,
8409,
8509,
8519,
8609,
8709,
9209,
9309,
9409,
9609),]

	if (nrow(chunk) != 0 && subdat[1,1] == 0){
	subdat <- chunk
	}

	if (nrow(chunk) != 0 && subdat[1,1] != 0){
	subdat <- rbind(subdat, chunk)
	}

skip = skip + chunksize
print(i)
}

subdat <- subdat[subdat$X7 %in% c("P"),]

trnsacts <- subset(subdat, select = c("X1", "X2", "X4", "X6", "X9", "X10", "X12"))   

names(trnsacts) = c("SKU", "STORE", "TRANNUM", "DATE", "ORGPRICE", "AMT", "INTERID") 

trandata <- ddply(trnsacts,c("TRANNUM", "DATE"),
                       function(df1)paste(df1$SKU,
                       collapse = ","))

names(trandata) = c("TRANNUM", "DATE", "SKUs")

#set column InvoiceNo of dataframe transactionData  
trandata$TRANNUM <- NULL
#set column Date of dataframe transactionData
trandata$DATE <- NULL

write.csv(trandata, file = "dillards_baskets_denver.csv", quote = FALSE, row.names = TRUE)

tr <- read.transactions('dillards_baskets_denver.csv', format = 'basket', sep=',')

# Make the rules.
association.rules <- apriori(tr, parameter = list(supp=0.005, conf=0.015,maxlen=10, minlen = 2))

#move data to data frame for exporting

df = data.frame(
  lhs = labels(lhs(association.rules)),
  rhs = labels(rhs(association.rules)), 
  association.rules@quality)

#with these parameters we have 111 unque SKU's, so we'll sort the rules by lift and pick the top 100 unique SKUs from the CSV

cutdf <- df[order(-df$lift),]

topSKU <- data.frame(unique(cutdf$rhs))

write.csv(topSKU, file = "top_SKUs.csv", row.names = TRUE)

#print off some some output code for the 308 sample output 

#the first 10 rules in the list
inspect(association.rules[1:10])

#the top 10 unique SKU's from the righthand side of the rules list
topSKUs[1:10,]
