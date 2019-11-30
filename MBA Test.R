## Follow up analysis of an MBA for the retail industry

library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)

setwd("~/Desktop/R_HomeDepot")

data1 <- read_excel("Online Retail.xlsx")
data1 <- data1[complete.cases(data1),]
data1 <- data1 %>% mutate(Description = as.factor(Description), Country = as.factor(Country),
                        Date = as.Date(InvoiceDate), Time = format(InvoiceDate, "%H:%M"))
data1$InvoiceNo <- gsub("C","",data1$InvoiceNo)
data1$InvoiceNo <- gsub("A","",data1$InvoiceNo)

data1$InvoiceNo <- as.numeric(as.character(data1$InvoiceNo))

data1$Time <- as.factor(data1$Time)
a <- hms(as.character(data1$Time))
data1$Time = hour(a)

data1 %>% 
  ggplot(aes(x=Time)) + 
  geom_histogram(stat="count",fill="indianred") +  ggtitle("Purchases based on time") + ylab("Frequencies") + xlab("Time")

## From the previous graph it can be inferred that the
## best time to try our appriori based model will
## be between 10 and 16 hours.

## Now we'll take a look on the number of items the people make per purchase
detach("package:plyr", unload=TRUE)
data1[data1$Quantity>=0,] %>% group_by(InvoiceNo) %>% summarize(meanQuant = mean(Quantity)) %>% ggplot(aes(x = meanQuant)) + geom_histogram(fill = "indianred", bins= 100000) + geom_rug() + coord_cartesian(xlim = c(0,100))

## Now lets gather the data to make the actual analysis

dataTest <- data1 %>% group_by(StockCode, Description) %>%
  summarize(Count = n()) %>% arrange(desc(Count))

dataTest <- dataTest[1:20,]

dataTest %>% ggplot(aes(x = reorder(Description, Count), y = Count)) +
  geom_bar(stat = "identity", color = "indianred") + coord_flip()

data1_sorted <-data1[order(data1$CustomerID),]

library(plyr)

data1_list <- ddply(data1_sorted, c("CustomerID", "Date"),
                    function(df1) paste(df1$Description, collapse = ","))
data1_list$CustomerID <- NULL
data1_list$Date <- NULL
colnames(data1_list) <- c("Items")

write.csv(data1_list, "market_ba.csv", quote = FALSE, row.names = TRUE)

## Now read the transactions for the data we previously
## Created

tr <- read.transactions("market_ba.csv", format = "basket", sep=",")
itemFrequencyPlot(tr, topN = 20, type ="absolute")
summary(tr)

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
ruleExplorer(rules)
topRules <- rules[1:10]
plot(topRules, method="graph")

