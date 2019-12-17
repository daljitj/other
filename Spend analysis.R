#load packages
library(tidyverse)
library(readr)
library(lubridate)
library(hrbrthemes)

## set work directory
setwd("C:/Users/User/Documents/R/Natwest")
getwd()

## import data
raw <- read_csv("NatWest-download-20191126.csv")


## strip paid in
spend <- filter(raw,Value < 0)

## get R to recognise dates
spend <- mutate(spend, Date = dmy(spend$Date))

## Clean description

spend$descriptionclean <- sapply(strsplit(spend$Description, ',\\s*'), `[`, 2)
spend <- (mutate(spend,descriptionclean =if_else(is.na(descriptionclean)==TRUE,Description,descriptionclean)))
spend <- (mutate(spend,descriptionclean =if_else(str_detect(descriptionclean,"\\'ROYAL BANK [:alnum:]*")==TRUE,"ROYAL BANK",descriptionclean)))

spend <- select(spend,-("descriptionclean"))

## Make all spend positive
spend <- mutate(spend, Value = Value*-1)

###################################################

## Unique vendors

unique(spend$descriptionclean)

## count instances by vendor

group_by(spend,descriptionclean) %>% count() %>% arrange(desc(n))

group_by(spend,descriptionclean) %>% count() %>% ungroup() %>%
  filter(n >= 5) %>% 
  mutate(descriptionclean = fct_reorder(descriptionclean, n)) %>%
  ggplot(aes(descriptionclean,n)) +
  geom_bar(stat="identity") + coord_flip() +theme_classic() +
  xlab("Vendor") + ylab("Frequency") + labs(title="Most frequently used vendors in the last 4 months",
                                            subtitle="Vendors with more than 5 visits only")
  

## Spend by vendor

spendbyvendor <- group_by(spend,descriptionclean) %>% summarise(totalspend = sum(Value)) %>% arrange(desc(totalspend))

filter(spendbyvendor,totalspend > 46) %>% mutate(descriptionclean = fct_reorder(descriptionclean, totalspend)) %>% 
  ggplot(aes(descriptionclean,totalspend)) + 
  geom_bar(stat="identity") + coord_flip() +theme_classic()

## spend by month

spendbymonth <- filter(spend, month(Date) > 7) %>% group_by(month(Date)) %>% summarise(totalspend = sum(Value))

ggplot(spendbymonth,aes(spendbymonth$`month(Date)`,totalspend)) + 
  geom_bar(stat="identity")  +theme_classic() +
  xlab("Month") + ylab("Total spend") +
  labs("Spend by month") 


## Plot spend over time

#filter(spend, !descriptionclean == "SWRAILWAYSELFSERVE" & !descriptionclean == "SELFSERVE TICKET ") %>% 
#  ggplot(aes(Date,Value)) +
#  geom_bar(stat="identity") + theme_classic()


#filter(spend, !descriptionclean == "SWRAILWAYSELFSERVE" & !descriptionclean == "SELFSERVE TICKET ") %>% 
#  ggplot(aes(Date,Value,fill=descriptionclean)) +
#  geom_bar(stat="identity",position = "stack") + theme_classic()    # stacked and filled


spend %>% ggplot( aes(Date,Value)) +
  geom_histogram(stat="identity") + theme_classic() 

## spend density
spend %>% ggplot( aes(Date,density=Value)) +
  geom_density(stat="density") + theme_classic()

## Savings over time

spend %>% ggplot( aes(Date,Balance)) +
  geom_line() + theme_classic() +
  ylab("Total savings") + labs(title = "My total savings over the past 4 months") +
  scale_y_continuous(labels = scales::dollar_format(prefix="£"))


## Most expensive days

spendbyday <- group_by(spend,Date) %>% summarise(totalspend = sum(Value)) %>% arrange(desc(totalspend)) # sorted from most to least
group_by(spend,Date) %>% summarise(totalspend = sum(Value)) %>% arrange(totalspend)  # sorted from least to most


left_join(spend,spendbyday) %>% filter(!descriptionclean == "SWRAILWAYSELFSERVE" & !descriptionclean == "SELFSERVE TICKET ") %>% 
  filter(totalspend > 50) %>%
  ggplot(aes(Date,Value,fill=descriptionclean)) +
  geom_bar(stat="identity",position = "stack") + theme_classic()    # stacked and filled

left_join(spend,spendbyday) %>% filter(!descriptionclean == "SWRAILWAYSELFSERVE" & !descriptionclean == "SELFSERVE TICKET ") %>% 
  filter(totalspend > 100) %>% View()


spendbyday <- filter(spend, !descriptionclean == "SWRAILWAYSELFSERVE" & !descriptionclean == "SELFSERVE TICKET ") %>% 
  group_by(Date) %>% summarise(totalspend = sum(Value)) %>% arrange(desc(totalspend)) # sorted from most to least


## plot spend by day with trains filtered

filter(spend, !descriptionclean == "SWRAILWAYSELFSERVE" & !descriptionclean == "SELFSERVE TICKET ") %>% 
left_join(spendbyday) %>%  
  filter(totalspend > 0) %>% 
  ggplot(aes(as.Date(Date),Value)) +
  geom_bar(stat="identity") + theme_classic() +
  ylab("Total spend") + xlab("Date") + labs(title="Total expenditure by day", subtitle="Spend on train tickets excluded") + 
  scale_y_continuous(labels = scales::dollar_format(prefix="£")) +
  annotate(
    geom = "curve", x = as.Date("2019-11-01")	, y = 260, xend = as.Date("2019-11-24"), yend = 200, 
    curvature = 0.3, arrow = arrow(length = unit(3, "mm")),ncp=10
  ) +
  annotate(geom = "text", x = as.Date("2019-11-01"), y = 270, label = "Jasper in London")


filter(spend,Date == "2019-11-25") %>% mutate(descriptionclean = fct_reorder(descriptionclean, Value)) %>%
  ggplot(aes(y=Value,x=descriptionclean)) +
  geom_bar(stat="identity") +coord_flip() +
  scale_y_continuous(labels = scales::dollar_format(prefix="£")) +
  labs(title="Spend when Jasper was in London by vendor")
  

