library(tidyverse)
library(lubridate)

tinder_data <- Tinder_data[,1:7]
messages <- Tinder_messages_for_R

tinder_data <- mutate(tinder_data,swipes = tinder_data$`Left swipes`+tinder_data$`Right swipes`)


group_by(messages$`Match id`) %>%  summarise(message)

s1 = gsub(".*, "," ",messages$Time)
s1 <- gsub(" GMT"," ",s1)

messages$Time <- s1

messages$Time

s2 <- dmy_hms(messages$Time)
messages$Time <- s2

tinder_data$Date <- dmy_hms(messages$Time)
tinder_data$Date <- gsub(":","",tinder_data$Date)

message_length <- group_by(messages,messages$`Match id`) %>% summarise(n())
message_length <- mutate(message_length,convo_length = message_length$`n()`)
message_length <- mutate(message_length,convo = case_when(convo_length==1 ~ "No reply",
                                                          convo_length<5 ~ "Short convo",
                                                          convo_length<10 ~ "Medium convo",
                                                          convo_length > 9 ~ "Long convo",
                                                          convo_length == 0 ~ "Bot"))
message_length$convo <- as.factor(message_length$convo)
