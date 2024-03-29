#loading tidyverse to read input
library(tidyverse)
# loading itunesr for retrieving itunes review data that we will use in this analysis
library(itunesr)
#loading the magical esquisse library
library(esquisse)
# Flipkart Reviews
reviews <- getReviews(742044692,'in',1)
#converting Rating to numeric type
reviews$Rating <- as.numeric(reviews$Rating)
#let us say we want to see if there's any correlation between rating and review length
reviews$len <- nchar(reviews$Review)

#let the magic begin

esquisse::esquisser(reviews)
