#install packages
install.packages("longurl")
install.packages("urltools")
install.packages("splitstackshape")
install.packages("readxl")


#load libraries
library(tidyverse)
library(splitstackshape)
library(longurl)
library(urltools)
library(readxl)


#load data
climatechange <- read_excel("Data/climatechangeSHORT.xlsx")
View(climatechange)

#create edge list
CC_CLEAN <- 
  climatechange %>% filter(tweet_type == "original", ! is.na(urls)) %>% mutate(links = as.character(urls) %>% gsub('^c[(]|"|[)]$','', . )) %>% 
  splitstackshape::cSplit(splitCols = "links", sep = " ", direction = "long") %>% as_tibble() %>% mutate(links = as.character(links), links_length = nchar(links))


#run on fill list - this may take a while

CC_Compact_URLS <- CC_CLEAN %>% filter(links_length <30) %>% count(links, sort = TRUE) %>% .$links %>% longurl::expand_urls(urls_to_expand = . , seconds = 5)

CC_Compact_URLS <- CC_CLEAN %>% left_join(CC_Compact_URLS %>% filter(status_code == "200") %>% select(-status_code), by = c("links" = "orig_url")) %>% 
  mutate(links = if_else(is.na(expanded_url), true = links, false = expanded_url)) %>%
  mutate(domain = domain(links))

#rename to edgelist and add weight 
CLIMATECHANGE_EDGELIST <- CC_Compact_URLS %>% rename(Source  = user_screen_name, Target = domain) %>% group_by(Source, Target) %>%                       ## The unit of analysis is the edge between Source and Target
  summarise(weight = n(), total_retweets = sum(retweet_count)) 


#export to csv
write.csv(CLIMATECHANGE_EDGELIST, file = "Data/CLIMATECHANGE_EDGELIST.csv")
#use this to create a node list in excel

CLIMATECHANGE_NODELIST <- read_excel("Data/CLIMATECHANGE_NODELIST.xlsx")
View(GLOBALWARMING_NODELIST)
