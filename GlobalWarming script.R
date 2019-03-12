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
globalwarming <- read_excel("Data/globalwarmingSHORT.xlsx")
View(globalwarming)

#create edge list
GW_CLEAN <- 
    globalwarming %>% filter(tweet_type == "original", ! is.na(urls)) %>% mutate(links = as.character(urls) %>% gsub('^c[(]|"|[)]$','', . )) %>% 
      splitstackshape::cSplit(splitCols = "links", sep = " ", direction = "long") %>% as_tibble() %>% mutate(links = as.character(links), links_length = nchar(links))

#run test on just 5 observations for ease of trial run
Test <- GW_CLEAN[order(GW_CLEAN$links_length), ]
TEST2 <- Test[1:5, ]    

TEST_Compact_URLS <- TEST2 %>% filter(links_length <30) %>% count(links, sort = TRUE) %>% .$links %>% longurl::expand_urls(urls_to_expand = . , seconds = 5)

TEST3 <- TEST2 %>% left_join(TEST_Compact_URLS %>% filter(status_code == "200") %>% select(-status_code), by = c("links" = "orig_url")) %>% 
  mutate(links = if_else(is.na(expanded_url), true = links, false = expanded_url)) %>%
  mutate(domain = domain(links))

TEST4 <- TEST3 %>% rename(Source  = user_screen_name, Target = domain) %>% group_by(Source, Target) %>%                       ## The unit of analysis is the edge between Source and Target
  summarise(weight = n(), total_retweets = sum(retweet_count)) 

#run on fill list - this may take a while

Compact_URLS <- GW_CLEAN %>% filter(links_length <30) %>% count(links, sort = TRUE) %>% .$links %>% longurl::expand_urls(urls_to_expand = . , seconds = 5)

GW_Compact_URLS <- GW_CLEAN %>% left_join(Compact_URLS %>% filter(status_code == "200") %>% select(-status_code), by = c("links" = "orig_url")) %>% 
  mutate(links = if_else(is.na(expanded_url), true = links, false = expanded_url)) %>%
  mutate(domain = domain(links))

#rename to edgelist and add weight 
GLOBALWARMING_EDGELIST <- GW_Compact_URLS %>% rename(Source  = user_screen_name, Target = domain) %>% group_by(Source, Target) %>%                       ## The unit of analysis is the edge between Source and Target
  summarise(weight = n(), total_retweets = sum(retweet_count)) 


#export to csv
write.csv(GLOBALWARMING_EDGELIST, file = "Data/GLOBALWARMING_EDGELIST.csv")
#use this to create a node list in excel

GLOBALWARMING_NODELIST <- read_excel("Data/GLOBALWARMING_NODELIST.xlsx")
View(GLOBALWARMING_NODELIST)
