# twitter user data analysis

library(tidyverse)
library(tidytext)
library(rtweet)
library(httpuv)
library(bbplot)
library(dplyr)


# authenticate with your Twitter API account
app_name <- "twitter"
consumer_key <- "#########"
consumer_secret <- "#########"
access_token <- "#########"
access_secret <- "#########"

# create an authentication object
twitter_token <- create_token(
  app = app_name,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret
)


# kennedy mcmann tweets 
kmm <- get_timeline("@kennedymcmann", n=5000, include_rts = FALSE)

# clean data
kmm$text <- gsub("https\\S*", " ", kmm$text) # removes links
kmm$text <- gsub("@\\S*", " ", kmm$text) # removes @s
kmm$text <- gsub("#\\S*", " ", kmm$text) # removes hashtags
kmm$text <- gsub("amp", " ", kmm$text) # removes &
kmm$text <- gsub("[[:digit:]]", " ", kmm$text) # removes digits

# mutate data
kmm <- mutate(kmm, text = trimws(text), text = tolower(text))

# removing blank cells in text
kmm_organic <- kmm %>% 
  filter(str_detect(as.character(text), "[[:alpha:]]")) %>%
  filter(!is.na(text))

# extract words from text
kmm_words <- kmm_organic %>% 
  select(id, text) %>% 
  unnest_tokens(word, text)

# count words in text 
kmm_count <- kmm_words %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n))

# remove the stop words / frequent unwanted words
data("stop_words")

kmm_clean <- kmm_count %>%
  filter(!word %in% stop_words$word)


# bar chart plot
kmm_clean %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "#1380A1") +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in Kennedy McMann's tweets",
       subtitle = "Top 15 words")
