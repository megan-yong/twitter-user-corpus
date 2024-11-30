### twitter user data analysis

# import libraries 
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
kennedyTweets <- get_timeline("@kennedymcmann", n=5000, include_rts = FALSE)

# clean data
kennedyTweets$text <- gsub("https\\S*", " ", kennedyTweets$text) # removes links
kennedyTweets$text <- gsub("@\\S*", " ", kennedyTweets$text) # removes @s
kennedyTweets$text <- gsub("#\\S*", " ", kennedyTweets$text) # removes hashtags
kennedyTweets$text <- gsub("amp", " ", kennedyTweets$text) # removes &
kennedyTweets$text <- gsub("[[:digit:]]", " ", kennedyTweets$text) # removes digits

# mutate data
kennedyTweets <- mutate(kennedyTweets, text = trimws(text), text = tolower(text))

# removing blank spaces in text
kennedyTweets_organic <- kennedyTweets %>% 
  filter(str_detect(as.character(text), "[[:alpha:]]")) %>%
  filter(!is.na(text))

# extract words from text
kennedyTweets_words <- kennedyTweets_organic %>% 
  select(id, text) %>% 
  unnest_tokens(word, text)

# count words in text 
kennedyWord_count <- kennedyTweets_words %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n))

# remove the stop words
data("stop_words")

kennedyTweets_clean <- kennedyWord_count %>%
  filter(!word %in% stop_words$word)


# bar chart plot
kennedyTweets_clean %>%
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
