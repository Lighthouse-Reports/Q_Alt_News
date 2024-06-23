# Author: Justin-Casimir Braun
# Date: 2023-07-26
# 
# Description: Spread analysis pipeline. Analysis includes
# posts over time and by language, most frequent posters, most succsessful posts, and most shared links. 
# Always by topic and language.
# The script should be easily adaptable to other datasets which were extracted using the same query format (https://euq.bellingcat.com/queries/123)
# -- simply change search for 'CHECK' throughout the script and change parameters as is most useful to your analysis.

setwd("/Users/justin-casimirbraun/GitHub/Q_Alt_News")

### load libraries ###
library(dplyr)
library(deeplr)
library(readr)
library(ggplot2)
library(urltools)
library(stringi)
library(tidyr)
library(readxl)
library(lubridate)

### load data ###
posts <- read.csv('data/231015_raw_data_with_topics.csv') #check the location of the posts dataset


#remove languages not in lang_vector
lang_vector <- c('de', 'en', 'fr', 'it', 'nl') #CHECK: languages to be kept

#keep only posts in lang_vector
posts <- posts %>%
  dplyr::filter(detected_language %in% lang_vector) %>%
  dplyr::filter(!(topic %in% c('C40', 'plandemic')))


# format dat column
posts$date <- as.Date(posts$post_date, '%Y-%m-%d')

# count daily posts 
posts_day <- posts %>%
  group_by(date) %>%
  count()

ggplot(posts_day, aes(x = date, y = n))+
  geom_point()+
  ggtitle('Posts per day across all topics')+
  xlab('Date')+
  ylab('# posts')

#save
ggsave(filename = 'results/spread/posts_day.png')

#count daily posts by topic
posts_day_topic <- posts %>%
  group_by(topic, date) %>%
  count()

#plot daily posts by topic
ggplot(posts_day_topic, aes(x = date, y = n, color = topic)) +
  geom_point(size = 0.2)+
  labs(title = 'Posts by day and topic')+
  ylab('Posts per day')

#save image and dataframe
ggsave('results/spread/posts_day_topic.png')
write.csv(posts_day, 'results/spread/posts_day_topic.csv')



### posts by language ###
posts_lang <- posts %>%
  group_by(topic, detected_language) %>%
  count()

#bar graph of posts by language & topic
ggplot(posts_lang, aes(x = reorder(detected_language, -n), y = n, fill = topic)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ggtitle('Posts by Language')+
  ylab('# Posts') +
  xlab('language')

#save plot
ggsave('results/spread/posts_lang_topic.png')


### posts over language and time ###
#count posts by language and day
posts_day_lang <- posts %>%
  group_by(date, detected_language, topic) %>%
  count()

#plot posts by language and day
ggplot(posts_day_lang, aes(x = date, y = n, color = topic)) +
  geom_point(size = 0.2)+
  facet_grid(detected_language ~ .) +
  ggtitle('Posts by language and day')

#save posts by language and day
ggsave('results/spread/posts_lang_day_topic.png')

write.csv(posts_day_lang, 'results/spread/posts_lang_day_topic.csv')

### most prolific posters by topic ###
#count posts by channel and keep top 10 channels
all_channels <- read.csv('data/all_telegram_channels_2023_10_11.csv')
top_channels <- posts %>%
  filter(chat == 'false') %>%
  group_by(id, topic) %>%
  count() %>%
  arrange(desc(n)) %>%
  tidyr::pivot_wider(names_from = 'topic', values_from = 'n') %>%
  left_join(all_channels, by = 'id')


#save top channels
write.csv(top_channels, 'results/spread/top_channels.csv')

### most prolific posters by language by topic###
#count most prolific posters by language and keep top 5 for each language
top_channels_lang <- posts %>%
  filter(chat == 'false') %>%
  group_by(id, detected_language, topic) %>%
  count() %>%
  tidyr::pivot_wider(names_from = 'topic', values_from = 'n') %>%
  group_by(detected_language)%>%
  left_join(all_channels, by = 'id')

#save top channels by language
write.csv(top_channels_lang, 'results/spread/top_channels_lang.csv')

### most forwarded channels by topic###
top_reposted_channels <- posts %>%
  dplyr::group_by(detected_language, forwarded_from, topic) %>%
  dplyr::count() %>%
  dplyr::filter(!is.na(forwarded_from)) %>%
  tidyr::pivot_wider(names_from = 'topic', values_from = 'n') %>%
  dplyr::left_join(all_channels, by = c('forwarded_from' = 'id'))
write.csv(top_reposted_channels, 'results/spread/top_reposted_channels.csv')

### Shared URL and domain analysis ###
# extract urls and domains
posts$link_url <- stri_extract_all_regex(posts$links, "(?<=u').*?(?=')") #extract links
url_df <- as.data.frame(unnest(posts, cols=link_url)) %>% #unnest the dataframe
  dplyr::filter(!is.na(.$link_url))
url_df$domain <- domain(url_df$link_url) #extract domains from urls

#exclude social media domains from analysis
excluded_domains <- c('t.me', 'rumble.com', 'youtu.be', 'youtube.com', 'www.youtube.com','m.youtube.com', ', u', 'bit.ly', 'twitter.com', 'mobile.twitter.com', 'instagram.com', 'truthsocial.com', 'm.facebook.com')


#count number of times each url was shared by language
top_urls <- url_df %>%
  filter(!(domain %in% excluded_domains)) %>% #exclude social media domains
  group_by(link_url, detected_language, topic) %>%
  dplyr::count() %>%
  tidyr::pivot_wider(names_from = 'topic', values_from = 'n') 

#save dataframe
write.csv(top_urls, 'results/spread/top_urls.csv')

# count forwarded domains by language
top_domains <- url_df %>%
  filter(!(domain %in% excluded_domains)) %>%
  group_by(domain, detected_language, topic) %>%
  dplyr::count() %>%
  tidyr::pivot_wider(names_from = 'topic', values_from = 'n') 


#save dataframe
write.csv(top_domains, 'results/spread/top_domains.csv')

###mainstream vs alt news over time and by language###
media_classification <- read_xlsx('data/media_classification.xlsx')

legacy_domains <- media_classification %>%
  filter(type=='legacy') %>%
  select(domain)
alt_news_domains <- media_classification %>%
  filter(type=='alt') %>%
  select(domain)

url_df <- url_df %>%
  dplyr::mutate(alt = ifelse(grepl(paste(alt_news_domains$domain, collapse = '|'), link_url), 1, 0),
                legacy = ifelse(grepl(paste(legacy_domains$domain, collapse = '|'), link_url), 1, 0))

media_type_conspiracy <- url_df %>%
  group_by(topic) %>%
  summarise(n_alt = sum(alt),
            n_legacy = sum(legacy))
write.csv(media_type_conspiracy, 'results/spread/media_type_topic.csv')

media_type_topic_day <- url_df %>%
  dplyr::mutate(month = floor_date(date, 'month')) %>%
  dplyr::group_by(month, topic) %>%
  dplyr::summarise(n_alt = sum(alt),
                   n_legacy = sum(legacy)) %>%
  pivot_longer(c(n_alt, n_legacy), names_to = 'media_type', values_to = 'count')
#TODO: save

ggplot(media_type_topic_day, aes(x = month, y = count, shape = media_type))+
  geom_point(size = 1.5)+
  facet_grid(topic ~ ., scales = 'free_y')
ggsave('results/spread/media_type_topic_month.png', plot = last_plot())


posts_media <- posts %>%
  dplyr::mutate(alt = ifelse(grepl(paste(alt_news_domains$domain, collapse = '|'), link_url), 1, 0),
                legacy = ifelse(grepl(paste(legacy_domains$domain, collapse = '|'), link_url), 1, 0)) %>%
  dplyr::select(-link_url) %>%
  as.data.frame()

#write.csv(posts_media, 'data/231130_posts_media.csv')
