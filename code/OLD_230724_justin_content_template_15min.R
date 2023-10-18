# Author: Justin-Casimir Braun
# Date: 2023-07-20
# 
# Description: Spread analysis pipeline applied to the 15 minute cities dataset. Analysis includes
# posts over time and by language, most frequent posters, most succsessful posts, and most shared links.
# 
# The script should be easily adaptable to other datasets which were extracted using the same query format (https://euq.bellingcat.com/queries/123)
# -- simply change search for 'CHECK' throughout the script and change parameters as is most useful to your analysis.

setwd("/Users/justin-casimirbraun/Documents/Lighthouse Reports/Q Europe/Alt Media/fifteen_minutes")

### load libraries ###
library(dplyr)
library(deeplr)
library(readr)
library(ggplot2)
library(urltools)
library(stringi)
library(tidyr)

### load data ###
posts <- read.csv('data/justin_fifteen_minutes_2023_07_24.csv') #check the location of the posts dataset

#remove languages not in lang_vector
lang_vector <- c('de', 'en', 'es', 'fr', 'it', 'nl') #CHECK: languages to be kept

#keep only posts in lang_vector
posts <- posts %>%
  dplyr::filter(detected_language %in% lang_vector)

### random sample to make sure that keywords yielded relevant results ###
set.seed(123456) #set seed

# take a random sample of size sample_size from each language
sample_size = 3
random_sample <- posts %>%
  dplyr::group_by(detected_language) %>%
  dplyr::sample_n(sample_size, replace = F) %>%
  dplyr::select(post_date, content, detected_language, channel)

# translate non english posts in random sample to Enlish
auth_key_path <- '../auth_key.txt' #CHECK: change filepath to where your authentication key is saved
my_key <- read_file(auth_key_path) #load Deepl R API authentication token (https://www.deepl.com/docs-api/api-access)
random_sample$translated_content = ifelse(random_sample$detected_language == 'en', 
                                          random_sample$content, deeplr::toEnglish2(random_sample$content, auth_key = my_key))
#save random sample
write.csv(random_sample, 'results/random_sample.csv')

### posts over time ###
# format dat column
posts$date <- as.Date(posts$post_date, '%d/%m/%y')

#count daily posts
start_date <- as.Date('2022-10-01') #only keep posts after this date
posts_day <- posts %>%
  group_by(date) %>%
  count() %>%
  filter(date > start_date)

#plot daily posts
ggplot(posts_day, aes(x = date, y = n)) +
  geom_line()+
  labs(title = 'Posts by day October 22-June 2023')+
  ylab('Posts per day')

#save image and dataframe
ggsave('results/posts_day.png')
write.csv(posts_day, 'results/posts_day.csv')

### posts by language ###
posts_lang <- posts %>%
  group_by(detected_language) %>%
  count()

#bar graph of posts by language
ggplot(posts_lang, aes(x = reorder(detected_language, -n), y = n)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ggtitle('Posts by Language')+
  ylab('# Posts')

#save plot
ggsave('results/posts_lang.png')

### posts over language and time ###
#count posts by language and day
posts_day_lang <- posts %>%
  group_by(date, detected_language) %>%
  count() %>%
  filter(date > start_date)

#plot posts by language and day
ggplot(posts_day_lang, aes(x = date, y = n)) +
  geom_line()+
  facet_grid(detected_language ~ .) +
  ggtitle('Posts by language and day')

#save posts by language and day
ggsave('results/posts_lang_day.png')

### most prolific posters ###
#count posts by channel and keep top 10 channels
top_channels <- posts %>%
  filter(chat == 'false') %>%
  group_by(channel_url) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(10) #CHECK: change the number of channels you want to extract

#save top channels
write.csv(top_channels, 'results/top_channels.csv')
  
### most prolific posters by language ###
#count most prolific posters by language and keep top 5 for each language
top_channels_lang <- posts %>%
  filter(chat == 'false') %>%
  group_by(channel_url, detected_language) %>%
  count() %>%
  arrange(desc(n)) %>%
  group_by(detected_language) %>%
  slice_head(n=5) #CHECK: change the number of channels you want to extract

#save top channels by language
write.csv(top_channels_lang, 'results/top_channels_lang.csv')

### most forwarded channels ###
all_channels <- read.csv('data/All_channels_2023_07_18.csv')
top_reposted_channels <- posts %>%
  dplyr::group_by(detected_language, forwarded_from) %>%
  dplyr::count() %>%
  dplyr::filter(!is.na(forwarded_from)) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::group_by(detected_language) %>%
  dplyr::slice_head(n=5) %>%
  dplyr::left_join(all_channels, by = c('forwarded_from' = 'id'))
write.csv(top_reposted_channels, 'results/top_reposted_channels.csv')


### posts with the highest number of views ###
#count posts with the highest number of views that aren't duplicates
top_posts_views_lang <- posts %>%
  filter(chat == 'false') %>% #remove chat channels
  group_by(detected_language) %>%
  arrange(desc(views)) %>%
  distinct(content, .keep_all = T) %>% #remove duplicates
  slice_max(views, n = 5) %>% #CHECK: change the number of posts you want to extract
  select(date, views, content, detected_language, channel, channel_url)

#save top viewed posts
write.csv(top_posts_views_lang, 'results/top_posts_views_lang.csv')

### most forwarded posts ###
#find most forwarded posts by language (with duplicates removed)
top_posts_forwards_lang <- posts %>%
  group_by(detected_language) %>%
  arrange(desc(forwards)) %>%
  distinct(content, .keep_all = T) %>% #remove duplicates
  slice_max(forwards, n = 5) %>% #CHECK: change the number of posts you want to extract
  select(date, forwards, content, detected_language, channel, channel_url)

#save most forwarded posts
write.csv(top_posts_forwards_lang, 'results/top_posts_forwards_lang.csv')

### Shared URL and domain analysis ###
# extract urls and domains
posts$link_url <- stri_extract_all_regex(posts$links, "(?<=u').*?(?=')") #extract links
url_df <- as.data.frame(unnest(posts, cols=link_url)) %>% #unnest the dataframe
  dplyr::filter(!is.na(.$link_url))
url_df$domain <- domain(url_df$link_url) #extract domains from urls

#exclude social media domains from analysis
excluded_domains <- c('t.me', 'rumble.com', 'youtu.be', 'youtube.com', 'www.youtube.com', ', u', 'bit.ly', 'twitter.com', 'mobile.twitter.com', 'instagram.com', 'truthsocial.com', 'm.facebook.com')

#count number of occurences of each url by language and extract the 5 most shared urls
top_x <- 5 #CHECK: number of most shared to keep

#count number of times each url was shared by language
top_urls <- url_df %>%
  filter(!(domain %in% excluded_domains)) %>% #exclude social media domains
  group_by(link_url, detected_language) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  group_by(detected_language) %>%
  slice_max(count, n = top_x) #keep top_x most frequently shared urls

#save dataframe
write.csv(top_urls, 'results/top_urls.csv')

# count forwarded domains by language
top_domains <- url_df %>%
  filter(!(domain %in% excluded_domains)) %>%
  group_by(domain, detected_language) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  group_by(detected_language) %>%
  slice_max(count, n = top_x)  #keep top_x most frequently shared urls

#save dataframe
write.csv(top_domains, 'results/top_domains.csv')
