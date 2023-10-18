require(dplyr)
library(deeplr)
library(readr)
library(ggplot2)
library(urltools)
library(stringi)
library(tidyr)
library(purrr)

#set working dir
setwd("/Users/justin-casimirbraun/GitHub/Q_Alt_News")

#all files pulled from the Redash
raw_files <- list.files(path = 'data/Raw ElasticSearch/', pattern = '231014_justin.*')

posts <- data.frame()

#import all files, set up topic column and merge into single df
for(f in raw_files){
  print(f)
  cur_df <- read.csv(paste0('data/Raw ElasticSearch/', f))
  if(grepl('15M', f)){
    cur_df$topic <- 'fifteen_minutes'
  }
  if(grepl('climate_lockdown', f)){
    cur_df$topic <- 'climate_lockdown'
  }
  if(grepl('insect', f)){
    cur_df$topic <- 'insect'
  }
  if(grepl('ulez', f)){
    cur_df$topic <- 'ulez'
  }
  if(nrow(posts) == 0){
    posts <- cur_df
  } else {
    posts <- dplyr::bind_rows(posts, map2_df(cur_df, map(posts, class), ~{class(.x) <- .y;.x}))
  }
}

#save raw posts
write.csv(posts, 'data/231015_justin_posts_raw.csv')

#import df with all channels
channels <- read.csv('data/all_telegram_channels_2023_10_11.csv') %>%
  dplyr::rename(channel_url = url)%>%
  dplyr::select(id, name, screenname, channel_url, category, public, chat, source, platform_id, notes, country, followers)

#format posts to match analysis pipeline
posts <- posts %>%
  dplyr::rename(post_id = id,
                post_date = date,
                post_url = url,
                links = outlinks,
                id = channel)%>%
  dplyr::select(post_id, id, post_date, post_url, content, detected_language, likes, links, views, forwards, 
                forwarded_from, cryptocurrency_addresses, reply_to, named_entities, topic)

#merge posts and channels dfs
posts_merged <- dplyr::left_join(posts, channels, by = 'id')%>%
  dplyr::filter(source != 'linked_channel')

#terms identified as being associated with FPs for the insect topic
no_insect_keywords <- c('zangrillo', 'grill', 'cricket', 'grasshopper', 'zombi', 'insecticide', 'drone', 'drohne')

#remove FPs fromt the insect subset
insect_posts <- posts_merged %>%
  filter(topic == 'insect') %>%
  filter(!grepl(paste(no_insect_keywords, collapse = '|'), content, ignore.case = TRUE))%>%
  filter(channel_url != 'https://t.me/s/GrasshopperChannel')

#remove FPs and remerge with insect df
posts_merged_clean <- posts_merged %>%
  filter(!(grepl('VOULEZ', content, ignore.case = FALSE) & topic == 'ulez')) %>%
  filter(topic != 'insect')%>%
  rbind(insect_posts)
  
###Checks###
#get random sample to check for relevance
set.seed(123456) #set seed

# take a random sample of size sample_size from each language
sample_size = 3


random_sample <- posts_merged_clean %>%
  dplyr::filter(detected_language %in% c('de', 'nl', 'fr', 'en', 'es', 'it'))%>%
  dplyr::group_by(topic, detected_language) %>%
  dplyr::sample_n(sample_size, replace = F) %>%
  dplyr::select(post_date, content, detected_language, id, topic)

# translate non english posts in random sample to Enlish
auth_key_path <- 'data/auth_key.txt' #CHECK: change filepath to where your authentication key is saved
my_key <- read_file(auth_key_path) #load Deepl R API authentication token (https://www.deepl.com/docs-api/api-access)
random_sample$translated_content = ifelse(random_sample$detected_language == 'en',
                                          random_sample$content, deeplr::toEnglish2(random_sample$content, auth_key = my_key))
#save random sample
write.csv(random_sample, 'results/random_sample.csv')

#Check that old posts are contained in current sample
old_posts <- read.csv('data/230718_justin_template_multitopic_2023_07_26.csv') #check the location of the posts dataset
new_post_ids <- posts_merged_clean$post_id

old_posts$id_matches <- ifelse(old_posts$post_id %in% new_post_ids, 1, 0)

table(old_posts$id_matches)
table(old_posts[old_posts$fifteen_minutes == 1,]$id_matches)
table(old_posts[old_posts$fifteen_minutes == 0,]$id_matches)

#save data
write.csv(posts_merged_clean, 'data/231015_raw_data_with_topics.csv')
