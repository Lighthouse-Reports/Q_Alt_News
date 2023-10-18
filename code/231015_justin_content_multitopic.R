# Author: Justin-Casimir Braun
# Date: 2023-07-27
# 
# Description: This script can be used to analzye the topics associated with a narrative. It includes keyword extraction (nouns and cooccurences),
# topic modelling, and will soon include named entity recognition as well.
# The script is meant as a template and it will need to be adapted based on what you find. For instance, if the keyword analysis yields interesting terms,
# you can specify relevant terms and find out how they were used over time and by which channels they were used. Places where users might want to adapt
# the script are commented with 'CHECK' -- just command-F.
# The script is meant to be used with results from queries that follow this format: https://euq.bellingcat.com/queries/123/source#153
# TODO: implement NER, etxtract results from stm and lda models

setwd("/Users/justin-casimirbraun/GitHub/Q_Alt_News")#CHECK: specify working dir

### load libraries ###
require(dplyr)
require(tidyr)
require(ggplot2)
require(quanteda)
require(quanteda.textmodels)
require(seededlda)
require(udpipe)
require(textrank)
require(stringr)
require(data.table)
require(hash)
require(scales)
require(stm)
require(stringr)
require(peRspective)
require(lubridate)

### load data ###
posts <- read.csv('data/231015_raw_data_with_topics.csv') #CHECK: specify dataset location

lang_vector <- c('de', 'en', 'es', 'fr', 'it', 'nl') #CHECK: languages to be kept

#keep only posts in lang_vector & create topic and date cols
posts <- posts %>%
  dplyr::filter(detected_language %in% lang_vector)

#set up date column inclyding hours, minutes, so duplicate posts can be removed
posts$date <- as.Date(posts$post_date, '%Y-%m-%d')

### Split by lang ###
posts_lang_dic <- hash() #dictionary, where key is a language from lang_vector and value is a dataframe containing all posts in that language
for(lang in lang_vector){
  posts_lang_dic[[lang]] <- posts %>%
    dplyr::filter(detected_language == lang)# %>% #only keep posts in language lang
    # dplyr::group_by(content) %>% #CHECK: this removes duplicate posts (i.e., reposts). Comment out if you don't want this to happen
    # dplyr::filter(date == min(date)) %>% #CHECK: this removes duplicate posts (i.e., reposts). Comment out if you don't want this to happen
    # dplyr::distinct(content, .keep_all = T) #CHECK: this removes duplicate posts (i.e., reposts). Comment out if you don't want this to happen
}


### extract most important keywords ###

#CHECK: the lines below load ud_pipe (https://cran.r-project.org/web/packages/udpipe/index.html) models which can be used for parts of speech tagging. 
#If you haven't downloaded them yet, uncomment and run the code below

# ud_model_de <- udpipe_download_model(language = "german")
# ud_model_en <- udpipe_download_model(language = "english")
# ud_model_es <- udpipe_download_model(language = "spanish")
# ud_model_fr <- udpipe_download_model(language = "french")
# ud_model_it <- udpipe_download_model(language = "italian")
# ud_model_nl <- udpipe_download_model(language = "dutch")

ud_models_dic <- hash() #dictionary, where key is a language from lang_vector and value is a udpipe model of the corresponding language
#populate the ud pipe dictionary
ud_models_dic[['de']] <- udpipe_load_model(file = 'data/udpipes/german-gsd-ud-2.5-191206.udpipe')
ud_models_dic[['en']] <- udpipe_load_model(file = 'data/udpipes/english-ewt-ud-2.5-191206.udpipe')
ud_models_dic[['es']] <- udpipe_load_model(file = 'data/udpipes/spanish-gsd-ud-2.5-191206.udpipe')
ud_models_dic[['fr']] <- udpipe_load_model(file = 'data/udpipes/french-gsd-ud-2.5-191206.udpipe')
ud_models_dic[['it']] <- udpipe_load_model(file = 'data/udpipes/italian-isdt-ud-2.5-191206.udpipe')
ud_models_dic[['nl']] <- udpipe_load_model(file = 'data/udpipes/dutch-alpino-ud-2.5-191206.udpipe')

#annotate posts using ud pipes
posts_annotated_dic <- hash() #dictionary, where key is a language from lang_vector and value is a df with annotated posts
for(lang in lang_vector){
  posts_annotated_dic[[lang]] <- udpipe_annotate(ud_models_dic[[lang]], #udpipe model
                                                 x = posts_lang_dic[[lang]]$content, #posts in the corresponding language
                                                 doc_id = posts_lang_dic[[lang]]$post_id) %>% #document identifier
    as.data.frame()
}

topics <- unique(posts$topic) #unique topics

### Most frequent nouns ###
dir.create('results/content/nouns', showWarnings = F) #set up directory to save results

for(lang in lang_vector){
  #find most frequently used nouns in posts for each language
  noun_freq <- posts_annotated_dic[[lang]]%>%
    dplyr::filter(upos == 'NOUN') %>% #keep only nouns
    dplyr::mutate(token_clean = stringr::str_replace_all(token, "[^[:alnum:]]", "")) %>% #remove non alpha-numerical characters
    dplyr::filter(token_clean != '') %>% #remove non alpha-numerical characters
    dplyr::group_by(lemma) %>% #CHECK: grouping by lemma here, you could also go with 'token'
    dplyr::count() %>% #count noun frequency
    dplyr::ungroup() %>%
    dplyr::mutate(share = n/sum(n, na.rm = T)) %>% # for each noun, calculate share of all the nouns in the corpus (i.e., posts in a given language)
    dplyr::arrange(desc(n)) #sort in descending order
  
  for(cur_topic in topics){
    noun_freq_topic <- posts_annotated_dic[[lang]] %>%
      dplyr::mutate(doc_id = as.numeric(doc_id)) %>%
      dplyr::left_join(select(posts, post_id, topic), by = c('doc_id' = 'post_id')) %>%
      dplyr::filter(topic == cur_topic, upos == 'NOUN') %>%
      dplyr::mutate(token_clean = stringr::str_replace_all(token, "[^[:alnum:]]", "")) %>% #remove non alpha-numerical characters
      dplyr::filter(token_clean != '') %>% #remove non alpha-numerical characters
      dplyr::group_by(lemma) %>% #CHECK: grouping by lemma here, you could also go with 'token'
      dplyr::summarise(!!cur_topic:= n()) #count noun frequency
    
    noun_freq <- left_join(noun_freq, noun_freq_topic, by = 'lemma')
  }
  
  #sace noun frequency files
  write.csv(noun_freq, paste0('results/content/nouns/noun_freq_', lang,'.csv'))
  
  noun_freq_long <- noun_freq %>%
    dplyr::arrange(desc(n)) %>%
    head(15) %>%
    tidyr::pivot_longer(cols = all_of(c('n', topics)), names_to = 'topic', values_to = 'count') %>%
    mutate(count = ifelse(is.na(count), 0, count))
  
  temp <- noun_freq_long
  
  #plot 15 most frequently used nouns
  p <- ggplot(noun_freq_long, aes(x = reorder(lemma, -count), y = count, fill = topic))+
    geom_bar(stat='identity', position = 'dodge', )+
    theme(axis.text.x = element_text(angle = 45, hjust=1))+
    xlab('term')+
    ggtitle(paste0('Most Frequently used ', lang, ' nouns (lemmas)'))
  
  print(p)
  #save plot
  ggsave(filename = paste0('results/content/nouns/noun_freq_', lang,'.png'), plot = p)
}
#CHECK: you can easily copy and adapt the noun frequency code for other parts of speech, such as dates, adjectives etc.

### Cooccurrence analysis ###
dir.create('results/content/cooccurrence', showWarnings = F) #set up directory to save results

#find the adjectives and nouns which most frequently cooccur in any of the language specific corpora
for(lang in lang_vector){
  cooccurrence_df <- cooccurrence(x = posts_annotated_dic[[lang]]$lemma, 
                                  relevant = posts_annotated_dic[[lang]]$upos %in% c("NOUN", "ADJ")) %>%
    as.data.table()
  
  for(cur_topic in topics){
    topic_df <- posts_annotated_dic[[lang]] %>%
      dplyr::mutate(doc_id = as.numeric(doc_id)) %>%
      dplyr::left_join(select(posts, post_id, topic), by = c('doc_id' = 'post_id')) %>%
      dplyr::filter(topic == cur_topic)
    
    cooc_topic <- udpipe::cooccurrence(x = topic_df$lemma, relevant = topic_df$upos %in% c('NOUN', 'ADJ')) %>%
      as.data.table() %>%
      dplyr::rename(!!cur_topic:= cooc)
    
    cooccurrence_df <- left_join(cooccurrence_df, cooc_topic, by = c('term1', 'term2'))
  }
  
  #save results
  write.csv(cooccurrence_df, paste('results/content/cooccurrence/cooccurrence_',lang,'.csv'))
}
#CHECK: you could look at cooccurrences between other parts of speech, simply adapt the code


### Analyze keywords identified through the above analysis ###

#set up data frame
keywords_df <- data.frame(lang = character(),
                          topic = character(),
                          keywords = character())
#CHECK: add rows to the dataframe based on the keyword analysis. Each row should include keywords for one language and topic
keywords_df <- keywords_df %>%
  #CHECK: The below row is just an example; change it and add your own
  dplyr::add_row(lang = 'de', #language corpus to analyze (should be one of the languages from lang_vector)
                 topic = 'world conspiracy', #name the topic
                 keywords = paste(c('globalist','nwo', 'soros', 'schwab', 'wef', 'weltordnung', 'reset', 'gates'), collapse='|')) %>% #specify keywords relevant to the topic
  dplyr::add_row(lang = 'en', #language corpus to analyze (should be one of the languages from lang_vector)
                 topic = 'world conspiracy', #name the topic
                 keywords = paste(c('globalist','nwo', 'soros', 'schwab', 'wef', 'world order', 'reset', 'gates'), collapse='|')) %>% #specify keywords relevant to the topic
  dplyr::add_row(lang = 'fr', #language corpus to analyze (should be one of the languages from lang_vector)
                 topic = 'world conspiracy', #name the topic
                 keywords = paste(c('globalist', 'nwo', 'mondialist', 'ordre mondial', 'soros', 'réinitialisation', 'reinitialisation', 'reset', 'schwab', 'wef', 'gates'), collapse='|')) %>% #specify keywords relevant to the topic
  dplyr::add_row(lang = 'nl', #language corpus to analyze (should be one of the languages from lang_vector)
                 topic = 'world conspiracy', #name the topic
                 keywords = paste(c('globalist', 'nwo', 'world order','wereldorde', 'soros', 'schwab', 'wef', 'reset', 'gates'), collapse='|')) %>% #specify keywords relevant to the topic
  dplyr::add_row(lang = 'it', #language corpus to analyze (should be one of the languages from lang_vector)
                 topic = 'world conspiracy', #name the topic
                 keywords = paste(c('globalist', 'nwo', 'world order','ordine mondial', 'soros', 'schwab', 'wef', 'reset', 'gates'), collapse='|')) %>% #specify keywords relevant to the topic
  dplyr::add_row(lang = 'es', #language corpus to analyze (should be one of the languages from lang_vector)
                 topic = 'world conspiracy', #name the topic
                 keywords = paste(c('globalist', 'nwo', 'world order','orden mundial', 'soros', 'schwab', 'wef', 'gran reinicio', 'gates'), collapse='|')) %>% #specify keywords relevant to the topic
  dplyr::add_row(lang = 'de', #language corpus to analyze (should be one of the languages from lang_vector)
                 topic = 'freedom', #name the topic
                 keywords = paste(c('freiheit', 'kontroll', 'gefängnis', 'tyrannei', 'autoritär', 'gulag', 'credit'), collapse='|')) %>%#specify keywords relevant to the topic
  dplyr::add_row(lang = 'de', #language corpus to analyze (should be one of the languages from lang_vector)
                 topic = 'covid', #name the topic
                 keywords = paste(c('covid', 'pandemie', 'impfung', 'gesundheit', 'epidemisch'), collapse='|')) %>% #specify keywords relevant to the topic
  dplyr::add_row(lang = 'en', #language corpus to analyze (should be one of the languages from lang_vector)
                 topic = 'freedom', #name the topic
                 keywords = paste(c('surveillance', 'freedom', 'control', 'camera', 'prison', 'credit', 'camera', 'plate recognition'), collapse='|')) %>%#specify keywords relevant to the topic
  dplyr::add_row(lang = 'en', #language corpus to analyze (should be one of the languages from lang_vector)
                 topic = 'covid', #name the topic
                 keywords = paste(c('covid', 'pandemic', 'vacc', 'health'), collapse='|'))


#adds a columns for each topic in the relevant language specific corpus
for(i in 1:nrow(keywords_df)){
  row <- keywords_df[i,]
  posts_lang_dic[[row$lang]] <- posts_lang_dic[[row$lang]] %>%
    dplyr::mutate(content_lower = tolower(content))%>%
    dplyr::mutate(!!row$topic := ifelse(grepl(row$keywords, content_lower), 1, 0)) #if any of the keywords was found in the corpus, set the topic variable to 1
}


# keywords over time and top channels using keywords
for(i in 1:nrow(keywords_df)){
  row <- keywords_df[i,]
  dir.create(paste0('results/content/', row$topic, '_', row$lang)) #set up directory to save results for each topic
  
  # number of weekly posts for a specific topic
  topic_posts_week <- posts_lang_dic[[row$lang]] %>%
    dplyr::mutate(week = cut(date, "week")) %>%
    group_by(topic, week) %>%
    summarise(count_topic = sum(eval(as.symbol(row$topic)), na.rm = T), 
              share = mean(eval(as.symbol(row$topic)), na.rm = T),
              count_total = n())
  
  #plot weekly posts for a specific topic
  p <- ggplot(topic_posts_week, aes(x = as.Date(week), y = count_topic)) +
    geom_point(size = 0.3)+
    geom_line(aes(x = as.Date(week), y = count_total))+
    ggtitle(paste0(row$topic, ': posts per week ', row$lang))+
    xlab('week')+
    facet_grid(topic ~ .)
  
  #save plot
  ggsave(filename = paste0('results/content/', row$topic,'_', row$lang, '/posts_per_week.png'))
  
  # find channels who post on a given topic and how often they post
  temp <- posts_lang_dic[[row$lang]] %>%
    group_by(channel_url) %>%
    summarise(n = n(),
              count = sum(eval(as.symbol(row$topic)), na.rm = T), #sum of posts on a given topic
              share = mean(eval(as.symbol(row$topic)), na.rm = T)) %>% #share of a channels posts which are on a given topic
    dplyr::filter(count !=0) %>%
    dplyr::arrange(desc(count))
  
  #save top poster dataframe
  write.csv(temp, paste0('results/content/', row$topic,'_', row$lang, '/top_channels.csv'))
}

###named entity recognition###
posts_NER <- posts %>%
  dplyr::mutate(ents = stringr::str_extract_all(named_entities, "\\{(.*?)\\}")) %>%
  unnest(cols = ents) %>%
  as.data.frame() %>%
  mutate(NER_text = stringr::str_extract(ents, "(?<=u'text': u').*?(?=',)"),
         NER_type = stringr::str_extract(ents, "(?<=u'type': u').*?(?='\\})"))

top_PERS_ner <- posts_NER %>%
  dplyr::filter(NER_type == 'PERSON') %>%
  dplyr::group_by(topic, detected_language, NER_text) %>%
  dplyr::count() %>%
  dplyr::arrange(desc(n))

write.csv(top_PERS_ner, 'results/content/top_Pers_ner.csv')

top_ORG_ner <- posts_NER %>%
  dplyr::filter(NER_type == 'ORG') %>%
  dplyr::group_by(topic, detected_language, NER_text) %>%
  dplyr::count() %>%
  dplyr::arrange(desc(n))
write.csv(top_ORG_ner, 'results/content/top_ORG_ner.csv')



## TOXICITY ###
# posts$TOXICITY <- NA
# posts$INSULT <- NA
# posts$THREAT <- NA
# for (i in 12691:nrow(posts)){
#   row <- posts[i,]
#   if(!row$detected_language %in% lang_vector){
#     next
#   }
#   perspective_score <- as.data.frame(prsp_score(
#     row$content,
#     languages = lang,
#     score_model = c('TOXICITY', 'INSULT', 'THREAT')
#   ))
#   posts[i,]$TOXICITY <- perspective_score$TOXICITY
#   posts[i,]$INSULT <- perspective_score$INSULT
#   posts[i,]$THREAT <- perspective_score$THREAT
# }
# 
# posts_toxicity <- posts
# write.csv(posts_toxicity, 'results/content/posts_toxicity.csv')
posts_toxicity <- read.csv('results/content/posts_toxicity.csv')

#toxicity over time
month_tox_topic <- posts_toxicity %>%
  mutate(month = lubridate::floor_date(as.Date(date), 'month')) %>%
  group_by(month, topic, detected_language) %>%
  summarise(mean_tox = mean(TOXICITY, na.rm = T),
            mean_insult = mean(INSULT, na.rm = T),
            mean_threat = mean(THREAT, na.rm = T),
            count = n()) %>%
  filter(topic != 'fifteen_minutes climate_lockdown ', detected_language != 'es', count > 10)

ggplot(month_tox_topic, aes(x = month, y = mean_tox, color = topic)) +
  geom_point()+
  geom_smooth(method = 'lm', se = T, aes(group = topic))+
  facet_wrap(.~ detected_language)+
  ggtitle('Toxicity by month, topic, and language')
ggsave('results/content/toxicity/month_tox_topic.png', plot = last_plot())

ggplot(month_tox_topic, aes(x = month, y = mean_insult, color = topic)) +
  geom_point()+
  geom_smooth(method = 'lm', se = T, aes(group = topic))+
  facet_wrap(.~ detected_language)+
  ggtitle('Insult by month, topic, and language')
ggsave('results/content/toxicity/month_insult_topic.png', plot = last_plot())

ggplot(month_tox_topic, aes(x = month, y = mean_threat, color = topic)) +
  geom_point()+
  geom_smooth(method = 'lm', se = T, aes(group = topic))+
  facet_wrap(.~ detected_language)+
  ggtitle('Threat by month, topic, and language')
ggsave('results/content/toxicity/month_threat_topic.png', plot = last_plot())

posts_toxicity$min_date <- min(posts_toxicity$date)
posts_toxicity$date_diff <- difftime(posts_toxicity$date, posts_toxicity$min_date, units = 'days')
posts_toxicity$date_diff <- as.numeric(posts_toxicity$date_diff)

topics <- unique(posts_toxicity$topic)
for(lang in lang_vector){
  for(cur_top in topics){
    cur_df <- posts_toxicity %>%
      filter(detected_language == lang, topic == cur_top, date > as.Date('2022-06-01'))
    print(paste0('Topic: ', cur_top, ', Language: ', lang))
    tox_fit <- lm(data = cur_df, TOXICITY ~ date_diff)
    write.csv(summary(tox_fit)$coefficients, paste0('results/content/toxicity/fit_tox', cur_top, '_', lang, '.csv'))
    threat_fit <- lm(data = cur_df, THREAT ~ date_diff)
    write.csv(summary(threat_fit)$coefficients, paste0('results/content/toxicity/fit_threat', cur_top, '_', lang, '.csv'))
    insult_fit <- lm(data = cur_df, INSULT ~ date_diff)
    write.csv(summary(insult_fit)$coefficients, paste0('results/content/toxicity/fit_insult', cur_top, '_', lang, '.csv'))
  }
}
#toxicity by language
lang_tox_topic <- posts_toxicity %>%
  group_by(detected_language, topic) %>%
  summarise(mean_tox = mean(TOXICITY, na.rm = T),
            mean_insult = mean(INSULT, na.rm = T),
            mean_threat = mean(THREAT, na.rm = T)) %>%
  pivot_longer(cols = starts_with('mean'), names_to = 'type', values_to = 'value') %>%
  filter(detected_language != 'es', topic != 'fifteen_minutes climate_lockdown ')

ggplot(lang_tox_topic, aes(x = detected_language, y = value, fill = topic))+
  geom_bar(stat = 'identity', position = 'dodge')+
  facet_wrap(. ~ type)+
  ggtitle('Toxicity, Threat, and Insult topic, and language')
ggsave('results/content/toxicity/lang_tox_topic.png', plot = last_plot())

#toxicity by channel
tox_channel <- posts_toxicity %>%
  group_by(channel_url, topic) %>%
  summarise(mean_tox = mean(TOXICITY, na.rm = T),
            mean_insult = mean(INSULT, na.rm = T),
            mean_threat = mean(THREAT, na.rm = T),
            count = n()) %>%
  filter(count > 3)%>%
  filter(topic != 'fifteen_minutes climate_lockdown ')
write.csv(tox_channel, 'results/content/toxicity/top_channels.csv')
