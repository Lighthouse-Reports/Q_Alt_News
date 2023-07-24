# Author: Justin-Casimir Braun
# Date: 2023-07-21
# 
# Description: This script can be used to analzye the topics associated with a narrative. It includes keyword extraction (nouns and cooccurences),
# topic modelling, and finding trends in monetization.
# The script is meant as a template and it will need to be adapted based on what you find. For instance, if the keyword analysis yields interesting terms,
# you can specify relevant terms and find out how they were used over time and by which channels they were used. Places where users might want to adapt
# the script are commented with 'CHECK' -- just command-F.
# The script is meant to be used with results from queries that follow this format: https://euq.bellingcat.com/queries/123/source#153

setwd("/Users/justin-casimirbraun/Documents/Lighthouse Reports/Q Europe/Alt Media/fifteen_minutes") #CHECK: specify working dir

### load libraries ###
require(dplyr)
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

### load data ###
posts <- read.csv('data/justin_fifteen_minutes_2023_07_24.csv') #CHECK: specify dataset location

lang_vector <- c('de', 'en', 'es', 'fr', 'it', 'nl') #CHECK: languages to be kept

#keep only posts in lang_vector
posts <- posts %>%
  dplyr::filter(detected_language %in% lang_vector)
#set up date column inclyding hours, minutes, so duplicate posts can be removed
posts <- posts %>%
  dplyr::mutate(date = as.POSIXlt(posts$post_date, '%d/%m/%y %H:%M', tz=Sys.timezone()))

### Split by lang ###
posts_lang_dic <- hash() #dictionary, where key is a language from lang_vector and value is a dataframe containing all posts in that language
for(lang in lang_vector){
  posts_lang_dic[[lang]] <- posts %>%
    dplyr::filter(detected_language == lang) %>% #only keep posts in language lang
    dplyr::group_by(content) %>% #CHECK: this removes duplicate posts (i.e., reposts). Comment out if you don't want this to happen
    dplyr::filter(date == min(date)) %>% #CHECK: this removes duplicate posts (i.e., reposts). Comment out if you don't want this to happen
    dplyr::distinct(content, .keep_all = T) #CHECK: this removes duplicate posts (i.e., reposts). Comment out if you don't want this to happen
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
ud_models_dic[['de']] <- udpipe_load_model(file = 'german-gsd-ud-2.5-191206.udpipe')
ud_models_dic[['en']] <- udpipe_load_model(file = 'english-ewt-ud-2.5-191206.udpipe')
ud_models_dic[['es']] <- udpipe_load_model(file = 'spanish-gsd-ud-2.5-191206.udpipe')
ud_models_dic[['fr']] <- udpipe_load_model(file = 'french-gsd-ud-2.5-191206.udpipe')
ud_models_dic[['it']] <- udpipe_load_model(file = 'italian-isdt-ud-2.5-191206.udpipe')
ud_models_dic[['nl']] <- udpipe_load_model(file = 'dutch-alpino-ud-2.5-191206.udpipe')

#annotate posts using ud pipes
posts_annotated_dic <- hash() #dictionary, where key is a language from lang_vector and value is a df with annotated posts
for(lang in lang_vector){
  posts_annotated_dic[[lang]] <- udpipe_annotate(ud_models_dic[[lang]], #udpipe model
                                                 x = posts_lang_dic[[lang]]$content, #posts in the corresponding language
                                                 doc_id = posts_lang_dic[[lang]]$post_id) %>% #document identifier
    as.data.frame()
}

### Most frequent nouns ###
dir.create('results/nouns', showWarnings = F) #set up directory to save results

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
  
  #sace noun frequency files
  write.csv(noun_freq, paste0('results/nouns/noun_freq_', lang,'.csv'))
  
  #plot 15 most frequently used nouns
  p <- ggplot(head(noun_freq, 15), aes(x = reorder(lemma, -n), y = n))+
    geom_bar(stat='identity', position = 'dodge')+
    theme(axis.text.x = element_text(angle = 45, hjust=1))+
    xlab('term')+ 
    theme_bw()+ 
    ggtitle(paste0('Most Frequently used ', lang, ' nouns (lemmas)'))
  
  #save plot
  ggsave(filename = paste0('results/nouns/noun_freq_', lang,'.png'), plot = p)
}
#CHECK: you can easily copy and adapt the noun frequency code for other parts of speech, such as dates, adjectives etc.

### Cooccurrence analysis ###
dir.create('results/cooccurrence', showWarnings = F) #set up directory to save results

#find the adjectives and nouns which most frequently cooccur in any of the language specific corpora
for(lang in lang_vector){
  cooccurrence_df <- cooccurrence(x = posts_annotated_dic[[lang]]$lemma, 
                                 relevant = posts_annotated_dic[[lang]]$upos %in% c("NOUN", "ADJ")) %>%
    as.data.table()
  #save results
  write.csv(cooccurrence_df, paste('results/cooccurrence/cooccurrence_',lang,'.csv'))
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
                 topic = 'turkey_topic', #name the topic
                 keywords = paste(c('türk', 'hatay', 'erdbeben'), collapse='|')) #specify keywords relevant to the topic

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
  dir.create(paste0('results/', row$topic, '_', row$lang)) #set up directory to save results for each topic
  
  # number of weekly posts for a specific topic
  topic_posts_week <- posts_lang_dic[[row$lang]] %>%
    dplyr::mutate(week = cut(date, "week")) %>%
    group_by(week) %>%
    summarise(count = sum(eval(as.symbol(row$topic)), na.rm = T), 
              share = mean(eval(as.symbol(row$topic)), na.rm = T))
  
  #plot weekly posts for a specific topic
  p <- ggplot(topic_posts_week, aes(x = as.Date(week), y = count)) +
    geom_line( color = "blue")+
    ggtitle(paste0(row$topic, ': posts per week', lang))+
    xlab('week')
  
  #save plot
  ggsave(filename = paste0('results/', row$topic, '/posts_per_week.png'))
  
  # find channels who post on a given topic and how often they post
  temp <- posts_lang_dic[[row$lang]] %>%
    group_by(channel_url) %>%
    summarise(n = n(),
              count = sum(eval(as.symbol(row$topic)), na.rm = T), #sum of posts on a given topic
              share = mean(eval(as.symbol(row$topic)), na.rm = T)) %>% #share of a channels posts which are on a given topic
    dplyr::filter(count !=0) %>%
    dplyr::arrange(desc(count))
  
  #save top poster dataframe
  write.csv(temp, paste0('results/', row$topic, '/top_channels.csv'))
}

### topic modelling ###
# set up dfms by language
dfm_dic <- hash() #dictionary where keys are languages in lang_vector and values are language specific DFMs
for(lang in lang_vector){
  dfm_dic[[lang]] <- posts_lang_dic[[lang]] %>%
    quanteda::corpus(text_field = 'content', docid_field = 'post_id') %>%
    quanteda::tokens(remove_punct = T, #remove punctuation
                     remove_symbols = T, #remove symbols
                     remove_numbers = T, #remove numbers
                     remove_url = T, #remove urls
                     remove_separators = T, #remove separators
                     split_hyphens = T, #split hyphens
                     include_docvars = T) %>% #keep doc vars
    quanteda::tokens_remove(pattern = stopwords(lang)) %>% #remove stopwords
    quanteda::dfm() %>% #set up dfm
    quanteda::dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile", #CHECK: trim dfm, change specs if necessary
                       max_docfreq = 0.1, docfreq_type = "prop")
}

# lda analysis
k_num = 4 #CHECK: number of topics; experiment with various numbers
dir.create('results/lda', showWarnings = F) #set up directory to save results
for(lang in lang_vector){
  #run lda algo
  lda_model <- seededlda::textmodel_lda(dfm_dic[[lang]], k = k_num)
  
  #identify 20 top terms for each topic identified
  lda_terms <- terms(lda_model, 20)
  #save terms
  write.csv(lda_terms, paste0('results/lda/terms', lang, '.csv'))
  
  #assign each post a dominant topic
  topics_df <- topics(lda_model) %>%
    as.data.frame() %>%
    dplyr::rename(topic = '.')
  topics_df$post_id <- as.numeric(rownames(topics_df))
  #join posts with assigned topic
  posts_lang_dic[[lang]] <- dplyr::left_join(posts_lang_dic[[lang]], topics_df, by = "post_id")
  
  # count topic occurrences by week
  posts_topic_date <- posts_lang_dic[[lang]] %>%
    dplyr::mutate(week = cut(date, "week")) %>%
    dplyr::filter(!is.na(topic)) %>%
    group_by(week, topic) %>%
    count()
  # plot topic occurences over time
  p <- ggplot(posts_topic_date, aes(x = as.Date(week), y = n, color = topic))+
    geom_point()+
    ggtitle(paste0('Weekly posts by Topic: ', lang))
  #save plot
  ggsave(filename = paste0('results/lda/topics_week', lang, '.png'))
  
}
#CHECK: additional topic modelling approaches, such as 'stm' can easily be implemented on the basis of the same DFMs


### Monetization ###
#keywords for monetization
money_keys <- hash() #monetization keyword dictionary, where key is a language in lang_vector and value is a vector of keywords for monetization in that language
money_keys[['de']] <- c('spende', 'iban', 'paypal', 'bitcoin', 'amazon', 'btc', 'unterstütz', 'schenk')
#CHECK: add keywords for other languages using the format above

dir.create('results/monetization', showWarnings = F) #set up directory to save monetization results

#extract all monetization results
#CHECK results to make sure there aren't false positives or false negatives
for(lang in lang_vector){
  if(is.null(money_keys[[lang]])) next #only analyze languages for which monetization keywords have been specified
  
  keys <- money_keys[[lang]]
  posts_lang_dic[[lang]] <- posts_lang_dic[[lang]] %>%
    dplyr::mutate(content_lower = tolower(content)) %>%
    dplyr::mutate(monetization = ifelse(grepl(paste(keys, collapse = '|'), content_lower), 1, 0 ))
  
  mon_posts <- posts_lang_dic[[lang]] %>%
    dplyr::filter(monetization == 1) %>%
    dplyr::select(channel_url, content)
  
  #save monetization posts
  write.csv(mon_posts, paste0('results/monetization/money_posts_', lang, '.csv'))
}

# Monetization by week (might be interesting to compare to topic evolution graphs generated above)
for(lang in lang_vector){
  if(!('monetization' %in% colnames(posts_lang_dic[[lang]]))) next  #only analyze languages for which monetization keywords have been specified
  mon_posts_week <- posts_lang_dic[[lang]] %>%
    dplyr::filter(is.na(forwarded_from))%>%
    dplyr::mutate(week = cut(date, "week")) %>%
    group_by(week) %>%
    summarise(n = n(),
              count = sum(monetization, na.rm = T),
              share = mean(monetization, na.rm = T))
  
  # plot monetization by week
  p <- ggplot(mon_posts_week) +
    geom_line(aes(x = as.Date(week), y = count), color = "blue") +
    geom_line(aes(x = as.Date(week), y = share), color = "red") +
    scale_y_continuous(name = "count", sec.axis = sec_axis(~ ., name = "share of total posts in week (%)", labels = percent_format(scale = 100)))+
    ggtitle(paste0('Monetization posts ', lang))+
    xlab('week')
  
  #save plot
  ggsave(filename = paste0('results/monetization/money_posts_week', lang, '.png'))
}


#key channels monetizing (might be interesting to compare to list of channels covering a particular topic)
for(lang in lang_vector){
  if(!('monetization' %in% colnames(posts_lang_dic[[lang]]))) next #only analyze languages for which monetization keywords have been specified
  
  top_monetizing_channels <- posts_lang_dic[[lang]] %>%
    dplyr::filter(is.na(forwarded_from))%>%
    group_by(channel_url) %>%
    summarise(n = n(),
              count = sum(monetization, na.rm = T), #number of posts monetizing by channel
              share = mean(monetization, na.rm = T)) %>% #share of a channel's posts which are monetizing
    dplyr::filter(count !=0) %>% #only keep channels which are monetizing
    dplyr::arrange(desc(count)) #sort in descending order
  
  #save results
  write.csv(top_monetizing_channels, paste0('results/monetization/top_monetizing_channels_', lang, '.csv'))
}
