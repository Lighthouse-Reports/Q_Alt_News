# Author: Justin-Casimir Braun
# Date: 2023-09-21
# 
# This script analyzes Telegram content for monetization. It includes both separate analyses for amazon, cashapps, ibans, etc.
# and compares these various monetization channels with each other. The script is run on data extracted using the following queries:
# https://euq.bellingcat.com/queries/133, https://euq.bellingcat.com/queries/132, https://euq.bellingcat.com/queries/131, https://euq.bellingcat.com/queries/130,
# https://euq.bellingcat.com/queries/129. These queries shouldn't need to be run again, since the scraping has stopped. Rather
# restrict the sample by filtering using post ids or channel urls as you see fit.

#load packages
require(dplyr)
require(ggplot2)
require(tidyr)
require(httr)
require(jsonlite)
require(stringr)
require(rvest)
require(RSelenium)
require(openxlsx)
require(eurostat)
require(sf)
require(rgeos)
require(geosphere)
require(lubridate)
require(stringi)

#CHECK: set to your working dir
setwd("/Users/justin-casimirbraun/Documents/Lighthouse Reports/Q Europe/Alt Media/analysis")

#load data
iban <- read.csv('data/justin_iban_230918_2023_09_18.csv') %>%
  dplyr::mutate(type = 'iban',
                date = as.Date(post_date, '%d/%m/%y'))
amazon <- read.csv('data/justin_amazon_230918_2023_09_18.csv')%>%
  dplyr::mutate(type = 'amazon',
                date = as.Date(post_date, '%d/%m/%y'))
crowdfunding <- read.csv('data/justin_monetization_crowdfunding_230918_2023_09_18.csv')%>%
  dplyr::mutate(type = 'crowibanunding',
                date = as.Date(post_date, '%d/%m/%y'))
crypto <- read.csv('data/justin_monetization_crypto_230918_2023_09_18.csv')%>%
  dplyr::mutate(type = 'crypto',
                date = as.Date(post_date, '%d/%m/%y'))
cashapps <- read.csv('data/justin_monetization_secondary_cashapps_230918_2023_09_19.csv')%>%
  dplyr::mutate(type = 'cashapps',
                date = as.Date(post_date, '%d/%m/%y'))
paypal <- read.csv('data/justin_monetization_cashapps_paypal1_230918_2023_09_19.csv') %>%
  dplyr::bind_rows(read.csv('data/justin_monetization_cashapps_paypal2_230918_2023_09_19.csv')) %>%
  dplyr::bind_rows(read.csv('data/justin_monetization_cashapps_paypal3_230918_2023_09_19.csv')) %>%
  dplyr::bind_rows(read.csv('data/justin_monetization_cashapps_paypal4_230918_2023_09_19.csv')) %>%
  dplyr::bind_rows(read.csv('data/justin_monetization_cashapps_paypal5_230918_2023_09_19.csv')) %>%
  dplyr::bind_rows(read.csv('data/justin_monetization_cashapps_paypal6_230918_2023_09_19.csv')) %>%
  dplyr::bind_rows(read.csv('data/justin_monetization_cashapps_paypal7_230918_2023_09_19.csv'))%>%
  dplyr::mutate(type = 'paypal',
                date = as.Date(post_date, '%d/%m/%y'))

#bind all separate data frames into a single one
complete <- bind_rows(iban, amazon) %>%
  bind_rows(crowdfunding) %>%
  bind_rows(crypto) %>%
  bind_rows(cashapps) %>%
  bind_rows(paypal)

write.csv(complete, 'results/monetization/complete.csv')
### IBAN ###
#Extract IBANs
iban <- iban %>%
  dplyr::mutate(iban = stringr::str_extract(content, '[A-Z]{2}[0-9]{2}(?:[ ]?[0-9]{4}){4}(?!(?:[ ]?[0-9]){3})(?:[ ]?[0-9]{1,2})?')) %>%
  dplyr::mutate(iban = gsub("\\s", "", iban))

#Scrape IBAN data
#IBAN lookup and merging
# iban_data <- data.frame(bankCode = character(),
#                         name = character(),
#                         zip = character(),
#                         city = character(),
#                         bic = character(),
#                         iban = character())
# ibans <- unique(iban$iban)
# rest_start <- 'https://openiban.com/validate/'
# rest_end <- '?getBIC=true&validateBankCode=true'
# for(cur in ibans){
#   res = GET(paste0(rest_start, cur, rest_end))
#   data = fromJSON(rawToChar(res$content))
#   bank_data <- data.frame(data$bankData)
#   bank_data$iban <- cur
#   iban_data <- dplyr::bind_rows(iban_data, bank_data)
# }
# write.xlsx(iban_data, 'data/iban_data.xlsx', overwrite = F)

#Scraping takes a long time, just import the IBAN dataset, which I includes the scraped data and manual checks
iban_data <- read.xlsx('data/iban_data.xlsx')

#merge IBAN information with raw data
iban <- iban %>%
  dplyr::left_join(iban_data, by = 'iban')
#exclude missing data
iban <- iban %>%
  dplyr::filter(!is.na(iban), !is.na(bic))
#extract bank country in which IBAN is registered
iban$bank_country <- stringr::str_extract(iban$iban, "^.{2}")

#most used IBANs
top_ibans_posts <- iban %>%
  group_by(iban) %>%
  count() %>%
  left_join(iban_data, by = 'iban') %>%
  arrange(desc(n))
write.csv(top_ibans_posts, 'results/monetization/iban_top_posts.csv')

#ibans shared by the largest number of channels
top_iban_channels <- iban %>%
  group_by(iban, channel_url) %>%
  count() %>%
  group_by(iban) %>%
  count() %>%
  left_join(iban_data, by = 'iban') %>%
  arrange(desc(n))
write.csv(top_iban_channels, 'results/monetization/iban_top_channels.csv')

#top channels
top_channels <- iban %>%
  group_by(channel_url) %>%
  count() %>%
  arrange(desc(n))
write.csv(top_channels, 'results/monetization/iban_top_posters.csv')

#channels posting the largest number of ibans
top_channels_n_ibans <- iban %>%
  group_by(iban, channel_url) %>%
  count() %>%
  group_by(channel_url) %>%
  count() %>%
  arrange(desc(n))
write.csv(top_channels_n_ibans, 'results/monetization/iban_highest_number_of_ibans_channel')

#post language by country
top_lang <- iban %>%
  group_by(detected_language) %>%
  count() %>%
  arrange(desc(n))
write.csv(top_lang, 'results/monetization/iban_top_lang.csv')

#bank density by country
shp_eu <- get_eurostat_geospatial(resolution = 10, 
                                  nuts_level = 0, 
                                  year = 2021)
#Bank countries with the largest number of posts
top_bank_country_posts <- iban %>%
  group_by(bank_country) %>%
  count() %>%
  arrange(desc(n))%>%
  right_join(shp_eu, by = c('bank_country' = 'CNTR_CODE'))%>%
  mutate(n = ifelse(is.na(n), 0, n))%>% 
  st_as_sf()

my_breaks_country_posts = c(10, 100, 1000, 10000)
#Display countries by number of posts
ggplot(top_bank_country_posts, aes(fill = n))+
  geom_sf()+
  scale_fill_gradient(name = 'count', trans = 'log', breaks = my_breaks_country_posts, labels = my_breaks_country_posts)+
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void() +
  ggtitle('Bank country by number of posts')
ggsave('results/monetization/iban_top_bank_countries_posts.png', plot = last_plot())

#Bank country by number of accounts
top_bank_country_accounts <- iban %>%
  group_by(iban, bank_country) %>%
  count() %>%
  group_by(bank_country) %>%
  count() %>%
  arrange(desc(n))%>%
  right_join(shp_eu, by = c('bank_country' = 'CNTR_CODE'))%>%
  mutate(n = ifelse(is.na(n), 0, n))%>% 
  st_as_sf()
my_breaks_country_accounts = c(1, 10, 100, 1000)
ggplot(top_bank_country_posts, aes(fill = n))+
  geom_sf()+
  scale_fill_gradient(name = 'count', trans = 'log', breaks = my_breaks_country_accounts, labels = my_breaks_country_accounts)+
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void()+
  ggtitle('Bank country by number of accounts')

ggsave('results/monetization/iban_top_bank_countries_accounts.png', plot = last_plot())

#Most used banks
top_banks <- iban_data %>%
  na.omit() %>%
  group_by(bic, city, zip, bank_name) %>%
  count() %>%
  arrange(desc(n))

write.csv(top_banks, 'results/monetization/iban_top_banks.csv')

#Which bank countries are most popular in which detected language?
country_country <- iban %>%
  group_by(detected_language, bank_country) %>%
  count() %>%
  arrange(desc(n)) %>%
  mutate(detected_language = toupper(detected_language))
write.csv(country_country, 'results/monetization/iban_country_country_connections.csv')

#IBAN use over time
posts_day <- iban %>%
  group_by(date) %>%
  count()

ggplot(posts_day, aes(x = date, y = n))+
  geom_point()+
  ggtitle('Number of IBAN posts per day')

ggsave('results/monetization/iban_day.png', plot = last_plot())

#Number of IBAN posts by month and country
posts_day_country <- iban %>%
  mutate(month = floor_date(date, unit = 'month')) %>%
  group_by(month, bank_country) %>%
  count()

ggplot(posts_day_country, aes(x = month, y = n, color = bank_country))+
  geom_point()+
  ggtitle('Number of IBAN posts by country and month')

ggsave('results/monetization/iban_day_lang.png', plot = last_plot())

###Paypal###

#over time
paypal_posts_day <- paypal %>%
  group_by(date) %>%
  count()
ggplot(paypal_posts_day, aes(x = date, y = n))+
  geom_point()+
  ggtitle('Number of Paypal posts per day')
ggsave('results/monetization/paypal_day.png', plot = last_plot())

#over time and country
paypal_posts_day_country <- paypal %>%
  group_by(date, detected_language) %>%
  count() %>%
  filter(detected_language %in% c('de', 'en', 'fr', 'it', 'nl'))
ggplot(paypal_posts_day_country, aes(x = date, y = n, color = detected_language))+
  geom_point()+
  ggtitle('Number of Paypal posts per day and language')
ggsave('results/monetization/paypal_day_lang.png', plot = last_plot())

#top channels
paypal_top_channels <- paypal %>%
  group_by(channel_url) %>%
  count() %>%
  arrange(desc(n))
write.csv(paypal_top_channels, 'results/monetization/paypal_top_channels.csv')

#top paypal links
paypal$link_url <- stri_extract_all_regex(paypal$links, "(?<=u').*?(?=')") #extract links
paypal_url_df <- as.data.frame(unnest(paypal, cols=link_url)) %>% #unnest the dataframe
  dplyr::filter(!is.na(.$link_url)) %>%
  dplyr::filter(grepl('paypal', link_url))

paypal_top_urls <- paypal_url_df %>%
  group_by(link_url) %>%
  count() %>%
  arrange(desc(n))
write.csv(paypal_top_urls, 'results/monetization/paypal_top_urls.csv')

#Paypal pools
#extract all paypal pools
paypal_pools <- paypal_url_df %>%
  dplyr::filter(grepl('pool', link_url))
#count number of times a given paypal pool was shared
paypal_top_pools <- paypal_pools %>%
  group_by(link_url) %>%
  count() %>%
  arrange(desc(n))
#save csv of Paypal Pools
write.csv(paypal_top_pools, 'results/monetization/paypal_top_pools.csv')

#DON'T OVERWRITE XLSX OF PAYPAL POOLS -- I have manually collected data on most of them
#write.xlsx(paypal_top_pools, 'results/monetization/paypal_pools.xlsx', overwrite = F)


### Amazon ###
#over time
amazon_posts_day <- amazon %>%
  group_by(date) %>%
  count()
ggplot(amazon_posts_day, aes(x = date, y = n))+
  geom_point()+
  ggtitle('Amazon posts per day')
ggsave('results/monetization/amazon_day.png', plot = last_plot())

#over time and country
amazon_posts_day_country <- amazon %>%
  group_by(date, detected_language) %>%
  count() %>%
  filter(detected_language %in% c('de', 'en', 'fr', 'it', 'nl'))
ggplot(amazon_posts_day_country, aes(x = date, y = n, color = detected_language))+
  geom_point()+
  ggtitle('Amazon posts by day and language')
ggsave('results/monetization/amazon_day_lang.png', plot = last_plot())

#top channels
amazon_top_channels <- amazon %>%
  group_by(channel_url) %>%
  count() %>%
  arrange(desc(n))
write.csv(amazon_top_channels, 'results/monetization/amazon_top_channels.csv')

#top amazon links
amazon$link_url <- stri_extract_all_regex(amazon$links, "(?<=u').*?(?=')") #extract links
amazon_url_df <- as.data.frame(unnest(amazon, cols=link_url)) %>% #unnest the dataframe
  dplyr::filter(!is.na(.$link_url)) %>%
  dplyr::filter(grepl('amazon', link_url))

amazon_top_urls <- amazon_url_df %>%
  group_by(link_url) %>%
  count() %>%
  arrange(desc(n))
write.csv(amazon_top_urls, 'results/monetization/amazon_top_urls.csv')

#DON'T OVERWRITE; analyze random sample of amazon urls
# set.seed(12345)
# amazon_random_sample <- amazon_url_df %>%
#   sample_n(50)
# write.xlsx(amazon_random_sample, 'results/monetization/amazon_sample.xlsx', overwrite = F)

### CRYPTO ###
#over time
crypto_posts_day <- crypto %>%
  group_by(date) %>%
  count()
ggplot(crypto_posts_day, aes(x = date, y = n))+
  geom_point()+
  ggtitle('Crypto posts per day')
ggsave('results/monetization/crypto_posts_day.png', plot = last_plot())

#over time and country
crypto_posts_day_country <- crypto %>%
  group_by(date, detected_language) %>%
  count() %>%
  filter(detected_language %in% c('de', 'en', 'fr', 'it', 'nl'))
ggplot(crypto_posts_day_country, aes(x = date, y = n, color = detected_language))+
  geom_point()
ggsave('results/monetization/crypto_posts_day_lang')

#top channels
crypto_top_channels <- crypto %>%
  group_by(channel_url) %>%
  count() %>%
  arrange(desc(n))
write.csv(crypto_top_channels, 'results/monetization/crypto_top_channels.csv')

#set up df where each row corresponds to unique mention of a single wallet
crypto$cryptocurrency_addresses_list <- stri_extract_all_regex(crypto$cryptocurrency_addresses, "(?<=u').*?(?=')") #extract links
crypto_address_df <- as.data.frame(unnest(crypto, cols=cryptocurrency_addresses_list)) %>% #unnest the dataframe
  dplyr::filter(!is.na(.$cryptocurrency_addresses_list))

# scrape total incoming transaction values for each wallet in our dataset. This is really hacky, but I didn't find a good API
# #most common wallets
# crypto_top_wallets <- crypto_address_df %>%
#   group_by(cryptocurrency_addresses_list) %>%
#   rename(address = cryptocurrency_addresses_list) %>%
#   count() %>%
#   arrange(desc(n)) %>%
#   mutate(coin = case_when(str_detect(address, '^0x[a-fA-F0-9]{40}$') ~ 'eth',
#                           str_detect(address, '^(bc1|[13])[a-zA-HJ-NP-Z0-9]{25,39}$') ~ 'btc',
#                           str_detect(address, 'X[1-9A-HJ-NP-Za-km-z]{33}$') ~ 'dash',
#                           str_detect(address, '4[0-9AB][1-9A-HJ-NP-Za-km-z]{93}$') ~ 'xmr'),
#          coin_amount = NA,
#          dollar_amount = NA)
# 
# 
# eth_url <- 'https://www.blockchain.com/explorer/addresses/eth/'
# btc_url <- 'https://www.blockchain.com/explorer/addresses/btc/'
# rD <- rsDriver(browser="firefox", port=4444L, chromever = NULL)
# remDr <- rD[["client"]]
# 
# for(i in 927:nrow(crypto_top_wallets)){
#   address <- crypto_top_wallets[i,'address'][[1]]
#   url <- ifelse(crypto_top_wallets[i,'coin'][[1]]=='btc', paste0(btc_url, address), paste0(eth_url, address))
#   remDr$navigate(url)
#   Sys.sleep(1)
#   crypto_top_wallets[i,'coin_amount'] <- remDr$getPageSource()[[1]] %>% 
#     read_html() %>%
#     html_element(css = '#__next > div.sc-ebbe524c-0.knptKs > div.sc-9bfbc8f4-0.epKoIb > main > div > div > div > div.sc-ab5af294-0.lbiPxT > div > div.sc-553f6f13-0.cdGIwG > div.sc-553f6f13-2.Tzfin > div.sc-553f6f13-1.lcPIKF > div:nth-child(1) > div.sc-92d5245a-2.cWXKHx')%>%
#     html_text()
#   crypto_top_wallets[i, 'dollar_amount'] <- remDr$getPageSource()[[1]] %>% 
#     read_html() %>%
#     html_element(css = '#__next > div.sc-ebbe524c-0.knptKs > div.sc-9bfbc8f4-0.epKoIb > main > div > div > div > div.sc-ab5af294-0.lbiPxT > div > div.sc-553f6f13-0.cdGIwG > div.sc-553f6f13-2.Tzfin > div.sc-553f6f13-1.lcPIKF > div:nth-child(1) > div.sc-92d5245a-4.eacMWo')%>%
#     html_text()
# }
# 
# ###save and close
# remDr$close()
# rD$server$stop()
# 
# crypto_top_wallets <- crypto_top_wallets %>%
#   mutate(dollar_numeric = as.numeric(gsub('[\\$\\,]','',dollar_amount)))
# write.csv(crypto_top_wallets, 'results/monetization/crypto_top_wallets.csv')
crypto_top_wallets <- read.csv('results/monetization/crypto_top_wallets.csv')

#total amount by coin
crypto_coin_dollar <- crypto_top_wallets %>%
  group_by(coin) %>%
  summarise(dollar_sum = sum(dollar_numeric, na.rm = TRUE),
            dollar_mean = mean(dollar_numeric, na.rm = TRUE))
write.csv(crypto_coin_dollar, 'results/monetization/crypto_coin_dollar.csv')

### CROWDFUNDING ###
#over time
crowdfunding_posts_day <- crowdfunding %>%
  group_by(date) %>%
  count()
ggplot(crowdfunding_posts_day, aes(x = date, y = n))+
  geom_point()+
  ggtitle('Crowdfunding posts per day')
ggsave('results/monetization/crowdfunding_posts_day.png', plot = last_plot())

#over time and country
crowdfunding_posts_day_country <- crowdfunding %>%
  group_by(date, detected_language) %>%
  count() %>%
  filter(detected_language %in% c('de', 'en', 'fr', 'it', 'nl'))
ggplot(crowdfunding_posts_day_country, aes(x = date, y = n, color = detected_language))+
  geom_point()+
  ggtitle('crowdfunding posts per day and language')
ggsave('results/monetization/crowdfunding_posts_day_lang.png', plot = last_plot())

#top channels
crowdfunding_top_channels <- crowdfunding %>%
  group_by(channel_url) %>%
  count() %>%
  arrange(desc(n))
write.csv(crowdfunding_top_channels, 'results/monetization/crowdfunding_top_channels.csv')

#top crowdfunding links
crowdfunding$link_url <- stri_extract_all_regex(crowdfunding$links, "(?<=u').*?(?=')") #extract links
crowdfunding_url_df <- as.data.frame(unnest(crowdfunding, cols=link_url)) %>% #unnest the dataframe
  dplyr::filter(!is.na(.$link_url)) %>%
  dplyr::filter(grepl('kickstarter|patreon', link_url))

#extract patreon links
patreon <- crowdfunding_url_df %>%
  filter(grepl('patreon', link_url))
#extract kickstarter links
kickstarter <- crowdfunding_url_df %>%
  filter(grepl('kickstarter', link_url))

#most frequent patreon links
patreon_top <-patreon %>%
  group_by(link_url) %>%
  count() %>%
  arrange(desc(n))
write.csv(patreon_top, 'results/monetization/patreon_top.csv')

#most frequent kickstarter links
kickstarter_top <- kickstarter %>%
  group_by(link_url) %>%
  count() %>%
  arrange(desc(n))

#DON'T OVERWRITE
#write.xlsx(kickstarter_top, 'results/monetization/kickstarter_top.xlsx', overwrite = F)

### COMPARISON BETWEEN SOURCE ###
#number and share of monetization types by date
complete_type <- complete %>%
  mutate(month = floor_date(date, 'month'))%>%
  group_by(type, month) %>%
  count() %>%
  group_by(month) %>%
  mutate(percentage = n/sum(n, na.rm = T)*100)
ggplot(complete_type, aes(x = month, y = n, color = type))+
  geom_point()+
  ggtitle('Number of Monetization posts by type')
ggsave('complete_post_number_type.png', plot = last_plot())

ggplot(complete_type, aes(x = month, y = percentage, color = type))+
  geom_point()+
  ggtitle('Share of Monetization posts by type')
ggsave('complete_post_share_type.png', plot = last_plot())

#number and share of monetization types by month and language
complete_type_lang <- complete %>%
  mutate(month = floor_date(date, 'month'))%>%
  group_by(type, month, detected_language) %>%
  count() %>%
  group_by(month, detected_language) %>%
  mutate(percentage = n/sum(n, na.rm = T)*100) %>%
  filter(detected_language %in% c('de', 'en', 'fr', 'nl', 'it'), month > as.Date('2020-01-01'))

ggplot(complete_type_lang, aes(x = month, y = n, color = type))+
  geom_point()+
  ggtitle('Share of Monetization posts by type') +
  facet_grid(detected_language ~ .)
ggsave('complete_post_number_type_lang.png', plot = last_plot())
ggplot(complete_type_lang, aes(x = month, y = percentage, color = type))+
  geom_point()+
  ggtitle('Share of Monetization posts by type') +
  facet_grid(detected_language ~ .)
ggsave('complete_post_share_type_lang.png', plot = last_plot())
