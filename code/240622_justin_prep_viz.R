library(dplyr)
library(lubridate)

setwd("/Users/justin-casimirbraun/GitHub/Q_Alt_News")

output_fp <- 'results/viz/'
dir.create(output_fp, showWarnings = F)

fig1_raw <- read.csv('data/major_topics.csv')
fig1_ready <- fig1_raw %>%
  mutate(date = as.Date(date1, format = '%d/%m/%y'),
         month = lubridate::floor_date(date, 'month')) %>%
  group_by(month) %>%
  summarise(total_count = sum(total_count, na.rm = T),
            covid_count = sum(covid_count, na.rm = T),
            climate_count = sum(climate_count, na.rm = T),
            ukraine_count = sum(ukraine_count, na.rm = T)) %>%
  filter(month > as.Date('2019-12-31'))

write.csv(fig1_ready, paste0(output_fp, 'fig1_topic_entire_database.csv'))  

fig2_raw <- read.csv('results/spread/posts_lang_day_topic.csv')

fig2_ready <- fig2_raw %>%
  mutate(date = as.Date(date),
         detected_language = case_when(detected_language == 'de' ~ 'German',
                                       detected_language == 'en' ~ 'English',
                                       detected_language == 'fr' ~ 'French',
                                       detected_language == 'nl' ~ 'Dutch',
                                       detected_language == 'it' ~ 'Italian',
                                       .default = NA),
         topic = case_when(topic == 'ulez' ~ 'ULEZ',
                           topic == 'insect' ~ 'Insect Food',
                           topic == 'climate_lockdown' ~ 'Climate Lockdown',
                           topic == 'fifteen_minutes' ~ 'Fifteen Minute Cities',
                           .default = NA)) %>%
  filter(date > as.Date('2019-12-31'))

write.csv(fig2_ready, paste0(output_fp, 'fig2_day_topic_lang.csv'))


fig3_raw <- read.csv('data/231015_raw_data_with_topics.csv') #CHECK: specify dataset location

fig3_ready <- fig3_raw %>%
  group_by(channel_url, followers) %>%
  summarise(sum_views = sum(views, na.rm = TRUE),
            mean_views = mean(views, na.rm = TRUE),
            sum_forwards = sum(forwards, na.rm = TRUE),
            mean_forwards = mean(forwards, na.rm = TRUE)) %>%
  pivot_longer(cols = c('sum_views', 'mean_views', 'sum_forwards', 'mean_forwards'), names_to = 'metric') %>%
  mutate(metric = case_when(metric == 'sum_views' ~ 'Total Views',
                            metric == 'mean_views' ~ 'Mean Views',
                            metric == 'sum_forwards' ~ 'Total Forwards',
                            metric == 'mean_forwards' ~ 'Mean Fowards',
                            .default = NA))

write.csv(fig3_ready, paste0(output_fp, 'fig3_followers_views_forwards.csv'))


fig4_raw <- read.csv('results/monetization_month_type_lang.csv')

fig4_ready <- fig4_raw %>%
  filter(type != 'cashapps') %>%
  mutate(month = as.Date(month)) %>%
  filter(detected_language %in% c('de', 'fr', 'en', 'it', 'nl'), month > as.Date('2019-12-31')) %>%
  mutate(type = case_when(type == 'amazon' ~ 'Amazon',
                          type == 'crowibanunding' ~ 'Crowdfunding',
                          type == 'crypto' ~ 'Crypto currencies',
                          type == 'iban' ~ 'IBAN',
                          type == 'paypal' ~ 'PayPal',
                          .default = NA),
         detected_language = case_when(detected_language == 'de' ~ 'German',
                                       detected_language == 'en' ~ 'English',
                                       detected_language == 'fr' ~ 'French',
                                       detected_language == 'nl' ~ 'Dutch',
                                       detected_language == 'it' ~ 'Italian',
                                       .default = NA))

write.csv(fig4_ready, paste0(output_fp, 'fig4_monetization_month_strategy.csv'))
