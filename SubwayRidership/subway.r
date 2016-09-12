df_subway <- read.csv("turnstile_data_master_with_weather.csv")
str(df_subway)

library(ggplot2)

# hourly entries, colored by rain
ggplot(aes(x = Hour, y = ENTRIESn_hourly, color = rain), data = df_subway)+
  geom_point()

#extract day of the week from date
df_subway$DATEn <- as.Date(df_subway$DATEn)
df_subway$dayofweek <- weekdays(df_subway$DATEn)

# hourly entries, colored by day of the week, faceted by rain
ggplot(aes(x = Hour, y = ENTRIESn_hourly, color = dayofweek), data = df_subway)+
  geom_point()+
  facet_wrap(~rain)

# find correlations
df_subset <- df_subway[,c('ENTRIESn_hourly','maxpressurei','maxdewpti','mindewpti', 'meandewpti','minpressurei','meanpressurei', 'meanwindspdi', 'mintempi','maxtempi')]
cor(df_subset, y = df_subset$ENTRIESn_hourly, method = 'spearman')
