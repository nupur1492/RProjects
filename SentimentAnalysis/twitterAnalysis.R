# reference: https://mkmanu.wordpress.com/2014/08/05/sentiment-analysis-on-twitter-data-text-analytics-tutorial/

library(httr)
library(devtools)
library(twitteR)
library(ggplot2)
library(base64enc)
library(Rstem)
library(sentiment)
library(tm)
library(wordcloud)


api_key <- ""
api_secret <- ""
access_token <- ""
access_token_secret <- ""
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


#devtools::install_version("httr", version="0.6.0", repos="http://cran.us.r-project.org")
#install_github("twitteR", username="geoffjentry")

## Alternative method to access API

#oauth_endpoints("twitter")
#set_config( config( ssl_verifypeer = 0L ) )
#myapp <- oauth_app("twitter",
#                  key = "",
#                 secret = "")

# Get OAuth credentials
#twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)

# Use API
#req <- GET("https://api.twitter.com/1.1/statuses/home_timeline.json",config(token = twitter_token))
#stop_for_status(req)
#content(req)

#search for tweets
tweets = searchTwitter("brexit", n=2000, lang="en")
#replace brexit with any keyword of choice

tweets_txt = sapply(tweets, function(x) x$getText())

# remove retweet entities from the stored tweets (text)
tweets_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets_txt)

#remove "@people"
tweets_txt = gsub("@\\w+", "",tweets_txt)

#remove punctuations
tweets_txt = gsub("[[:punct:]]","", tweets_txt)

#remove numbers
tweets_txt = gsub("[[:digit:]]","",tweets_txt)

#remove html links
tweets_txt = gsub("http\\w+","",tweets_txt)

#remove blank spaces
tweets_txt = gsub("[ \t]{2,}","",tweets_txt)
tweets_txt = gsub("^\\s+|\\s+$","",tweets_txt)


#convert all letters to lower case
catch.error = function(x){
  # let us create a missing value for test purpose
  y = NA
  
  # try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  
  # check result if error exists, otherwise the function works fine.
  return(y)
}

tweets_txt = sapply(tweets_txt,catch.error)

#remove NAs
tweets_txt = tweets_txt[!is.na(tweets_txt)]

#remove column headings
names(tweets_txt) = NULL

keyword_emotions = classify_emotion(tweets_txt,algorithm = "bayes", prior=1.0)

#remove unknowns
emotion = keyword_emotions[,7]
emotion[is.na(emotion)] = NULL

keyword_polarity = classify_polarity(tweets_txt,algorithm = "bayes")

polarity = keyword_polarity[,4]       #1: positive sentiment; 2: negative sentiment; 3; pos/neg ratio; 4; best-fit

#create data frame with all values
df_sentiment = data.frame(text=tweets_txt, emotion = emotion, polarity = polarity, stringsAsFactors = FALSE)

#rearrange dataframe by sorting
df_sentiment = within(df_sentiment, 
                      emotion <- factor(emotion, 
                                    levels=names(sort(table(emotion), 
                                                      decreasing=TRUE))))

#remove duplicate entries
df_sentiment <- df_sentiment[!duplicated(df_sentiment$text),]

#plot emotions
ggplot(df_sentiment, aes(x=emotion))+
  geom_bar(aes(y=..count..,fill=emotion))+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle('Sentiment analysis')+
  theme(legend.position = 'right')+
  ylab('Number of tweets')+
  xlab('Emotions')

#plot polarity
ggplot(df_sentiment, aes(x=polarity))+
  geom_bar(aes(y=..count..,fill=polarity))+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle('Sentiment analysis')+
  theme(legend.position = 'right')+
  ylab('Number of tweets')+
  xlab('Polarities')

#word cloud
all_emotions = levels(factor(df_sentiment$emotion))
len_emotions = length(all_emotions)

allemos.docs = rep("",len_emotions)


for(i in 1:len_emotions){
  tmp = tweets_txt[emotion == all_emotions[i]]
  allemos.docs[i] = paste(tmp,collapse = " ")
}

#remove slangs/ other words
#allemos.docs = removeWords(allemos.docs,stopwords("zombie"))

keyword.corpus = Corpus(VectorSource(allemos.docs))
keyword.tdm = TermDocumentMatrix(keyword.corpus)
keyword.tdm = as.matrix(keyword.tdm)
colnames(keyword.tdm) = all_emotions

# creating, comparing and plotting the words on the cloud
comparison.cloud(keyword.tdm, colors = brewer.pal(len_emotions, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)
