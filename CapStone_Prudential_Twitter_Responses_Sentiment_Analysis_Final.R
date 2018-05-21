## Web Scrapping using R POC

install.packages("rvest")

install.packages("twitteR")

install.packages("ROAuth")

install.packages("httr")

install.packages("base64enc")

install.packages("devtools")

install.packages("curl")

install.packages("httpuv")

install.packages("plyr")

install.packages("stringr")

install.packages("wordcloud")

install.packages("tm")

install.packages("SnowballC")

install.packages("syuzhet")

install.packages("ggplot2")

install.packages("dplyr")

library("syuzhet")

library("devtools")

library("rvest")

library("twitteR")

library("ROAuth")

library("httr")

library("base64enc")

library("curl")

library("httpuv")

library("plyr")

library("stringr")

library("wordcloud")

library("tm")

library("SnowballC")

library("ggplot2")

library("dplyr")

#set_config(use_proxy(url='proxy.cognizant.com', port = 6050, username = "******", password = "*********"))





# Set the working directory

setwd("D:/Srimatha_BABI/Sri_Capstone_Project/Code")



# Set the API Values

consumer_key<-"ypEQ33XnxtvewuruPXKSULbwV"

consumer_secret<-"HSRkLqyFBrV4wby2hhqZRNouVk3AqamFkGDGwji9l7TZUIhpLa"

access_token<-"634842528-5Ye1XUE4Hf2NcG9yaINuT2ifHlWNk8yXYV2KaGWM"

access_secret<-"trnij2Guxf98BahhpXMrs2hp8p00WNtt7abzhT6QAE4AQ"



setup_twitter_oauth(consumer_key,consumer_secret,access_token=NULL,access_secret=NULL)



# Search & Grab tweets.
# Confine only to English

tweets_at_Prudential <- searchTwitter('@Prudential', n=1500, lang = "en")
#tweets_hash_Prudential <- searchTwitteR('#Prudential', n= 1500, lang = "en")

# Convert to data frame

tweets_at_Prudential_df = twListToDF(tweets_at_Prudential)
#tweets_hash_Prudential_df = twListToDF(tweets_hash_Prudential)
View(tweets_at_Prudential_df)
#View(tweets_hash_Prudential_df)
write.csv(tweets_at_Prudential_df, row.names = TRUE, file = "tweets_Prudential.csv")

## PART - 1

# First Priority is given to tweets that are 
# in response to Prudential
#Sri_tweets = userTimeline("SaiPavanKumarB",n = 100) 
#Sri_tweets_DF = twListToDF(Sri_tweets)
#View(Sri_tweets_DF)

tweets_Prudential_filter <- subset(tweets_at_Prudential_df,replyToSN == "Prudential"|replyToSN == "PruTalent", select = text)
View(tweets_Prudential_filter)
write.csv(tweets_Prudential_filter, row.names = TRUE, file = "tweets_response_to_Prudential.csv")

# Check the total number of tweets
num_of_tweets=count(tweets_Prudential_filter)
str(num_of_tweets)
tweet_final <- data.frame("Tweet_Text" = character(1000), "Emotion" = character(100), "Emotion_Score" = integer(20),stringsAsFactors=FALSE)
#colnames(tweet_final)<- c("text","Emotion","Emotion_Score")
View(tweet_final)
#str(tweet_final)
#View(num_of_tweets)
## Loop through each comment, parse & identify the positivity, negativity of the tweet
i=1
for(i in 1:as.integer(num_of_tweets)){
  # Read each tweet
  print(i)
  tweet_text=""
  tweet_text=tweets_Prudential_filter$text[i]
  # Replace all special characters, punctuation marks
  #View(tweet_text)
  tweets_usableText=gsub("[^0-9A-Za-z]","'" , tweet_text ,ignore.case = TRUE) # Remov special chars
  tweets_usableText=gsub("'"," ",tweets_usableText) # Remove single quotes
  tweets_usableText=gsub("http"," ",tweets_usableText) # Remove https values
  #View(tweets_usableText)
  
  # Create word corpus

  tweets_corpus = Corpus(VectorSource((tweets_usableText)))
  
  # Remove punctuation marks

  tweets_corpus = tm_map(tweets_corpus, removePunctuation)

  # to lower

  tweets_corpus = tm_map(tweets_corpus, content_transformer(tolower))

  # Remove Numbers

  tweets_corpus = tm_map(tweets_corpus, removeNumbers)

  # Remove Spaces

  tweets_corpus = tm_map(tweets_corpus, stripWhitespace)

  # Remove Stop words from English

  tweets_corpus = tm_map(tweets_corpus, removeWords,stopwords('english'))

  # Stem Document

  tweets_corpus = tm_map(tweets_corpus, stemDocument)

  ##let's clean html links

  #tweets_corpus = gsub("http[^[:blank:]]+","",tweets_corpus)

  ##let's remove people names

  #tweets_corpus=gsub("@\\w+","",tweets_corpus)

  # Visualize Word cloud

  wordcloud(tweets_corpus)

  # Create Document Term Matrix

  tweets_dtm = DocumentTermMatrix(VCorpus(VectorSource(tweets_corpus[[1]]$content)))

  tweets_freq = ((as.matrix(tweets_dtm$dimnames$Terms)))

  tweets_freq_df = as.data.frame(tweets_freq)

  #View(tweets_freq)

  #View(tweets_freq_df)

  # Analyze the sentiment

  tweets_sentiment =  get_nrc_sentiment(as.character(tweets_freq_df$V1))

  #View(tweets_sentiment)

  # Sum up the positive & negative sentiments

  senti_anger = sum(tweets_sentiment$anger)

  senti_anticipation = sum(tweets_sentiment$anticipation)

  senti_disgust = sum(tweets_sentiment$disgust)

  senti_fear = sum(tweets_sentiment$fear)

  senti_joy = sum(tweets_sentiment$joy)

  senti_sad = sum(tweets_sentiment$sadness)

  senti_surprise = sum(tweets_sentiment$surprise)

  senti_trust =sum(tweets_sentiment$trust)

  senti_negative = sum(tweets_sentiment$negative)

  senti_positive = sum(tweets_sentiment$positive)

  senti_score_values = c(senti_anger,senti_anticipation,senti_disgust,
                  senti_fear,senti_joy,senti_sad,senti_surprise,
                  senti_trust,senti_negative,senti_positive)
  senti_score_names = c("Anger","Anticipation","Disgust",
                         "Fear","Joy","Sad","Surprise",
                         "Trust","Negative","Positive")
  senti_score = data.frame(senti_score_names,senti_score_values)
  #View(senti_score)
  
  # Select the emotion(s) having value greater than 0
  # Create final data frame with text,sentiment & value
  temp_data<-data.frame()
  
  #View(temp_data)
  if (sum(senti_score_values)>0){
      temp_data <- (cbind((tweet_text),(subset(senti_score,senti_score_values>0, select = c(senti_score_names,senti_score_values)))))
      colnames(temp_data)<-c("Tweet_Text","Emotion","Emotion_Score")
      tweet_final <- rbind(tweet_final,temp_data)
  } else {
    temp_data <- (cbind((tweet_text),"No Emotion",0))
    colnames(temp_data)<-c("Tweet_Text","Emotion","Emotion_Score")
    tweet_final <- rbind((tweet_final),temp_data)
  }
    #View(i)
    #View(tweet_final)
    #View(temp_data)
}
  #View(tweet)  
View(tweet_final2)

# Remove the blank lines & retain only rows with text

tweet_final2 <- subset(tweet_final,Tweet_Text!='')

write.csv(tweet_final2, row.names = TRUE,"tweets_with_emotion.csv")

## PART-2

# Aggregate the emotion scores
tweet_final_score <- as.data.frame(aggregate(as.integer(tweet_final2$Emotion_Score) ~ tweet_final2$Emotion, FUN = sum))
colnames(tweet_final_score)<- c("Emotion","Aggregated Emotion Score")

View(tweet_final_score)
# Plot the bar chart showing the overall emotion of responses

y_axis <- tweet_final_score$`Aggregated Emotion Score`

x_axis <- tweet_final_score$Emotion



colors <- c("tomato1","yellow","maroon3","red4","royalblue3","darkgrey","orangered","green","purple","violet","pink4")



yRange <- range(0,num_of_tweets)

#View(yRange)
par(bg = 'lightgoldenrodyellow')
tweet_graph <- barplot(y_axis, names.arg = x_axis,
        
        xlab = "Sentiment", ylab = "Score", main = "Twitter Sentiment Analysis",  col = colors,
        
        border = "black", ylim = yRange)
text(tweet_graph, y = tweet_final_score$`Aggregated Emotion Score`, labels = tweet_final_score$`Aggregated Emotion Score`, pos = 1, cex = 1, col = "black")


