## Code to get individual tweets & analyze the anger quotient 
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


# Set the working directory

setwd("D:/Srimatha_BABI/Sri_Capstone_Project/Code")

# Set the API Values

consumer_key<-"ypEQ33XnxtvewuruPXKSULbwV"

consumer_secret<-"HSRkLqyFBrV4wby2hhqZRNouVk3AqamFkGDGwji9l7TZUIhpLa"

access_token<-"634842528-5Ye1XUE4Hf2NcG9yaINuT2ifHlWNk8yXYV2KaGWM"

access_secret<-"trnij2Guxf98BahhpXMrs2hp8p00WNtt7abzhT6QAE4AQ"


setup_twitter_oauth(consumer_key,consumer_secret,access_token=NULL,access_secret=NULL)

# Search individual's timeline & get all the tweets
# Confine only to English

Sri_tweets = userTimeline("praveenb4",n = 200) 
Sri_tweets_DF = twListToDF(Sri_tweets)
View(Sri_tweets_DF)

# Write the tweets to a file
write.csv(Sri_tweets_DF, row.names = TRUE,"Individual_tweets.csv")

# Select the tweet text alone

Sri_tweet_text = as.data.frame(Sri_tweets_DF$text)
colnames(Sri_tweet_text)<- c("Tweets")
View(Sri_tweet_text)
str(Sri_tweet_text)

# Check the total number of tweets
num_of_tweets=count(Sri_tweets_DF)
#View(num_of_tweets)


#View(Bagofwords_final)
tweet_final <- data.frame("Tweet_Text" = character(1000), "Emotion" = character(100), "Emotion_Score" = integer(20),stringsAsFactors=FALSE)
# Start the loop
i=1

for(i in 1:as.integer(num_of_tweets)){
  
  # Print each tweet
  print(i)
  tweet_text=""
  tweet_text=Sri_tweets_DF$text[i]
  #View(tweet_text)
  
  # Replace all special characters, punctuation marks
  tweets_usableText=gsub("[^0-9A-Za-z]","'" , tweet_text ,ignore.case = TRUE) # Remov special chars
  tweets_usableText=gsub("'"," ",tweets_usableText) # Remove single quotes
  tweets_usableText=gsub("http","",tweets_usableText)# Removing http
  
  
  tweets_usableText=gsub("https"," ",tweets_usableText) # Remove https values
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
  
  # Visualize Word cloud
  
  #wordcloud(tweets_corpus, scale = c(3,.5),ordered.colors = TRUE)

  # Create Document Term Matrix
  
  tweets_dtm = DocumentTermMatrix(VCorpus(VectorSource(tweets_corpus[[1]]$content)))
  
  tweets_freq = ((as.matrix(tweets_dtm$dimnames$Terms)))
  
  tweets_freq_df = as.data.frame(tweets_freq)
  
  # Analyze the sentiment
  
  tweets_sentiment =  get_nrc_sentiment(as.character(tweets_freq_df$V1))
  
  # Sum up the various emotions
  
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
  
  
  
}

tweet_final2 <- subset(tweet_final,Tweet_Text!='')
# Aggregate the emotion scores
tweet_final_score <- as.data.frame(aggregate(as.integer(tweet_final2$Emotion_Score) ~ tweet_final2$Emotion, FUN = sum))
colnames(tweet_final_score)<- c("Emotion","Aggregated Emotion Score")

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


