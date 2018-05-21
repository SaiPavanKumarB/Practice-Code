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
install.packages("caret")
library("caret")

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

Alcohol_tweets = userTimeline("alcohol_tweets",n = 500) 
Alcohol_tweets_DF = twListToDF(Alcohol_tweets)
View(Alcohol_tweets_DF)

# Set working direcotry

setwd("D:/Srimatha_BABI/Sri_Capstone_Project/Code")

# Write the tweets to a file
write.csv(Alcohol_tweets_DF$text, row.names = TRUE,"Alcoholic_tweets.csv")

# Split the data to train & test
# Creating corpus to train the model



# Read the train data set & test data set 
# Combine both
alcohol_train <- read.csv('Alcoholic_tweets_train.csv')# 400 rows
alcohol_test <- read.csv('Alcohol_tweets_test.csv') # 101 rows

train_size = nrow(alcohol_train)
test_size = nrow(alcohol_test)


# Create a full data set
alcohol_full <- rbind(alcohol_train,alcohol_test)
View(alcohol_full)

# Now, train records will have sentiment value, where as test records will have
# sentiment values as NA


# Remove all special characters before creating corpus
alcohol_full$Text = gsub("[^0-9A-Za-z]","'" , alcohol_full$Text
                          ,ignore.case = TRUE)

# Remove single quotes
alcohol_full$Text = gsub("'"," ",alcohol_full$Text) # Remove single quotes

# Remove http values
alcohol_full$Text = gsub("http","",alcohol_full$Text)

#View(alcohol_train)

# Start with actual process

# Step-1 Creating word corpus
Alcohol_corpus = Corpus(VectorSource(alcohol_full$Text))
View(Alcohol_corpus)

# Step-2 Convert words to lower
Alcohol_corpus = tm_map(Alcohol_corpus, tolower)

# Step-3 Remove punctuation
Alcohol_corpus = tm_map(Alcohol_corpus,removePunctuation)

# Step-4 Remove Numbers
Alcohol_corpus = tm_map(Alcohol_corpus,removeNumbers)

# Step-5 Remove Space
Alcohol_corpus = tm_map(Alcohol_corpus, stripWhitespace)

# Step-6 Remove Stop Words
Alcohol_corpus = tm_map(Alcohol_corpus, removeWords, stopwords('english'))

# Step-7 Prepare stem document
Alcohol_corpus = tm_map(Alcohol_corpus, stemDocument)
#View(Alcohol_corpus)
# Step-8 Prepare DocumentTermMatrix
Alcohol_DTM =  DocumentTermMatrix(Alcohol_corpus)

# Check & remove sparse terms
findFreqTerms(Alcohol_DTM, lowfreq = 2)
Alcohol_RemoveSparse = removeSparseTerms(Alcohol_DTM,0.99)
View(Alcohol_RemoveSparse)

# Convert to Data Frame
Alcohol_DTM_DF = as.data.frame(as.matrix(Alcohol_RemoveSparse))
Alcohol_DTM_DF$Sentiment  = alcohol_full$Sentiment
#Alcohol_DTM_DF$Tweet_text = alcohol_full$Text
#View(Alcohol_DTM_DF)

# Now split the DTM into test & train
Alcohol_DTM_DF_train = Alcohol_DTM_DF[1:train_size,]
Alcohol_DTM_DF_test = Alcohol_DTM_DF[(train_size+1):(train_size+test_size),]


# Build Logistic Regression model to predict 


Alcohol_Model_LR = glm(Sentiment ~ ., Alcohol_DTM_DF_train, family = "binomial", control = list(maxit = 100))

Alcohol_DTM_DF_train$Pred_Sent = fitted.values(Alcohol_Model_LR, type = "response")
Alcohol_DTM_DF_train$Pred_Sent[Alcohol_DTM_DF_train$Pred_Sent<=0.3] = 0
Alcohol_DTM_DF_train$Pred_Sent[Alcohol_DTM_DF_train$Pred_Sent>0.3]=1
Alcohol_DTM_DF_train$Tweet = alcohol_train$Text
#View(Alcohol_DTM_DF_train)

install.packages("e1071")
library('e1071')
confusionMatrix(Alcohol_DTM_DF_train$Sentiment,Alcohol_DTM_DF_train$Pred_Sent)

##   Reference
##  Prediction   0     1
##            0 167   31
##            1   5  197

##  Accuracy : 0.91        



########################################################################
########################################################################
####               TEST THE DATA                                    ####
########################################################################
########################################################################

# Get the test data


Alcohol_DTM_DF_test$Pred_Sent = predict(Alcohol_Model_LR,Alcohol_DTM_DF_test, type = "response")
View(Alcohol_DTM_DF_test)

ifelse(Alcohol_DTM_DF_test$Pred_Sent>=0.3,1,0) -> Alcohol_DTM_DF_test$Sentiment
# Below statements to be enabled only when we split the data
# to test & train
Alcohol_DTM_DF_test$Tweet = alcohol_test$Text
View(c(Alcohol_DTM_DF_test$Tweet,Alcohol_DTM_DF_test$Sentiment))
View(Alcohol_DTM_DF_test)


write.csv(Alcohol_DTM_DF_test, row.names = TRUE,"Alcohol_Test_Final.csv")

# As 1 indicates that the person has consumed alcohol at some point
# We'll take count, Percentage of alcoholic tweets w.r.t. overall tweets
Total_tweets = nrow(Alcohol_DTM_DF_test)
Alcohol_tweet_Cnt = count(subset(Alcohol_DTM_DF_test,Alcohol_DTM_DF_test$Sentiment==1,select = Sentiment))
Total_tweets
Tweet_Percentage = round((Alcohol_tweet_Cnt/Total_tweets)*100)
View(Tweet_Percentage)

