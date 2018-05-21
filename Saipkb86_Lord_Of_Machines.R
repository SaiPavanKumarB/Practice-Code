install.packages("splitstackshape")
install.packages("plyr")
install.packages("rvest")
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
library("splitstackshape")
library("plyr")


# Set the working directory
setwd("D:/Sri_DataScience_Practice")

# Load the train data & campaign data

campaign = read.csv("campaign_data.csv")
train = read.csv("train.csv")
test = read.csv("test.csv")


# Verify for any missing values in both data sets
train[is.na(train)==TRUE,]
campaign[is.na(campaign)==TRUE,]
test[is.na(test)==TRUE,]
NROW(train)
#1023191
NROW(test)
#773858

# No missing values in any of the data set

# Combine train & test to create uniform corpus

train_test = rbind(train[,c("id","campaign_id","user_id","send_date")],test)
NROW(train_test)
#1797049

# Split send date column to 2 i.e. date & time part seperate
list_1 = strsplit(as.character(train_test$send_date)," ")
df1 = ldply(list_1)
colnames(df1)= c("send_dt","send_time")

# Append the seperated date time to train data set
train_test = cbind.data.frame(train_test, df1)


# Combine both train & campaign data
train_test_campaign = merge(train_test, campaign, by="campaign_id")
#write.csv(train_campaign, file = "train_campaign.csv")

# Add day of the week using send date column
train_test_campaign$day_of_week = weekdays(as.Date(train_test_campaign$send_dt))

# Split time to office hrs, evening & late evening categories
list_2=strsplit(as.character(df1$send_time),":")
df2 = ldply(list_2)
colnames(df2) = c("Hour","Min")

train_test_campaign = cbind.data.frame(train_test_campaign,df2)

train_test_campaign$send_time_of_day = ifelse(train_test_campaign$Hour<18,"Office Hrs",ifelse(train_test_campaign$Hour< 21,"Evening","Late Evening")) 
View(train_test_campaign)
remove(df1)
remove(df2)
remove(list_1)
remove(list_2)

# Create a new dat set with filtered columns
train_test_campaign_new = train_test_campaign[,c("campaign_id","communication_type","total_links","no_of_internal_links","no_of_images","no_of_sections","email_body","subject","email_url","day_of_week","send_time_of_day")]

train_test_campaign_new$day_of_week= as.factor(train_test_campaign_new$send_time_of_day)
train_test_campaign_new$send_time_of_day= as.factor(train_test_campaign_new$send_time_of_day)


# Dividing the problem to 2 parts
# PART -1 . Mails are opened based on the subject
# Prepare DTM from the subject text to understand words that encourage the user
# to open the mail



 # Create word corpus
 subjectcorpus = Corpus(VectorSource(train_test_campaign_new$subject))

 # Remove punctuation marks
 
 subjectcorpus = tm_map(subjectcorpus, removePunctuation)
 
 # to lower
 
 subjectcorpus = tm_map(subjectcorpus, content_transformer(tolower))
 
 # Remove Numbers
 
 subjectcorpus = tm_map(subjectcorpus, removeNumbers)
 
 # Remove Spaces
 
 subjectcorpus = tm_map(subjectcorpus, stripWhitespace)
 
 # Remove Stop words from English
 
 subjectcorpus = tm_map(subjectcorpus, removeWords,stopwords('english'))
 
 # Stem Document
 
 subjectcorpus = tm_map(subjectcorpus, stemDocument)
 
 # Visualize Word cloud
 
 wordcloud(subjectcorpus)
 
 # Prepare DTM
 subject_dtm = DocumentTermMatrix(subjectcorpus)
 
  # Check & remove sparse terms
 #findFreqTerms(Alcohol_DTM, lowfreq = 2)
 subject_removesparse = removeSparseTerms(subject_dtm,0.95)

 # Convert to final data frame
 subject_DTM_DF = as.data.frame(as.matrix(subject_removesparse))
 View(subject_DTM_DF)

 # Split to training & test data set now
 train_test_campaign_new2 = cbind(train_test_campaign_new,subject_DTM_DF)

 remove(subjectcorpus) 
 remove(subject_dtm)
 remove(subject_removesparse)
 

Final_train = train_test_campaign_new2[1:1023191,]
Final_test = train_test_campaign_new2[1023192:1797049,]

# Add target variable is_open
 Final_train_1 = cbind(Final_train, train$is_open)
 
 names(Final_train_1)[names(Final_train_1)=='train$is_open'] <- 'is_open'
 
 # Perform chi square test
 chisq.test(table(Final_train_1$communication_type,Final_train_1$is_open))
  # p value greater than 0.5 insignificant
 
 chisq.test(table(Final_train_1$day_of_week,Final_train_1$is_open))
 # p value less than 0.5 significant
 
 chisq.test(table(Final_train_1$send_time_of_day,Final_train_1$is_open))
 # p value less than 0.5 significant
 
# Prepare final data set omitting insignificant variables
 
 Final_train_data_temp = Final_train_1[,!names(Final_train_1) %in% c("campaign_id","communication_type","total_links","no_of_internal_links","no_of_images","no_of_sections","email_body","subject","email_url")]

 remove(Final_train_1)
# Have few rows for accuracy testing purpose
  Final_train_2 = Final_train_data_temp[1:1020000,]
 
 
 dummy_test = Final_train_data_temp[1020001:1023191,]
 str(Final_train_1)

 # Create model to predict open mails
 open_LR = glm(is_open ~ ., Final_train_data_temp, family = "binomial", control = list(maxit = 100))
 
 Final_train_data_temp$open_pred = fitted.values(open_LR,type = "response")

 Final_train_data_temp$open_pred[Final_train_data_temp$open_pred<=0.065]=0
 Final_train_data_temp$open_pred[Final_train_data_temp$open_pred>0.065]=1

 library('caret') 
 library('ROCR')
 install.packages('ROCR')
 confusionMatrix(Final_train_data_temp$is_open,Final_train_data_temp$open_pred)
 
 # Work on the test
 Final_test$is_open = predict(open_LR,Final_test,type = "response")

 Final_test$is_open[Final_test$is_open<=0.065]=0
 Final_test$is_open[Final_test$is_open>0.065]=1
 
 test$is_open = Final_test$is_open
View(test) 

# Working on is click
# Click will work only when mails are opened. Selecting opened mails

train_campaign = merge(train, campaign, by = "campaign_id")

summary(train_campaign$total_links)
# Convert total links to categorical variable
train_campaign$total_links= ifelse(train_campaign$total_links<=24,1,ifelse(train_campaign$total_links<=67,2,ifelse(train_campaign$total_links<104,3,4)))

hist(train_campaign$no_of_internal_links)
summary(train_campaign$no_of_internal_links)

train_campaign$no_of_internal_links = ifelse(train_campaign$no_of_internal_links<19,1,ifelse(train_campaign$no_of_internal_links<61,2,ifelse(train_campaign$no_of_internal_links<100,3,4)))

hist(train_campaign$no_of_images)
summary(train_campaign$no_of_images)

train_campaign$no_of_images=ifelse(train_campaign$no_of_images<7,1,ifelse(train_campaign$no_of_images<12,2,ifelse(train_campaign$no_of_images<10,3,4)))

hist(train_campaign$no_of_sections)
summary(train_campaign$no_of_sections)

train_campaign$no_of_sections=as.factor(train_campaign$no_of_sections)
train_campaign$total_links=as.factor(train_campaign$total_links)
train_campaign$no_of_internal_links=as.factor(train_campaign$no_of_internal_links)
train_campaign$no_of_images=as.factor(train_campaign$no_of_images)

list_1 = strsplit(as.character(train_campaign$send_date)," ")
df1 = ldply(list_1)
colnames(df1)= c("send_dt","send_time")

# Append the seperated date time to train data set
train_campaign = cbind.data.frame(train_campaign, df1)

# Add day of the week using send date column
train_campaign$day_of_week = weekdays(as.Date(train_campaign$send_dt))

# Split time to office hrs, evening & late evening categories
list_2=strsplit(as.character(df1$send_time),":")
df2 = ldply(list_2)
colnames(df2) = c("Hour","Min")

train_campaign = cbind.data.frame(train_campaign,df2)

train_campaign$send_time_of_day = ifelse(train_campaign$Hour<18,"Office Hrs",ifelse(train_campaign$Hour< 21,"Evening","Late Evening")) 

chisq.test(table(train_campaign$day_of_week,train_campaign$is_click))
chisq.test(table(train_campaign$send_time_of_day,train_campaign$is_click))
chisq.test(table(train_campaign$is_open,train_campaign$is_click))
chisq.test(table(train_campaign$communication_type,train_campaign$is_click))
chisq.test(table(train_campaign$total_links,train_campaign$is_click))
chisq.test(table(train_campaign$no_of_internal_links,train_campaign$is_click))
chisq.test(table(train_campaign$no_of_images,train_campaign$is_click))
chisq.test(table(train_campaign$no_of_sections,train_campaign$is_click))


# Prepare final data set of modelling
 click_LR = glm(is_click ~ day_of_week+send_time_of_day+is_open+communication_type+total_links+no_of_internal_links+no_of_images+no_of_sections, train_campaign, family = "binomial", control = list(maxit = 100))

 train_campaign$click_pred = fitted.values(click_LR,type = "response")

 train_campaign$click_pred[train_campaign$click_pred<0.0001]=0
 train_campaign$click_pred[train_campaign$click_pred>=0.0001]=1

 confusionMatrix(train_campaign$is_click,train_campaign$click_pred)
 
 # 91 accuracy
 
 # Work on the test
 
 test_campaign = merge(test,campaign, by = "campaign_id")
 
 summary(test_campaign$total_links)
 # Convert total links to categorical variable
 test_campaign$total_links= ifelse(test_campaign$total_links<=24,1,ifelse(test_campaign$total_links<=67,2,ifelse(test_campaign$total_links<104,3,4)))
 
 hist(test_campaign$no_of_internal_links)
 summary(test_campaign$no_of_internal_links)
 
 test_campaign$no_of_internal_links = ifelse(test_campaign$no_of_internal_links<19,1,ifelse(test_campaign$no_of_internal_links<61,2,ifelse(test_campaign$no_of_internal_links<100,3,4)))
 
 hist(test_campaign$no_of_images)
 summary(test_campaign$no_of_images)
 
 test_campaign$no_of_images=ifelse(test_campaign$no_of_images<7,1,ifelse(test_campaign$no_of_images<12,2,ifelse(test_campaign$no_of_images<10,3,4)))
 
 hist(test_campaign$no_of_sections)
 summary(train_campaign$no_of_sections)
 
 test_campaign$no_of_sections[test_campaign$no_of_sections==2]=1
 test_campaign$no_of_sections[test_campaign$no_of_sections==5]=6
 test_campaign$no_of_sections=as.factor(test_campaign$no_of_sections)
 test_campaign$total_links=as.factor(test_campaign$total_links)
 test_campaign$no_of_internal_links=as.factor(test_campaign$no_of_internal_links)
 test_campaign$no_of_images=as.factor(test_campaign$no_of_images)
 
 list_1 = strsplit(as.character(test_campaign$send_date)," ")
 df1 = ldply(list_1)
 colnames(df1)= c("send_dt","send_time")
 
 # Append the seperated date time to train data set
 test_campaign = cbind.data.frame(test_campaign, df1)
 
 # Add day of the week using send date column
 test_campaign$day_of_week = weekdays(as.Date(test_campaign$send_dt))
 
 # Split time to office hrs, evening & late evening categories
 list_2=strsplit(as.character(df1$send_time),":")
 df2 = ldply(list_2)
 colnames(df2) = c("Hour","Min")
 
 test_campaign = cbind.data.frame(test_campaign,df2)
 
 test_campaign$send_time_of_day = ifelse(test_campaign$Hour<18,"Office Hrs",ifelse(test_campaign$Hour< 21,"Evening","Late Evening")) 
 
 
 test_campaign$click_pred = predict(click_LR, test_campaign, type = "response")
 
 test_campaign$click_pred[test_campaign$click_pred<=0.065]=0
 test_campaign$click_pred[test_campaign$click_pred>0.065]=1
 
 test$is_click = test_campaign$click_pred
 
 Final_submission = test[,c("id","is_click")]

 View(test) 
 
 write.csv(Final_submission,file = "Final_submission.csv")
 