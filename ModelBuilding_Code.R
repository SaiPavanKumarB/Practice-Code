setwd("E:/Pavan/GL/capstone/modelBuildingFinal")

# Prepare environment

rm(list = ls())
setwd("../Capstone")
install.packages("readxl")
install.packages("caTools")
install.packages("randomForest")

# Load clean data

library(readxl)
dfIns <- read_xlsx("ins_data.xlsx")
#View(dfIns)
anyNA(dfIns)
#str(dfIns)

# Convert categorical and discrete columns into factors

dfIns$Product_Info_1 = as.factor(dfIns$Product_Info_1)
dfIns$Product_Info_2 = as.factor(dfIns$Product_Info_2)
dfIns$Product_Info_3 = as.factor(dfIns$Product_Info_3)
dfIns$Product_Info_5 = as.factor(dfIns$Product_Info_5)
dfIns$Product_Info_6 = as.factor(dfIns$Product_Info_6)
dfIns$Product_Info_7 = as.factor(dfIns$Product_Info_7)
dfIns$Employment_Info_2 = as.factor(dfIns$Employment_Info_2)
dfIns$Employment_Info_3 = as.factor(dfIns$Employment_Info_3)
dfIns$Employment_Info_5 = as.factor(dfIns$Employment_Info_5)
dfIns$InsuredInfo_1 = as.factor(dfIns$InsuredInfo_1)
dfIns$InsuredInfo_2 = as.factor(dfIns$InsuredInfo_2)
dfIns$InsuredInfo_3 = as.factor(dfIns$InsuredInfo_3)
dfIns$InsuredInfo_4 = as.factor(dfIns$InsuredInfo_4)
dfIns$InsuredInfo_5 = as.factor(dfIns$InsuredInfo_5)
dfIns$InsuredInfo_6 = as.factor(dfIns$InsuredInfo_6)
dfIns$InsuredInfo_7 = as.factor(dfIns$InsuredInfo_7)
dfIns$Insurance_History_1 = as.factor(dfIns$Insurance_History_1)
dfIns$Insurance_History_2 = as.factor(dfIns$Insurance_History_2)
dfIns$Insurance_History_3 = as.factor(dfIns$Insurance_History_3)
dfIns$Insurance_History_4 = as.factor(dfIns$Insurance_History_4)
dfIns$Insurance_History_7 = as.factor(dfIns$Insurance_History_7)
dfIns$Insurance_History_8 = as.factor(dfIns$Insurance_History_8)
dfIns$Insurance_History_9 = as.factor(dfIns$Insurance_History_9)
dfIns$Family_Hist_1 = as.factor(dfIns$Family_Hist_1)
dfIns$Medical_History_2 = as.factor(dfIns$Medical_History_2)
dfIns$Medical_History_3 = as.factor(dfIns$Medical_History_3)
dfIns$Medical_History_4 = as.factor(dfIns$Medical_History_4)
dfIns$Medical_History_5 = as.factor(dfIns$Medical_History_5)
dfIns$Medical_History_6 = as.factor(dfIns$Medical_History_6)
dfIns$Medical_History_7 = as.factor(dfIns$Medical_History_7)
dfIns$Medical_History_8 = as.factor(dfIns$Medical_History_8)
dfIns$Medical_History_9 = as.factor(dfIns$Medical_History_9)
dfIns$Medical_History_11 = as.factor(dfIns$Medical_History_11)
dfIns$Medical_History_12 = as.factor(dfIns$Medical_History_12)
dfIns$Medical_History_13 = as.factor(dfIns$Medical_History_13)
dfIns$Medical_History_14 = as.factor(dfIns$Medical_History_14)
dfIns$Medical_History_16 = as.factor(dfIns$Medical_History_16)
dfIns$Medical_History_17 = as.factor(dfIns$Medical_History_17)
dfIns$Medical_History_18 = as.factor(dfIns$Medical_History_18)
dfIns$Medical_History_19 = as.factor(dfIns$Medical_History_19)
dfIns$Medical_History_20 = as.factor(dfIns$Medical_History_20)
dfIns$Medical_History_21 = as.factor(dfIns$Medical_History_21)
dfIns$Medical_History_22 = as.factor(dfIns$Medical_History_22)
dfIns$Medical_History_23 = as.factor(dfIns$Medical_History_23)
dfIns$Medical_History_25 = as.factor(dfIns$Medical_History_25)
dfIns$Medical_History_26 = as.factor(dfIns$Medical_History_26)
dfIns$Medical_History_27 = as.factor(dfIns$Medical_History_27)
dfIns$Medical_History_28 = as.factor(dfIns$Medical_History_28)
dfIns$Medical_History_29 = as.factor(dfIns$Medical_History_29)
dfIns$Medical_History_30 = as.factor(dfIns$Medical_History_30)
dfIns$Medical_History_31 = as.factor(dfIns$Medical_History_31)
dfIns$Medical_History_33 = as.factor(dfIns$Medical_History_33)
dfIns$Medical_History_34 = as.factor(dfIns$Medical_History_34)
dfIns$Medical_History_35 = as.factor(dfIns$Medical_History_35)
dfIns$Medical_History_36 = as.factor(dfIns$Medical_History_36)
dfIns$Medical_History_37 = as.factor(dfIns$Medical_History_37)
dfIns$Medical_History_38 = as.factor(dfIns$Medical_History_38)
dfIns$Medical_History_39 = as.factor(dfIns$Medical_History_39)
dfIns$Medical_History_40 = as.factor(dfIns$Medical_History_40)
dfIns$Medical_History_41 = as.factor(dfIns$Medical_History_41)
dfIns$Medical_History_1  = as.factor(dfIns$Medical_History_1)
dfIns$Medical_Keyword_1 = as.factor(dfIns$Medical_Keyword_1)
dfIns$Medical_Keyword_2 = as.factor(dfIns$Medical_Keyword_2)
dfIns$Medical_Keyword_3 = as.factor(dfIns$Medical_Keyword_3)
dfIns$Medical_Keyword_4 = as.factor(dfIns$Medical_Keyword_4)
dfIns$Medical_Keyword_5 = as.factor(dfIns$Medical_Keyword_5)
dfIns$Medical_Keyword_6 = as.factor(dfIns$Medical_Keyword_6)
dfIns$Medical_Keyword_7 = as.factor(dfIns$Medical_Keyword_7)
dfIns$Medical_Keyword_8 = as.factor(dfIns$Medical_Keyword_8)
dfIns$Medical_Keyword_9 = as.factor(dfIns$Medical_Keyword_9)
dfIns$Medical_Keyword_10 = as.factor(dfIns$Medical_Keyword_10)
dfIns$Medical_Keyword_11 = as.factor(dfIns$Medical_Keyword_11)
dfIns$Medical_Keyword_12 = as.factor(dfIns$Medical_Keyword_12)
dfIns$Medical_Keyword_13 = as.factor(dfIns$Medical_Keyword_13)
dfIns$Medical_Keyword_14 = as.factor(dfIns$Medical_Keyword_14)
dfIns$Medical_Keyword_15 = as.factor(dfIns$Medical_Keyword_15)
dfIns$Medical_Keyword_16 = as.factor(dfIns$Medical_Keyword_16)
dfIns$Medical_Keyword_17 = as.factor(dfIns$Medical_Keyword_17)
dfIns$Medical_Keyword_18 = as.factor(dfIns$Medical_Keyword_18)
dfIns$Medical_Keyword_19 = as.factor(dfIns$Medical_Keyword_19)
dfIns$Medical_Keyword_20 = as.factor(dfIns$Medical_Keyword_20)
dfIns$Medical_Keyword_21 = as.factor(dfIns$Medical_Keyword_21)
dfIns$Medical_Keyword_22 = as.factor(dfIns$Medical_Keyword_22)
dfIns$Medical_Keyword_23 = as.factor(dfIns$Medical_Keyword_23)
dfIns$Medical_Keyword_24 = as.factor(dfIns$Medical_Keyword_24)
dfIns$Medical_Keyword_25 = as.factor(dfIns$Medical_Keyword_25)
dfIns$Medical_Keyword_26 = as.factor(dfIns$Medical_Keyword_26)
dfIns$Medical_Keyword_27 = as.factor(dfIns$Medical_Keyword_27)
dfIns$Medical_Keyword_28 = as.factor(dfIns$Medical_Keyword_28)
dfIns$Medical_Keyword_29 = as.factor(dfIns$Medical_Keyword_29)
dfIns$Medical_Keyword_30 = as.factor(dfIns$Medical_Keyword_30)
dfIns$Medical_Keyword_31 = as.factor(dfIns$Medical_Keyword_31)
dfIns$Medical_Keyword_32 = as.factor(dfIns$Medical_Keyword_32)
dfIns$Medical_Keyword_33 = as.factor(dfIns$Medical_Keyword_33)
dfIns$Medical_Keyword_34 = as.factor(dfIns$Medical_Keyword_34)
dfIns$Medical_Keyword_35 = as.factor(dfIns$Medical_Keyword_35)
dfIns$Medical_Keyword_36 = as.factor(dfIns$Medical_Keyword_36)
dfIns$Medical_Keyword_37 = as.factor(dfIns$Medical_Keyword_37)
dfIns$Medical_Keyword_38 = as.factor(dfIns$Medical_Keyword_38)
dfIns$Medical_Keyword_39 = as.factor(dfIns$Medical_Keyword_39)
dfIns$Medical_Keyword_40 = as.factor(dfIns$Medical_Keyword_40)
dfIns$Medical_Keyword_41 = as.factor(dfIns$Medical_Keyword_41)
dfIns$Medical_Keyword_42 = as.factor(dfIns$Medical_Keyword_42)
dfIns$Medical_Keyword_43 = as.factor(dfIns$Medical_Keyword_43)
dfIns$Medical_Keyword_44 = as.factor(dfIns$Medical_Keyword_44)
dfIns$Medical_Keyword_45 = as.factor(dfIns$Medical_Keyword_45)
dfIns$Medical_Keyword_46 = as.factor(dfIns$Medical_Keyword_46)
dfIns$Medical_Keyword_47 = as.factor(dfIns$Medical_Keyword_47)
dfIns$Medical_Keyword_48 = as.factor(dfIns$Medical_Keyword_48)
dfIns$Response = as.factor(dfIns$Response)

# Merge many levels to few

for(level in unique(dfIns$Product_Info_2)) {
  if(nrow(dfIns[which(dfIns$Product_Info_2 == level),]) < 500) {
    levels(dfIns$Product_Info_2)[levels(dfIns$Product_Info_2) == level] = "others"
  }
}

for(level in unique(dfIns$Product_Info_3)) {
  if(nrow(dfIns[which(dfIns$Product_Info_3 == level),]) < 500) {
    levels(dfIns$Product_Info_3)[levels(dfIns$Product_Info_3) == level] = "others"
  }
}

for(level in unique(dfIns$Employment_Info_2)) {
  if(nrow(dfIns[which(dfIns$Employment_Info_2 == level),]) < 500) {
    levels(dfIns$Employment_Info_2)[levels(dfIns$Employment_Info_2) == level] = "others"
  }
}

for(level in unique(dfIns$InsuredInfo_3)) {
  if(nrow(dfIns[which(dfIns$InsuredInfo_3 == level),]) < 500) {
    levels(dfIns$InsuredInfo_3)[levels(dfIns$InsuredInfo_3) == level] = "others"
  }
}

for(level in unique(dfIns$Medical_History_1)) {
  if(nrow(dfIns[which(dfIns$Medical_History_1 == level),]) < 500) {
    levels(dfIns$Medical_History_1)[levels(dfIns$Medical_History_1) == level] = "others"
  }
}

for(level in unique(dfIns$Medical_History_2)) {
  if(nrow(dfIns[which(dfIns$Medical_History_2 == level),]) < 500) {
    levels(dfIns$Medical_History_2)[levels(dfIns$Medical_History_2) == level] = "others"
  }
}


# Stratified sampling

library(caTools)

set.seed(123)

split = sample.split(dfIns$Response, SplitRatio = 0.7)

#prop.table(table(dfIns$Response))
           
dfTrain = subset(dfIns, split == TRUE)      
dfTest = subset(dfIns, split == FALSE) 

#prop.table(table(dfTrain$Response))
#prop.table(table(dfTest$Response))

# Perform sanity checks before model building

cor(dfTrain$BMI,dfTrain$Wt)

chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_1))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_2))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_3))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_4))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_5))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_6))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_7))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_8))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_9))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_10))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_11))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_12))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_13))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_14))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_15))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_16))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_17))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_18))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_19))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_20))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_21))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_22))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_23))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_24))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_25))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_26))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_27))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_28))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_29))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_30))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_31))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_32))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_33))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_34))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_35))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_36))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_37))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_38))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_39))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_40))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_41))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_42))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_43))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_44))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_45))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_46))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_47))
chisq.test(table(dfTrain$Response,dfTrain$Medical_Keyword_48))


#Drop highly correlated variables and least significant
dfTrain <- dfTrain[,!names(dfTrain) %in% c("Id","Risk","Wt","Medical_Keyword_8","Medical_Keyword_20","Medical_Keyword_32","Medical_Keyword_45")]
dfTest <- dfTest[,!names(dfTest) %in% c("Id","Risk","Wt","Medical_Keyword_8","Medical_Keyword_20","Medical_Keyword_32","Medical_Keyword_45")]

# Handle unseen levels based on intuition

table(dfTest$Response,dfTest$Insurance_History_2)
dfTest$Insurance_History_2 <- as.numeric(dfTest$Insurance_History_2)
dfTest[which(dfTest$Insurance_History_2 == 2),]$Insurance_History_2 <- 1
dfTest$Insurance_History_2 <- as.factor(dfTest$Insurance_History_2)

dfTrain$Insurance_History_2 <- as.numeric(dfTrain$Insurance_History_2)
dfTrain$Insurance_History_2 <- as.factor(dfTrain$Insurance_History_2)

table(dfTrain$Response,dfTrain$Insurance_History_3)
dfTrain$Insurance_History_3 <- as.numeric(dfTrain$Insurance_History_3)
dfTrain[which(dfTrain$Insurance_History_3 == 2),]$Insurance_History_3 <- 3
dfTrain$Insurance_History_3 <- as.factor(dfTrain$Insurance_History_3)

dfTest$Insurance_History_3 <- as.numeric(dfTest$Insurance_History_3)
dfTest$Insurance_History_3 <- as.factor(dfTest$Insurance_History_3)

table(dfTrain$Response,dfTrain$Medical_History_3)
dfTrain$Medical_History_3 <- as.numeric(dfTrain$Medical_History_3)
dfTrain[which(dfTrain$Medical_History_3 == 1),]$Medical_History_3 <- 2
dfTrain$Medical_History_3 <- as.factor(dfTrain$Medical_History_3)

dfTest$Medical_History_3 <- as.numeric(dfTest$Medical_History_3)
dfTest$Medical_History_3 <- as.factor(dfTest$Medical_History_3)

table(dfTrain$Response,dfTrain$Medical_History_5)
dfTrain$Medical_History_5 <- as.numeric(dfTrain$Medical_History_5)
dfTrain[which(dfTrain$Medical_History_5 == 3),]$Medical_History_5 <- 1
dfTrain$Medical_History_5 <- as.factor(dfTrain$Medical_History_5)

dfTest$Medical_History_5 <- as.numeric(dfTest$Medical_History_5)
dfTest$Medical_History_5 <- as.factor(dfTest$Medical_History_5)

table(dfTrain$Response,dfTrain$Medical_History_9)
dfTrain$Medical_History_9 <- as.numeric(dfTrain$Medical_History_9)
dfTrain[which(dfTrain$Medical_History_9 == 3),]$Medical_History_9 <- 2
dfTrain$Medical_History_9 <- as.factor(dfTrain$Medical_History_9)

dfTest$Medical_History_9 <- as.numeric(dfTest$Medical_History_9)
dfTest$Medical_History_9 <- as.factor(dfTest$Medical_History_9)

table(dfTrain$Response,dfTrain$Medical_History_12)
dfTrain$Medical_History_12 <- as.numeric(dfTrain$Medical_History_12)
dfTrain[which(dfTrain$Medical_History_12 == 1),]$Medical_History_12 <- 2
dfTrain$Medical_History_12 <- as.factor(dfTrain$Medical_History_12)

dfTest$Medical_History_12 <- as.numeric(dfTest$Medical_History_12)
dfTest$Medical_History_12 <- as.factor(dfTest$Medical_History_12)

table(dfTrain$Response,dfTrain$Medical_History_13)
dfTrain$Medical_History_13 <- as.numeric(dfTrain$Medical_History_13)
dfTrain[which(dfTrain$Medical_History_13 == 2),]$Medical_History_13 <- 3
dfTrain$Medical_History_13 <- as.factor(dfTrain$Medical_History_13)

dfTest$Medical_History_13 <- as.numeric(dfTest$Medical_History_13)
dfTest$Medical_History_13 <- as.factor(dfTest$Medical_History_13)

table(dfTrain$Response,dfTrain$Medical_History_16)
dfTrain$Medical_History_16 <- as.numeric(dfTrain$Medical_History_16)
dfTrain[which(dfTrain$Medical_History_16 == 2),]$Medical_History_16 <- 1
dfTrain$Medical_History_16 <- as.factor(dfTrain$Medical_History_16)

dfTest$Medical_History_16 <- as.numeric(dfTest$Medical_History_16)
dfTest$Medical_History_16 <- as.factor(dfTest$Medical_History_16)

table(dfTrain$Response,dfTrain$Medical_History_17)
dfTrain$Medical_History_17 <- as.numeric(dfTrain$Medical_History_17)
dfTrain[which(dfTrain$Medical_History_17 == 1),]$Medical_History_17 <- 3
dfTrain$Medical_History_17 <- as.factor(dfTrain$Medical_History_17)

dfTest$Medical_History_17 <- as.numeric(dfTest$Medical_History_17)
dfTest$Medical_History_17 <- as.factor(dfTest$Medical_History_17)

table(dfTest$Response,dfTest$Medical_History_20)
dfTest$Medical_History_20 <- as.numeric(dfTest$Medical_History_20)
dfTest[which(dfTest$Medical_History_20 == 3),]$Medical_History_20 <- 2
dfTest$Medical_History_20 <- as.factor(dfTest$Medical_History_20)

dfTrain$Medical_History_20 <- as.numeric(dfTrain$Medical_History_20)
dfTrain$Medical_History_20 <- as.factor(dfTrain$Medical_History_20)

table(dfTrain$Response,dfTrain$Medical_History_21)
dfTrain$Medical_History_21 <- as.numeric(dfTrain$Medical_History_21)
dfTrain[which(dfTrain$Medical_History_21 == 3),]$Medical_History_21 <- 1
dfTrain$Medical_History_21 <- as.factor(dfTrain$Medical_History_21)

dfTest$Medical_History_21 <- as.numeric(dfTest$Medical_History_21)
dfTest$Medical_History_21 <- as.factor(dfTest$Medical_History_21)

table(dfTrain$Response,dfTrain$Medical_History_23)
dfTrain$Medical_History_23 <- as.numeric(dfTrain$Medical_History_23)
dfTrain[which(dfTrain$Medical_History_23 == 2),]$Medical_History_23 <- 3
dfTrain$Medical_History_23 <- as.factor(dfTrain$Medical_History_23)

dfTest$Medical_History_23 <- as.numeric(dfTest$Medical_History_23)
dfTest$Medical_History_23 <- as.factor(dfTest$Medical_History_23)

table(dfTrain$Response,dfTrain$Medical_History_30)
dfTrain$Medical_History_30 <- as.numeric(dfTrain$Medical_History_30)
dfTrain[which(dfTrain$Medical_History_30 == 1),]$Medical_History_30 <- 2
dfTrain$Medical_History_30 <- as.factor(dfTrain$Medical_History_30)

dfTest$Medical_History_30 <- as.numeric(dfTest$Medical_History_30)
dfTest$Medical_History_30 <- as.factor(dfTest$Medical_History_30)

table(dfTrain$Response,dfTrain$Medical_History_31)
dfTrain$Medical_History_31 <- as.numeric(dfTrain$Medical_History_31)
dfTrain[which(dfTrain$Medical_History_31 == 2),]$Medical_History_31 <- 3
dfTrain$Medical_History_31 <- as.factor(dfTrain$Medical_History_31)

dfTest$Medical_History_31 <- as.numeric(dfTest$Medical_History_31)
dfTest$Medical_History_31 <- as.factor(dfTest$Medical_History_31)

table(dfTrain$Response,dfTrain$Medical_History_34)
dfTrain$Medical_History_34 <- as.numeric(dfTrain$Medical_History_34)
dfTrain[which(dfTrain$Medical_History_34 == 2),]$Medical_History_34 <- 3
dfTrain$Medical_History_34 <- as.factor(dfTrain$Medical_History_34)

dfTest$Medical_History_34 <- as.numeric(dfTest$Medical_History_34)
dfTest$Medical_History_34 <- as.factor(dfTest$Medical_History_34)

table(dfTrain$Response,dfTrain$Medical_History_35)
dfTrain$Medical_History_35 <- as.numeric(dfTrain$Medical_History_35)
dfTrain[which(dfTrain$Medical_History_35 == 2),]$Medical_History_35 <- 1
dfTrain$Medical_History_35 <- as.factor(dfTrain$Medical_History_35)

dfTest$Medical_History_35 <- as.numeric(dfTest$Medical_History_35)
dfTest$Medical_History_35 <- as.factor(dfTest$Medical_History_35)

table(dfTrain$Response,dfTrain$Medical_History_39)
dfTrain$Medical_History_39 <- as.numeric(dfTrain$Medical_History_39)
dfTrain[which(dfTrain$Medical_History_39 == 2),]$Medical_History_39 <- 3
dfTrain$Medical_History_39 <- as.factor(dfTrain$Medical_History_39)

dfTest$Medical_History_39 <- as.numeric(dfTest$Medical_History_39)
dfTest$Medical_History_39 <- as.factor(dfTest$Medical_History_39)

table(dfTrain$Response,dfTrain$Medical_History_40)
dfTrain$Medical_History_40 <- as.numeric(dfTrain$Medical_History_40)
dfTrain[which(dfTrain$Medical_History_40 == 2),]$Medical_History_40 <- 3
dfTrain$Medical_History_40 <- as.factor(dfTrain$Medical_History_40)

dfTest$Medical_History_40 <- as.numeric(dfTest$Medical_History_40)
dfTest$Medical_History_40 <- as.factor(dfTest$Medical_History_40)

table(dfTrain$Response,dfTrain$Medical_History_41)
dfTrain$Medical_History_41 <- as.numeric(dfTrain$Medical_History_41)
dfTrain[which(dfTrain$Medical_History_41 == 2),]$Medical_History_41 <- 1
dfTrain$Medical_History_41 <- as.factor(dfTrain$Medical_History_41)

dfTest$Medical_History_41 <- as.numeric(dfTest$Medical_History_41)
dfTest$Medical_History_41 <- as.factor(dfTest$Medical_History_41)

table(dfTrain$Response)
table(dfTest$Response)

##Random forest for variable importance

library(randomForest)

tRF <- tuneRF(x = dfTrain[,-113], 
              y=as.factor(dfTrain$Response),
              mtryStart = 10, 
              ntreeTry=100, 
              stepFactor = 2, 
              improve = 0.01, 
              trace=FALSE, 
              plot = FALSE,
              doBest = TRUE,
              nodesize = 200, 
              importance=TRUE
)


varImpPlot(tRF)


# CART model


library(rpart)

cart.ctrl = rpart.control(minsplit=floor(0.02*nrow(dfTrain)), minbucket = floor(0.02*nrow(dfTrain))/3, cp = 0, xval = 10)

CartM1<-rpart(formula = Response ~ BMI + Product_Info_2 + Product_Info_4 + Medical_Keyword_3 + Medical_Keyword_15 + Ins_Age + Medical_History_1 + Medical_History_2 + Medical_History_4 + Medical_History_5 + Medical_History_18 + Medical_History_23 + Medical_History_39 + Medical_History_40 + Medical_History_30 + InsuredInfo_2 + InsuredInfo_5 + InsuredInfo_6, data = dfTrain, method = "class", control = cart.ctrl)

library(rpart.plot)
library(RColorBrewer)

library(rattle)

Cprune<- prune(CartM1,cp=  CartM1$cptable[which.min(CartM1$cptable[,"xerror"]),"CP"])
rattle::fancyRpartPlot(Cprune)

dfTest$predCartScore<-predict(Cprune,dfTest)

for(i in 1:nrow(dfTest)) {
  if(max(dfTest$predCartScore[i,]) == dfTest$predCartScore[i,"1"]) {
    dfTest[i,"predCARTClass"] <- "1"
  }
  else if(max(dfTest$predCartScore[i,]) == dfTest$predCartScore[i,"2"]) {
    dfTest[i,"predCARTClass"] <- "2"
  }
  else if(max(dfTest$predCartScore[i,]) == dfTest$predCartScore[i,"3"]) {
    dfTest[i,"predCARTClass"] <- "3"
  }
}


library(caret)

caret::confusionMatrix(as.factor(dfTest$predCARTClass),as.factor(dfTest$Response))

# Predict using tuneRF model

dfTest$predRFScore<-predict(tRF,dfTest)

dfTest$predRFScore<-predict(tRF,dfTest, type = "prob")

for(i in 1:nrow(dfTest)) {
  if(max(dfTest$predRFScore[i,]) == dfTest$predRFScore[i,"1"]) {
    dfTest[i,"predRFClass"] <- "1"
  }
  else if(max(dfTest$predRFScore[i,]) == dfTest$predRFScore[i,"2"]) {
    dfTest[i,"predRFClass"] <- "2"
  }
  else if(max(dfTest$predRFScore[i,]) == dfTest$predRFScore[i,"3"]) {
    dfTest[i,"predRFClass"] <- "3"
  }
}

caret::confusionMatrix(as.factor(dfTest$predRFClass),as.factor(dfTest$Response))

# Neural network model

dfTrainNN=dfTrain[,c("BMI","Product_Info_2","Product_Info_4","Medical_Keyword_3","Medical_Keyword_15","Ins_Age","Medical_History_1","Medical_History_2","Medical_History_4","Medical_History_5","Medical_History_18","Medical_History_23","Medical_History_39","Medical_History_40","Medical_History_30","InsuredInfo_2","InsuredInfo_5","InsuredInfo_6","Response")]

ncol(dfTrainNN)

library(dummies)

dfTrainNN_Encoded = dummies::dummy.data.frame(dfTrainNN[,-19], sep = "_")

colnames(dfTrainNN_Encoded)

dfTrainNN2<-dfTrainNN_Encoded

dfTrainNN2$Response=as.factor(dfTrainNN$Response)

View(dfTrainNN2)

library(nnet)

NNModel <- multinom(Response~., data=dfTrainNN2, maxit=500, trace=T)

dfTestNN=dfTest[,c("BMI","Product_Info_2","Product_Info_4","Medical_Keyword_3","Medical_Keyword_15","Ins_Age","Medical_History_1","Medical_History_2","Medical_History_4","Medical_History_5","Medical_History_18","Medical_History_23","Medical_History_39","Medical_History_40","Medical_History_30","InsuredInfo_2","InsuredInfo_5","InsuredInfo_6","Response")]

dfTestNN2<-as.data.frame(dummy.data.frame(dfTestNN[,-19], sep = "_"))

dfTestNN2$Response=as.factor(dfTestNN$Response)

dfTestNN2$PredNNScore <- predict(NNModel, type="probs", newdata=dfTestNN2)

for(i in 1:nrow(dfTestNN2)) {
  if(max(dfTestNN2$PredNNScore[i,]) == dfTestNN2$PredNNScore[i,"1"]) {
    dfTestNN2[i,"predNNClass"] <- "1"
  }
  else if(max(dfTestNN2$PredNNScore[i,]) == dfTestNN2$PredNNScore[i,"2"]) {
    dfTestNN2[i,"predNNClass"] <- "2"
  }
  else if(max(dfTestNN2$PredNNScore[i,]) == dfTestNN2$PredNNScore[i,"3"]) {
    dfTestNN2[i,"predNNClass"] <- "3"
  }
}

caret::confusionMatrix(dfTestNN2$Response,dfTestNN2$predNNClass)

# SVM model

library(e1071)

modelSVM <- svm(Response ~ ., data = dfTrainNN2)

dfTestNN2$PredSVMScore <- predict(modelSVM, newdata=dfTestNN2)

caret::confusionMatrix(dfTestNN2$Response,dfTestNN2$PredSVMScore)

# XG Boost Model

library(xgboost)

dfTrainXGB = dfTrain[,c("BMI","Product_Info_2","Product_Info_4","Medical_Keyword_3","Medical_Keyword_15","Ins_Age","Medical_History_1","Medical_History_2","Medical_History_4","Medical_History_5","Medical_History_18","Medical_History_23","Medical_History_39","Medical_History_40","Medical_History_30","InsuredInfo_2","InsuredInfo_5","InsuredInfo_6","Response")]

dfTrainXGB_Encoded = dummies::dummy.data.frame(dfTrainXGB[,-19], sep = "_")

str(dfTrainXGB_Encoded)
colnames(dfTrainXGB_Encoded)

dfTrainXGB2<-as.data.frame(dfTrainXGB_Encoded)

dfTrainXGB2$Response=as.factor(dfTrainXGB$Response)

param       = list("objective" = "multi:softmax", # multi class classification
                   "num_class"= 3 ,  		# Number of classes in the dependent variable.
                   "eval_metric" = "mlogloss",  	 # evaluation metric 
                   "nthread" = 8,   			 # number of threads to be used 
                   "max_depth" = 6,    		 # maximum depth of tree 
                   "eta" = 0.05,    			 # step size shrinkage 
                   "gamma" = 0.01,    			 # minimum loss reduction 
                   "subsample" = 0.8,    		 # part of data instances to grow tree 
                   "colsample_bytree" = 0.5, 		 # subsample ratio of columns when constructing each tree 
                   "min_child_weight" = 50  		 # minimum sum of instance hessian weight needed in a child 
)


#Identify the Predictors and the dependent variable, aka label.

predictors = colnames(dfTrainXGB2[-ncol(dfTrainXGB2)])

#xgboost works only if the labels are numeric. Hence, convert the labels (Response) to numeric.

label = as.numeric(dfTrainXGB2[,ncol(dfTrainXGB2)])

print(table (label))


# xgboost works only if the numeric labels start from 0. Hence, subtract 1 from the label.

label = as.numeric(dfTrainXGB2[,ncol(dfTrainXGB2)])-1

print(table (label))

rm(xgbModel)

xgbModel = xgboost(
  param=param,
  data =as.matrix(dfTrainXGB2[,predictors]),
  label = label,
  nrounds=2500)


# Make prediction on the testing data.

dfTestXGB = dfTest[,c("BMI","Product_Info_2","Product_Info_4","Medical_Keyword_3","Medical_Keyword_15","Ins_Age","Medical_History_1","Medical_History_2","Medical_History_4","Medical_History_5","Medical_History_18","Medical_History_23","Medical_History_39","Medical_History_40","Medical_History_30","InsuredInfo_2","InsuredInfo_5","InsuredInfo_6","Response")]

dfTestXGB_Encoded = dummies::dummy.data.frame(dfTestXGB[,-19], sep = "_")

colnames(dfTestXGB_Encoded)

dfTestXGB2<-as.data.frame(dfTestXGB_Encoded)

dfTestXGB2$Response=as.factor(dfTestXGB$Response)

dfTestXGB2$PredXgbClass = predict(xgbModel, as.matrix(dfTestXGB2[,predictors]))


#Translate the prediction to the original class or Species.

dfTestXGB2$PredXgbClass = ifelse(dfTestXGB2$PredXgbClass==0,1,ifelse(dfTestXGB2$PredXgbClass==1,2,3))

#Compute the accuracy of predictions.

caret::confusionMatrix( dfTestXGB2$PredXgbClass,dfTestXGB2$Response)

View(dfTest)

dfTest$predNNClass<-dfTestNN2$predNNClass
dfTest$PredNNScore<-dfTestNN2$PredNNScore
dfTest$PredXgbClass<-dfTestXGB2$PredXgbClass

