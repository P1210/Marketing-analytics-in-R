library(tidyverse)
library(dplyr)
library(plyr) #count()
library(ggplot2)
require(scales)

#Data Cleaning and Manipulation

df <- read.csv('C:\\Users\\Pranjal Gupta\\Downloads\\marketing_campaign.csv',
               header = TRUE,sep = ";")
head(df)

str(df)

sum(is.na(df))
colSums(is.na(df))

sapply(df,function(x) length(unique(x)))

df <- df %>%
  mutate(Dt_Customer = as.Date(Dt_Customer)) #create Date column

count(df$Marital_Status)
#there are two many values, hence merging to form better categories
df$Rel_Status[df$Marital_Status %in% c('Alone', 'Divorced', 'Widow', 'Single')] <- 'Single'
df$Rel_Status[df$Marital_Status %in% c('Married', 'Together')] <- 'Together'
df$Rel_Status[df$Marital_Status %in% c('Absurd', 'YOLO')] <-''

count(df$Education)
# nothing to change in this as everything indicates to a conclusion 

summary(df$Income)
ggplot(df, aes(x = Income)) +geom_boxplot()+ scale_x_continuous(labels = comma)
#As we can see a few outliers, it is disturbing the overall value of the column as seen in summary
#there are also, 24 missing values, to fill those as well we need to remove outliers
outliers <- boxplot(df$Income, plot = FALSE)$out
df <- df %>%
  filter(Income < max(outliers) - 1)
# now we fill NA values in Income variable with mean
df$Income[is.na(df$Income)] <- mean(df$Income, na.rm = TRUE)
colSums(is.na(df))

summary(df$Z_CostContact)
summary(df$Z_Revenue)
#these columns are not required anymore for further Analysis

drop <- c("Marital_Status","Kidhome","Teenhome","Z_CostContact","Z_Revenue")
df = df[,!(names(df) %in% drop)]
head(df)

#Date signed up 
ggplot(df, aes(Dt_Customer)) + geom_density() +
  geom_vline(aes(xintercept = mean(Dt_Customer)), color = 'red')

#Year born - to find the popular age group of the company
ggplot(df, aes(Year_Birth)) + geom_density() +
  geom_vline(aes(xintercept = mean(Year_Birth)), color = 'red')

df <- df %>%
  #creating new variables based off old ones
  mutate(MntSpent = MntFishProducts + MntMeatProducts + MntFruits + MntSweetProducts + MntWines + MntGoldProds) %>%
  mutate(NumPurchases = NumCatalogPurchases + NumStorePurchases + NumWebPurchases) %>%
  mutate(AcceptedCmp = AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5) %>%
  mutate(Age = as.numeric(format(Dt_Customer, format = '%Y')) - Year_Birth) 
head(df)

library(reshape) #melt()
#melt data frame into long format
rev <- c('ï..ID','Year_Birth','AcceptedCmp1', 'AcceptedCmp2', 'AcceptedCmp3', 'AcceptedCmp4', 'AcceptedCmp5', 'Complain',
                 'Education', 'Rel_Status', 'Dt_Customer', 'Response', 'AcceptedCmp')
df_new <- df %>%
  select(-one_of(rev)) %>%
  melt()

ggplot(df_new, aes(factor(variable), value)) +
  geom_boxplot(color = 'steelblue') +
  facet_wrap(~variable,scale='free') +
  labs(title = 'Boxplots of Various Variables')

#remove outliers from age variable
outliers <- boxplot(df$Age, plot = FALSE)$out
df <- df %>%
  filter(Age < min(outliers))

#list of products
products <- c('MntWines', 'MntFruits', 'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds')
#sum amounts spent on products and set these values in df
products_df <- df %>%
  select(products) %>% 
  summarize_each(sum) %>%
  t() %>% #to calculate transpose of a matrix or Data Frame.
  as.data.frame() %>%
  rownames_to_column('Products')
products_df
ggplot(products_df,aes(x=Products, y=V1)) + geom_bar(fill="steelblue",stat = "identity") +
  scale_y_continuous(labels = comma)

#list of purchases
purchases <- c('NumCatalogPurchases', 'NumStorePurchases', 'NumWebPurchases')
#sum amounts spent on purchases and set these values in df
purchases_df <- df %>%
  select(purchases) %>% 
  summarize_each(sum) %>%
  t() %>% #to calculate transpose of a matrix or Data Frame.
  as.data.frame() %>%
  rownames_to_column('Purchases')
purchases_df
ggplot(purchases_df,aes(x=Purchases, y=V1)) + geom_bar(fill="steelblue",stat = "identity") +
  scale_y_continuous(labels = comma)

#library(GGally) #ggcorr() and ggpairs()
library(ggcorrplot)
df_new1 <- df %>%
  select(-one_of(rev))
head(df_new1)
correlation_matrix <- round(cor(df_new1),1)
ggcorrplot(correlation_matrix, hc.order =TRUE, type ="lower", method ="square",lab =TRUE)

#income v/s mntspent
ggplot(df, aes(x = MntSpent, y = Income)) + geom_point() + geom_smooth(method = lm,color='red') +
  labs(x = 'Amount Spent ($)', y = 'Yearly Income ($)')       

#income v/s age
ggplot(df, aes(x = NumWebVisitsMonth, y = Income)) + geom_point() + geom_smooth(method = lm,color='red') +
  labs(x = 'Web Visits per Month', y = 'Yearly Income')

unique(df[c("AcceptedCmp")])

#boxplot Income by accepted previous
ggplot(df, aes(x = AcceptedCmp, y = Income)) + geom_boxplot()+
  labs(x = 'Previously Accepted Campaigns')

#boxplot Age by accepted previous
ggplot(df, aes(x = AcceptedCmp, y = Age)) + geom_boxplot()+
  labs(x = 'Previously Accepted Campaigns')

#boxplot Recency by Response
ggplot(df, aes(x = Response, y = Recency)) + geom_boxplot() +
  labs(x = 'Response', y = 'Recency')

#Education by Accepted previous
ggplot(df, aes(x = AcceptedCmp, fill = Education)) + geom_bar()

#bar chart of most successful marketing campaign
campaigns <- c('AcceptedCmp1', 'AcceptedCmp2', 'AcceptedCmp3', 'AcceptedCmp4', 'AcceptedCmp5','Response')
campaign_df <- df %>%
  select(campaigns) %>% 
  summarize_each(sum) %>%
  t() %>% #to calculate transpose of a matrix or Data Frame.
  as.data.frame() %>%
  rownames_to_column('campaigns')
campaign_df
ggplot(campaign_df,aes(x=campaigns, y=V1)) + geom_bar(fill="steelblue",stat = "identity") +
  scale_y_continuous(labels = comma)

#Model Predictions
library(randomForest)
library(party)
library(datasets)
library(party)
library(dplyr)
library(magrittr)
library(e1071)
library(caTools)
library(class)
library(caret)

sample_data = sample.split(df, SplitRatio = 0.7)
train_data <- subset(df, sample_data == TRUE)
test_data <- subset(df, sample_data == FALSE)

#ID3 and Decision tree doesn't support categorical binary data, hence we use Randomforest decison tree
set.seed(101)
rf <- randomForest(Response ~ ., data = train_data,importance=TRUE)
print(rf)
out.importance <- round(importance(rf), 2)
print(out.importance )

print(importance(rf,type = 2))



#KNN Model
train <- train_data %>% dplyr::select(where(is.numeric))
test <- test_data %>% dplyr::select(where(is.numeric))

train.knn <- as.data.frame(train)
test.knn <- as.data.frame(test)

# Fitting KNN Model 
# to training dataset
kn <- knn(train.knn, test.knn, train.knn$Response, k = 1)
kn
misClassError <- mean(kn != test.knn$Response)
print(paste('Accuracy =', 1-misClassError))

# K = 5
kn <- knn(train.knn, test.knn, train.knn$Response, k = 5)
misClassError <- mean(kn != test.knn$Response)
print(paste('Accuracy =', 1-misClassError))

# K = 15
kn <- knn(train.knn, test.knn, train.knn$Response, k = 15)
misClassError <- mean(kn != test.knn$Response)
print(paste('Accuracy =', 1-misClassError))

# K = 25
kn <- knn(train.knn, test.knn, train.knn$Response, k = 25)
misClassError <- mean(kn != test.knn$Response)
print(paste('Accuracy =', 1-misClassError))

#K=25 reached the highest from the last four, hence increasing the value wont affect the accuracy much
#confusion matrix
cm <- table(test.knn$Response, kn)
cm
confusionMatrix(table(kn, test.knn$Response), positive = '1')
