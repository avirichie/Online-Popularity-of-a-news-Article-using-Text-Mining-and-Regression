library(tidyverse)
library(ggplot2)
library(dplyr)
library(forecast) 
library(leaps)
library(reshape)
library(gains)
library(caret)
library(Hmisc)

setwd("E:/UTD/Semester 1/6356 BA with R/Project/OnlineNewsPopularity")




news.df <- read.csv("E:/UTD/Semester 1/6356 BA with R/Project/OnlineNewsPopularity/OnlineNewsPopularity.csv")
dim(news.df)



####Removing the url of the article
news.df <- news.df[,c(-1,-2)]

####removing the rows with 0 n tokens content
news.df <- news.df[news.df$n_tokens_content!=0,]

#####initial model
init_mod <- lm(formula = shares ~.,data = news.df)
summary(init_mod)

####Distribution of the dependent variable
ggplot(news.df) + geom_histogram(mapping = aes(x = shares)) 

###It is totally skewed so checking the distribution of the log of the dependent variable
ggplot(news.df) + geom_histogram(mapping = aes(x = log(shares))) 
####The distribution seems normal 

###COnverting the shares variable to log
news.df$shares <- log(news.df$shares)

summary(news.df$shares)

#####converting the keyword share values to integers
news.df$kw_min_min <- as.integer(news.df$kw_min_min)
news.df$kw_max_min <- as.integer(news.df$kw_max_min)

news.df$kw_avg_min <- as.integer(news.df$kw_avg_min)
news.df$kw_min_max <- as.integer(news.df$kw_min_max)
news.df$kw_min_avg <- as.integer(news.df$kw_min_avg)
news.df$kw_avg_avg <- as.integer(news.df$kw_avg_avg)
news.df$kw_avg_max <- as.integer(news.df$kw_avg_max)
news.df$kw_max_avg <- as.integer(news.df$kw_max_avg)
news.df$kw_max_max <- as.integer(news.df$kw_max_max)

news.df$kw_min_min <- ifelse(news.df$kw_min_min<0,0,news.df$kw_min_min)
news.df$kw_max_min <- ifelse(news.df$kw_max_min<0,0,news.df$kw_max_min)
news.df$kw_avg_min <- ifelse(news.df$kw_avg_min<0,0,news.df$kw_avg_min)
news.df$kw_min_max <- ifelse(news.df$kw_min_max<0,0,news.df$kw_min_max)
news.df$kw_min_avg <- ifelse(news.df$kw_min_avg<0,0,news.df$kw_min_avg)
news.df$kw_avg_avg <- ifelse(news.df$kw_avg_avg<0,0,news.df$kw_avg_avg)
news.df$kw_avg_max <- ifelse(news.df$kw_avg_max<0,0,news.df$kw_avg_max)
news.df$kw_max_avg <- ifelse(news.df$kw_max_avg<0,0,news.df$kw_max_avg)
news.df$kw_max_max <- ifelse(news.df$kw_max_max<0,0,news.df$kw_max_max)

str(news.df)
summary(news.df)

dim(news.df)


#######plots based on the week day
news.df1 <- news.df
news.df1$news_day <- rep("Sunday", nrow(news.df))
news.df1$news_day[news.df$weekday_is_monday==1] <- "Monday"
news.df1$news_day[news.df$weekday_is_tuesday==1] <- "Tuesday"
news.df1$news_day[news.df$weekday_is_wednesday==1] <- "Wednesday"
news.df1$news_day[news.df$weekday_is_thursday==1] <- "Thursday"
news.df1$news_day[news.df$weekday_is_friday==1] <- "Friday"
news.df1$news_day[news.df$weekday_is_saturday==1] <- "Saturday"
p1 <- ggplot(data=news.df1, aes(as.factor(news_day), log(shares)))
p1 + geom_boxplot()

names(news.df1)


#######plots based on the type of article day
news.df1$topic <- rep("Social Media", nrow(news.df))
news.df1$topic[news.df$data_channel_is_lifestyle==1] <- "Lifestyle"
news.df1$topic[news.df$data_channel_is_world==1] <- "World"
news.df1$topic[news.df$data_channel_is_bus==1] <- "Business"
news.df1$topic[news.df$data_channel_is_entertainment==1] <- "Entertainment"
news.df1$topic[news.df$data_channel_is_tech==1] <- "Technology"
p2 <- ggplot(data=news.df1, aes(as.factor(topic), log(shares)))
p2 + geom_boxplot()


par(mfrow=c(1,1))
plot(news.df[,1], xlab=names(news.df)[1])


####Correlation plots


corr.df <- news.df %>% select(n_tokens_title,n_tokens_content,n_unique_tokens,n_non_stop_words,n_non_stop_unique_tokens,num_hrefs,num_self_hrefs,num_imgs,num_videos,average_token_length,num_keywords,self_reference_min_shares,self_reference_max_shares,self_reference_avg_sharess)
cor.mat1 <- round(cor(corr.df),2)
View(cor.mat1)

melted.cor.mat1 <- melt(cor.mat1)
View(melted.cor.mat1)

#colnames(melted.cor.mat1)
ggplot(melted.cor.mat1, aes(x = X1, y = X2, fill = value))+
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value))



######plots
pairs(news.df[1:500,c(1:10)],upper.panel=NULL)


#we need to reduce the no of variables
#FInding the correltion btw the variable and removing the highly correlated features
corr = cor(news.df)
correlated.df= findCorrelation(corr, cutoff=0.7)
view(correlated.df)
reduced_news_df=news.df[,-c(correlated.df)]
dim(reduced_news_df)


#####Splitting the reduced_news_df into training and validation sets
set.seed(123)
numberOfRows <- nrow(reduced_news_df)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- reduced_news_df[train.index,]
valid.df <- reduced_news_df[-train.index,]

###splitting for the full data set
train.df1 <- news.df[train.index,]
valid.df1 <- news.df[-train.index,]

#####Applying exhaustive search on the reduced_news_df
######using exhaustive based on 
search <- regsubsets(shares ~., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")

sum <- summary(search)
summary(search)
plot(search, scale="r2")

######Taking the features based on the r2 of the plot which is 0.11
model_exhaustive <- lm(formula = shares ~ num_hrefs+num_self_hrefs+num_imgs+average_token_length+num_keywords+
                         +data_channel_is_lifestyle+data_channel_is_entertainment+data_channel_is_bus+
                         data_channel_is_socmed+data_channel_is_world +kw_min_min+kw_min_max
                         +kw_avg_max+kw_min_avg+kw_max_avg+self_reference_min_shares+self_reference_max_shares+
                         weekday_is_monday+weekday_is_friday+weekday_is_saturday+LDA_01+
                         global_rate_positive_words+max_positive_polarity+min_positive_polarity+title_sentiment_polarity +weekday_is_sunday
                       ,data=train.df)
summary(model_exhaustive)
step_exhaustive_news.df.pred <- predict(model_exhaustive, valid.df1)
accuracy(step_exhaustive_news.df.pred, valid.df1$shares)



####linear model for the unreduced data set
model_full <- lm(formula = shares ~ ., data = train.df1)
model_reduced <- lm(formula = shares ~ ., data = train.df)


####Applying backward search on news.df
step_backward_news.df<- step(object = model_full, direction = "backward")
summary(step_backward_news.df)
step_backward_news.df.pred <- predict(step_backward_news.df, valid.df1)
accuracy(step_backward_news.df.pred, valid.df1$shares)

####Applying backward search on reduced_news_df
step_backward_reduced_news_df<- step(object = model_reduced, direction = "backward")
summary(step_backward_reduced_news_df)
step_backward_reduced_news_df.pred <- predict(step_backward_reduced_news_df, valid.df)
accuracy(step_backward_reduced_news_df.pred, valid.df$shares)

####Applying forward search on news.df
step_forward_news.df<- step(object = model_full, direction = "forward")
summary(step_forward_news.df)
step_forward_news.df.pred <- predict(step_forward_news.df, valid.df1)
accuracy(step_forward_news.df.pred, valid.df1$shares)



####Applying forward search on reduced_news_df
step_forward_reduced_news_df<- step(object = model_reduced, direction = "forward")
summary(step_forward_reduced_news_df)
step_forward_reduced_news_df.pred <- predict(step_forward_reduced_news_df, valid.df)
accuracy(step_forward_reduced_news_df.pred, valid.df$shares)

####Applying both search on news.df
step_both_news.df<- step(object = model_full, direction = "both")
summary(step_both_news.df)
step_both_news_df.pred <- predict(step_both_news.df, valid.df1)
accuracy(step_both_news_df.pred, valid.df1$shares)



####Applying both search on reduced_news_df
step_both_news_df<- step(object = model_reduced, direction = "both")
summary(step_both_news_df)
step_both_reduced_news_df.pred <- predict(step_both_news_df, valid.df)
accuracy(step_both_reduced_news_df.pred, valid.df$shares)




view(step_backward_news.df)
view(step_backward_news.df$terms)
write.csv(step_backward_news.df$terms,"backward_full.csv")


plot(step_backward_news.df)



#####Actual vs predicted graph for the exhaustive 
df <- data.frame("Predicted" = step_exhaustive_news.df.pred, "Actual" = valid.df$shares)
View(df)

df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

##Plotting actual vs predicted values for Training and Validation data
p1<- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted shares" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Shares")) +ylab("predicted mean")+title("Actual vs Predicted")

p1





