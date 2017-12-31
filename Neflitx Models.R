#Dan Ehrlich and Johnny Ma
#Netflix Challenge
#Econmetrics B
#Data Analysis

library(plyr)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(proxy)
library(reshape2)
library(np)

setwd("~/Dropbox/Netflix Challenge/Data")

ratings <- read_csv("long_ratings_2.csv")
films  <- read_csv("long_films_2.csv") 
users   <- read_csv("long_users_2.csv") 

#rename appropirate columns
colnames(ratings)[2] <- "user"

c <- merge(ratings, a, by = ("user"))
#create RMSE function to test model predicitons
RMSE <- function(true_r, predicted_r){
  rmse = sqrt(mean((true_r - predicted_r)^2, na.rm=TRUE))
  return(rmse)
}

##########################################################
############ SUMMARY STATISTICS ##########################
##########################################################

##################
# Ratings Data 
##################

#Number of ratings vs average rating
avg_film = aggregate(ratings~films, ratings, mean)
num_film = aggregate(ratings~films, ratings, function(x) length(which(!is.na(x))))
colnames(num_film)[2] <- "freq"
colnames(avg_film)[2] <- "avg"
sumdata <- merge(avg_film,num_film,by="films")

sum1 <- ggplot(sumdata, aes(x=freq, y=avg)) +
  geom_point() +
  theme_bw()+
  ggsave("sum1.png")
sum1

#Average rating by Movie distribution
sum2 <- ggplot(avg_film, aes(x=avg)) + 
  geom_density() +
  theme_bw()+
  ggsave("sum2.png")
sum2

#Number of films reviewed by user distribution
num_user = aggregate(ratings~user, ratings, function(x) length(which(!is.na(x))))
sum3 <- ggplot(num_user, aes(x=ratings)) +
  geom_density()+
  theme_bw()+
  ggsave("sum3.png")
sum3

#Kernel Regression

#############
# User Data 
#############

# getting the frequency of 'favorites' 
freq_fav <- aggregate(fav~film, users, function(x) length(which(!is.na(x))))
freq_fav <- freq_fav[order(-freq_fav$fav),] 
fav <- ggplot(freq_fav, aes(x = fav)) + geom_bar() + theme_bw() + ggsave("fav.png")
fav

##########################################################
############ Prediction Models ###########################
##########################################################

# split data into training and test data sets
#set test data set to 25 percent of all data
set.seed(123)
test_size <- floor(0.25 * nrow(ratings)) # taking 25 %
test_ind <- sample(seq_len(nrow(ratings)), test_size, replace=FALSE) # drawing random rows
test <- ratings[test_ind,] 
train <- ratings[-test_ind,]

#Naive Model 1: Average Rating
average_rating <- mean(test$ratings, na.rm=TRUE) 
test$model1 <- rep(average_rating, nrow(test))
model1_rmse <- RMSE(test$ratings, test$model1)
model1_rmse

#Naive Model 2: Average Rating by Movie
test$model2 <- join(test, avg_film, by='films', type='left', match='all')$avg
model2_rmse <- RMSE(test$ratings, test$model2)
model2_rmse

#Naive Model 3: Average Rating by User
avg_user = aggregate(ratings~user, ratings, mean, drop = FALSE)
colnames(avg_user)[2] <- "avg"
test$model3 <- join(test, avg_user, by='user', type='left', match='all')$avg
model3_rmse <- RMSE(test$ratings, test$model3)
model3_rmse

#Baseline Model 4: Combining User and Movie Information
mu <- mean(test$ratings, na.rm=TRUE) 
lambda_i = 25
lambda_u = 10

movie_bias <- train %>% 
  filter(!is.na(ratings)) %>%
  group_by(films) %>% 
  summarize(count = n(), b_i = sum(ratings - mu, na.rm=TRUE)/(count+lambda_i)) %>%
  select(-count)

train <- join(train, movie_bias, by="films", type='left', match='all')
user_bias <- train %>% 
  filter(!is.na(ratings)) %>%
  group_by(user) %>% 
  summarize(count = n(), b_u = sum(ratings - mu - b_i, na.rm=TRUE)/(count+lambda_u)) %>%
  select(-count)
  
test$b_i <- join(test, movie_bias, by="films", type='left', match='all')$b_i
test$b_i[is.na(test$b_i)] <- 0
test$b_u <- join(test, user_bias, by='user', type='left', match='all')$b_u
test$b_u[is.na(test$b_u)] <- 0

test$model4 <- mu + test$b_i + test$b_u
test$model4[test$model4 >10] <- 10
model4_rmse <- RMSE(test$ratings, test$model4)
model4_rmse

# test$resid4 = test$ratings  - test$model4
# resid4 <- ggplot(test, aes(x=ratings, y=resid4)) +
#   geom_point() +
#   theme_bw() +
#   ggsave("resid4.png")
# resid4
# 
# resid4A <-ggplot(test, aes(x=model4, y=resid4)) +
#   geom_point() +
#   theme_bw()+
#   ggsave("resid4A.png")
# resid4A

# Model 5: Neighborhood Model

ratings_wide <- dcast(data = ratings[,2:5], user + id ~ films, value.var = "ratings") # reputting into long

ratings_wide$user <- c(1:nrow(ratings_wide)) # counting the users

corr <- simil(ratings_wide[,3:nrow(ratings_wide)], method = "Pearson", by_rows = TRUE) # correlation matrix
cor <- as.matrix(corr)


output <- NULL
final <- NULL

for(i in 1:nrow(cor))
  {
  tryCatch({ # because /users/ links to a staff account. Also http errors.
    
  neighbors_30 <- tail(sort(cor[i,]), 10, addrownums = TRUE) # picking 30 neighbors
  index <- as.numeric(names(neighbors_30)) 
  values <- as.vector(neighbors_30)
  neighbors <- cbind(index, values) # getting neighbor index and Pearson R value.
  
  top <- 0
  bot <- 0
  
    for(j in 1:nrow(neighbors)) # for each neighbor, calculate the values
    {
      index <- as.numeric(neighbors[j,1]) 
       
      ratings_vec <- ratings$ratings[ratings$user == index] # pulling ratings from neighbor
      avg_vec <- matrix(rep(-1 * avg_user$avg[avg_user$user == index], length(ratings_vec)), ncol = 1) # average rating for neighbor. needed.
      x <- cbind(ratings_vec, avg_vec)
      
      top <- top + rowSums(x) * as.numeric(neighbors[j,2]) # calculating the top of equation
      bot <- bot + as.numeric(neighbors[j,2]) # calculating the bottom
    }
  

  preds <- avg_user$avg[avg_user$user == i] + top/bot # prediction is average for that user, plus top/bot
  temp <- matrix(rep(i,length(preds)), ncol = 1) # user index to refer to for merge. 
  films <- ratings$films[ratings$user == i] # getting the list of films for that user
  
  output <- as.data.frame(cbind(temp, films, preds))
  colnames(output) <- c("user", "films", "model5")
  
  final <- rbind(final, output) # adding each user underneath the other user.
  
  }, error=function(e){cat("ERROR :",conditionMessage(e),i,  "\n")}) # printing error
  
  print(i)
}

a <- join(test, final, by = c("user", "films"), type= "left", match = "all") # merging test and 
test$model5 <- a[,ncol(a)]
test$model5 <- as.numeric(levels(test$model5))[test$model5]

test$model5[test$model5 <= 0] <- 1
test$model5[test$model5 >= 10] <- 10

model5_rmse <- RMSE(test$model5, test$ratings)
model5_rmse
