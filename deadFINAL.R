# Johnny Ma
# Yelp Food Taste Formation Project
# 6-1-2016

################## 
# Section 1: Reading Data and Instantiation
##################
rm()

library(stringr)
install.packages("stargazer")
library(stargazer)


# setwd("~/Desktop")
# setwd("~/Downloads")
# setwd("D:/Dropbox (Personal)/UChicago/Research/Yelp Addiction")
 setwd("C:/Users/johnn/Dropbox/UChicago/Research/Yelp Addiction")
data <- read.csv("date_filtered_reviews_with_types2.csv", stringsAsFactors=FALSE) # all future reviews after initial period
biz <- read.csv("businesses.csv", stringsAsFactors=FALSE) # all businesses
initial <- read.csv("users_initial_reviews.csv", stringsAsFactors = FALSE) # initial review
user <- read.csv("all_users_30_reviews_with_types.csv", stringsAsFactors = FALSE) # all the >30 users reviews


data$eated <- 0 # did they eat at same type again? 1 for same type as initial
data$asian <- 0 # is it asian? 1 for asian 
data$initialasian <- 0 # was the initial place asian? 1 for initial asian
data$againasian <- 0 # did they eat at an asian restaurant again? 1 for NOT initial and asian return
data$howmany <- 1 # how many places did they eat at
initial$howmany <- 0 # how many places did they eat at, stored in initial
initial$avgrev <- 0 # average review score of user
initial$count12 <- 0 
initial$count_bef <- 0 
initial$count14 <- 0 # blahv la
initial$count15 <- 0 #lasdjf
initial$location <- 0 #location dummy
initial$user_avg_asian <- 0  # average review of asian restaurants prior 

asian <- c("Korean", "Chinese", "Japanese", "Indian", "Vietnamese", "Thai", "Sushi", "Asian") # making the asian criteria

compareNA <- function(v1,v2) { # function to compare NAs
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

for(i in 1:nrow(initial)) # function to generate prices and ratings for initial restaurants
{
  index <- match(initial$business_id[i], biz$business_id)
  initial$price[i] <- biz$attributes.Price.Range[index]
  initial$avgstar[i] <- biz$stars[index]
  initial$location[i] <- biz$state[index]
}


#calculating average price and rating of asian restaurants, in order to replace 0s in price15 and avgstar15 with the averages
count <- 0 
avg2015asian <- 0 
avg2015price <- 0

for(i in 1:nrow(biz))
{
  if(biz$categories1[i] %in% asian | biz$categories2[i] %in% asian | biz$categories3[i] %in% asian | biz$categories4[i] %in% asian )
  {
    count <- count + 1
    avg2015asian <- avg2015asian + biz$stars[i]
  }
}

avg2015asian <- avg2015asian/count

bizz <- biz[complete.cases(biz$attributes.Price.Range),]

count <- 0 

for(i in 1:nrow(bizz))
{
  if(bizz$categories1[i] %in% asian | bizz$categories2[i] %in% asian | bizz$categories3[i] %in% asian | bizz$categories4[i] %in% asian )
  {
    count <- count + 1
    avg2015price <- avg2015price + bizz$attributes.Price.Range[i]
  }
}

avg2015price <- avg2015price/count


#######################
# State Level Characteristics
#######################

initial$isPA <- 0
initial$isNC <- 0
initial$isAZ <- 0
initial$isNV <- 0
initial$isWI <- 0 
initial$isQC <- 0
initial$isburg <- 0

for(i in 1:nrow(initial))
{
if(initial$location[i] == "PA")
{
  initial$isPA[i] <- 1
}
  if(initial$location[i] == "NC")
  {
    initial$isNC[i] <- 1
  }
  if(initial$location[i] == "AZ")
  {
    initial$isAZ[i] <- 1
  }
  if(initial$location[i] == "NV")
  {
    initial$isNV[i] <- 1
  }
  if(initial$location[i] == "WI")
  {
    initial$isWI[i] <- 1
  }
  if(initial$location[i] == "QC")
  {
    initial$isQC[i] <- 1
  }
}

three <- length(unique(data$business_id[(substring(data$date, 1, 4) == "2013") & data$asian == 1]))
four <- length(unique(data$business_id[(substring(data$date, 1, 4) == "2014") & data$asian == 1]))
five <- length(unique(data$business_id[(substring(data$date, 1, 4) == "2015") & data$asian == 1]))

initial$gdpcap = 0
for (i in 1:nrow(initial)){
  if(initial$location[i] == "PA")
  {
    initial$gdpcap[i] <- 0
  }
  if(initial$location[i] == "NC")
  {
    initial$gdpcap[i] <- 1.1
  }
  if(initial$location[i] == "AZ")
  {
    initial$gdpcap[i] <- 0.7
  }
  if(initial$location[i] == "NV")
  {
    initial$gdpcap[i] <- 1.3
  }
  if(initial$location[i] == "WI")
  {
    initial$gdpcap[i] <- .3
  }
  if(initial$location[i] == "QC")
  {
    initial$gdpcap[i] <- 1.45
  }
}

# years stuff


#######################
# Adding to Data
#######################


for(i in 1:nrow(data)) # loop to assign 1 to all asian restaurants
{
  if(data$categories1[i] %in% asian | data$categories2[i] %in% asian | data$categories3[i] %in% asian | data$categories4[i] %in% asian )
  {
    data$asian[i] <- 1
  }
}

for(i in 1:nrow(data)) # loop to assign 1 if initial restaurant is asian
{
  if(data$initial[i] %in% asian)
  {
    data$initialasian[i] <- 1
  }
}

#######################
# Adding to Initial
#######################

initial$avgrev <- 0

for (i in 1:nrow(initial)) { # average review score of reviewer
  u_id = initial$user_id[i]
  initial$avgrev[i] = mean(data[data$user_id == u_id,]$stars)
}

initial$like <- 0

for(i in 1:nrow(initial)) # initial review "liked" if above 3
{
  if(initial$star[i] > 3)
  {
    initial$like[i] <- 1
  }
}

for(i in 1:nrow(initial)) # counting how many times they reviewed after initial
{
  initial$howmany[i] <- sum(str_count(data$user_id, initial$user_id[i]))
}

initial$isasian <- 0 

for(i in 1:nrow(initial)) # loop to assign 1 if initial restaurant is asian
{
  if(initial$foodtype[i] %in% asian)
  {
    initial$isasian[i] <- 1
  }
}

for(i in 1:nrow(data)) # if the initial is the same type of restaurant as later reviews
{
  if((compareNA(data$initial[i],data$categories1[i])) | (compareNA(data$initial[i],data$categories2[i])) | (compareNA(data$initial[i],data$categories3[i])) | (compareNA(data$initial[i],data$categories4[i]))) 
   {
    data$eated[i] <- 1
  }
}

for(i in 1:nrow(data)) # loop to test if they eat at asian place again that is NOT the initial place
{
  for(j in 1:nrow(initial))
  {
  if((data$initialasian[i] == 1) && (data$asian[i] == data$initialasian[i]) && (initial$text[j] != data$text[i]))
  {
    data$againasian[i] <- 1
  }
  }
}

initial$countreturnasian <- 0 # how many times they return to asian places
initial$goback <- 0  # if they go back to the same type of place
data$secondtime <- 0 # if the restaurant is NOT the initial one

for(i in 1:nrow(data)) # do they go back to the same type of restaurant?
{
  for(j in 1:nrow(initial))
  {
  if((initial$user_id[j] == data$user_id[i]) && (data$eated[i] == 1) && (initial$text[j] != data$text[i]))
    {
    initial$goback[j] <- 1
    data$secondtime[i] <- 1 # it is the second time, not the initial
    }
  }
}

initial$returnasian <- 0
initial$is_bef <- 0
initial$is2014 <- 0 
initial$is2015 <- 0

initial$price_bef <- 0
initial$avgstar_bef <- 0
initial$price14 <- 0
initial$avgstar14 <- 0
initial$price15 <- 0
initial$avgstar15 <- 0

for(i in 1:nrow(data)) #  counting up the number of returns to asian restauarants
{
  for (j in 1:nrow(initial))
  {
if((data$againasian[i] == 1) && (initial$user_id[j] == data$user_id[i]))
   {
   initial$returnasian[j] <- 1
   initial$countreturnasian[j] = initial$countreturnasian[j] + 1
}
}
}

###########################################
# Controlling for User Characteristics 
###########################################

user$asian <- 0

for(i in 1:nrow(user)) # loop to assign 1 to all asian restaurants
{
 if(user$categories1[i] %in% asian | user$categories2[i] %in% asian | user$categories3[i] %in% asian | user$categories4[i] %in% asian )
 {
   user$asian[i] <- 1
 }
}


# 
# for(i in 1:nrow(user))
# {
#   for(j in 1:nrow(initial))
#   {
#     if(user$asian[i] == 1)
#     {
#   if((substring(user$date[i], 1, 4) == "2014"))
#   {
#     initial$is2014[j] <- 1
#     initial$count14[j] = initial$count14[j] + 1
#     index <- match(user$business_id[i], biz$business_id)
#     initial$price14[j] <- initial$price14[j] + biz$attributes.Price.Range[index]
#     initial$avgstar14[j] <- initial$avgstar14[j] + biz$stars[index]
#   }
#   if((substring(user$date[i], 1, 4) == "2015"))
#   {
#     initial$is2015[j] <- 1
#     initial$count15[j] = initial$count15[j] + 1
#     index <- match(user$business_id[j], biz$business_id)
#     initial$price15[j] <- initial$price15[j] + biz$attributes.Price.Range[index]
#     initial$avgstar15[j] <- initial$avgstar15[j] + biz$stars[index]
#   }
#   if((substring(user$date[i], 1, 4) != "2014") & (substring(user$date[i], 1, 4) != "2015"))
#   {
#     initial$is_bef[j] <- 1
#     initial$count_bef[j] = initial$count_bef[j] + 1
#     index <- match(user$business_id[i], biz$business_id)
#     initial$price_bef[j] <- initial$price_bef[j] + biz$attributes.Price.Range[index]
#     initial$avgstar_bef[j] <- initial$avgstar_bef[j] + biz$stars[index]
#   }
# }
# }
# }

user$numdate <- 0
user$numdate = as.numeric(as.Date(user$date))

initial$numdate <-0
initial$numdate = as.numeric(as.Date(initial$date))

for (j in 1:nrow(initial)) 
{
  for(i in 1:nrow(user))
  {
  if( (initial$user_id[j] == user$user_id[i]) & (user$asian[i] == 1) & (substring(user$date[i], 1, 4) == "2015"))
  {
    initial$is2015[j] <- 1
    initial$count15[j] = initial$count15[j] + 1
    index <- match(user$business_id[i], biz$business_id)
    initial$price15[j] <- initial$price15[j] + biz$attributes.Price.Range[index]
    initial$avgstar15[j] <- initial$avgstar15[j] + biz$stars[index]
  }
  if( (initial$user_id[j] == user$user_id[i]) & (user$asian[i] == 1) & (substring(user$date[i], 1, 4) == "2014"))
  {
    initial$is2014[j] <- 1
    initial$count14[j] = initial$count14[j] + 1
    index <- match(user$business_id[i], biz$business_id)
    initial$price14[j] <- initial$price14[j] + biz$attributes.Price.Range[index]
    initial$avgstar14[j] <- initial$avgstar14[j] + biz$stars[index]
  }
    if( (initial$user_id[j] == user$user_id[i]) & (user$asian[i] == 1) & (user$numdate[i] < initial$numdate[j]))
    {
      initial$user_avg_asian[j] = initial$user_avg_asian[j] + user$stars[i]
      initial$is_bef[j] <- 1
      initial$count_bef[j] = initial$count_bef[j] + 1
      index <- match(user$business_id[i], biz$business_id)
      initial$price_bef[j] <- initial$price_bef[j] + biz$attributes.Price.Range[index]
      initial$avgstar_bef[j] <- initial$avgstar_bef[j] + biz$stars[index]
    }
  }
}

initial[is.na(initial)] <- 0

for(i in 1:nrow(initial))
{
if(initial$count14[i] > 0)
{
initial$price14[i] <- initial$price14[i]/initial$count14[i]
initial$avgstar14[i] <- initial$avgstar14[i]/initial$count14[i]
}

if(initial$count15[i] > 0)
{
initial$price15[i] <- initial$price15[i]/initial$count15[i]
initial$avgstar15[i] <- initial$avgstar15[i]/initial$count15[i]
}

if(initial$count_bef[i] > 0)
{
initial$price_bef[i] <- initial$price_bef[i]/initial$count_bef[i]
initial$avgstar_bef[i] <- initial$avgstar_bef[i]/initial$count_bef[i]
initial$user_avg_asian[i] <- initial$user_avg_asian[i]/initial$count_bef[i]
}
}


initial$porpreturn <- 0

for(i in 1:nrow(initial))
{
  initial$porpreturn[i] <- initial$countreturnasian[i] / initial$howmany[i]
}

for(i in 1:nrow(initial))
{
  if(initial$user_avg_asian[i] < 1)
  {
    initial$user_avg_asian[i] <- avg2015asian
  }
}

initial$diffstar <- initial$star - initial$user_avg_asian # difference in reviewer and average review
initial$shock <- 0

for (i in 1:nrow(initial)) # shock if initial is greater than their usual
{
if(initial$diffstar[i] > 0)
{
  initial$shock[i] <-1
}
}

data$price2 <- 0
data$avgstar2 <- 0

for(i in 1:nrow(data))
{
  index <- match(data$business_id[i], biz$business_id)
  data$price2[i] <- biz$attributes.Price.Range[index]
  data$avgstar2[i] <- biz$stars[index]
}

initial$price2 <- 0
initial$avgstar2 <- 0 
initial$rating2 <- 0

for(i in 1:nrow(data)) # need to consider more than once revisit
{
  if(data$secondtime[i] == 1)
  {
  index <- match(data$user_id[i], initial$user_id)
  initial$price2[index] <- data$price2[i]
  initial$avgstar2[index] <- data$avgstar2[i]
  initial$rating2[index] <- data$stars[i]
  }
}

for(i in 1:nrow(initial))
  {
if(initial$price15[i] < 1)
{
  initial$price15[i] <- avg2015price
}
  if(initial$avgstar15[i] < 1)
  {
    initial$avgstar15[i] <- avg2015asian
  }
}

########################################
# TIME TO EXPLAIN ALL THE VARIBLES 
########################################

# returnasian = binary, 1 if they return to an asian restaurant after initial exposure
# stars = initial rating 
# price = initial price 
# avgstar = initial place's average rating
# avgrev = the reviewer's average review

# goback = binary, 1 if they return to the SAME TYPE of restaurant as initial. Eg go back to Thai if Thai was initial exposure
# countreturnasian = how many times you return to asian place
# rating2 = IF they go back to the same type of restaurant (goback == 1), the rating of the SECOND restaurant
# avgstar 2, price2 = the characteristics of the SECOND restaurant

# count15 = how many times did they go to an asian restaurant in 2015
# count14 = how many times did they go to an asian restaurant in 2014
# is___ = if the reviwer is from said state. 

# NOTE lm(rating2 ~ stars + price) is the regression: RATING2 = beta0 + beta1*stars + beta2*price + error

newinitial <- subset(initial, rating2 > 0)
newasian <- subset(initial, returnasian > 0)
newasian <- subset(initial, ((initial$count14 > 0) | (initial$count15 >0) ))

initialbef2015 <- subset(initial, (substring(initial$date, 1, 4) != "2015"))
initialbef2014 <- subset(initialbef2015, (substring(initial$date, 1, 4) != "2014"))
initialbef2014NEW <- 


initial2014 <- subset(initial, (substring(initial$date, 1, 4) == "2014") | (substring(initial$date, 1, 4) == "2013"))

probitmodel <- glm(returnasian ~ stars + user_avg_asian + price + avgstar + isPA + isNC + isAZ, family=binomial(link="probit"), data=initial2014) # regressing on if they return to asian restaurants
summary(probitmodel)

probitmodel2 <- glm(goback ~ stars + avgrev + isPA + isNC + isAZ + isNV + isWI + isQC, family=binomial(link="probit"), data=initial2014) # regressing on if they go back
summary(probitmodel2)

summary (lm(porpreturn ~ stars + avgstar + price + avgrev, data  = newasian)) # include
summary (lm(countreturnasian ~ stars + price + avgstar + avgrev, data = initial))

summary (lm(rating2 ~ stars + avgstar + avgstar2 + avgrev + isPA + isNC + isAZ + isNV + isWI + isQC, data = newinitial))
summary (lm(avgstar2 ~ rating2 + avgrev, data = newinitial))
summary (lm(avgstar ~ stars + price + avgrev, data = initial))

summary (lm(stars ~ avgstar + price + avgrev + isPA + isNC + isAZ + isNV + isWI + isQC, data = initial)) # regression on initial rating

summary (lm(count14 ~ price14 + avgstar14 + isPA + isNC + isAZ + isNV + isWI + isQC, data = initialbef2014)) 


summary (lm(count14 ~ count13 + isPA + isNC + isAZ + isNV + isWI + isQC, data = initial))
summary (lm(count13 ~ count12 + isPA + isNC + isAZ + isNV + isWI + isQC, data = initial))

sumstat <- data.frame(c(initial2014$howmany))
sumstat$avg_review <- initial2014$avgrev
avg_price <- mean(initial2014$price)
sumstat$avg_asian_review <- initial2014$user_avg_asian
sumstat$first_rating <- initial2014$stars
sumstat$visits_2014 <- initial2014$count14
sumstat$visits_2015 <- initial2014$count15

################################
# Output Tables: Stargazer
################################

write.csv(initial2014, file = "FINALasiandata.csv")
write.csv(initial, file = "FINALdata.csv")

probitmodel <- glm(goback ~ diffstar + isPA + isNC + isAZ, family=binomial(link="probit"), data=initial2014) # regressing on if they go back
summary(probitmodel) # should add 2015 quality control for each type of restaurant

probitmodel2 <- glm(returnasian ~ diffstar + isPA + isNC + isAZ, family=binomial(link="probit"), data=initial2014) # regressing on if they return to asian restaurants
summary(probitmodel2)

badfirstreview <- (lm(diffstar ~ price + avgstar + isPA + isNC + isAZ + isNV + isWI + isQC, data = initial)) # regression on initial rating
summary(badfirstreview)

habit <- lm(count15 ~ count14 + price15 + avgstar15 + isPA + isNC + isAZ, data = initial2014) # regressing 2014 going to 2015 going, controlling for states 
summary(habit)

consumption <- lm(count15 ~ diffstar + price15 + avgstar15 + isPA + isNC + isAZ, data = initial2014) # regressing 2014 going to 2015 going, controlling for states 
summary(consumption)

stargazer(sumstat)
stargazer(probitmodel, probitmodel2, title = "Two Probit Models", style = "qje")
stargazer(badfirstreview, title = "First Review is Trash")
stargazer(habit, title = "Habit Model")
stargazer(consumption, title = "Diff Star model")
