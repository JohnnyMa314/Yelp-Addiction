# Johnny Ma
# 2/20/17
# Metrics B Project 
# Letterboxd Data Scraper

###########################
# Extracting List of Users
###########################

#install.packages("hash")
#install.packages("mail")

library(hash)
library(rvest)
library(stringr)
library(mail)
library(reshape2)
library(readr)
library(lfe)
library(stargazer)

#setwd("C:/Users/johnn//Dropbox/UChicago/Spring 2017/ECON 32140 - Econometrics of Social Interactions")
setwd("C:/Users/Johnny Ma/Dropbox/UChicago/Spring 2017/ECON 32140 - Econometrics of Social Interactions")

##########################################################
############ USER LEVEL CHARACTERISTICS ##################
##########################################################

################### GET USER LIST ########################


# extracting the list of users, sorted by Letterboxd "most popular." 25 per page, iterate over pages.
get_user_list <- function(startpage, endpage)
  
{
  
  users <- NULL
    
    for(i in startpage:endpage)
    {
      # reading the html, indexed by number of pages 
      
        ppl <- read_html(
                        paste(
                        c("https://letterboxd.com/people/popular/this/all-time/page/", 
                        as.character(i), "/") 
                        ,collapse = "")) 
    
      names <- html_nodes(ppl, ".table-person .person-summary .-a40")
      users <- c(users, html_attr(names, "href")) # adds on new users from each iteration
      
      #security. hey, you wanna get sued?
      chance <- runif(1, 0, .1)
      Sys.sleep(chance) 
    }
  
  users <- gsub("/", "", users)
  
  return(users)
}


print(
  'output is USERS, a list of users sorted by popularity.
  ')

########################
# User Characteristics
########################


####### FAVORITES #########

get_user_favs <- function(users)

{
  user_info <- matrix(ncol= 5, nrow=length(users))
  
  for(i in 1:length(users))
  {
    tryCatch({ # because some users don't have 4 favorites, we catch and throw out. 
    url <- read_html(
                    paste(
                    c("https://letterboxd.com/", users[i])
                    ,collapse = ""))
    
    # favorites 
    select <- html_nodes(url, "#favourites .-horizontal .react-component") 
    film <- html_attr(select, "data-target-link")
    film_name <- html_attr(select, "data-film-name")
    
    # messy, but needed in cases where not up to 4 favorites 
    user_info[i,1] <- users[i]
    user_info[i,2] <- film[1]
    user_info[i,3] <- film[2]
    user_info[i,4] <- film[3]
    user_info[i,5] <- film[4]
    
    #security. hey, you wanna get sued?
    chance <- runif(1, 0, .1)
    Sys.sleep(chance)
    
    print(users[i])
    user_info <- gsub("/", "", gsub("/film/", "", user_info))
    
  }
  , error=function(e){cat("ERROR :",conditionMessage(e), users,  "\n")}) # printing user errors
  }
  
  return(user_info)
}


######################################
# Get Users Friends
######################################

get_user_friends <- function(users, num_friends)
  
{
    user_friend_list <- NULL
    
    for(i in 1:length(users))
    {
      user_friends <- NULL
      j <- 1
      stop <- FALSE
      
      while(stop == FALSE) # while the pages still have data on them.
        {
          network <- read_html(
            paste(
              c("https://letterboxd.com/", users[i], "/following/page/", 
                as.character(j), "/") 
              ,collapse = "")) 
          
          people <- html_nodes(network, ".name")
          
          vec_people <- as.vector(html_attr(people, "href"))
          user_friends <- c(user_friends, vec_people)
          #security. hey, you wanna get sued?
          chance <- runif(1, 0, .1)
          Sys.sleep(chance) 
          
          j = j + 1 # increasing index
          
          if(identical(vec_people,character(0)) | j > 1) # checking if ratings is filled/page is empty, list of movies completed.
          {
            stop <- TRUE
          }
        }
     
      length(user_friends) <- num_friends
      user_friend_list <- rbind(user_friend_list, user_friends)
    }
    
    user_friend_list <- gsub("/", "", user_friend_list)
    
    return(user_friend_list)
}

######################################
# Selecting List of Films off of User
######################################
  

# makes a 2:n matrix of movies [1] and ratings [2] for each user passed through.

get_user_ratings <- function(username){
  
  user_ratings <- NULL
  i <- 1
  stop <- FALSE
  
while(stop == FALSE) # while the pages still have data on them.
  {
      flix <- read_html(
                        paste(
                        c("https://letterboxd.com/", username, "/films/page/", 
                        as.character(i), "/") 
                        ,collapse = "")) 
      
      s_rating <- html_nodes(flix, ".-grid .poster-container")
      s_film   <- html_nodes(s_rating, ".film-poster")
      
      ratings <- html_attr(s_rating, "data-owner-rating")
      films <- html_attr(s_film, "data-target-link")
      
      user_ratings_temp <- rbind(films, ratings)
      user_ratings <- cbind(user_ratings, user_ratings_temp)
      
      #security. hey, you wanna get sued?
      chance <- runif(1, 0, .1)
      Sys.sleep(chance) 

      i = i + 1 # increasing index
      
      if(identical(ratings,character(0))) # checking if ratings is filled/page is empty, list of movies completed.
        {
          stop <- TRUE
        }
  }
  
  user_ratings[2,] <- as.numeric(user_ratings[2,])
  user_ratings[1,] <- gsub("/", "", gsub("/film/", "", user_ratings[1,]))
  return(user_ratings)
}














##########################################################
############ FILM LEVEL CHARACTERISTICS ##################
##########################################################


###############################
# SELECTING LIST OF ALL FILMS
###############################


# this page was previously a huge nuisance for some obscure reason that Erik Hupp (bless his soul)
# later pointed out was related to AJAX, a javascript loader in html/
# turns out the original "popular films" page is populated with a script based on popularity.
# the current url is just the raw data it pulls from, infinitely easiser to use.

get_film_list <- function(pages)
{
  
  l_films <- NULL 
  
  for(i in 1:pages)
  {
    flix <- read_html(
                      paste(
                      c("https://letterboxd.com/films/ajax/popular/size/small/page/", 
                      as.character(i), "/") 
                      ,collapse = "")) 
    
    selects <- html_nodes(flix, ".-grid .film-poster")
    l_films <- c(l_films, html_attr(selects, "data-target-link"))
    
    chance <- runif(1, 0, .1)
    Sys.sleep(chance) 
  }
  
  l_films <- gsub("/", "", gsub("/film/", "", l_films))
  
  return(l_films)
}

#######################
# Film Characteristics
#######################

# information collected is:
# title, year, director, actor1, actor2, runtime, avg_rating, watches, genres, language.
# Pulls all from the film page. 

get_film_info <- function(films)
{
  
  film_info <- NULL
  
  for(i in 1:length(films))
  {
    print(films[i])
    # reading film page
    page <- read_html(
                      paste(
                      c("https://letterboxd.com/film/", 
                      as.character(films[i]), "/")
                      ,collapse = "")) 
    
    # getting year and director
    head <- html_nodes(page, "#featured-film-header p a") # location of year and director
    info <- html_attr(head, "href")
    
    year <- gsub("/", "", gsub("/films/year/", "", info[1]))
    director <- gsub("/", "", gsub("/director/", "", info[2]))
    
    # getting all actors/actresses
    cast <- html_nodes(page, ".cast-list a")
    actor <- html_attr(cast, "href")
    
    actors <- gsub("/", "", gsub("/actor/", "", c(actor[1],actor[2]))) # only take the first two credited actors 
    
    # get runtime
    runtime <- html_nodes(page, ".text-footer")
    runtime <- gsub("\\n|\\t", "", gsub("mins.*", "", html_text(runtime)))
    runtime <- as.integer(gsub("^\\s+|\\s+$", "", runtime))
    
    # getting the average rating
    avg <- html_nodes(page, ".display-rating")
    avg_rating <- as.double(html_text(avg))
    
    # getting the number of watches 
    watches <- html_nodes(page, "#poster-col .icon-watched")
    views <- html_text(watches)
    
    if(grep("k", views) > 0)
    {
      views <- 1000*as.double(gsub("k", "", views))
    }
    
    # reading in the genres page
    page <- read_html( 
                      paste(
                      c("https://letterboxd.com/film/", 
                      as.character(films[i]), "/genres/")
                      ,collapse = "")) 
    
    body <- html_nodes(page, "#tab-genres .capitalize a") # location of genres in "genres" page
    genres <- html_text(body, "a")
    
    page <- read_html( 
                    paste(
                    c("https://letterboxd.com/film/", 
                    as.character(films[i]), "/details/")
                    ,collapse = "")) 
    
    # getting the language of the film
    lang <- html_nodes(page, ".text-sluglist:nth-child(6) p")
    language <- gsub("\\s.*", "", gsub("\\n|\\t", "",  html_text(lang)[2]))
    
    
    # Putting it all together
    info_vec <- c(films[i], year, runtime, avg_rating, views, language, director, actors, genres[1], genres[2], genres[3], genres[4]) # the vector of final info

    chance <- runif(1, 0, .1)
    Sys.sleep(chance) 
    
    film_info <- rbind(film_info, info_vec)
  }
  
  
  return(film_info)
}












##########################################################
############ COMBINING USERS AND FILMS ###################
##########################################################

combine_user_film <- function(users, films)

{
  
  # make a matrix that can contain our data
  data <- matrix(ncol =  length(films), nrow = length(users)) 
  colnames(data) <- films # name the column film names
  
  rownames(data) <- users
  
  # the hash key that matches films from the list of films to the index of the colname name where it is in the matrix.
  h <- hash(colnames(data)[1:ncol(data)], lapply(seq_len(ncol(data))-1, function(i) i+1))
  
    for(i in 1:length(users))
    {
      tryCatch({ # because /users/ links to a staff account. Also http errors.
        
      
      # long step
      ratings <- get_user_ratings(data[i,1]) # getting user ratings from a user, populating 2:n vector.
      
      for(j in 1:ncol(ratings)) 
      {
        name <- ratings[1,j] # name of film, checking if it appears in hash
        if(has.key(name, h))
        {
          data[i,h[[name]]] = ratings[2,j] # if its in the hash, place rating into appropraite row-column for that user and that movie.
        }
      }
      }
      , error=function(e){cat("ERROR :",conditionMessage(e),data[i,1],  "\n")}) # printing error
      
    }
  return(data)
}
# Get ratings of data[i,1]
# Iterate through ratings of data[i,1], by movie
# Put rating in cell : data[i,h[movie_name]] = rating



################################
# ABOVE OR BELOW AVERAGE MOVIE
################################

# What this does is gets the entire rating and watch behavior of a user
# Next, it calculates the average for that user.
# Finally, it fills the 'films' provided by 1 if their film rating was above and 0 if below, NA if not watched.

get_surprise <- function(users, films)
{

  data <- matrix(ncol = (length(films)), nrow = length(users)) 
  colnames(data) <- films # name the column film names
  rownames(data) <- users # populate first row with users
  
  # the hash key that matches films from the list of films to the index of the colname name where it is in the matrix.

  h <- hash(colnames(data)[1:ncol(data)], lapply(seq_len(ncol(data))-1, function(i) i+1))
  
  for(i in 1:length(users))
  {
    print(users[i])
    tryCatch({ # because /users/ links to a staff account. Also http errors.
      
      # long step
      ratings <- get_user_ratings(users[i]) # getting user ratings from a user, populating 2:n vector.
      avg_rating <- mean(as.numeric(ratings[2,])) # getting average rating over all movies from a single user
      
      for(j in 1:ncol(ratings)) 
      {
        name <- ratings[1,j] # name of film, checking if it appears in hash
        if(has.key(name, h))
        {
          data[i,h[[name]]] <- (as.numeric(ratings[2,j]) > avg_rating) # if its in the hash, check if rating is above average. 
        }
      }
    }
    , error=function(e){cat("ERROR :",conditionMessage(e),users[i], name, avg_rating, "\n")}) # printing error
  }
  
  return(data)
}

######################################
# Share of Friends
######################################

# using surprise_matrix so we dont have to calculate the surprise_matrix a ton of times.
# this calculates the share of friends who rated a movie above their own average. 
# this function needs the friend list for each user as well.
# it adds one if friends rated above, then divides by total friends who have seen the film and given a rating.

make_share_surprise <- function(users, films, surprise_matrix, friend_matrix)
{
  data <- matrix(ncol = (length(films)), nrow = length(users)) 
  colnames(data) <- films # name the column film names
  rownames(data) <- users
  
  # iterating over users.
  for(i in 1:length(users))
  {
    print(users[i])
    print(i)
    # getting a user's friends.
    user_friends <- friend_matrix[i,]
    
    # doing this over the entire vector of films at once. Greatly improved speeds. Thank god.
      num_friends <- 0
      count <- 0
      watch_count <- 0
      
      # the hash of users in the possible friend network
      g <- hash(rownames(data)[1:nrow(data)], lapply(seq_len(nrow(data))-1, function(i) i+1))

      # iterating over friends
      for(k in 1:length(user_friends))
      {
        name <- as.character(user_friends[k])
        
        # getting the location of the friend if they are in user-list
        if(has.key(name, g))
        {
          loc <- g[[name]]
          
            #TRUE if above average, FALSE if below 
            surprise <- as.logical(surprise_matrix[loc,])
            plus <- as.numeric(surprise)
            
            # add if above 0, or the friend has seen the movie.
            watch_counter <- as.numeric(!is.na(plus))   
            watch_count <- watch_count + watch_counter
            
            # since you can't add NAs, just making all the non-1s 0s so the adding doens't affect. 
            plus[is.na(plus)] <- 0
            # adding a count if friend-film is above average
            count <- count + plus
          }
      }
        # if the count is not empty (meaning no friends are in the surprise matrix or no friends have seen the film)
          if(!identical(count, numeric(0)))
          {
          share <- count/watch_count
          data[i,] <- share
          
    }
  }
  return(data)
}


#######################################


#######################################
# Getting Watching Behavior
#######################################

# This is similar in structure to the get_ratings, except for the diary
# collects the film [1,] and the date watched [2,]

get_diary <- function(username)
{
  user_diary <- NULL
  page <- 1
  stop <- FALSE
  
  while(stop == FALSE) # while the pages still have data on them.
  {
    print(page)
    
    diary <- read_html(
      paste(
        c("https://letterboxd.com/", username, "/films/diary/page/", 
          as.character(page), "/") 
        ,collapse = ""))
    
    # getting dates for each diary entry
    dates <- html_nodes(diary, ".diary-day a")
    date_vec <- html_attr(dates, "href")
    date_vec <- gsub(".*/for/", "", date_vec)
    
    # getting movies for each diary entry
    movies <- html_nodes(diary, ".prettify a")
    film_vec <- html_attr(movies, "href")
    film_vec <- gsub("/1/", "", film_vec)
    film_vec <- gsub("/2/", "", film_vec)
    film_vec <- gsub("/3/", "", film_vec)
    
    film_vec <- gsub("/", "", gsub(".*/film/", "", film_vec))
    
    if(identical(film_vec,character(0))) # checking if ratings is filled/page is empty, list of movies completed.
    {
      break
    } 
    
    # dates and movies are always lined up 1-1
    diary_page <- rbind(film_vec, date_vec)
    
    user_diary <- cbind(user_diary, diary_page)
    page <- page + 1
    
    chance <- runif(1, 0, .05)
    Sys.sleep(chance) 
  }
  return(user_diary)
}

#########################
# Making the Watch Range
#########################

# This takes in the users and films and a range of dates, in string format.
# Using the get_diary function over each user, it fills the 'films' with 
# 2 if it falls within the start-end, 1 if not but is watched outside, and 0 if not watched at all.
# Later these are transformed into meaningful variables.

make_diary_range <- function(users, films, startdate, enddate)
{
  
  data <- matrix(ncol = (length(films)), nrow = length(users))
  colnames(data) <- films # name the column film names
  rownames(data) <- users
  
  # the Date class objects of the string-dates.
  start <- as.Date(startdate)
  end <- as.Date(enddate)
  
  # the hash on films, identifying the columns.
  h <- hash(colnames(data)[1:ncol(data)], lapply(seq_len(ncol(data))-1, function(i) i+1))
  
  for(i in 1:length(users))
  {
    diary <- get_diary(users[i])
    
    for(j in 1:ncol(diary))
    {
      name <- diary[1,j] # name of film, checking if it appears in hash
      if(has.key(name, h))
      {
        data[i,h[[name]]] <- diary[2,j] # if its in the hash, put the date in
      }
    }
    
    # replacing all the string-dates with useable Date class variables.
    data[i,!is.na(data[i,])] <- strftime(as.Date(data[i,!is.na(data[i,])]), format = "")
    
    # x_t+1 < x_t returns 1 for this ifelse
    # this replaces the date with "2" if it falls between the specified time period, and 1 otherwise. NAs (not watched films) are left alone.
    
    x <- data[i,!is.na(data[i,])]
    strt <- ifelse(strptime(start, format = "%Y-%m-%d") < strptime(x, format = "%Y-%m-%d"),TRUE,FALSE)
    endr <- ifelse(strptime(end, format = "%Y-%m-%d") > strptime(x, format = "%Y-%m-%d"),TRUE,FALSE)
    data[i,!is.na(data[i,])] <- strt + endr
    
    # makes all the NAs (not seen films) equal to 0.
    data[i,is.na(data[i,])] <- 0
  }
}

###################
# MAKE PANEL DATA
###################

# The idea here is to make the dummies for films (which can take the form of a diagonal)
# then the dummies for users, which is just the watch behavior of all the people

make_panel_dummies <- function(users, films, date_data, share_data)
{
  
  # this is the matrix of film dummies. Due to construction of the data, the panel is the length of all the films
  # and since the films will always be sorted in the same order. we can just append a diagonal across the panel
  # so every movie is matched column-row wise to its respective title.
  
  matrix <- matrix(0, ncol = length(films), nrow = length(films)) 
  rownames(matrix) <- films
  colnames(matrix) <- films
  diag(matrix) <- 1

  data <- matrix(NA, ncol = length(films) + length(films) + 2, nrow = length(films) * length(users))
  data <- as.data.frame(data)
  watched <- NULL
  hold <- NULL
  index_rows <- c(1,length(films))
  
  for(i in 1:length(users))
  {
      print(users[i])
      probit <- date_data[i,]
      shares <- share_data[i,]
      
      # user fixed effects, which is the "taste", 
      # a matrix of 1 if watch a movie, 0 if not watch. Based off of the date_data matrix.
      # replicate 
      # taste <- get_user_ratings(users[i])
      # taste_mat <- do.call("rbind", replicate(length(taste), watch, simplify = FALSE))
      
      watched <- as.numeric(date_data[i,] > 0)
      watch_mat <- do.call("rbind", replicate(length(watched), watched, simplify = FALSE))
    
      test <- as.data.frame(cbind(t(probit), t(shares)))
      colnames(test) <- c("watch", "shares")
      
      hold <- cbind(test, matrix, watch_mat)
      hold <- data.frame(lapply(hold, function(x) as.numeric(as.character(x))))
      
      data[index_rows[1]:index_rows[2],] <- hold
      index_rows <- index_rows + length(films)
  }
  return(data)
 
  
  return(data)
  # changing all factors into integers
  dat2 <- data.frame(lapply(data, function(x) as.numeric(as.character(x))))

  #drop the observations of movies that were already watched. 
  # this is done because we're interested in looking at probability of watching a film that hasn't been watched
  # the effect of friends' shares on choice to watch films from the pool of not watched films
  # in a specific time period. 
  
  # remove all rows with 1
  
  dat2 <- dat2[dat2[,1] != 1,]
  
  # replace all the 2s with 1s, except for the NA values
  # what does NA mean? not really sure how they pop up besides for people who have never seen anything
  
  dat2[dat2[,1] == 2 && !is.na(dat2[,1]),1] <- 1
  
  
  return(dat2)  
}

###################
# FACTOR VERSION
###################

# a much simplier version (that works better lol) that just displays the user and film
# these create fator levels that can be seen as user and film fixed effects

make_panel_factors <- function(users, films, date_data, share_data)
{

  data <- matrix(NA, ncol = 4, nrow = length(films) * length(users))
  data <- as.data.frame(data)
  watched <- NULL
  hold <- NULL
  index_rows <- c(1,length(films))
  
  for(i in 1:length(users))
  {
    print(users[i])
    probit <- date_data[i,]
    shares <- share_data[i,]
    
    # user fixed effects, which is the "taste", 
    # a matrix of 1 if watch a movie, 0 if not watch. Based off of the date_data matrix.
    # replicate 
    # taste <- get_user_ratings(users[i])
    # taste_mat <- do.call("rbind", replicate(length(taste), watch, simplify = FALSE))
    
    hold <- cbind(t(probit), t(shares), users[i], films)

    data[index_rows[1]:index_rows[2],] <- hold
    index_rows <- index_rows + length(films)
  }
  return(data)
  
  
  return(data)
  # changing all factors into integers
  dat2 <- data.frame(lapply(data, function(x) as.numeric(as.character(x))))
  
  #drop the observations of movies that were already watched. 
  # this is done because we're interested in looking at probability of watching a film that hasn't been watched
  # the effect of friends' shares on choice to watch films from the pool of not watched films
  # in a specific time period. 
  
  # remove all rows with 1
  
  dat2 <- dat2[dat2[,1] != 1,]
  
  # replace all the 2s with 1s, except for the NA values
  # what does NA mean? not really sure how they pop up besides for people who have never seen anything
  
  dat2[dat2[,1] == 2 && !is.na(dat2[,1]),1] <- 1
  
  
  return(dat2)  
}

################################################
############### ANALYSIS #######################
################################################

###################
# Reading in Data
###################

# The importing of the data. 
# read_csv is weird in that it spits out the rowanmes into the first row. I just get rid of this.

surprise_matrix <- as.data.frame(read_csv("surprise_matrix.csv"))
friends <- as.data.frame(read_csv("friends.csv", col_names = TRUE))

films <- colnames(surprise_matrix) 
users <- as.data.frame(read_csv("5000users.csv", col_names = FALSE))[,1]

watch <- as.data.frame(read_csv("5000diary.csv"))
rownames(watch) <- watch[,1]
watch <- watch[,2:ncol(watch)]

shares <- as.data.frame(read_csv("shares_data.csv"))
rownames(shares) <- shares[,1]
shares <- shares[,2:ncol(shares)]

################################
# CUTS
################################

# If I need to cut the data for computation reasons, 

num_users <- 500
num_films <- 100

shares_cut <- shares[1:num_users,1:num_films]
watch_cut <- watch[1:num_users,1:num_films]
friends_cut <- friends[1:num_users,1:num_films]
users_cut <- users[1:num_users]
films_cut <- films[1:num_films]

#######################################
# Creating the factor version.
#######################################

long_factor <- make_panel_factors(users = users_cut, films = films_cut, date_data = watch_cut, share_data = shares_cut)

# removing the 1s, which are not interesting in this context of looking at films that haven't been watched
long_factor <- long_factor[long_factor[,1] != 1,]
test <-  long_factor[,1]

# replacing the 2s with 1s to make it interpretable
test[test == 2] <- 1
long_factor[,1] <- test

colnames(long_factor) <- c("watch", "shares", "user", "film")

#lm(watch ~ shares, data = long_factor)

summary(both_factors)

# the four possible combinations of controls.
no_factor <- felm(watch ~ shares, data = long_factor)
user_factor <- felm(watch ~ shares| user , data = long_factor)
film_factor <- felm(watch ~ shares| film, data = long_factor)
both_factors <- felm(watch ~ shares| user + film, data = long_factor)

stargazer(no_factor, user_factor, film_factor, both_factors, 
          add.lines = list(c("Fixed effcts?", "No", "User", "Film", "Both")),
          title = "Multiple Group Fixed Effects Model",
          dep.var.labels = "Watched in 2017 If Not Seen Already")
#######################
# LEAVING OUT DUMMIES
#######################

no_dummies <- lm(watch ~ shares, data = long)

##################
film_dummies <- paste("V", 3:(length(films_cut)+3-1), sep = "")
formula1 <- as.formula(paste("watch ~ shares +", paste(film_dummies, collapse = "+")))

with_film_dummies <- lm(formula1, data = long)

##################
user_dummies <- paste("V", 303:(303 + length(films_cut)-1), sep = "")
formula2 <- as.formula(paste("watch ~ shares +", paste(user_dummies, collapse = "+")))

with_user_dummies <- lm(formula2, data = long)


########################
# ANALYSIS 
###########################

#shares_data <- make_share_surprise(users = users, films = films, surprise_matrix = surprise_matrix, friend_matrix = friends)
# #write.csv(shares_data, "shares_data.csv")
# 


# colnames(long)[3:ncol(long)]
# 
# library(dplyr)
# library(readr)
# df <- list.files(full.names = TRUE) %>% 
#   lapply(read_csv) %>% 
#   bind_rows
# 
# rownames(df) <- users
# df <- df[,2:ncol(df)]
# write.csv(df, "5000diary.csv")

#okay so there is one guy who is "NA" in the data set, so I just replaced him with myself. This needs to be checked and fixed in future runs
################
# RUN OVERNIGHT
###############
# 
# user_list <- get_user_list(pages = 500)
# film_list <- get_film_list(pages = 40)
# 
# system.time(
#   data <- combine_user_film(user_list, film_list))
# 
# write.csv(data, file = "Letterboxd6.csv")
# sendmail("johnnyma@uchicago.edu", subject="Notification from R", message="Data Scrapped!", password="rmail")
# 
# 
# system.time(
#   film_info <- get_film_info(films = film_list))
# write.csv(film_info, file = "film_info.csv")
# 
# system.time(
#   user_info <- get_user_info(users = user_list)
# )
# write.csv(user_info, file = "user_info2.csv")
# sendmail("johnnyma@uchicago.edu", subject="Notification from R", message="COMPLETE", password="rmail")
# 

#################################################
# Tests on User FROSTMAGMA (me)
##################################################
# 
# user <- get_user_list(startpage = 1, endpage = 200)
# info <- get_user_favs(user)
# favs <- get_user_favs(user)
# 
# #surprise_matrix <- get_surprise(users = user, films = film)
# surprise_matrix <- read.csv("surprise_matrix.csv")
# friends <- read.csv("friends.csv")
# rownames(surprise_matrix)
# 
# shares_data <- make_share_surprise(users = user, films = film, surprise_matrix)\
# write.csv(probit, "probit.csv")
# 
# film <- "the-social-network"
# film <- get_film_list(pages = 10)
# f_info <- get_film_info(film)
# colnames(f_info) <- c("title", "year", "runtime", "avg_rating", "views", "language", "director", "actor1", "actor2", "genre1", "genre2", "genre3", "genre4")
# View(f_info)
