#Install R libraries

if(!require("tidyverse")) install.packages("tidyverse")
require("tidyverse")
if(!require("caret")) install.packages("caret")
require("caret")
if(!require("stringr")) install.packages("stringr")
require("stringr")
if(!require("lubridate")) install.packages("lubridate")
require("lubridate")

options(timeout = 120)

############################################################################
############################################################################
##                                                                        ##
##                            DATA SCIENCE                              ####
##                      MODULE 9: CAPSTONE PROJECT                      ####
##                                                                        ##
############################################################################
############################################################################

############################################################################
##                                                                        ##
##                           INTRODUCTION                               ####
##                                                                        ##
############################################################################

# Create Train and Final Hold-out Test Sets

# Introduction
# 
# You will use the following code to generate your datasets. Develop your 
# algorithm using the edx set. For a final test of your final algorithm, predict
# movie ratings in the final_holdout_test set as if they were unknown. RMSE will
# be used to evaluate how close your predictions are to the true values in the 
# final_holdout_test set.
# 
# Important: The final_holdout_test data should NOT be used for training,
# developing, or selecting your algorithm and it should ONLY be used for 
# evaluating the RMSE of your final algorithm. The final_holdout_test set should
# only be used at the end of your project with your final model. It may not be 
# used to test the RMSE of multiple models during model development. You should 
# split the edx data into separate training and test sets and/or use 
# cross-validation to design and test your algorithm.
# 
# Also remember that by accessing this site, you are agreeing to the terms of the
# edX Honor Code. This means you are expected to submit your own work and can be 
# removed from the course for substituting another student's work as your own.

#Create edx and final_holdout_test sets

##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

# if(!require(tidyverse)) install.packages("tidyverse", 
#                                          repos = "http://cran.us.r-project.org")
# if(!require(caret)) install.packages("caret", repos = 
#                                        "http://cran.us.r-project.org")

# Read.me file URL is the following:

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), 
                                   simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), 
                                  simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, 
                                  list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

############################################################################
##                                                                        ##
##                           DATA-CLEANING                              ####
##                                                                        ##
############################################################################

#Completeness

#This line is only run to match the original edx dataset
edx <- edx[,-c(7:10)]

#Generate a table to determine the completeness of the rating variable
table(edx[,3], useNA = "always")

#Generate a for-loop to identify the completeness of the other categorical variables
for (i in c(1:2,4:6)){
  print(sum(is.na(edx[,i])|edx[,i]==""))
}

#Consistency

#Review quicky that movieId and title are consistent
sum(duplicated(edx$movieId) == F)
sum(duplicated(edx$title) == F)

#userId and movieId are strings and not numbers
edx$userId <- as.character(edx$userId)
edx$userId <- as.character(edx$userId)

#Clarity

#Change format of timestamp variable
edx <- edx %>% 
  mutate(ratingDate = as_datetime(timestamp))

#Change format of genres variable
movie_genres <- c("Action",
                  "Adventure",
                  "Animation",
                  "Children's",
                  "Comedy",
                  "Crime",
                  "Documentary",
                  "Drama",
                  "Fantasy",
                  "Film-Noir",
                  "Horror",
                  "Musical",
                  "Mystery",
                  "Romance",
                  "Sci-Fi",
                  "Thriller",
                  "War",
                  "Western")
for(i in 1:length(movie_genres)){
  edx[,7+i] <- str_detect(edx$genres, movie_genres[i])
}
genresNames <- c("action",
                 "adventure",
                 "animation",
                 "childrens",
                 "comedy",
                 "crime",
                 "documentary",
                 "drama",
                 "fantasy",
                 "filmNoir",
                 "horror",
                 "musical",
                 "mystery",
                 "romance",
                 "sciFi",
                 "thriller",
                 "war",
                 "western")
names(edx)[8:25] <- genresNames


############################################################################
##                                                                        ##
##                           DATA-EXPLORING                             ####
##                                                                        ##
############################################################################

#Generate a table to summarize the mean, median and standard deviation of the 
#rating variable
ratingSum <- edx %>% 
  summarize(meanRating = mean(rating), 
            stdvRating = sd(rating), 
            medianRating = median(rating))
ratingSum

#Generate table to identify the proportion of each rating' score
propRating <- round(prop.table(table(edx$rating))*100,2)
for(i in 1:length(propRating)) {
  propRating[[i]] <- paste0(propRating[[i]], "%")  
}
propRating

#Generate a variable to identify movies with several genres
edx <- edx %>% 
  mutate(nroGenres = action + adventure + animation + childrens + comedy +
           crime + documentary + drama + fantasy + filmNoir + horror + musical +
           mystery + romance + sciFi + thriller + war + western)

##Generate a table to summarize the mean, median and standard deviation of the 
#rating variable
nroGenresSum <- edx %>% 
  summarize(meanNroGenres = mean(nroGenres), 
            stdvNroGenres = sd(nroGenres), 
            medianNroGenres = median(nroGenres))
nroGenresSum

#Generate table to identify the proportion of each rating' score
propNroGenres <- round(prop.table(table(edx$nroGenres))*100,2)
for(i in 1:length(propNroGenres)) {
  propNroGenres[[i]] <- paste0(propNroGenres[[i]], "%")  
}
propNroGenres

#Split the rating_date variable to extract the year and month of rating
edx <- edx %>% 
  mutate(yearRating = year(ratingDate),
         monthRating = month(ratingDate),
         yearMonthRating = paste(yearRating, monthRating, sep = "-"),
         yearRating = yearRating + monthRating/13,
         title = str_trim(title),
         movieYear = substr(title, nchar(title)-4,nchar(title)-1),
         yearDiffRating = yearRating - as.numeric(movieYear))

#Generate table to identify the proportion of each year of rating
propYearRating <- round(prop.table(table(round(edx$yearRating,0)))*100,2)
for(i in 1:length(propYearRating)) {
  propYearRating[[i]] <- paste0(propYearRating[[i]], "%")  
}
propYearRating

#Generate table to identify the proportion of each month of rating
propMonthRating <- round(prop.table(table(edx$monthRating))*100,2)
for(i in 1:length(propMonthRating)) {
  propMonthRating[[i]] <- paste0(propMonthRating[[i]], "%")  
}
propMonthRating

############################################################################
##                                                                        ##
##                            VISUALIZATION                             ####
##                                                                        ##
############################################################################

#Graph 1
#Change format of database structure to create barplot of genres as rows

#Generate a sample of the observations in order to avoid consuming too much
#computing power to generate the graphs

x <- sample(nrow(edx), 10000)

edxGraphs <- edx[x,]

edxLonger <- edxGraphs %>%
  pivot_longer(`action`:`western`)

edxLonger <- edxLonger %>%
  filter(value == T)

#Generate a histogram to see the distribution of genres and the year of rating
edxLonger <- edxLonger %>%
  mutate(yearRating = as.character(round(yearRating,0))) %>%
  rename(movieGenres = name)

edxLonger %>%
  ggplot(aes(movieGenres, fill = yearRating)) +
  geom_bar(stat = "count", position = "stack") +
  coord_flip()

#Generate a boxplot of genres and rating to see their dispersion
edxLonger %>%
  mutate(yearRating = reorder(movieGenres, rating, FUN = median)) %>%
  ggplot(aes(movieGenres, rating)) +
  geom_boxplot() +
  coord_flip() #+ jitter does not work because too many data points made graph overcrowded
#geom_jitter(width = 0.1, alpha = 0.2)

#Generate a boxplot of year and rating to see their dispersion
edxGraphs %>%
  mutate(yearRating = as.character(round(yearRating,0))) %>%
  ggplot(aes(yearRating, rating)) +
  geom_boxplot() +
  coord_flip() #+ jitter does not work - too many data points and too much computing power
#geom_jitter(width = 0.1, alpha = 0.2)



############################################################################
##                                                                        ##
##                               INSIGHTS                               ####
##                                                                        ##
############################################################################

# Reported exclusively in the Rmarkdown file

#Create a variable to capture the different rating scores after 2003
edx <- edx %>%
  mutate(before2003 = yearRating < 2003)

#Generate variables to determine user and movie effects
user_effects <- edx %>% 
  group_by(userId) %>% 
  summarize(uEffSum = n(), uEffAv = mean(rating))

movie_effects <- edx %>% 
  group_by(movieId) %>% 
  summarize(mEffSum = n(), mEffAv = mean(rating))

edx <- edx %>% 
  left_join(user_effects, by = "userId") %>% 
  left_join(movie_effects, by = "movieId")


############################################################################
##                                                                        ##
##                           MODELING APPROACH                          ####
##                                                                        ##
############################################################################

#userId and movieId are strings and not numbers
final_holdout_test$userId <- as.character(final_holdout_test$userId)
final_holdout_test$userId <- as.character(final_holdout_test$userId)

#Change format of timestamp variable
final_holdout_test <- final_holdout_test %>% 
  mutate(ratingDate = as_datetime(timestamp))

#Change format of genres variable
for(i in 1:length(movie_genres)){
  final_holdout_test[,7+i] <- str_detect(final_holdout_test$genres, 
                                         movie_genres[i])
}
names(final_holdout_test)[8:25] <- genresNames

#Generate a variable to identify movies with several genres
final_holdout_test <- final_holdout_test %>% 
  mutate(nroGenres = action + adventure + animation + childrens + comedy +
           crime + documentary + drama + fantasy + filmNoir + horror + musical +
           mystery + romance + sciFi + thriller + war + western)

#Split the rating_date variable to extract the year and month of rating
final_holdout_test <- final_holdout_test %>% 
  mutate(yearRating = year(ratingDate),
         monthRating = month(ratingDate),
         yearMonthRating = paste(yearRating, monthRating, sep = "-"),
         yearRating = yearRating + monthRating/13,
         title = str_trim(title),
         movieYear = substr(title, nchar(title)-4,nchar(title)-1),
         yearDiffRating = yearRating - as.numeric(movieYear))

#Create a variable to capture the different rating scores after 2003
final_holdout_test <- final_holdout_test %>%
  mutate(before2003 = yearRating < 2003)

#Generate variables to determine user and movie effects
user_effects <- final_holdout_test %>% 
  group_by(userId) %>% 
  summarize(uEffSum = n(), uEffAv = mean(rating))

movie_effects <- final_holdout_test %>% 
  group_by(movieId) %>% 
  summarize(mEffSum = n(), mEffAv = mean(rating))

final_holdout_test <- final_holdout_test %>% 
  left_join(user_effects, by = "userId") %>% 
  left_join(movie_effects, by = "movieId")


############################################################################
##                                                                        ##
##                              RESULTS                                 ####
##                                                                        ##
############################################################################

############################################################################
##                     Train and run de model                           ####
############################################################################

#Define the RMSE function for the models
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Define the collection of variables with the NeartoZero Variance approach
genreDummies <- edx[,c(8:25)]

nzv <- nearZeroVar(genreDummies)

col_index <- setdiff(1:ncol(genreDummies), nzv)
#length(col_index)

#Select a sample due to the large number of observations
n <- 1000
b <- 5
index <- sample(nrow(edx), n)
control <- trainControl(method = "cv", number = b, p = .9)


#Model 1 
#(Movie Genres, before2003, YearDiff, and user and movie effects)
#names(edx)
#col_index
predictorsInd <- c(7 + col_index, 31:36)

#Run the linear regression model
train_lm <- train(edx[,predictorsInd], edx[,3],
                  method = "lm",
                  trControl = control)

#Test the model and estimate the parameters with the RMSE performance metric
y_hat_lm <- predict(train_lm,
                    final_holdout_test[, predictorsInd])

results_lm <- RMSE(y_hat_lm, final_holdout_test$rating)

rmse_results <- data_frame(method = "Model 1: movieGenres + before2003 + yearDiff + user & movie effects",
                           RMSE = results_lm)


#Model 2
#(before2003, YearDiff, and user and movie effects)
predictorsInd2 <- c(31:36)

#Run the linear regression model
train_lm2 <- train(edx[,predictorsInd2], edx[,3],
                   method = "lm",
                   trControl = control)

#Test the model and estimate the parameters with the RMSE performance metric
y_hat_lm2 <- predict(train_lm2,
                     final_holdout_test[, predictorsInd2])

results_lm2 <- RMSE(y_hat_lm2, final_holdout_test$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 2: before2003 + yearDiff + user & movie effects",
                                     RMSE = results_lm2))

#Model 3
#(User and movie effects)
predictorsInd3 <- c(33:36)

#Run the linear regression model
train_lm3 <- train(edx[,predictorsInd3], edx[,3],
                   method = "lm",
                   trControl = control)

#Test the model and estimate the parameters with the RMSE performance metric
y_hat_lm3 <- predict(train_lm3,
                     final_holdout_test[, predictorsInd3])

results_lm3 <- RMSE(y_hat_lm3, final_holdout_test$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 3: user & movie effects",
                                     RMSE = results_lm3))
rmse_results %>% knitr::kable()
