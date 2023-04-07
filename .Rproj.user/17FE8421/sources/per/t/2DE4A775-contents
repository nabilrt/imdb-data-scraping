
setwd("D:/tes/Scrape")

total_movies=read.csv("movie_scraped_data_part_1.csv")
total_movies

#Handling Missing Data
sum(total_movies$CINEMA_POPULARITY=="")
total_movies <- total_movies[!(total_movies$CINEMA_POPULARITY == ""), ]

#Data Transformation
#Replacing Number of Reviews with Actual Numbers
for (i in 1:length(total_movies$CINEMA_NUMBER_OF_REVIEWS)){
  if(grepl("M",total_movies$CINEMA_NUMBER_OF_REVIEWS[i]))
{
    total_movies$CINEMA_NUMBER_OF_REVIEWS[i]=gsub("M","",total_movies$CINEMA_NUMBER_OF_REVIEWS[i])
    total_movies$CINEMA_NUMBER_OF_REVIEWS[i]=(as.numeric(total_movies$CINEMA_NUMBER_OF_REVIEWS[i])*1000000)
    
  }else if(grepl("K",total_movies$CINEMA_NUMBER_OF_REVIEWS[i])){
    total_movies$CINEMA_NUMBER_OF_REVIEWS[i]=gsub("K","",total_movies$CINEMA_NUMBER_OF_REVIEWS[i])
    total_movies$CINEMA_NUMBER_OF_REVIEWS[i]=(as.numeric(total_movies$CINEMA_NUMBER_OF_REVIEWS[i])*1000)
  }
}

#Converting Hour and Minute string into numeric minutes
convert_to_minutes <- function(time_str) {
  if (grepl("h", time_str)) {
    time_vec <- strsplit(time_str, "h ")[[1]]
    hours <- as.numeric(gsub("h", "", time_vec[1]))
    minutes <- ifelse(length(time_vec) == 2, as.numeric(gsub("m", "", time_vec[2])), 0)
    total_minutes <- (hours * 60) + minutes
  } else {
    total_minutes <- as.numeric(gsub("m", "", time_str))
  }
  return(total_minutes)
}

for (i in 1:length(total_movies$CINEMA_LENGTH)){
  total_movies$CINEMA_LENGTH[i]=convert_to_minutes(total_movies$CINEMA_LENGTH[i])
}

#Scaling the Popularity values to be in 0 to 1
total_movies$CINEMA_POPULARITY=as.numeric(gsub(",","",total_movies$CINEMA_POPULARITY))

install.packages("dplyr")
library(scales)

total_movies$CINEMA_POPULARITY <- rescale(total_movies$CINEMA_POPULARITY)

install.packages("rlang")

library(dplyr)
total_movies <- total_movies %>% 
  mutate(CINEMA_POPULARITY = round(CINEMA_POPULARITY, 3))

#Properly Pre-processed Data set
write.csv(total_movies, "cleaned_data.csv", row.names = FALSE)

#Descriptive Statistics
total_movies$CINEMA_LENGTH=as.numeric(total_movies$CINEMA_LENGTH)
total_movies$CINEMA_NUMBER_OF_REVIEWS=as.numeric(total_movies$CINEMA_NUMBER_OF_REVIEWS)

summary_df=summary(total_movies)

#Correlation between variables
print(paste0("CINEMA RATING AND POPULARITY - ",cor(total_movies$CINEMA_RATING,total_movies$CINEMA_POPULARITY)))
print(paste0("CINEMA RATING AND LENGTH ",cor(total_movies$CINEMA_RATING,total_movies$CINEMA_LENGTH)))
print(paste0("CINEMA RATING AND NUMBER OF REVIEWS - ",cor(total_movies$CINEMA_RATING,total_movies$CINEMA_NUMBER_OF_REVIEWS)))
print(paste0("CINEMA POPULARITY AND LENGTH - ",cor(total_movies$CINEMA_POPULARITY,total_movies$CINEMA_LENGTH)))
print(paste0("CINEMA POPULARITY AND NUMBER OF REVIEWS - ",cor(total_movies$CINEMA_POPULARITY,total_movies$CINEMA_NUMBER_OF_REVIEWS)))
print(paste0("CINEMA LENGTH AND NUMBER OF REVIEWS - ",cor(total_movies$CINEMA_LENGTH,total_movies$CINEMA_NUMBER_OF_REVIEWS)))


uncleaned_movies=read.csv("movie_scraped_data_part_1.csv")



