#Data Visualization
library(ggplot2)
library(dplyr)
#TOP 10 MOVIES BASED ON NUMBER OF REVIEWS
total_movies_up <- total_movies %>%
  arrange(desc(CINEMA_NUMBER_OF_REVIEWS))
top10 <- head(total_movies_up, 10)
top_10_movies_by_review=ggplot(top10, aes(x = reorder(CINEMA_NAME, CINEMA_NUMBER_OF_REVIEWS), y = CINEMA_NUMBER_OF_REVIEWS)) +
  geom_bar(stat = "identity") +
  xlab("Cinema Name") +
  ylab("Total Reviews") +
  ggtitle("Top 10 Cinemas based on Number of Reviews")+  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#TOP 10 MOVIES BASED ON POPULARITY
total_movies_up_2 <- total_movies %>%
  arrange(desc(CINEMA_POPULARITY))
top10_2 <- head(total_movies_up_2, 10)
top_10_movies_by_popularity=ggplot(top10_2, aes(x = reorder(CINEMA_NAME, CINEMA_POPULARITY), y = CINEMA_POPULARITY)) +
  geom_bar(stat = "identity") +
  xlab("Cinema Name") +
  ylab("Popularity") +
  ggtitle("Top 10 Cinemas based on Popularity")+  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#TOP 10 MOVIES BASED ON NUMBER OF REVIEWS AND LENGTH

mydata_new <- total_movies %>%
  mutate(CINEMA_NAME_LENGTH = paste(CINEMA_NAME, "(", CINEMA_LENGTH, "min)", sep = " ")) %>%
  select(CINEMA_NAME_LENGTH, CINEMA_NUMBER_OF_REVIEWS)

mydata_grouped <- mydata_new %>%
  group_by(CINEMA_NAME_LENGTH) %>%
  summarise(cinema_num_of_review = sum(CINEMA_NUMBER_OF_REVIEWS)) %>%
  arrange(desc(cinema_num_of_review))

top10_3 <- head(mydata_grouped, 10)

top_10_based_on_name_length=ggplot(top10_3, aes(x = CINEMA_NAME_LENGTH, y = cinema_num_of_review)) +
  geom_bar(stat = "identity") +
  xlab("Cinema Name and Length") +
  ylab("Total Reviews") +
  ggtitle("Top 10 Cinemas based on Cinema Length and Number of Reviews") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#RATING RANGES WITH FREQUENCIES

mydata=data.frame(total_movies$CINEMA_RATING)
bins <- seq(8, 10, by=1)

labels <- c("8-9", "9-10")
mydata$bin <- cut(mydata$total_movies.CINEMA_RATING, breaks=bins, labels=labels)

mydata$bin <- as.numeric(mydata$bin)

rating_frequency=ggplot(mydata, aes(x=bin)) +
  geom_histogram() +
  xlab("Ratings") +
  ylab("Frequency") +
  ggtitle("Frequency based on Cinema Ratings") +
  scale_x_continuous(breaks=c(1,2), labels=c("8-9", "9-10"))

mydata_2=data.frame(total_movies$CINEMA_LENGTH)
bins_2 <- seq(50, 250, by=50)

labels_2 <- c("50-100", "100-150","150-200","200-250")
mydata_2$bin <- cut(mydata_2$total_movies.CINEMA_LENGTH, breaks=bins_2, labels=labels_2)

mydata_2$bin <- as.numeric(mydata_2$bin)

movie_length_frequency=ggplot(mydata_2, aes(x=bin)) +
  geom_histogram() +
  xlab("Length") +
  ylab("Frequency") +
  ggtitle("Frequency based on Cinema Length") +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("50-100", "100-150","150-200","200-250"))
