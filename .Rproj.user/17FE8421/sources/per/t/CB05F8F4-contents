library(rvest)

base_url <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"

data_list <- list()

url <- paste0(base_url, num_pages)

page <- read_html(base_url)

for (i in 1:250){
  node=paste("#main > div > span > div > div > div.lister > table > tbody > tr:nth-child(",") > td.titleColumn > a",sep = as.character(i))
  movie_link <- page %>% html_nodes(node) %>% html_attr("href") 
  movie_link=paste("https://www.imdb.com",movie_link,sep = "")
  movie_name<-page %>% html_nodes(node) %>% html_text()
  page_data <- data.frame(MOVIE_NAME = movie_name, MOVIE_LINK = movie_link)
  data_list[[i]] <- page_data
}

final_data <- do.call(rbind, data_list)
write.csv(final_data, "scraped_data.csv", row.names = FALSE)

movie_data=list()

val = 1

while (val <= 150)
{
  cinema_name=read_html(final_data$MOVIE_LINK[val]) %>% html_nodes("#__next > main > div > section.ipc-page-background.ipc-page-background--base.sc-f9e7f53-0.ifXVtO > section > div:nth-child(4) > section > section > div.sc-385ac629-3.kRUqXl > div.sc-2971dade-0.YVoIO > h1 > span") %>% html_text()
  cinema_rating=read_html(final_data$MOVIE_LINK[val]) %>% html_nodes("#__next > main > div > section.ipc-page-background.ipc-page-background--base.sc-f9e7f53-0.ifXVtO > section > div:nth-child(4) > section > section > div.sc-385ac629-3.kRUqXl > div.sc-c6e5278a-0.fmlsaT.sc-2971dade-1.kpsuyq > div > div:nth-child(1) > a > span > div > div.sc-e457ee34-0.kqTStR > div.sc-e457ee34-2.liUcIh > span.sc-e457ee34-1.squoh")
  cinema_rating=cinema_rating %>% html_text()
  cinema_popularity=read_html(final_data$MOVIE_LINK[val]) %>% html_nodes("#__next > main > div > section.ipc-page-background.ipc-page-background--base.sc-f9e7f53-0.ifXVtO > section > div:nth-child(4) > section > section > div.sc-385ac629-3.kRUqXl > div.sc-c6e5278a-0.fmlsaT.sc-2971dade-1.kpsuyq > div > div:nth-child(3) > a > span > div > div.sc-8bc809cd-0.bgcQfg > div.sc-8bc809cd-1.eFIYaF") 
  cinema_popularity=cinema_popularity%>% html_text()
  cinema_length=read_html(final_data$MOVIE_LINK[val]) %>% html_nodes("#__next > main > div > section.ipc-page-background.ipc-page-background--base.sc-f9e7f53-0.ifXVtO > section > div:nth-child(4) > section > section > div.sc-385ac629-3.kRUqXl > div.sc-2971dade-0.YVoIO > ul > li:nth-child(3)")
  cinema_length=cinema_length%>% html_text()
  cinema_num_of_review=read_html(final_data$MOVIE_LINK[val]) %>% html_nodes("#__next > main > div > section.ipc-page-background.ipc-page-background--base.sc-f9e7f53-0.ifXVtO > section > div:nth-child(4) > section > section > div.sc-385ac629-3.kRUqXl > div.sc-c6e5278a-0.fmlsaT.sc-2971dade-1.kpsuyq > div > div:nth-child(1) > a > span > div > div.sc-e457ee34-0.kqTStR > div.sc-e457ee34-3.frEfSL")
  cinema_num_of_review=cinema_num_of_review%>%html_text()
  print(val)
  if(identical(cinema_popularity, character(0))){
    cinema_popularity=""
    mov_data=data.frame(CINEMA_NAME=cinema_name,CINEMA_RATING=cinema_rating,CINEMA_POPULARITY=cinema_popularity,CINEMA_LENGTH=cinema_length,CINEMA_NUMBER_OF_REVIEWS=cinema_num_of_review)
    movie_data[[val]]<-mov_data
    
  }else if(identical(cinema_name, character(0))){
    cinema_name=""
    mov_data=data.frame(CINEMA_NAME=cinema_name,CINEMA_RATING=cinema_rating,CINEMA_POPULARITY=cinema_popularity,CINEMA_LENGTH=cinema_length,CINEMA_NUMBER_OF_REVIEWS=cinema_num_of_review)
    movie_data[[val]]<-mov_data
    
  }
  else if(identical(cinema_rating, character(0))){
    cinema_rating=""
    mov_data=data.frame(CINEMA_NAME=cinema_name,CINEMA_RATING=cinema_rating,CINEMA_POPULARITY=cinema_popularity,CINEMA_LENGTH=cinema_length,CINEMA_NUMBER_OF_REVIEWS=cinema_num_of_review)
    movie_data[[val]]<-mov_data
    
  }
  else if(identical(cinema_num_of_review, character(0))){
    cinema_num_of_review=""
    mov_data=data.frame(CINEMA_NAME=cinema_name,CINEMA_RATING=cinema_rating,CINEMA_POPULARITY=cinema_popularity,CINEMA_LENGTH=cinema_length,CINEMA_NUMBER_OF_REVIEWS=cinema_num_of_review)
    movie_data[[val]]<-mov_data
    
  }
  else if(identical(cinema_popularity, character(0))){
    cinema_length=""
    mov_data=data.frame(CINEMA_NAME=cinema_name,CINEMA_RATING=cinema_rating,CINEMA_POPULARITY=cinema_popularity,CINEMA_LENGTH=cinema_length,CINEMA_NUMBER_OF_REVIEWS=cinema_num_of_review)
    movie_data[[val]]<-mov_data
  }else if(!identical(cinema_popularity, character(0)) & !identical(cinema_name, character(0)) & !identical(cinema_length, character(0)) & !identical(cinema_rating, character(0)) & !identical(cinema_num_of_review, character(0))){
    mov_data=data.frame(CINEMA_NAME=cinema_name,CINEMA_RATING=cinema_rating,CINEMA_POPULARITY=cinema_popularity,CINEMA_LENGTH=cinema_length,CINEMA_NUMBER_OF_REVIEWS=cinema_num_of_review)
    movie_data[[val]]<-mov_data
  }
  val = val + 1
  
}

final_movie_data<-do.call(rbind,movie_data)
write.csv(final_movie_data, "movie_scraped_data.csv", row.names = FALSE)
