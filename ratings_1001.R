GOODREADS_KEY = ""

library(tidyverse)
library(httr)
library(XML)
library(purrr)
library(rvest)

files <- list.files('goodreads_1001books//list_books_members//')
files <- str_subset(files,"list_books_member_")
total_books_list <- list()
for (f in files){
  total_books_list[[f]] <- readRDS(str_c('goodreads_1001books//list_books_members//',f))
}
total_books_df <- bind_rows(total_books_list)

book_list <- readRDS('1001books_df.rds') %>% arrange(title)
#correct 1st book
ind <- which(book_list$author=='Ivan Vazov')
book_list$title[ind] <- "Under the Yoke"
book_list$book_id[ind] <- "897120"
saveRDS(book_list,'1001books_df.rds')
getAllEditions <- function(book_id){
  work_id1 <- GET('https://www.goodreads.com/book/id_to_work_id', query=list(key=GOODREADS_KEY,id=book_id))
  work_id2<- xmlToList(content(work_id1,'text'))
  work_id<- work_id2$`work-ids`$item
  
  url <- str_c("https://www.goodreads.com/work/editions/",work_id,"?expanded=true")
  html <- read_html(url)
  df_tmp <- html %>%
    html_nodes(".bookTitle") %>%
    html_attr("href") %>%
    str_match('/book/show/(.*?)\\.|/book/show/(.*?)\\-') 
  if (nrow(df_tmp)>0){
    book_ids <- (df_tmp%>% as_tibble() %>% mutate(V4=if_else(is.na(V3),V2,V3)))$V4
  } else {
    book_ids <- c()
  }
  
  
  
  book_ids <- book_ids[!is.na(book_ids)]
  page <- 1
  while(nrow(df_tmp)>0){
    page <- page +1
    url <- str_c("https://www.goodreads.com/work/editions/",work_id,"?expanded=true&page=",page)
    html <- read_html(url)
    df_tmp <- html %>%
      html_nodes(".bookTitle") %>%
      html_attr("href") %>%
      str_match('/book/show/(.*?)\\.|/book/show/(.*?)\\-') 
    if (nrow(df_tmp)>0){
      book_ids_now <- (df_tmp%>% as_tibble() %>% mutate(V4=if_else(is.na(V3),V2,V3)))$V4
      book_ids_now <- book_ids_now[!is.na(book_ids_now)]
      book_ids <- c(book_ids,book_ids_now)
    }

  }
  return(tibble(book_id=book_id,alt_book_id=book_ids))
}

for (i in 1:nrow(book_list)){
  book_id <- book_list$book_id[i]
  files <- list.files('goodreads_1001books//list_book_ids//')
  files <- str_subset(files,"alt_")
  if (!(str_c("alt_",book_id,'.rds') %in% files)){
    df <- getAllEditions(book_id)
    saveRDS(df,paste0('goodreads_1001books//list_book_ids//alt_',book_id, '.rds'))
  }

}


files <- list.files('goodreads_1001books//list_book_ids//')
files <- str_subset(files,"alt_")
alt_books_list <- list()
for (f in files){
  alt_books_list[[f]] <- readRDS(str_c('goodreads_1001books//list_book_ids//',f))
  alt_books_list[[f]]$book_id <- as.character(alt_books_list[[f]]$book_id) 
  book_id <- str_match(f,"alt_(.*).rds")[,2]
  if (alt_books_list[[f]]$book_id[1]!= book_id){
    print(str_c("problem with book_id=",book_id))
  }
}
alt_books_df <- bind_rows(alt_books_list)

colnames(total_books_df) <- c('member_id','alt_book_id')
colnames(alt_books_df) <- c('ref_book_id','alt_book_id')

alt_books_df <- unique(alt_books_df)
# special for Faraoah
df <- getAllEditions("1904852")
df$book_id <- "18195269"
names(df)[1] <- "ref_book_id"
alt_books_df <-bind_rows(alt_books_df,df)
# special for Sunset Song
df <- getAllEditions("292602")
df$book_id <- "1203812"
names(df)[1] <- "ref_book_id"
alt_books_df <-bind_rows(alt_books_df,df)
# special for the three kingdoms
df <- getAllEditions("1349242")
df$book_id <- "1059283"
names(df)[1] <- "ref_book_id"
alt_books_df <-bind_rows(alt_books_df,df)

# Lord of the rings
df1 <- getAllEditions("34")
df2 <- getAllEditions("15241")
df3 <- getAllEditions("18512")
df <- bind_rows(df1,df2,df3)
names(df)[1] <- "ref_book_id"
alt_books_df <-bind_rows(alt_books_df,df)
books_1001_read <- total_books_df %>% inner_join(alt_books_df) 

saveRDS(total_books_df,'total_books_df.rds')
saveRDS(books_1001_read,'books_1001_read.rds')
saveRDS(alt_books_df,'alt_editions_books.rds')

files <- list.files('goodreads_1001books//list_books_members//')
files <- str_subset(files,"rating_book")
for (i in 1:nrow(books_1001_read)){
  
  member_id <- books_1001_read$member_id[i]
  book_id <- books_1001_read$alt_book_id[i]
  if (!(str_c('rating_book_',book_id, 'member_',member_id,'.rds') %in% files)){
    member_book_rating <- GET('https://www.goodreads.com/review/show_by_user_and_book.xml',
                              query=list(key="oX1jg1db1s4jb01qXCqMoA",
                                         user_id=member_id,
                                         book_id=book_id))
    
    member_content <- xmlToList(content(member_book_rating,'text'))$review
    if (!is.null(member_content$id)){
      review_id <- member_content$id
      read_count <- ""
      if (!is.null(member_content$read_count)){
        read_count <- member_content$read_count
      }
      rating <- ""
      if (!is.null(member_content$rating)){
        rating <- member_content$rating
      }
      read_at <- ""
      if (!is.null(member_content$read_at)){
        read_at <- member_content$read_at
      }
    }
    
    
    df <- tibble(review_id = review_id,
                       user_id = member_id,
                       book_id= book_id,
                       rating=rating,
                       read_at = read_at,
                       read_count=read_count)
    saveRDS(df,paste0('goodreads_1001books//list_books_members//rating_book_',book_id, 'member_',member_id,'.rds'))
  }

  
}