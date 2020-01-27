GOODREADS_KEY = ""

library(tidyverse)
library(httr)
library(XML)
library(purrr)
library(rvest)
getBooksMember <- function(member_id) {
  page <- 1
  url <- str_c('https://www.goodreads.com/review/list', "?page=", page,"&id=",member_id,'&shelf=read')
  
  html <- read_html(url)
  
  title <- html %>%
    html_nodes(".title a") %>%
    html_text(trim = TRUE) #%>%
  #discard(!str_detect(., "[A-Z0-9]"))
  title_cur <- title
  if (length(title_cur)>0){
    book_id <- html %>%
      html_nodes(".title a") %>%
      html_attr("href") %>% 
      str_match('/book/show/(.*?)\\.|/book/show/(.*?)\\-') %>% 
      as_tibble() %>% mutate(V4=if_else(is.na(V3),V2,V3))
    book_id <- book_id$V4
  }
  while (length(title_cur)>0){
    page <- page+1
    url <- str_c('https://www.goodreads.com/review/list', "?page=", page,"&id=",member_id,'&shelf=read')
    
    html <- read_html(url)
    
    title_cur <- html %>%
      html_nodes(".title a") %>%
      html_text(trim = TRUE) #%>%
    #discard(!str_detect(., "[A-Z0-9]"))
    if (length(title_cur)>0){
      book_id_cur <- html %>%
        html_nodes(".title a") %>%
        html_attr("href") %>% 
        str_match('/book/show/(.*?)\\.|/book/show/(.*?)\\-') %>% 
        as_tibble() %>% mutate(V4=if_else(is.na(V3),V2,V3))
      book_id_cur <- book_id_cur$V4
      title <- c(title,title_cur)
      book_id <- c(book_id,book_id_cur)
    }
    
  }
  if (length(title)>0){
    read_books <- tibble(member_id=member_id,book_id=book_id)  
    return(read_books)
  } else {
    return(tibble(member_id=character(0),book_id=character(0))  )
  }
  
}


list_members <- readRDS('list_members.rds')

private_members <- c()
for (m in 1:length(list_members)){
  member_id <- list_members[m]
  member_privacy <- GET(str_c("https://www.goodreads.com/user/show/",member_id,".xml?key=",GOODREADS_KEY))
  member_privacy_content <- xmlToList(content(member_privacy,'text'))
  if (is.null(member_privacy_content$user$private)){
    tryCatch(saveRDS(getBooksMember(member_id),paste0('goodreads_1001books//list_books_members//list_books_member_',member_id,'.rds')))
  } else {
    private_members <- c(private_members,member_id)
    saveRDS(TRUE,paste0("goodreads_1001books//list_books_members//private_member_",member_id,".rds"))
  }
  
}  