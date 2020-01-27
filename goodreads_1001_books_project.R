GOODREADS_KEY = ""



library(tidyverse)
library(httr)
library(XML)
library(purrr)
search_group <- GET('https://www.goodreads.com/group/search.xml?q=1001_books',query=list(key="oX1jg1db1s4jb01qXCqMoA"))
xmlToList(content(search_group,'text'))$groups

# find members of 1001 books group

list_members <- c()
comments <- c()

group_members <- GET('https://www.goodreads.com/group/members/970.xml',query=list(key="oX1jg1db1s4jb01qXCqMoA",page=1))
res <- xmlToList(content(group_members,'text'))$group_users
page <- 1
while (status_code(group_members)==200 & length(res)>2){
  page <- page+1

  group_members <- GET('https://www.goodreads.com/group/members/970.xml',query=list(key="oX1jg1db1s4jb01qXCqMoA",page=page))
  res <- xmlToList(content(group_members,'text'))$group_users
  for (i in 1:length(res)){
    if (class(res[[i]])=='list'){
      list_members <- c(list_members,res[[i]]$user$id$text)
      comments <- c(comments,res[[i]]$comments_count$text)
       
    }
    
  }
}

members <- tibble(member_id=list_members,comments=comments)
active_members <- members %>% filter(comments!="0")
write_csv(x=tibble(member_id=list_members,comments=comments),'group_members.csv')

list_members <-  active_members$member_id
saveRDS(list_members,'list_members.rds')

library(xml2)
library(tidyverse)
library(stringr)
library(rvest)

member_id = "42806904"
book_id <- "2657"
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
library(doParallel)

book_list <- readRDS('1001books_df.rds')
res <- list()
private_members <- c()
for (m in 101:length(list_members)){
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





startUrl <- "https://www.goodreads.com/list/show/952.1001_Books_You_Must_Read_Before_You_Die"

getBookDescription <- function(bookLink) {
  url <- str_c("https://www.goodreads.com", bookLink)
  read_html(url) %>% html_node("#descriptionContainer") %>% html_text() %>% trimws()
}

getBooks <- function(i) {
  cat(i, "\n")
  url <- str_c(startUrl, "?page=", i)
  
  html <- read_html(url)
  
  title <- html %>%
    html_nodes(".bookTitle") %>%
    html_text(trim = TRUE) #%>%
  #discard(!str_detect(., "[A-Z0-9]"))
  
  book_id <- html %>%
    html_nodes(".bookTitle") %>%
    html_attr("href") %>% 
    str_match('/book/show/(.*?)\\.|/book/show/(.*?)\\-') %>% 
    as_tibble() %>% mutate(V4=if_else(is.na(V3),V2,V3))
  
  author <- html %>%
    html_nodes(".authorName") %>%
    html_text(trim = TRUE) %>%
    discard(str_detect(., "^\\("))
  
  author_id <- html %>%
    html_nodes(".authorName") %>%
    html_attr("href") %>% 
    str_match('/author/show/(.*?)\\.|/author/show/(.*?)\\-') %>% 
    as_tibble() %>% mutate(V4=if_else(is.na(V3),V2,V3))
  
  rate <- html %>%
    html_nodes(".minirating") %>%
    html_text(trim = TRUE) %>%
    str_extract_all("[0-9.,]+", simplify = TRUE) %>%
    as_tibble() %>%
    magrittr::set_colnames(c("avg", "nRaters")) %>%
    mutate(nRaters = str_replace_all(nRaters, ",", "")) %>%
    mutate_all(as.numeric)
  
  score <- html %>%
    html_nodes("a") %>%
    html_text() %>%
    discard(!str_detect(., "score: [0-9,]+")) %>%
    str_extract("[0-9,]+") %>%
    str_replace_all(",", "") %>%
    as.numeric()
  
  nVoters <- html %>%
    html_nodes("a") %>%
    html_text() %>%
    discard(!str_detect(., "([0-9,]+ people voted)|(1 person voted)")) %>%
    str_extract("[0-9,]+") %>%
    str_replace_all(",", "") %>%
    as.numeric()
  
  bookLinks <- html %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    discard(!str_detect(., "^/book/show")) %>%
    na.omit() %>%
    unique()
  
  bookDescription <- bookLinks %>%
    map_chr(getBookDescription)
  
  return(tibble(
    book_id= book_id$V4,
    title = title,
    author = author,
    author_id=author_id
    rating = rate$avg,
    nRaters = rate$nRaters,
    score = score,
    nVoters = nVoters,
    bookDescription = bookDescription
  ))
}

book_list <- c(1:14) %>%
  map_dfr(getBooks)

write_csv(book_list,'1001books_complete_list.csv')
saveRDS(book_list,'1001books_df.rds')

member_id = "42806904"
book_id <- "2657"

res <- list()

for (m in 1:length(list_members)){
  member_id <- list_members[m]
  read_count <- rep('',nrow(book_list))
  rating <- rep('',nrow(book_list))
  user_v <- rep(member_id,nrow(book_list))
  book_v <- rep('',nrow(book_list))
  read_at <- rep('',nrow(book_list))
  review_id <- rep('',nrow(book_list))
  T1 <- Sys.time()
  for (b in 1:nrow(book_list)){
    book_id <- book_list$book_id[b]
    member_book_rating <- GET('https://www.goodreads.com/review/show_by_user_and_book.xml',
                              query=list(key="oX1jg1db1s4jb01qXCqMoA",
                                         user_id=member_id,
                                         book_id=book_id))
    
    member_content <- xmlToList(content(member_book_rating,'text'))$review
    if (!is.null(member_content$id)){
      review_id[b] <- member_content$id
      if (!is.null(member_content$read_count)){
        read_count[b] <- member_content$read_count
      }
      if (!is.null(member_content$rating)){
        rating[b] <- member_content$rating
      }
      if (!is.null(member_content$read_at)){
        read_at[b] <- member_content$read_at
      }
    }

  }
  T2 <- Sys.time()
  difftime(T2,T1)
  res[[m]] <- tibble(review_id = review_id,
                     user_id = user_v,
                     book_id= book_v,
                     rating=rating,
                     read_at = read_at,
                     read_count=read_count)
  saveRDS(res[[m]],paste0('rating_book_member_',m,'.rds'))
  
}



