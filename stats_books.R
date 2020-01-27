GOODREADS_KEY = ""

library(tidyverse)
library(httr)
library(XML)
library(purrr)
library(rvest)

files <- list.files('goodreads_1001books//list_books_members//')
files <- str_subset(files,"rating_book_")
ratings_1001 <- list()
for (f in files){
  ratings_1001[[f]] <- readRDS(str_c('goodreads_1001books//list_books_members//',f))
}
ratings_1001_df <- bind_rows(ratings_1001)


books_1001_read <- readRDS('books_1001_read.rds')

ratings_1001_df2 <-books_1001_read %>% left_join(ratings_1001_df,by=c("alt_book_id"='book_id','member_id'='user_id'))


alt_books_df <- readRDS('alt_editions_books.rds')
lotr <- ratings_1001_df %>% left_join(alt_books_df,by=c('book_id'='alt_book_id')) %>% filter(ref_book_id %in% c('34','15241', '18512'))
# books from lord of the rings trilogy
read_3lotr <- ratings_1001_df2 %>% filter(ref_book_id %in% c('34','15241','18512')) %>% 
                     group_by(member_id) %>% 
                      summarise(count=n_distinct(ref_book_id),read_at=last(read_at),rating=round(mean(as.numeric(ifelse(rating=='0',NA,rating)),na.rm=TRUE)),
                                review_id =last(review_id),alt_book_id=last(alt_book_id),read_count=min(as.numeric(read_count))) %>%
                      filter(count==3) %>% select(-count) %>% 
                      mutate(rating=as.character(rating),read_count=as.character(read_count),ref_book_id='33')

ratings_1001_df2 <- bind_rows(ratings_1001_df2,read_3lotr)

book_list <- readRDS('1001books_df.rds')
colnames(book_list)[1] <- 'ref_book_id'
stats_books <- ratings_1001_df2 %>% mutate(rating=ifelse(rating=="0",NA, as.numeric(rating))) %>%
                             group_by(ref_book_id) %>% summarise(count=n(),
                                                    rating_mean=mean(rating,na.rm=TRUE),
                                                    rating_count=sum(!is.na(rating)),
                                                    pct5stars=sum(rating==5,na.rm=TRUE)/rating_count) %>%
                    arrange(-count) %>% right_join(book_list) %>% mutate(count=ifelse(is.na(count),0,count)) 

missing_books <- book_list %>% anti_join(stats_books) %>% mutate(count=ifelse(is.na(count),0,count))

top25books <- stats_books[1:25,]

saveRDS(stats_books, "stats_books.rds")
saveRDS(ratings_1001_df2, "all_ratings.rds")
library(showtext)
font_add_google('Lato','lato')
top25books$title <- factor(top25books$title, levels = rev(top25books$title))
p <- ggplot(top25books,aes(x=title,y=count)) + geom_col(fill="#553b08") + 
  coord_flip() +xlab('') + ylab('Number of members who have read the book') + theme_minimal() +
  geom_text(aes(label=count),color="#553b08",hjust=-0.5) + ggtitle("Top 25 most read books") + ylim(c(0,1000))+
  theme(axis.title=element_text(color="#553b08"),axis.text=element_text(color="#553b08",size=12),title=element_text(color="#553b08"))

ggsave("top_read_25.png", p, width = 8, height = 10, dpi = 96)dd