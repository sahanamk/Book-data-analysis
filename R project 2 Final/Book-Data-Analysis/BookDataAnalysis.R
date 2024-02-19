library(data.table) #powerful extension of the basic data.frame in R, data manipulation library
library(ggplot2) #popular data visualization library
library(dplyr) #data manipulation library
library(gridExtra) #arrange and display multiple plots together.
library(DT) #display and manipulate data tables with features such as sorting, searching, and pagination.
library(corrplot) #visualizing correlation matrices
library(tidyr) #data tidying tasks.
library(stringr) #string manipulation library
library(qgraph) #visualizing complex networks and correlation matrices

## read the data
books <- fread('books.csv')
ratings <- fread('ratings.csv')
book_tags <- fread('book_tags.csv')
tags <- fread('tags.csv')

cat(nrow(ratings))

## remove the duplicate ratings.
ratings[, N := .N, .(user_id, book_id)]
cat('Number of duplicate ratings: ', nrow(ratings[N > 1]))
ratings <- ratings[N == 1]

## keep users who rated more than 2 books
ratings[, N := .N, .(user_id)]
cat('Number of users who rated less than 2 books: ', uniqueN(ratings[N <= 2, user_id]))
ratings <- ratings[N > 2]

## Data Exploration

## i) Distribution of ratings
ratings %>% 
  ggplot(aes(x = rating, fill = factor(rating))) +
  ggtitle("Distribution of Ratings") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(color = "grey20") + scale_fill_brewer(palette = "Oranges") + guides(fill = "none")

## ii) Number of ratings per user
ratings %>% 
  group_by(user_id) %>% 
  summarize(number_of_ratings_per_user = n()) %>% 
  ggplot(aes(number_of_ratings_per_user)) + 
  xlab("Ratings per User") + ylab("Rating count") +
  ggtitle("Number of ratings per user") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(fill = "cadetblue3", color = "grey20") + coord_cartesian(c(3, 50))

## iii) Distribution of mean user rating
ratings %>% 
  group_by(user_id) %>% 
  summarize(mean_user_rating = mean(rating)) %>% 
  ggplot(aes(mean_user_rating)) +
  xlab("Mean of User ratings") + ylab("Rating count") +
  ggtitle("Distribution of mean user rating") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(bins = 30, fill = "cyan", color = "grey20")

## iv) 10 highly Rated books
books %>% 
  mutate(image = paste0('<img src="', small_image_url, '"></img>')) %>% 
  arrange(-average_rating) %>% 
  top_n(10,wt = average_rating) %>% 
  select(image, title, ratings_count, average_rating) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

## v) 10 most popular books
books %>% 
  mutate(image = paste0('<img src="', small_image_url, '"></img>')) %>% 
  arrange(-ratings_count) %>% 
  top_n(10,wt = ratings_count) %>% 
  select(image, title, ratings_count, average_rating) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

##A.1) Collaborative Filtering
dimension_names <- list(user_id = sort(unique(ratings$user_id)), book_id = sort(unique(ratings$book_id)))
ratingmat <- spread(select(ratings, book_id, user_id, rating), book_id, rating) %>% select(-user_id)

ratingmat <- as.matrix(ratingmat)
dimnames(ratingmat) <- dimension_names
#ratingmat[1:5, 1:5]
#dim(ratingmat)

##A.2) Find similar users
current_user <- "17329"
rated_items <- which(!is.na((as.data.frame(ratingmat[current_user, ]))))
selected_users <- names(which(apply(!is.na(ratingmat[ ,rated_items]), 1, sum) >= 2))
head(selected_users, 40)

user1 <- data.frame(item=colnames(ratingmat),rating=ratingmat[current_user,]) %>% filter(!is.na(rating))
user2 <- data.frame(item=colnames(ratingmat),rating=ratingmat["1339",]) %>% filter(!is.na(rating))
tmp<-merge(user1, user2, by="item")
tmp
cor(tmp$rating.x, tmp$rating.y, use="pairwise.complete.obs")

user2 <- data.frame(item = colnames(ratingmat), rating = ratingmat["21877", ]) %>% filter(!is.na(rating))
tmp <- merge(user1, user2, by="item")
tmp

cor(tmp$rating.x, tmp$rating.y, use="pairwise.complete.obs")

# A.3)normalizing the user ratings
rmat <- ratingmat[selected_users, ]
user_mean_ratings <- rowMeans(rmat,na.rm=T)
rmat <- rmat - user_mean_ratings

similarities <- cor(t(rmat[rownames(rmat)!=current_user, ]), rmat[current_user, ], use = 'pairwise.complete.obs')
sim <- as.vector(similarities)
names(sim) <- rownames(similarities)
res <- sort(sim, decreasing = TRUE)
head(res, 40)

##A.4) Visualizing similarities
sim_mat <- cor(t(rmat), use = 'pairwise.complete.obs')
random_users <- selected_users[1:20]
qgraph(sim_mat[c(current_user, random_users), c(current_user, random_users)], layout = "spring", vsize = 5, theme = "TeamFortress", labels = c(current_user, random_users))

##A.5) Get predictions for other books.
similar_users <- names(res[1:4])

similar_users_ratings <- data.frame(item = rep(colnames(rmat), length(similar_users)), rating = c(t(as.data.frame(rmat[similar_users,])))) %>% filter(!is.na(rating))

current_user_ratings <- data.frame(item = colnames(rmat), rating = rmat[current_user,]) %>% filter(!is.na(rating))

predictions <- similar_users_ratings %>% 
  filter(!(item %in% current_user_ratings$item)) %>% 
  group_by(item) %>% summarize(mean_rating = mean(rating))

predictions %>% 
  datatable(class = "nowrap hover row-border", options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

#Get the best 5 predictions

 predictions %>% 
   arrange(-mean_rating) %>% 
  top_n(5, wt = mean_rating) %>% 
   mutate(book_id = as.numeric(as.character(item))) %>% 
   left_join(select(books, authors, title, book_id), by = "book_id") %>% 
   select(-item) %>% 
   datatable(class = "nowrap hover row-border", options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
