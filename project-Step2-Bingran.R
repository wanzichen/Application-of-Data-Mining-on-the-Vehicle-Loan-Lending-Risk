# 1.Dataset Description
## 1.1 Read dataset
library(Matrix)
setwd(dir="/Users/bingranchen/Documents/Macbook13''/UCF/MSDA-Courses/STA 6704 DataMingingII/Project/project-code")
data = read.csv(file="movie_metadata.csv", stringsAsFactors = F)

## 1.2 Structure of data sets
dim(data)
str(data)
head(data)
tail(data)

#Count the number of columns that consists of text data in training dataset
sum(sapply(data[,1:28],typeof) == "character")
#Count the number of columns that consists of numeric data in training dataset
sum(sapply(data[,1:28],typeof) == "integer")


### 2. Preprocessing and Cleaning
ls(data)

### Feature reduction
data2 <- data[ , -which(names(data) %in% c("budget", "aspect_ratio", "actor_1_name", "actor_2_name","actor_3_name", "plot_keywords", "movie_imdb_link", "director_name", "color", "cast_total_facebook_likes"))]

### missing values
sum(is.na(data2)) / (nrow(data2) *ncol(data2))
library(colorspace)
library(grid)
library(data.table)
library(VIM)
aggr(data2, numbers=T, prop=T, sortVars=T, labels=names(data2))
aggr(data2, numbers=T, prop=F, sortVars=T, labels=names(data2))

##### Compute missing values in each column
temp_col <- ncol(data2)

for( i in 1:temp_col) {
  rem = sum(is.na(data2[,i])) / 5043 *100
  if(rem > 0) {
    print(i)
    print(rem)
  }
}

data3 <- na.omit(data2)

### Feature constuction
#### "length_of_title"
library(stringr)
data3$movie_title <- str_trim(data3$movie_title)
data3$length_of_title <- str_length(data3$movie_title)

#### "years"(how many years until now)
bb <- 2018-data3$title_year
data3$years <- bb
#### " average_actor_likes"
data3$average_actor_likes <- round(rowMeans(data3[c("actor_3_facebook_likes", "actor_1_facebook_likes","actor_2_facebook_likes")]))

##remove excessive variables
data4 <- data3[ , -which(names(data3) %in% c("movie_title", "title_year", "actor_3_facebook_likes", "actor_1_facebook_likes","actor_2_facebook_likes"))]

#### "genres_new"
data4$temp <- gsub("\\|", " ", data4$genres)
data4$genres_new <- gsub(" .*$", "", data4$temp)

#unique(data4$genres_new)
#data4[data4$genres_new =="Musical", c("genres", "genres_new")]

data5 <- data4
data6 <- data5[ , -which(names(data5) %in% c("genres", "temp"))]

##################### Export a new movie dataset called "movie_clean"#######################
write.csv(data6, "movie_clean.csv", row.names = F)
