#project for IML class

#install packages
install.packages("jsonlite")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("http")
install.packages("tm")
install.packages("wordcloud")
install.packages("stringr")
install.packages("corpus")
install.packages("data.table")
install.packages("hash")
install.packages("timetk")
install.packages("tidyverse")
install.packages("anomalize")
install.packages("kml")
library(kml)
library(anomalize)
library(tidyverse)
library(lubridate)
library(timetk)
library("hash")
library("data.table")
library("corpus")
library("stringr")
library("tm")
library(jsonlite)
library(tidyverse)
library(dplyr)
library(httr)
packages <- c("wordcloud", "tm")
lapply(packages, require , character.only = TRUE)

##define values
max = 10 #number of times the posts are fetched
link = "https://www.reddit.com/r/AmItheAsshole/new.json?limit=100"

#fetching posts function
fetchPosts <- function(max, link) {
  i = 0
  ds <- jsonlite::fromJSON(txt = link)
  df0 <- data.table(
    Word = ds$data$children$data$selftext,
    ID = ds$data$children$data$id,
    Time = ds$data$children$data$created_utc
  )
  while (i < max)
  {
    ds <-
      jsonlite::fromJSON(txt = paste(paste(link, "&after=", sep = ""),
                                     ds[["data"]][["after"]],
                                     sep = ""))
    df0 <-
      bind_rows(
        df0,
        data.table(
          Word = ds$data$children$data$selftext,
          ID = ds$data$children$data$id,
          Time = ds$data$children$data$created_utc
        )
      )
    i = i + 1
  }
  return(df0)
}

#calculate time series distance
euclidean_dist <- function(str1, str2)
{
  m = length(str1)
  n = length(str2)
  for (i in 1:(m + 1)) {
    d(i, 1) <- i - 1
  }
  for (j in 1:(n + 1)) {
    d(1, j) <- j - 1
  }
  for (i in 2:(m + 1)) {
    for (j in 2:(n + 1)) {
      if (str1(i - 1) == str2(j - 1))
        cost = 0
      else
        cost = 1
    }
    temp = min((d(i - 1, j) + 1), (d(i, j - 1) + 1))
    d(i, j) = min(temp, (d(i - 1, j - 1) + cost))
  }
  return(d(m + 1, n + 1))
}

#stem text
stemWordsInOnePost <- function(unl)
{
  unl <- Corpus(VectorSource(unl))
  unl <- tm_map(unl, removePunctuation)
  unl <- tm_map(unl, content_transformer(tolower))
  unl <- tm_map(unl, removeNumbers)
  unl <- tm_map(unl, stripWhitespace)
  unl <- gsub("[^A-Za-z ]", " ", unl)
  #split words
  text_tokens(unl, stemmer = "en")
  s <- strsplit(as.character(unl), split = " ")
  s <- s[!sapply(s, is.null)]
  unl <- unlist(s)
  return(unl)
}

#find weights for the fetched Posts
findCounts <- function(Posts){
  postsN = length(Posts$Word)
  allPosts <- data.frame(
    Word = character(),
    WordCount = as.numeric(),
    DocCount = as.numeric(),
    stringsAsFactors = FALSE
  )
  totalWords <- character()
  totalWordCounts <- numeric()
  totalDocCounts <- numeric()
  for (i in 1:postsN)
  {
    words <- character()
    wordCounts <- numeric()
    docCounts <- numeric()
    singleText <- stemWordsInOnePost(Posts[i]$Word)
    for (word in singleText) {
      #eliminated long words since they usually correspond to links
      if (nchar(word) > 0 && nchar(word) < 20) {
        if (is.element(word, words))
        {
          wordCounts[[word]] <- wordCounts[[word]] + 1
          
        }
        else{
          words[[word]] <- word
          wordCounts[[word]] <- 1
          docCounts[[word]] <- 1
        }
      }
      
    }
    
    for (i in 1:length(words))
    {
      word = words[[i]]
      if (is.element(word, totalWords))
      {
        totalWordCounts[[word]] <-
          totalWordCounts[[word]] + wordCounts[[word]]
        totalDocCounts[[word]] <- totalDocCounts[[word]] + 1
      }
      else{
        totalWords[[word]] <- word
        totalWordCounts[[word]] <- wordCounts[[word]]
        totalDocCounts[[word]] <- 1
      }
    }
  }
  
  weightsTable <-
    data.table(Words = totalWords,
               DocsCount = totalDocCounts,
               WordsCount = totalWordCounts)
  return(weightsTable)
}

calculateWeights <- function(weightsTable){
  all_words_count <- sum(weightsTable$WordsCount)
  weightsTable[, TF := WordsCount / all_words_count]
  weightsTable[, IDF := log(postsNr / DocsCount)]
  weightsTable[, Weight := TF * IDF]
  total_weight <- sum(weightsTable$Weight)
  weightsTable[, Weight := 100000 * Weight / total_weight]
}

#use functions to calculate weight using tf and idf
Posts <- fetchPosts(max, link)
postsNr = length(Posts$Word)
weightsTable <- findCounts(Posts)
weightsTable <- calculateWeights(weightsTable)
all_words_count <- length(weightsTable$Words)

#plot time series
plot(
  1:all_words_count,
  weightsTable$Weight,
  type = "l",
  main = "T^3",
  xlab = "Time",
  ylab = "Weight"
)

#plot first 1000 words of the time series
plot(
  1:1000,
  weightsTable$Weight[1:1000],
  type = "l",
  main = "T^3",
  xlab = "Time",
  ylab = "Weight"
)

######################################
#data mining
displayCluster <- function(table) {
  nr = length(colnames(table))
  survey_data_cld <-
    kml::cld(table, timeInData = 2:nr, maxNA = 1)
  class(survey_data_cld)
  kml::kml(survey_data_cld, nbRedrawing = 5)
  
  x11(type = "Xlib")
  
  kml::choice(survey_data_cld)
}

#cluster all parameters
displayCluster(weightsTable)

#cluster word count and weight
table<-data.frame(id=weightsTable$Words, wordsCount = weightsTable$WordsCount,Weights=weightsTable$Weight)
displayCluster(table)

#cluster weight
table<-data.frame(id=weightsTable$Words, wordsCount = Weights=weightsTable$Weight)
displayCluster(table)

#####################################################

# Get the data
df1_mining <- weightsTable %>%
  # Get the columns
  select(Words,WordsCount,Weight) %>%
  # Change the columns name
  set_names(c("words","date","value"))
df1_mining[, date := as.Date('1986-07-04')]
df1_mining$date <-
  seq(as.Date('1986-07-04'),
      by = 'days',
      length = length(df1_mining$date))

df1_mining %>% 
  as_tibble() %>%
  time_decompose(value, method = "stl", frequency = "auto",
                 trend     = "1 year") %>%
  anomalize(remainder, alpha=0.005) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE)

data_mined<-df1_mining %>% 
  as_tibble() %>%
  time_decompose(value, method = "stl", frequency = "auto",
                 trend     = "1 year") %>%
  anomalize(remainder, alpha=0.005) %>%
  time_recompose()
data_mined <- data.frame(Words=weightsTable$Words,Anomaly=data_mined$anomaly)

#frame with anomalies
data_mined <- data_mined[data_mined$Anomaly == "Yes", ]


##############################################
# see anomaly results for the first 4000 words
df1_mining <- weightsTable[1:4000] %>%
  # Get the columns
  select(WordsCount,Weight) %>%
  # Change the columns name
  set_names(c("date","value"))
df1_mining[, date := as.Date('1986-07-04')]
df1_mining$date <-
  seq(as.Date('1986-07-04'),
      by = 'days',
      length = length(df1_mining$date))

df1_mining %>% 
  as_tibble() %>%
  time_decompose(value, method = "stl", frequency = "auto",
                 trend     = "1 year") %>%
  anomalize(remainder, alpha=0.015) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE)



#############################
#plot anomalies

df_anomalized %>% plot_anomalies(ncol = 3, alpha_dots = 0.75)

df_anomalized <- df1_mining %>% 
  as_tibble() %>%
  time_decompose(date, merge = TRUE) %>%
  anomalize(remainder) %>%
  time_recompose()
#df_anomalized %>% glimpse()
df_anomalized %>% plot_anomalies(ncol = 3, alpha_dots = 0.75)
# DATA MINING BY YEAR
#df1_mining %>% filter(!is.na(date)) %>% group_by(year(date)) %>%
# plot_anomaly_diagnostics(date, value,  .frequency="month")

############################
#do not stem numbers
stemWordsNumbersInOnePost <- function(unl)
{
  unl <- Corpus(VectorSource(unl))
  unl <- tm_map(unl, removePunctuation)
  unl <- tm_map(unl, content_transformer(tolower))
  unl <- tm_map(unl, stripWhitespace)
  unl <- gsub("[^A-Za-z ]", " ", unl)
  #split words
  text_tokens(unl, stemmer = "en")
  s <- strsplit(as.character(unl), split = " ")
  s <- s[!sapply(s, is.null)]
  unl <- unlist(s)
  return(unl)
}

#find weights for the fetched Posts
findWithNumbersCounts <- function(Posts){
  
  allPosts <- data.frame(
    Word = character(),
    WordCount = as.numeric(),
    DocCount = as.numeric(),
    stringsAsFactors = FALSE
  )
  totalWords <- character()
  totalWordCounts <- numeric()
  totalDocCounts <- numeric()
  for (i in 1:postsNr)
  {
    words <- character()
    wordCounts <- numeric()
    docCounts <- numeric()
    singleText <- stemWordsNumbersInOnePost(Posts[i]$Word)
    for (word in singleText) {
      #eliminated long words since they usually correspond to links
      if (nchar(word) > 0 && nchar(word) < 20) {
        if (is.element(word, words))
        {
          wordCounts[[word]] <- wordCounts[[word]] + 1
          
        }
        else{
          words[[word]] <- word
          wordCounts[[word]] <- 1
          docCounts[[word]] <- 1
        }
      }
      
    }
    
    for (i in 1:length(words))
    {
      word = words[[i]]
      if (is.element(word, totalWords))
      {
        totalWordCounts[[word]] <-
          totalWordCounts[[word]] + wordCounts[[word]]
        totalDocCounts[[word]] <- totalDocCounts[[word]] + 1
      }
      else{
        totalWords[[word]] <- word
        totalWordCounts[[word]] <- wordCounts[[word]]
        totalDocCounts[[word]] <- 1
      }
    }
  }
  
  weightsTable <-
    data.table(Words = totalWords,
               DocsCount = totalDocCounts,
               WordsCount = totalWordCounts)
  return(weightsTable)
}

Posts <- fetchPosts(max, link)
postsNr = length(Posts$Word)
weightsTable <- findWithNumbersCounts(Posts)
weightsTable <- calculateWeights(weightsTable)
all_words_count <- length(weightsTable$Words)

############################
#multiple time series, divide 
findOnePostCounts <- function(Posts,postsNr,chunkNr){
  
  allPosts <- data.frame(
    Word = character(),
    WordCount = as.numeric(),
    DocCount = as.numeric(),
    stringsAsFactors = FALSE
  )
  totalWords <- character()
  totalWordCounts <- numeric()
  totalDocCounts <- numeric()
  for (i in 1:postsNr)
  {
    words <- character()
    wordCounts <- numeric()
    docCounts <- numeric()
    singleText <- stemWordsNumbersInOnePost(Posts[i]$Word)
    for (word in singleText) {
      #eliminated long words since they usually correspond to links
      if (nchar(word) > 0 && nchar(word) < 20) {
        if (is.element(word, words))
        {
          wordCounts[[word]] <- wordCounts[[word]] + 1
          
        }
        else{
          words[[word]] <- word
          wordCounts[[word]] <- 1
          docCounts[[word]] <- 1
        }
      }
    }
      
    
    for (i in 1:length(words))
    {
      word = words[[i]]
      if (is.element(word, totalWords))
      {
        totalWordCounts[[word]] <-
          totalWordCounts[[word]] + wordCounts[[word]]
        totalDocCounts[[word]] <- totalDocCounts[[word]] + 1
      }
      else{
        totalWords[[word]] <- word
        totalWordCounts[[word]] <- wordCounts[[word]]
        totalDocCounts[[word]] <- 1
      }
    }
  }
  
  weightsTable <-
    data.table(Words = totalWords,
               DocsCount = totalDocCounts,
               WordsCount = totalWordCounts)
  return(weightsTable)
}
#let's use 1000 posts and divide them into 50 ts 
#that consist of 20 posts
Posts <- fetchPosts(max, link)
postsNr <- 1000
chunkNr <- 20
#weightsTable <- findOnePostCounts(Posts,postsNr,chunkNr)
j=1
#for(i in 1:postsNr){
table1 <- findCounts(Posts[1:101])
table1 <- calculateWeights(table1)
#all_words_count[1] <- length(table1$Words)
table2 <- findCounts(Posts[101:200])
table2 <- calculateWeights(table2)
#all_words_count2 <- length(table2$Words)
table3 <- findCounts(Posts[201:300])
table3 <- calculateWeights(table3)
