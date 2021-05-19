library(tidyverse)
library(curl)
library(stringr)
library(syuzhet) # for sentiment analysis MORE INFO ~ https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
library(data.table)
#### read data
df <- read_csv('data/vaccination_all_tweets.csv')

#### Preprocessing Data (clean up text data)
# For people who uses weird characters as English characters, transform them so we get more texts recovered
# Side effect - also removes weird English characters
df$user_name <- iconv(df$user_name, from = 'UTF-8', to = 'ASCII//TRANSLIT')
df$user_location <- iconv(df$user_location, from = 'UTF-8', to = 'ASCII//TRANSLIT')
df$user_description <- iconv(df$user_description, from = 'UTF-8', to = 'ASCII//TRANSLIT')
df$text <- iconv(df$text, from = 'UTF-8', to = 'ASCII//TRANSLIT')
df$hashtags <- iconv(df$hashtags, from = 'UTF-8', to = 'ASCII//TRANSLIT')

# Transform TRUE FALSE into 0, 1 
df$user_verified <- as.integer(as.logical(df$user_verified))
df$is_retweet <- as.integer(as.logical(df$is_retweet))

# Convert date columns
df$user_created <- as.Date(df$user_created)
df$date <- as.Date(df$date)

# remove mention @ in text
df$text <- gsub("@\\w+","",df$text)

# remove link in text
df$text <- gsub("https\\S*", "",df$text)


#### clean up location column
d <- df
url <- "https://gist.githubusercontent.com/kalinchernev/486393efcca01623b18d/raw/daa24c9fea66afb7d68f8d69f0c4b8eeb9406e83/countries"
countries <- read.delim(url, sep = '\n', header = FALSE)
country_list <- countries$V1
# preprocess location before matching
country_list <- tolower(country_list) 
country_list <- gsub('[[:punct:]]+', '', country_list)
d$user_location <- tolower(d$user_location)
d$user_location <- gsub('england', 'united kingdom', d$user_location)
d$user_location <- gsub('uk', 'united kingdom', d$user_location)
d$user_location <- gsub('america', 'united states', d$user_location)
d$user_location <- gsub('usa', 'united states', d$user_location)
d$user_location <- gsub('ireland', 'ireland republic', d$user_location)
d$user_location <- gsub('ca', 'united states', d$user_location)
d$user_location <- gsub('ny', 'united states', d$user_location)
d$user_location <- gsub('nyc', 'united states', d$user_location)
d$user_location <- gsub('new york', 'united states', d$user_location)
d$user_location <- gsub('wa', 'united states', d$user_location)
d$user_location <- gsub('washington', 'united states', d$user_location)
d$user_location <- gsub('texas', 'united states', d$user_location)
d$user_location <- gsub('tx', 'united states', d$user_location)
d$user_location <- gsub('us', 'united states', d$user_location)
d$user_location <- gsub('russia', 'russia federation', d$user_location)
d$user_location <- gsub('beijing', 'china', d$user_location)
d$user_location <- gsub('delhi', 'india', d$user_location)
d$user_location <- gsub('mumbai', 'india', d$user_location)
d$user_location <- gsub('selangor', 'malaysia', d$user_location)
d$user_location <- gsub('kuala lumpur', 'malaysia', d$user_location)
d$user_location <- gsub('uae', 'united arab emirates', d$user_location)
d$user_location <- gsub('dubai', 'united arab emirates', d$user_location)
d$user_location <- gsub('barcelona', 'spain', d$user_location)

d$extracted_location <- ""

for (row in 1:nrow(d)) {
  row_user_location <- d[row, "user_location"] # [row_index, column_name]
  print("Row User Location: ")
  print(row_user_location)
  
  if (row_user_location == "" || is.na(row_user_location)) { next }
  
  match = FALSE
  for (i in 1:length(country_list)) {
    local_country = country_list[i]
    tmp <- str_extract(row_user_location, local_country)
    if (tmp == "" || is.na(tmp)) { next }
    if (tmp == local_country) {
      match = TRUE
      d[row, "extracted_location"] <- tmp
      break
    }
  }
  
  if (! match) {
    print(d[row,])
    d[row, "extracted_location"] <- ""
  }
}

# Use anti_join to produce a dataframe of different rows
diff <- anti_join(d, df, by = 'user_location')
# Count the row of dataframe produced by anti_join
nrow(diff)

# cleaned up text columns & extracted location
write.csv(d, "data/V2_vaccination_all_tweets.csv", row.names = FALSE)




#=====================================================================================================================
### get Sentiments
df <- read_csv('data/V2_vaccination_all_tweets.csv')
sentimetns <- get_nrc_sentiment((df$text))
sentimetns$neutral <- ifelse(rowSums(sentimetns) == 0, 1, 0)

sentimetns$positives <- sentimetns$positive+sentimetns$trust+sentimetns$joy+sentimetns$surprise
sentimetns$negatives <- sentimetns$negative+sentimetns$anticipation+sentimetns$fear+sentimetns$sadness+sentimetns$disgust

df2 <- cbind(df, sentimetns)
# categories it
df2$sentiment_category <- ifelse(df2$negatives > df2$positives, 'negative', 'positive')
df2$sentiment_category <- ifelse(df2$negatives == df2$positives, 'neutral', df2$sentiment_category)
df2$sentiment_category <- ifelse(df2$neutral == 1, 'neutral', df2$sentiment_category)
df2$positives <- NULL
df2$negatives <- NULL

write.csv(df2, "data/V2_vaccination_all_tweet_with_sentiments.csv", row.names = FALSE)


#=====================================================================================================================
### sentiment by hashtags
library(stringr)
df <- read_csv('data/V2_vaccination_all_tweet_with_sentiments.csv')

temp <- str_extract_all(df$text, "(?<=^|\\s)#[^\\s]+")

h <- c()
pos_score <- c()
neg_score <- c()
neu_score <- c()
tru_score <- c()
anti_score <- c()
fear_score <- c()
sad_score <- c()
joy_score <- c()
anger_score <- c()
sup_score <- c()
dis_score <- c()
extracted_location <- c()
ind = 0
for (i in temp){
  ind = ind + 1
  for (j in i){
    h <- c(h,j)
    pos_score <- c(pos_score, df$positive[ind])
    neg_score <- c(neg_score, df$negative[ind])
    neu_score <- c(neu_score, df$neutral[ind])
    tru_score <- c(tru_score, df$trust[ind])
    anti_score <- c(anti_score, df$anticipation[ind])
    fear_score <- c(fear_score, df$fear[ind])
    sad_score <- c(sad_score, df$sadness[ind])
    joy_score <- c(joy_score, df$joy[ind])
    anger_score <- c(anger_score, df$anger[ind])
    dis_score <- c(dis_score, df$disgust[ind])
    sup_score <- c(sup_score, df$surprise[ind])
    extracted_location <- c(extracted_location, df$extracted_location[ind])
  }
}

hashtags_sentiments_score <- data.frame(hashtag=h,
                                        positive=pos_score,
                                        negative=neg_score,
                                        neutral=neu_score,
                                        trust=tru_score,
                                        anticipation=anti_score,
                                        fear=fear_score,
                                        sadness=sad_score,
                                        joy=joy_score,
                                        anger=anger_score,
                                        suprise=sup_score,
                                        disgust=dis_score,
                                        extracted_location=extracted_location)

hashtags_sentiments <- hashtags_sentiments_score %>%
  group_by(hashtag, extracted_location) %>%
  summarise(positive = sum(positive), 
            negative = sum(negative), 
            neutral  = sum(neutral),
            trust = sum(trust),
            anticipation = sum(anticipation),
            fear = sum(fear),
            sadness = sum(sadness),
            joy = sum(joy),
            anger = sum(anger),
            suprise = sum(suprise),
            disgust = sum(disgust))

hashtags_sentiments$total_positive <- hashtags_sentiments$positive+hashtags_sentiments$trust+hashtags_sentiments$joy+hashtags_sentiments$suprise
hashtags_sentiments$total_negative <- hashtags_sentiments$negative+hashtags_sentiments$anticipation+hashtags_sentiments$fear+hashtags_sentiments$sadness+hashtags_sentiments$disgust


write.csv(hashtags_sentiments, "data/hashtags_sentiments.csv", row.names = FALSE)

















