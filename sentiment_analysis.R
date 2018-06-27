rm(list=ls())

library(plyr)
#library(regex)

load('cleaned/reviews.bin')

reviews <- reviews[!is.na(reviews$comment), ]
reviews <- reviews[reviews$comment != '', ]

#reviews.text <- laply(reviews$comment, function(t)t$getText())

hu_liu_pos=scan('rawdata/positive_words.txt',what='character',comment.char=';')
hu_liu_neg=scan('rawdata/negative_words.txt',what='character',comment.char=';')

pos_words <- c(hu_liu_pos)
neg_words <- c(hu_liu_neg)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

#sample <- str_replace_all(reviews$comment, "[^[:graph:]]", " ")

sample <- reviews$comment
sample <- iconv(sample, 'UTF-8', 'ASCII')


result <- score.sentiment(sample, pos_words, neg_words)

result$id <- reviews$id
result$text <- NULL

result <- merge(result, reviews, by.x = 'id', by.y = 'id', all.x = T)

result$sentiment[result$score > 0] <- 'positive'
result$sentiment[result$score < 0] <- 'negative'
result$sentiment[result$score == 0] <- 'neutral'
#x <- merge(reviews, result, by.x = 'comment', by.y = 'text', all.x = T)
write.csv(result, file = 'output/forcomp/reviews_score.csv')

positive_reviews <- result[result$sentiment == 'positive', ]
positive_reviews <- data.table(positive_reviews_by_state)
positive_reviews <- positive_reviews[,{.N}, by = 'month']
write.csv(positive_reviews, file = 'output/positive_reviews.csv')

negative_reviews <- result[result$sentiment == 'negative', ]
negative_reviews <- data.table(negative_reviews_by_state)
negative_reviews <- negative_reviews_by_state[,{.N}, by = 'month']
write.csv(negative_reviews, file = 'output/mmreport/negative_reviews.csv')
