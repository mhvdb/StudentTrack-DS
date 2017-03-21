###### Voorbereiding ######
# Installeer de benodigde packages.
install.packages("wordcloud")
install.packages("tm")
install.packages("igraph")

library(stringr)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(igraph)

# Functies t.b.v. de labs
clean.text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}

mae <- function (actual, predicted) {
	lengte <- length(actual);
	if ( lengte != length(predicted) ) {
		stop("Length of actual and predicted values must be equal!");
	}
	
	som <- 0;
	for (i in 1:lengte) {
		som <- som + abs( actual[i] - predicted[i] );
	}
	return ( som / lengte );
}

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


###### Start Lab ######

# 1.0 Data inlezen

# 1.1 Verkennen van de dataset

# 1.2 Opschonen van de tweet tekst

# 1.3 Controleren van de tweet tekst

# 2.0 Wordcloud (1/2)

# 2.1 Wordcloud (2/2)

# 3.0 Sentiment woordenboeken inlezen

# 3.1 Het scoren van de tweets (1/2)

# 3.2 Het scoren van de tweets (2/2)

# 3.3 Plot het sentiment

# 4.0 Comparison word cloud (1/3)

# 4.1 Comparison word cloud (2/3)

# 4.2 Comparison word cloud (3/3)

# 5.0 Data inlezen

# 5.1 Bekijk de data

# 5.2 Retweets identificeren

# 5.3 (Re)tweeters selecteren

# 5.4 Poster en retweeters combineren

# 5.5 Resultaten modelleren

# 5.6 Resultaten plotten

# 6.0 Conclusie

###### Einde Lab ######