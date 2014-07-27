#' ismbTweetAnalysis.
#'
#' @name ismbTweetAnalysis
#' @docType package
NULL


#' ISMB tweets from 2012
#' 
#' A set of tweets over 5 days with the hashtag #ISMB from 2012. The variables are:
#' 
#' \itemize{
#'   \item text. the text of the tweet
#'   \item created. the date and time of the tweet
#'   \item id. the twitter id of the tweet
#'   \item screenName. who sent the tweet
#'   \item hashSearch. the hashtag that was searched
#' }
#' 
#' @format A data frame with 3162 rows and 5 variables
#' @name ismb2012
#' @source \url{https://github.com/neilfws/Twitter/tree/master/ismb/data}
#' @docType data
NULL

#' read and process ST tweet data
#' 
#' reads in Stephen Turners archived twitter data and reformats it into a nicer format
#' 
#' @param file the file to process
#' @param widths the widths of each field. Defaults based on ST's originals
#' @return data.frame
#' @importFrom stringr str_trim
#' @export
readTweetData <- function(file, hashSearch, widths=c(18, 14, 18, 1000)){
  tweetData <- read.fwf(file, widths, stringsAsFactors=FALSE, comment.char="")
  tweetData <- as.data.frame(sapply(tweetData, stringr::str_trim), stringsAsFactors=FALSE)
  names(tweetData) <- c("id", "created", "screenName", "text")
  
  tweetData$screenName <- gsub("@", "", tweetData$screenName)
  tweetData$created <- as.POSIXlt(tweetData$created, format="%b %d %H:%M")
  tweetData <- tweetData[, c("text", "created", "id", "screenName")]
  tweetData$hashSearch <- hashSearch
  
  return(tweetData)
}

#' ISMB tweets from 2014
#' 
#' A set of tweets from ISMB 2014 meeting collected by Stephen Turner
#' 
#' \itemize{
#'   \item text. the text of the tweet
#'   \item created. the date and time of the tweet
#'   \item id. the twitter id of the tweet
#'   \item screenName. who sent the tweet
#'   \item hashSearch. the hashtag that was searched
#' }
#' 
#' @format A data frame with 3235 rows and 5 variables
#' @name ismb2014
#' @source \url{https://github.com/stephenturner/twitterchive}
#' @docType data
NULL

#' user tweet counts
#' 
#' calculates the number of tweets for each user, giving the total tweets, 
#' total that were retweets, and total that were original. Assumes that a retweet
#' starts with the text "RT"
#' 
#' @param tweetDF data frame of tweets
#' @return data.frame with user, total, original, retweet
#' @export
tweetCounts <- function(tweetDF){
  isRT <- grepl("^RT", tweetDF$text)
  rtSplit <- split(isRT, tweetDF$screenName)
  
  nTweet <- sapply(rtSplit, length)
  nRT <- sapply(rtSplit, sum)
  
  tweetC <- data.frame(screenName = names(rtSplit), total = nTweet, original = nTweet - nRT, retweet = nRT, stringsAsFactors = FALSE)
  return(tweetC)
}

#' find rt counts
#' 
#' for each original tweet, find the number of times it was retweeted.
#' 
#' @param tweetDF the data.frame of tweets
#' @export
#' @return data.frame of original tweets with countRT added
retweetCount <- function(tweetDF){
  notRT <- !(grepl("^RT", tweetDF$text))
  isRT <- !notRT
  
  orgDF <- tweetDF[notRT, ]
  rtDF <- tweetDF[isRT, ]
  
  splitOrg <- split(orgDF$text, orgDF$screenName)
  
  nOrg <- sapply(splitOrg, length)
  
  countRT <- unlist(lapply(orgDF$text, function(findTweet){
    length(grep(findTweet, rtDF$text, fixed = TRUE))
  }), use.names = FALSE)
  
  orgDF$countRT <- countRT
  orgDF <- orgDF[order(orgDF$countRT, decreasing = TRUE), ]
  return(orgDF)
}

#' total tweet rank
#' 
#' calculates rank of a user by the percentage of total tweets
#' 
#' @param tweetCounts a vector of tweets by user
#' @return ranks by percentage of total from tweetCounts
#' @export
tweetRank <- function(tweetCounts){
  twTotal <- sum(tweetCounts)
  twPerc <- tweetCounts / twTotal
  twRank <- rank(twPerc)
  
  return(twRank)
}

#' total retweeted
#' 
#' how often a users tweets were retweeted
#' 
#' @param tweetDF data.frame of tweets
#' @param countColumn which column has the retweet counts (should be a text id)
#' @export
#' @return data.frame of counts
totalRT <- function(tweetDF, countColumn){
  totRT <- tapply(tweetDF[, countColumn], tweetDF[, "screenName"], sum)
  totRT <- data.frame(screenName = names(totRT), sumRT = totRT)
  return(totRT)
}