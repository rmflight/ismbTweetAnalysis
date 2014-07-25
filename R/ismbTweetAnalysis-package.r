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
#' }
#' 
#' @format A data frame with 3162 rows and 4 variables
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