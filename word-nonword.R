## How to determine if character sequence is English word or noise
## http://datascience.stackexchange.com/questions/11489/how-to-determine-if-character-sequence-is-english-word-or-noise


#' Return orthgraphic n-grams for word
#' @param w character vector of length 1
#' @param n integer type of n-gram
#' @param boundaries logical insert word boundary indicators
#' @return character vector of n-grams
#' 
getGrams <- function(w, n = 2, boundaries = TRUE) {
  require(ngram)
  
  # Insert boundary markers at beginning and end of word
  if (boundaries) {
    (w <- gsub("(^[A-Za-z])", "^\\1", w))
    (w <- gsub("([A-Za-z]$)", "\\1^", w))
  }


  # for ngram processing must add spaces between letters
  # !!! need to deal with non-alpha characters better
  w <- gsub("([A-Za-z^'])", "\\1 \\2", w)
  w <- gsub("[ ]$", "", w)

  ng <- ngram(w, n = n)
  grams <- get.ngrams(ng)
  out_grams <- sapply(grams, function(gram){return(gsub(" ", "", gram))}) #remove spaces
  return(out_grams)
}

words <- list("dog", "log", "bog", "frog")
res <- sapply(words, FUN = getGrams)
grams <- unlist(as.vector(res))
table(grams)
