#
# Given: 100 balls marked #1 - #100. Define process P as: randomly choose
# a ball; if ball has a star on it put it back, otherwise put a star on
# it and put it back.
#
# After repeating process P 100 times, locate ball #1. What is the
# probability it doesn't have a star on it?

rm(list = ls())
num_balls <- 100
num_trials <- 500

getBallOne <- function() {
  balls <- rep(FALSE, num_balls) # all balls have_star = FALSE

  for (t in 1:num_trials) { # for each trial
    b_index <- sample(1:num_balls, 1) # pick a ball at random
    balls[b_index] <- TRUE # if it doesn't already have a star, then add a star
  }

  b1_star <- balls[1] # does the first ball have a star?
  return(b1_star)
}


num_epochs <- 10000
results <- c(NA)
length(results) <- num_epochs

for (e in 1:num_epochs) {
  results[e] <- getBallOne()
}


prob_star <- sum(results)/length(results) # prob of a star
prob_notstar <- 1 - prob_star

print(paste("Probability of no star is", prob_notstar))


# 3 ants sit on corner of equilateral triangle. Each ant randomly
# picks a direction and starts walking. What is the probability
# that there will not be an ant collision? What is the probability
# for a polygon with n vertices and n ants? 
#
num_vertices <- 3

probNoCollision <- function(num_vertices) {
  if (num_vertices < 3) {stop("num_vertices must define a polygon i.e. num_vertices > 2")}
  0.5^(num_vertices - 1)
}

probNoCollision(3)


# Let X be the number of children in an ordinary American family.
# Let Pr(X = i) = pi, for i = 1,2,3,4 and Pr(X>4)=0. What is the 
# probability there are at least two girls in the family?

P1 + P2 + P3 + P4 = 1
??rollApply




read.table(header = T, text= "A B C
1 2 3
1 2 3
1 2 3")

.1 == (.3/3)


data_set <- c("the big dog", "the small cat", "the big and fat cow")
words <- strsplit(data_set, split = " ") #tokenize sentences
words
##[[1]]
##[1] "the" "big" "dog"
##
##[[2]]
##[1] "the"   "small" "cat"  
##
##[[3]]
##[1] "the" "big" "and" "fat" "cow"


vec <- unique(unlist(words)) #vector representation of sentences
##[1] "the"   "big"   "dog"   "small" "cat"   "and"  
##[7] "fat"   "cow" 

m <- matrix(nrow = length(data_set), ncol = length(vec))

for (i in 1:length(words)) { #iterate the index of tokenized sentences
  vec_rep <- vec %in% words[[i]] #create binary word-feature vector
  m[i,] <- vec_rep #update matrix
}

df <- data.frame(m, row.names = NULL)
names(df) <- vec
df
##   the   big   dog small   cat   and   fat   cow
##1 TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
##2 TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE
##3 TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE

(target_vec <- df[1,])

numAnimals <- function(target_vec) {
  animals <- c("dog", "cat", "sheep", "horse", "cow")
  target_cols <- base::intersect(animals, names(target_vec))
  
  
  num_animals <- sum(as.integer(target_vec[,target_cols]))
  return(num_animals)
}


dplyr::mutate(df, num_animals = numAnimals())



# 4 marbles; marbles come in o & x
#ways <- c(0,3,8,9,0)
#plaus <- ways/sum(ways)
#conjectures:  x o x ways plaus
#      1 oooo  0 4 0 0
#      2 xooo  1 3 1 3
#      3 xxoo  2 2 2 8
#      4 xxxo  3 1 3 9
#      5 xxxx  4 0 4 0
      
      
      observations <- c(1,0,1,1,1,0,1,0,1)
      size <- 9
      x <- 6
p <- .5
      
      getPredPerc <- function(x, size) {
        parameters <- seq(from = 0, to = 1, by = 0.001)
        
        out_vec <- numeric(length(parameters))
        i <- 1
        
        for (p in parameters) {
          y <- 1 - p
          res <- (p^x)*(y^(size - x))
          choose(size,6)*res
          
          
          out_vec[i] <- res
          i <- i + 1
        }
        print(paste("max plaus", max(out_vec)))
        p_index <- which(out_vec == max(out_vec))
        parameters[p_index]
      }  
      
    getPredPerc(x = x, size = size)      
    

x <- c(3,5,7,9)        
y <- c(2,4,6,8)
expected <- c(7,23,47,79)
f1 <- function(x,y) {x*y + x - 2} #x(y+1) - 2
f2 <- function(x,y) {x*y + y - 1} #y(x+1) - 1
f3 <- function(x) {x*x - 2} #x^2-2

assertthat::assert_that(sum(expected-f1(x,y))==0)
assertthat::assert_that(sum(expected-f2(x,y))==0)
assertthat::assert_that(sum(expected-f3(x))==0)

foo(10,9)





data_set <- c("the big bad dog", "the small bad cat", "a silly skunk on a log")
words <- strsplit(data_set, split = " ") #tokenize sentences
vec <- unique(unlist(words)) #vector representation of sentences
# [1] "the"   "big"   "bad"   "dog"   "small" "cat"   "a"     "silly" "skunk" "on"    "log" 

m <- matrix(nrow = length(data_set), ncol = length(vec))

for (i in 1:length(words)) { #iterate the index of tokenized sentences
  vec_rep <- as.integer(vec %in% words[[i]]) #create binary word-feature vector
  m[i,] <- vec_rep #update matrix
}

df <- data.frame(m, row.names = NULL)
names(df) <- vec
df

cosineSimilarity <- function(df, row1, row2){
  x <- as.numeric(df[row1,])
  y <- as.numeric(df[row2,])
  (x %*% y) / (sqrt(x%*%x * y%*%y))
  
}

cosineSimilarity(df,1,1)
cosineSimilarity(df,1,2)
cosineSimilarity(df,1,3)
