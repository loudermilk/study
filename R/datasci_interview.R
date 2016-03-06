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
      
      p <- seq(from = 0, to = 1, by = 0.001)
      observations <- c(1,0,1,1,1,0,1,0,1)
      
      getPredPerc <- function(observations, p) {
        
        out_vec <- numeric(length(p))
        i <- 1
        
        for (x in p) {
          y <- 1 - x
          num_x <-sum(observations)
          num_y <- length(observations) - num_x
          res <- (x^num_x)*(y^num_y)
          out_vec[i] <- res
          i <- i + 1
        }
        
        p_index <- which(out_vec == max(out_vec))
        p[p_index]
      }  
      
    getPredPerc(observations = observations, p = p)      
      
