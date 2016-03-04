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



 