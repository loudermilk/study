# Test of proposed ranking algorithm for optimal assignment of offers to members

library(dplyr)
library(tidyr)
set.seed(1)

agents <- 1:10
offers <- 1:10

# randomly assign p-scores between agents and offers
scores <- data.frame(matrix(data = runif(n = 100, min = 0, max = 1), 
                 nrow = length(agents), 
                 ncol = length(offers)))
names(scores) <- paste0("O",offers)
# add agent ids
df <- cbind(agents, scores)
head(df)


long_df <- gather(data = df, key = offer, value = p_score, ... = 2:11)
tail(long_df)

ordered <- long_df[order(-long_df$p_score, long_df$offer),]
head(ordered)

assignment <- data.frame(agent = agents, offer = NA, value = NA)
head(assignment)

max_iter <- 20
unassigned <- ordered
for (i in 1:max_iter) {
 for (row in 1:nrow(unassigned)) {
   a <- unassigned[row, "agents"]
   o <- unassigned[row, "offer"]
   p <- unassigned[row, "p_score"]
   if (is.na(assignment[a, "offer"]) & !(o %in% assignment$offer)) { # if agent doesnt have offer give them one
    assignment[a, "offer"] <- o
    assignment[a, "value"] <- p
   }
   
 } 
}

(orig_max <- sum(assignment$value))

set.seed(2)
max_iter <- 1e4
for (i in 1:max_iter) {
  available_offers <- offers
  assignment2 <- data.frame(agent = agents, offer = NA, value = NA)
  for (a in agents) {
    o <- sample(available_offers, size = 1, replace = FALSE)
    p <- df[a,o+1]
    #if (is.na(assignment2[a, "offer"]) & !(o %in% assignment2$offer)) { # if agent doesnt have offer give them one
    assignment2[a, "offer"] <- o
    assignment2[a, "value"] <- p
    available_offers <- setdiff(available_offers, o)
    #}
  }
  new_max <- sum(assignment2$value)
  print(paste(i, new_max))
  if (new_max > orig_max) {
    print(paste("random = ", new_max))
    print(paste("ranked = ", orig_max))
    break
  }
  
}
