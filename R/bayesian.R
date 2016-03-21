library(rethinking)
# 3.5 Practice

# EASY

# grid of parameters representing hypotheses of % water of globe
p_grid <- seq(from = 0, to = 1, length.out = 1000)

# assume a flat prior (i.e. no other knowledge)
prior <- rep(1, 1000)

# for each parameter, compute likelihood of 6 water in 9 trails
likelihood <- dbinom(6, size = 9, prob = p_grid)

# add effect of prior and normalize
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)

# from the grid of parameters weighted by their 
# posterior probability, sample with replacement
set.seed(100)
samples <- sample(p_grid, prob=posterior, size = 1e4, replace = TRUE)

# 3E1. How much posterior probability lies below p = 0.2?
sum(samples < 0.2)/length(samples)

# 3E2. How much posterior probability lies above p = 0.8?
sum(samples > 0.8)/length(samples)

# 3E3. How much posterior probability lies between p = 0.2 & p = 0.8?
sum(samples > 0.2 & samples < 0.8)/length(samples)

# 3E4. 20% of the posterior probability lies below what value of p?
quantile(samples, probs = 0.2)

# 3E5. 20% of the posterior probability lies above what value of p?
quantile(samples, probs = 0.8)

# 3E6. Which values of p contain the narrowest interval equal 
# to 66% of the posterior probability? (hint ?HPDI)
HPDI(samples, prob = 0.66)

# 3E7. Which values of p contain 66% of the posterior probability,
# assuming equal posterior probability both below and above the interval?
PI(samples, prob = 0.66)

# MEDIUM

# 3M1. Suppose the globe tossing data had turned out to be 8 water in 15 tosses.
# Construct the posterior distribution using grid approximation. Use the
# same flat prior as before.

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)

# 3M2. Draw 10,000 samples from the grid approximation above.
# Then use the samples to calculate the 90% HPDI for p.
set.seed(100)
samples <- sample(x = p_grid, size = 1e4, prob = posterior, replace = TRUE)
HPDI(samples, prob = 0.9)

# 3M3. Construct a posterior predictive check for this model and data.
# This means simulate the distribution of samples, averaging over the
# posterior uncertainty in p. What is the probability of observing
# 8 water in 15 tosses?
set.seed(100)
sample_size <- 1e4
d_samples <- rbinom(sample_size, size = 15, p = 0.66)
table(d_samples)/sample_size
sum(d_samples == 8)/length(d_samples)
simplehist(d_samples, xlab="Dummy water count")

# 3M4. Using the posterior distribution constructed from the new (8/15) data,
# now calculate the probability of observing 6 water in 9 tosses.

d_samples <- rbinom(sample_size, size = 9, p = 0.66)
table(d_samples)/sample_size
sum(d_samples == 6)/length(d_samples)
simplehist(d_samples, xlab="Dummy water count")

# 3M5. Start over at 3M1, but now use a prior that is zero below p = 0.5
# and a contstant above p = 0.5. This corresponds to prior information
# that a majority of the Earth's surface is water. Repeat each problem
# above and compare the inferences. What difference does the better
# prior make? If it helps, compare the inferences (using both priors)
# to the true value p = 0.7.



# 3M5.1 Suppose the globe tossing data had turned out to be 8 water in 15 tosses.
# Construct the posterior distribution using grid approximation. Use the
# same flat prior as before.
CONS <- 1
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <-  ifelse(p_grid < 0.5, 0, CONS)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)

# 3M5.2. Draw 10,000 samples from the grid approximation above.
# Then use the samples to calculate the 90% HPDI for p.
set.seed(100)
samples <- sample(x = p_grid, size = 1e4, prob = posterior, replace = TRUE)
HPDI(samples, prob = 0.9)


# 3M5.3. Construct a posterior predictive check for this model and data.
# This means simulate the distribution of samples, averaging over the
# posterior uncertainty in p. What is the probability of observing
# 8 water in 15 tosses?
set.seed(100)
sample_size <- 1e4
d_samples <- rbinom(sample_size, size = 15, p = 0.66)
table(d_samples)/sample_size
sum(d_samples == 8)/length(d_samples)
simplehist(d_samples, xlab="Dummy water count")

# 3M5.4. Using the posterior distribution constructed from the new (8/15) data,
# now calculate the probability of observing 6 water in 9 tosses.

d_samples <- rbinom(sample_size, size = 9, p = 0.66)
table(d_samples)/sample_size
sum(d_samples == 6)/length(d_samples)
simplehist(d_samples, xlab="Dummy water count")


# HARD
data(homeworkch3)

# 3H1. Using grid approximation, compute the posterior distribution for 
# the probability of a birth being a boy. Assume a uniform prior probablity.
# Which parameter value maximizes the posterior probability.

total_boys <- sum(birth1) + sum(birth2)
total_kids <- length(birth1) + length(birth2)
  
  
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(total_boys, size = total_kids, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)

p_grid[which.max(posterior)]

# 3H2. Using the sample function, draw 10,000 random parameter values from
# the posterior distribution you calculated above. Use the samples to estimate
# the 50%, 89%, and 97% highest posterior intervals.

set.seed(100)
samples <- sample(x = p_grid, size = 1e4, prob = posterior, replace = TRUE)
quantile(x = samples, probs = c(0.5, 0.89, 0.97))
dens(samples)


# 3H3. Use rbinom to simulate 10k replicates of 200 births. You should end up
# with 10k numbers, each one a count of boys out of 200 births. Compare the 
# distribution of predicted numbers of boys to the actual count in the data (111/200).
# There are many good ways to visualize the simulations, but dens command is
# probably the easiest. Does it look like the model fits the data well? That is
# does the distribution of predictions include the actual observation as 
# a central likely income.
set.seed(100)
d_samples <- rbinom(n = 1e4, size = 200,prob = 0.55)
dens(d_samples)
abline(v = total_boys)

# 3H4. Now compare 10k counts of boys from 100 simulated first borns only to
# the number of boys in the first births, birth1. How does the model look 
# in this light?
total_1b_boys <- sum(birth1)
total_1b_kids <- length(birth1)

set.seed(100)
d_samples <- rbinom(n = 1e4, size = 100,prob = 0.55)
dens(d_samples)
abline(v = total_1b_boys)

# 3H5. The model assumes that the sex of the first and the second births
# are independent. To check this assumption, focus now on the second births
# that followed female first borns. Compare 10k simulated counts of boys to
# only those second births that followed girls. To do this correctly, you need
# to count the number of first borns who were girls and simulate that many births
# 10,000 times. Compare the counts of boys in your simulations to the actual
# observed counts of boys in your simulations to the actual observed count of
# boys following girls. How does the model look in this light? Any guesses what is going
# on in this data?



