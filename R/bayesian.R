
# 3.2
p_grid <- seq(from = 0, to = 1, length.out = 1000)

prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)

posterior[which.max(posterior)]


# 3.3
set.seed(100)
samples <- sample(p_grid, prob=posterior, size = 1e4, replace = TRUE)

# how much posterior probability lies below p = 0.2
format(sum(samples < 0.2)/1e4, scientific = FALSE)


# how much posterior probability lies above p = 0.8
format(sum(samples > 0.8)/1e4, scientific = FALSE)

# how much posterior probability lies between p = 0.2 and p = 0.8
format(sum(samples > 0.2 & samples < 0.8)/1e4, scientific = FALSE)

# 20% of the posterior probability lies below what value of p?

quantile(x = samples, probs = .2)

# 20% of the posterior probability lies above what value of p?
quantile(x = samples, probs = c(.8,1))

# which value of p contains the narrowest interval equal to 66% of the posterior probability
HPDI(samples = samples, prob = .66)

# whuch values of p contain 66% of the posterior probability,
# assuming equal posterior probability both below and above the interval?
PI(samples, prob = .66)

# Suppose the globe tossing data had turned out to be 8 water in 15 tosses.
# Construct the posterior distribution using grid approximation. Use the
# same flat prior as before.

p_grid <- seq(from = 0, to = 1, length.out = 1000)

prior <- rep(1, 1000)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)

posterior[which.max(posterior)]

# 3M2
samples <- sample(p_grid, prob=posterior, size = 1e4, replace = TRUE)
HPDI(samples = samples, prob = .9)
3/64


3/sum(c(3,8,9))
#3M3


library(rethinking)
plot(samples)
dens(samples)

# 3.6
sum(posterior[p_grid < 0.5])
# 3.7
sum(samples < 0.5)/1e4



sum(samples > 0.5 & samples < 0.75)/1e4


probs <- seq(from = 0, to = 1, length=100)
ll <- dbinom(x = 6,size = 9,prob = probs)

probs[which.max(ll)]


plot(ll)
tail(probs)
