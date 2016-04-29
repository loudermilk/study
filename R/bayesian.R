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

# Ch. 4

# simulate num_people standing on mid-line of soccer field, each of
# which flips a coin num_flips times. Any process that adds together
# random values from the same distribution converges towards
# a normal.
num_people <- 1e5
num_flips <- 16
pos <- replicate(num_people, sum(runif(num_flips, -1, 1)))
hist(pos)
dens(pos)

# 4.1.2 Normal by multiplication

# sample 12 random numbers between 1 and 1.1 indicating proportional
# grow: 1 = no grow, 1.1 = 10% increase
prod(1 + runif(12,0,0.1))

growth <- replicate(1e4, prod(1 + runif(12,0,0.1)))
dens(x = growth, norm.comp = TRUE)

# small products are approximately normal
big <- replicate(1e4, prod(1 + runif(12,0,0.5)))
dens(big, norm.comp = TRUE)
small <- replicate(1e4, prod(1 + runif(12,0,0.01)))
dens(small, norm.comp = TRUE)


# 4.1.3 Normal by log-multiplication
# large deviates that are multiplied together do not produce
# guassian dirstributions, but they do tend to produce
# guassian distributions are the log scale.

log.big <- replicate(1e4, log(prod(1 + runif(12,0,0.5))))
dens(big, norm.comp = TRUE)
dens(log.big, norm.comp = TRUE)


# ~ indicates a stochastic relationship - a mapping of a 
# variable/parameter onto a distribution.

# 4.6

w <- 6; n <- 9
p_grid <- seq(from = 0, to = 1, length.out = 100)
posterior <- dbinom(x = w, size = n, prob = p_grid) *dunif(p_grid, 0, 1)
posterior <- posterior/sum(posterior)


# 4.7

library(rethinking)
data(Howell1)
d <- Howell1

head(d)
# going to only analyze adults, b/c age correlates with height

d2 <- d[d$age >= 18,]
dens(d2$height)

# general model
#h ~ Normal(m, s)
#m ~ Normal(178, 20)
#s - Uniform(0,50)

#plot priors
curve(dnorm(x, 178, 20), from = 100, to = 250)
curve(dunif(x, 0, 50), from = -10, to = 60)

sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)


# 4.14
mu.list <- seq(from=140, to=160, length.out = 200)
sigma.list <- seq(from=4, to=9, length.out = 200)
post <- expand.grid(mu=mu.list, sigma=sigma.list)

post$LL <- sapply(1:nrow(post), function(i){
  sum(dnorm(
    d2$height,
    mean = post$mu[i],
    sd = post$sigma[i],
    log = TRUE
  ))
})
post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) +
  dunif(post$sigma, 0, 50, TRUE)
post$prob <- exp(post$prod - max(post$prod))

contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob)

# 4.17
sample.rows <- sample(1:nrow(post), size = 1e4, replace = TRUE, prob = post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

plot(sample.mu, sample.sigma, cex = 0.5, pch=16, col=col.alpha(rangi2,0.1))
dens(sample.mu)
dens(sample.sigma)
HPDI(sample.mu)
HPDI(sample.sigma)


# 4.21
d3 <- sample(d2$height, size = 20)

mu.list <- seq(from=150, to=170, length.out = 200)
sigma.list <- seq(from=4, to=20, length.out = 200)
post2 <- expand.grid(mu = mu.list, sigma = sigma.list)

post2$LL <- sapply(1:nrow(post2), function(i)
  sum(dnorm(d3,mean=post2$mu[i], sd=post2$sigma[i], log = TRUE)))

post2$prod <- post2$LL + dnorm(post2$mu, 178, 20, TRUE) + 
  dunif(post2$sigma, 0, 50, TRUE)


post2$prob <- exp(post2$prod - max(post2$prod))

sample2.rows <- sample(1:nrow(post2), size = 1e4, replace = TRUE, prob = post2$prob)

sample2.mu <- post2$mu[sample2.rows]
sample2.sigma <- post2$sigma[sample2.rows]
plot(sample2.mu, sample2.sigma, cex = 0.5, 
     col = col.alpha(rangi2,0.1), xlab = "mu",
     ylab = "sigma", pch = 16)

# 4.23
#  shows large tail of uncertainty for high values
dens(sample2.sigma, norm.comp = TRUE)

## QUADRATIC APPROXIMATION
# the posterior's peak will lie at the maximum a posteriori estimate (MAP)
# and we can get a useful approximation of the images shape using quad approx.

library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

# model definition
# h_i ~ Normal(mu, sigma) 
# mu ~ Normal(178, 20)
# sigma ~ Uniform(0, 50)

# put R-code equivalents into a list
flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)
head(d2)
m4.1 <- map(flist = flist, data = d2)
summary(m4.1)

# specify a start for hill climbing
start <- list(
  mu = mean(d2$height),
  sigma = sd(d2$height)
)

m4.2 <- map(alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 0.1),
  sigma ~ dunif(0, 50)
), 
data = d2)

summary(m4.1)

vcov(m4.1)

diag(vcov(m4.1))
cov2cor(vcov(m4.1))

# sample vectors of values from a multi-dimensional gaussian distribution
library(rethinking)
post <- extract.samples(m4.1, n = 1e4)
head(post)
precis(post)
summary(m4.1)


plot(d2$height ~ d2$weight)

# let x = list of predictor values (weight)
# h_i ~ Normal(mu_i, sigma)
# mu_i = alpha + beta * x_i #not stochastic, rather deterministic
# alpha ~ Normal(178, 100)
# beta ~ Normal(0, 10)
# sigma ~ Uniform(0, 50)

library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18, ]

#fit model
flist <- alist(
  height ~ dnorm(mean = mu, sd = sigma),
  mu <- a + b*weight,
  a ~ dnorm(156, 100),
  b ~ dnorm(0, 10),
  sigma <- dunif(0, 50)
)

m4.3 <- map(flist = flist, data = d2)
precis(m4.3)

# choose plotting over tables - plot implcations of models to determine
# (1) whether or not the model fitting procedure worked correctly
# (2) abs magnitude of a relationship between outcome and predictor
# (3) uncertainty surrounding an average relationship
# (4) uncertainty surrounding implied predictions of the model, as these
# are distinct from mere parameter uncertainty.

# almost a perfect neg corr b/w parameters a and b
precis(m4.3, corr=TRUE)

# CENTERING - procedure of subtracting the mean of a variable from each value.
d2$weight.c <- d2$weight - mean(d2$weight)
mean(d2$weight.c) # mean is zero

# refit model
flist <- alist(
  height ~ dnorm(mean = mu, sd = sigma),
  mu <- a + b*weight.c,
  a ~ dnorm(156, 100),
  b ~ dnorm(0, 10),
  sigma <- dunif(0, 50)
)

m4.4 <- map(flist = flist, data = d2)
coef(m4.4)
precis(m4.4, corr = TRUE)
# intercept now means expected value of y when x is at average value

coef(m4.4)["a"]
# 4.45
plot(height ~ weight, data = d2)
abline(a = coef(m4.3)["a"], b = coef(m4.3)["b"], col = "red")

post <- extract.samples(m4.3)

# let's work with a subset of the data
set.seed(100)


N <- 10
dN <- d2[1:N, ]
flist <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b*weight ,
  a ~ dnorm(178, 100),
  b ~ dnorm(0, 10),
  sigma ~ dunif(0, 50)
)
mN <- map(flist = flist, data = dN)

post <- extract.samples(mN, n=20)

plot(dN$weight, dN$height, 
     xlim = range(d2$weight), ylim = range(d2$height), 
     col = rangi2, xlab = "weight", ylab= "height")
mtext(paste0("N = ", N))

for (i in 1:20) {
  abline(a = post$a[i], b = post$b[i], col = col.alpha("black", 0.3))
}
mu_at_50 <- post$a + post$b * 50

# not consistent with plot in book
dens(mu_at_50, col = rangi2, lwd = 2)

mu <- link(m4.3)
str(mu)

# 4.54

weight.seq <- seq(from = 25, to = 70, by = 1)
mu <- link(fit = m4.3, data = data.frame(weight = weight.seq))
str(mu)
plot(height ~ weight, d2, type="n")
for (i in 1:100) {
  points(weight.seq, mu[i,], pch=16, col = col.alpha(rangi2, 0.1))
}

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=.89)

# 4.57
plot(height ~ weight, data = d2, col = col.alpha(rangi2,0.5))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)

# recipe for generating predictions and intervals from the posterior of a fit model
# (1) use link to generate distributions of posterior values for mu. the default
# behavior of link is to use the original data, so you have to pass it a list of
# new horizontal axis values you want to plot posterior predictions across.
# (2) use summary functions like mean or HPDI or PI to find averages and lower/upper
# bounds for each mu for each value of the predictor variable.
# (3) use plotting functions like lines and shade to draw the lines and intervals.


