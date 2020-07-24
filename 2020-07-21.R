# I would like to better understand rstanarm's regularised linear models (Bayesian linear regression). 
# To this end, I will try to simulate data that is similar to selected variables from the "clouds" 
# dataset (package "HSAUR3"), and further use it within a Bayesian model.

library(ggplot2)
library(rstanarm)

data("clouds", package = "HSAUR3")

# The original data set is relatively small. Achieving similar results for the linear regression with
# the simulated and the original data seemed thus to be only possible with much simulated data.

n <- 24 # 24 simulated data points were not enough

# Prewetness = total rainfall in the target area one hour before seeding (in cubic metres times 1e+8)
ols <- lm(rainfall ~ prewetness, data = clouds)

clouds$res <- residuals(ols)

# There are two rather inconvenient outliers, that may make many simulated "observations" necessary.
p_res <- ggplot(clouds, aes(sample = res))
p_res + stat_qq() + stat_qq_line()

interc <- round(coef(ols), 3)[1]
coef_ols <- round(coef(ols), 3)[2]
sigma <- sigma(ols)

m_pred <- mean(clouds$prewetness)
sd_pred <- sd(clouds$prewetness)

x <- rnorm(n, m_pred, sd_pred) # simulating the predictor
y = rep(0, n) # simulating the response

for ( i in 1:n ){
  mu = x[i]*coef_ols + interc
  y[i] = rnorm(1, mu, sigma)
}

clouds_sim <- data.frame(prewetness = x, rainfall = y)

# Here, we check if we have got approximately the same coefficients. 
ols_sim <- lm(rainfall ~ prewetness, data = clouds_sim)

coef_sim <- round(coef(ols_sim), 3)

# Similarly, we check if we obtain approximately the same coefficients within a Bayesian analysis.

reglin_bayes <-
  stan_lm(
    rainfall ~ prewetness,
    data = clouds_sim,
    prior = NULL,
    seed = 12345
  )

reglin_bayes

# We find that the Bayesian model slightly overfits the simulated data (log-fit_ratio, ln omega), the 
# coefficients of the simulated data are quite close to the original ones, and the median absolute 
# deviation is small. 








