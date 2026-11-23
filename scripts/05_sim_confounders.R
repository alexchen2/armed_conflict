library(foreach)
library(xtable)

# Context: Testing for impact of adjusting vs ignoring confounder variables in a model;
# sample example studying the relationship between coffee drinking (X) and lung cancer (Y),
# with smoking (Z) as a confounder.

# set simulation parameters
n <- 500     # sample size
pz <- 0.2    # probability of Z = 1
alpha0 <- 0  # logit probability of x = 1 in non-smokers (z = 0)    
beta0 <- -3  # logit prob of y = 1 in non-coffee drinkers (x = 0) and non-smokers (z = 0)
beta1 <- 0       
beta2 <- 2
sig_lvl <- 0.05

# log odds ratio of being a coffee drinker (x = 1) 
# in smokers (z = 1) vs non-smokers; 
# simulating multiple scenarios for this alpha1 log odds ratios
alpha1_scen <- c(0, 1, 2)  
# number of simulations per scenario
nsim <- 1000  

# Conduct nsim=1000 simulations over 3 scenarios of alpha1, testing H0 : beta1 = 0
# under sig_lvl=0.05 level of significance
results <- foreach(alpha1=alpha1_scen, .combine=rbind) %do% {
  sim_pvals <- foreach(i=1:nsim, .combine=rbind) %do% {
    set.seed(i + 2024)  # set seed for reproducibility

    # generate confounder Z from a binomial distribution
    z <- rbinom(n, size = 1, prob = pz)
    # compute probability of observing X = 1 from the inverse logit function
    px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
    # randomly generate binary variable X from the above probability
    x <- rbinom(n, size = 1, prob = px)
    # randomly generate binary variable Y from the inverse logistic function
    py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
    y <- rbinom(n, size = 1, prob = py)

    # combine three random variables into a data frame 
    dat <- data.frame(lung = y, coffee = x, smoke = z)

    # fit unadjusted logistic regression model
    unadj.mod <- glm(lung ~ coffee, data = dat, family = "binomial")
    unadj.coef <- summary(unadj.mod)$coef

    # fit adjusted logistic regression model
    adj.mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
    adj.coef <- summary(adj.mod)$coef

    # return p-values for effect associated with coffee
    p_alpha1 <- list(
      "Unadjusted" = unadj.coef[2,4], 
      "Adjusted" = adj.coef[2,4]
    )
  }
  # Calculate emperical type 1 error: proportion of p-values rejecting null hyp over 
  # total num of simulations conducted

  # Number of p-values rejecting null for unadjusted model
  p_unrej_unadj <- sapply(
    sim_pvals[,"Unadjusted"],
    \(x) x <= sig_lvl
  )
  num_p_unrej_adj <- sum(p_unrej_unadj)

  # Number of p-values rejecting null for adjusted model
  p_rej_adj <- sapply(
    sim_pvals[,"Adjusted"],
    \(x) x <= sig_lvl
  )
  num_p_rej_adj <- sum(p_rej_adj) 

  # Emperical type 1 error for both models
  type1_err <- list(
    "Adjusted" = num_p_rej_adj / nsim,
    "Unadjusted" = num_p_unrej_adj / nsim 
  )
}
results <- as.data.frame(results, row.names = 1:nrow(df))
results <- cbind("alpha_1" = c(0, 1, 2), results)

# Export results to table in html file
# sink("reports/table05_sim_confounders.html")
xt <- xtable(
  results,
  caption = "Type I Error Rate",
  digits = 3
)
print(
  xt, 
  type="html", 
  file="reports/table05_sim_confounders.html",
  include.rownames = FALSE,
  caption.placement = "top",
  html.table.attributes = 'class="grayGridTable"'
)
# sink()

