# Install and load packages
# install.packages("dplyr")
# install.packages("invgamma")
# library(dplyr)
library(invgamma)

# Data Preparation #############################################################
# Import Data
setwd("C:/Users/dhwan/OneDrive/Documents/MyFiles/ACTL3301/")
# setwd("~/Desktop/3301 Ass1")

set.seed(19)

amd <- read.csv("AMD.csv")
nvda <- read.csv("NVDA.csv")

# Order
amd$Date <- as.Date(amd$Date, format="%Y-%m-%d")
nvda$Date <- as.Date(nvda$Date, format="%Y-%m-%d")

amd <- amd[order(amd$Date), ]
nvda <- nvda[order(nvda$Date), ]

# Daily Return
amd$Xt <- c(NA, diff(log(amd$Adj.Close)))
nvda$Xt <- c(NA, diff(log(nvda$Adj.Close)))

amd <- na.omit(amd)
nvda <- na.omit(nvda)

# Potential losses
amd$Lt <- 1 - exp(amd$Xt)
nvda$Lt <- 1 - exp(nvda$Xt)

# View(amd)
# View(nvda)

# Task 1 #######################################################################
alpha <- c(0.95, 0.99, 0.995)

l1 <- amd$Lt
l2 <- nvda$Lt

# Function to calculate VaR as the largest loss less than the given quantile
calculate_var <- function(losses, a) {
  threshold <- quantile(losses, a, type = 1)
  min(losses[losses >= threshold])
} 

# Function to calculate ES
calculate_es <- function(losses, var, a, cdf) {
  F_var <- cdf(var)
  delta <- abs(F_var - a)
  # cat("F_var:", F_var, "a:", a, "delta:", delta, "\n")
  ((var * delta) / (1 - a)) + mean(losses[losses >= var])  
}

var_95 <- calculate_var(l1, 0.95)
var_99 <- calculate_var(l1, 0.99)
var_995 <- calculate_var(l1, 0.995)

es_95 <- calculate_es(l1, var_95, 0.95, ecdf(l1))
es_99 <- calculate_es(l1, var_99, 0.99, ecdf(l1))
es_995 <- calculate_es(l1, var_995, 0.995, ecdf(l1))

amd_risk <- data.frame(
  Alpha = alpha,
  VaR = c(var_95, var_99, var_995),
  ES = c(es_95, es_99, es_995)
)


# Calculations for NVDA
var_95 <- calculate_var(l2, 0.95)
var_99 <- calculate_var(l2, 0.99)
var_995 <- calculate_var(l2, 0.995)

es_95 <- calculate_es(l2, var_95, 0.95, ecdf(l2))
es_99 <- calculate_es(l2, var_99, 0.99, ecdf(l2))
es_995 <- calculate_es(l2, var_995, 0.995, ecdf(l2))

nvda_risk <- data.frame(
  Alpha = alpha,
  VaR = c(var_95, var_99, var_995),
  ES = c(es_95, es_99, es_995)
)

# Empirical Joint #############################################################
l <- 0.5 * (l1 + l2)

# Calculations for NVDA
var_95 <- calculate_var(l, 0.95)
var_99 <- calculate_var(l, 0.99)
var_995 <- calculate_var(l, 0.995)

es_95 <- calculate_es(l, var_95, 0.95, ecdf(l))
es_99 <- calculate_es(l, var_99, 0.99, ecdf(l))
es_995 <- calculate_es(l, var_995, 0.995, ecdf(l))

e_risk <- data.frame(
  Alpha = alpha,
  VaR = c(var_95, var_99, var_995),
  ES = c(es_95, es_99, es_995)
)

# Task 2 #######################################################################

# Calculations for Portfolio

u1 <- runif(5000, 0, 1)
u2 <- runif(5000, 0, 1)

U1_independent <- u1
U2_independent <- u2

X1 <- sapply(U1_independent, function(u) calculate_var(nvda$Xt, u))
X2 <- sapply(U2_independent, function(u) calculate_var(amd$Xt, u))

l1_indep <- 1 - exp(X1)
l2_indep <- 1 - exp(X2)

l = 0.5 * (l1_indep + l2_indep)

var_95 <- calculate_var(l, 0.95)
var_99 <- calculate_var(l, 0.99)
var_995 <- calculate_var(l, 0.995)

es_95 <- calculate_es(l, var_95, 0.95, ecdf(l))
es_99 <- calculate_es(l, var_99, 0.99, ecdf(l))
es_995 <- calculate_es(l, var_995, 0.995, ecdf(l))

portfolio_risk <- data.frame(
  Alpha = alpha,
  VaR = c(var_95, var_99, var_995),
  ES = c(es_95, es_99, es_995)
)

# Print
e_risk
amd_risk
nvda_risk
portfolio_risk


# Task 3 #######################################################################
# Calculate Kendall's Tau
calculate_kendalls_tau <- function(L1, L2) {
  n <- length(L1)
  num_conc <- 0
  num_disc <- 0
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if ((L1[i] - L1[j]) * (L2[i] - L2[j]) > 0) {
        num_conc <- num_conc + 1
      } else if ((L1[i] - L1[j]) * (L2[i] - L2[j]) < 0) {
        num_disc <- num_disc + 1
      }
    }
  }
  
  tau <- (num_conc - num_disc) / choose(n, 2)
  return(tau)
}

kendalls_tau <- calculate_kendalls_tau(amd$Lt, nvda$Lt)

kendalls_tau

# Calculate Spearman's Rho
ecdf_1 = ecdf(l1)
ecdf_2 = ecdf(l2)

F_l1 = ecdf_1(l1)
F_l2 = ecdf_2(l2)

spearmans_rho <- cor(F_l1, F_l2)

correlation <- cor(l1, l2)
correlation

# Estimate Spearman's Rho
#estimate_spearmans_rho <- function(L1, L2) {
#rank_L1 <- rank(L1)
#rank_L2 <- rank(L2)
#n <- length(L1)
#diff_squared <- sum((rank_L1 - rank_L2)^2)
#cat("diff_squared:", diff_squared, "n:", n)

#rho <- 1 - (6 * diff_squared) / (n * (n^2 - 1))
#return(rho)
#}

#spearmans_rho_estimate <- estimate_spearmans_rho(amd$Lt, nvda$Lt)

# Validation Result
#spearmans_rho_validate <- cor(amd$Lt, nvda$Lt, method = "spearman")

# Print 
spearmans_rho
#spearmans_rho_estimate
#spearmans_rho_validate

# Task 4 #######################################################################

# Simulate Gaussian Copula
# Simulate m.v. Normal(0, rho)

# Transform u1, u2 into z1, z2 N(0,1) r.v.
z1 <- qnorm(u1)
z2 <- qnorm(u2)

# Consider correlation
rho <- cor(amd$Xt, nvda$Xt)

y1 <- z1
y2 <- (rho * z1) + (sqrt(1 - rho^2) * z2)

# U = F(Y)
U1 <- pnorm(y1)
U2 <- pnorm(y2)

U1_gaussian <- U1
U2_gaussian <- U2

# Plot 
plot(U1, U2, main="Simulated Samples of Gaussian Copula", xlab="U1", ylab="U2")

# Skylar's theorem
X1 <- sapply(U1_gaussian, function(u) calculate_var(nvda$Xt, u))
X2 <- sapply(U2_gaussian, function(u) calculate_var(amd$Xt, u))

L1_gauss <- 1 - exp(X1)
L2_gauss <- 1 - exp(X2)

plot(L1_gauss, L2_gauss, main="Gaussian Copula Model Losses", xlab="L_N", ylab="L_A")

#
#print(X1)
#print(X2)

l = 0.5 * ((1 - exp(X1)) + (1 - exp(X2)))

# Calculations for Portfolio
# ecdf_l <- rank(l) / length(l)

var_95 <- calculate_var(l, 0.95)
var_99 <- calculate_var(l, 0.99)
var_995 <- calculate_var(l, 0.995)

es_95 <- calculate_es(l, var_95, 0.95, ecdf(l))
es_99 <- calculate_es(l, var_99, 0.99, ecdf(l))
es_995 <- calculate_es(l, var_995, 0.995, ecdf(l))

sim_portfolio_risk <- data.frame(
  Alpha = alpha,
  VaR = c(var_95, var_99, var_995),
  ES = c(es_95, es_99, es_995)
)

sim_portfolio_risk



# Task 5 #######################################################################
# Define the parameters
params <- list(
  list(shape = 0.5 * 3, rate = 0.5 * 3, df = 3),
  list(shape = 0.5 * 10, rate = 0.5 * 10, df = 10),
  list(shape = 0.5 * 10000, rate = 0.5 * 10000, df = 10000)
)

# Generate W values
W <- lapply(params, function(p) 1 / rgamma(5000, p$shape, p$rate))

# Simulate y values
y_sim <- lapply(W, function(W_i) {
  list(
    y1 = sqrt(W_i) * y1,
    y2 = sqrt(W_i) * y2
  )
})

# Simulate U values
U <- lapply(seq_along(params), function(i) {
  list(
    U1 = pt(y_sim[[i]]$y1, params[[i]]$df),
    U2 = pt(y_sim[[i]]$y2, params[[i]]$df)
  )
})

U1_t3 <- U[[1]]$U1
U2_t3 <- U[[1]]$U2

U1_t10 <- U[[2]]$U1
U2_t10 <- U[[2]]$U2

U1_t10000 <- U[[3]]$U1
U2_t10000 <- U[[3]]$U2

# Define a function to calculate risk metrics
calculate_risk_metrics <- function(U1, U2, Xt_n, Xt_amd) {
  list(
    X1 = sapply(U1, function(u) calculate_var(Xt_n, u)),
    X2 = sapply(U2, function(u) calculate_var(Xt_amd, u)),
    l = 0.5 * ((1 - exp(sapply(U1, function(u) calculate_var(Xt_n, u)))) + (1 - exp(sapply(U2, function(u) calculate_var(Xt_amd, u)))))
  )
}

# Calculate risk metrics for each parameter set
risk_metrics <- lapply(seq_along(params), function(i) {
  metrics <- calculate_risk_metrics(U[[i]]$U1, U[[i]]$U2, nvda$Xt, amd$Xt)
  
  list(
    VaR = sapply(c(0.95, 0.99, 0.995), function(alpha) calculate_var(metrics$l, alpha)),
    ES = sapply(c(0.95, 0.99, 0.995), function(alpha) calculate_es(metrics$l, calculate_var(metrics$l, alpha), alpha, ecdf(metrics$l)))
  )
})

# Create data frames for each parameter set
sim_portfolio_risk <- lapply(seq_along(params), function(i) {
  data.frame(
    Alpha = c(0.95, 0.99, 0.995),
    VaR = risk_metrics[[i]]$VaR,
    ES = risk_metrics[[i]]$ES
  )
})

# Plot results
plot_titles <- c("Simulated Samples of t Copula - v = 3", "Simulated Samples of t Copula - v = 10", "Simulated Samples of t Copula - v = 10^4")
lapply(seq_along(params), function(i) {
  plot(U[[i]]$U1, U[[i]]$U2, main=plot_titles[i], xlab="U1", ylab="U2")
})

# Display results
sim_portfolio_risk



# Task 6 #######################################################################
# Comonotonicity copula
U1 <- u1
U2 <- u1

U1_co <- U1
U2_co <- U2

plot(U1_co, U2_co, main="Simulated Samples of (U1, U2)", xlab="U1", ylab="U2")

X1 <- sapply(U1, function(u) calculate_var(nvda$Xt, u))
X2 <- sapply(U2, function(u) calculate_var(amd$Xt, u))

l = 0.5 * ((1 - exp(X1)) + (1 - exp(X2)))

var_95 <- calculate_var(l, 0.95)
var_99 <- calculate_var(l, 0.99)
var_995 <- calculate_var(l, 0.995)

es_95 <- calculate_es(l, var_95, 0.95, ecdf(l))
es_99 <- calculate_es(l, var_99, 0.99, ecdf(l))
es_995 <- calculate_es(l, var_995, 0.995, ecdf(l))

sim_portfolio_risk <- data.frame(
  Alpha = c(0.95, 0.99, 0.995),
  VaR = c(var_95, var_99, var_995),
  ES = c(es_95, es_99, es_995)
)

sim_portfolio_risk

# Countermonotonicity copula
U1 <- u1
U2 <- 1 - U1

U1_counter <- U1
U2_counter <- U2

plot(U1_counter, U2_counter, main="Simulated Samples of (U1, U2)", xlab="U1", ylab="U2")

X1 <- sapply(U1, function(u) calculate_var(nvda$Xt, u))
X2 <- sapply(U2, function(u) calculate_var(amd$Xt, u))

l = 0.5 * ((1 - exp(X1)) + (1 - exp(X2)))

var_95 <- calculate_var(l, 0.95)
var_99 <- calculate_var(l, 0.99)
var_995 <- calculate_var(l, 0.995)

es_95 <- calculate_es(l, var_95, 0.95, ecdf(l))
es_99 <- calculate_es(l, var_99, 0.99, ecdf(l))
es_995 <- calculate_es(l, var_995, 0.995, ecdf(l))

sim_portfolio_risk <- data.frame(
  Alpha = c(0.95, 0.99, 0.995),
  VaR = c(var_95, var_99, var_995),
  ES = c(es_95, es_99, es_995)
)

sim_portfolio_risk

# Task 7 #######################################################################
# Pr(A | B) = Pr (AB) / Pr(B)

calc_tail_dep <- function(A, B, alpha) {
  sum(A[B > alpha] > alpha) / sum(B > alpha)
}


alpha_values <- c(0.95, 0.99, 0.995)

tail_dep_indep <- sapply(alpha_values, function(alpha) {
  calc_tail_dep(U1_independent, U2_independent, alpha)
})

tail_dep_indep

tail_dep_gaussian <- sapply(alpha_values, function(alpha) {
  calc_tail_dep(U1_gaussian, U2_gaussian, alpha)
})

tail_dep_gaussian

tail_dep_t3 <- sapply(alpha_values, function(alpha) {
  calc_tail_dep(U1_t3, U2_t3, alpha)
})

tail_dep_t3

tail_dep_t10 <- sapply(alpha_values, function(alpha) {
  calc_tail_dep(U1_t10, U2_t10, alpha)
})

tail_dep_t10

tail_dep_t10000 <- sapply(alpha_values, function(alpha) {
  calc_tail_dep(U1_t10000, U2_t10000, alpha)
})

tail_dep_t10000

tail_dep_co <- sapply(alpha_values, function(alpha) {
  calc_tail_dep(U1_co, U2_co, alpha)
})

tail_dep_co

tail_dep_counter <- sapply(alpha_values, function(alpha) {
  calc_tail_dep(U1_counter, U2_counter, alpha)
})

tail_dep_counter

X1_empirical <- nvda$Lt
X2_empirical <- amd$Lt

U1_empirical <- ecdf(X1_empirical)(X1_empirical)
U2_empirical <- ecdf(X2_empirical)(X2_empirical)

tail_dep_empirical <- sapply(alpha_values, function(alpha) {
  calc_tail_dep(U1_empirical, U2_empirical, alpha)
})

tail_dep_empirical


## IGNORE ######################################################################

L <- 0.5 * (nvda$Lt + amd$Lt)

export <- data.frame(
  date = nvda$Date,
  X1 = nvda$Xt,
  X2 = amd$Xt,
  L1 = nvda$Lt,
  L2 = amd$Lt
)

write.csv(export, "z5421168_Dhwanish_Kshatriya", row.names = FALSE)
