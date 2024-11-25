# Data
substrate <- c(...)  # Substrate concentration
growth_rate <- c(...)  # Specific growth rate

# Monod model
monod <- function(S, mu_max, K_s) {
    (mu_max * S) / (K_s + S)
}

# Fit model
fit <- nls(growth_rate ~ (mu_max * substrate) / (K_s + substrate), 
           start = list(mu_max = 0.5, K_s = 1))


# Tessier model function
tessier <- function(S, mu_max, K_s) {
  mu_max * (1 - exp(-S / K_s))
}

fit <- nls(
  growth_rate ~ tessier(substrate, mu_max, K_s),
  start = list(mu_max = 0.5, K_s = 1),
  trace = TRUE
)

summary(fit)
