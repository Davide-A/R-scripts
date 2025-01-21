# Value at Risk.
# è una misura del rischio di perdita di un investimento. Il var stima quanto un insieme di investimenti potrebbe perdere (con una certa probabilità).
# misura la volatilità del portafoglio e quanto si stima di perdere valore in un dato intervallo temporale
library(MASS)
library(tidyverse)
df <- read.csv("C:/Users/utente/OneDrive/Documenti/Dati/sp500/SnP500 All assets.csv", header=TRUE)

# estraggo i valori in chiusura di ogni asset
close_columns <- grep("Close.*", colnames(df), value = TRUE)
closing_prices <- df[, close_columns, drop = FALSE]
colnames(closing_prices) <- closing_prices[1, ]
closing_prices <- closing_prices[-c(1, 2), ]
rownames(closing_prices) <- NULL
closing_prices <- as.data.frame(lapply(closing_prices, as.numeric))

# calcolo del logaritmo del rendimento giornaliero
portfolio_returns <- closing_prices %>%
  mutate_all(~ log(. / lag(.))) %>%
  na.omit()

#valore atteso
expected_returns <- colMeans(portfolio_returns)
)

#calcolo della matrice di varianza-covarianza
cov_matrix <- cov(portfolio_returns)
head(cov_matrix)

# definizione del portafoglio di valori
# Supponendo che ogni asset ha lo stesso peso
n_assets <- ncol(portfolio_returns)
portfolio_weights <- rep(1 / n_assets, n_assets)

n_simulations <- 100000
portfolio_value <- 11e3
confidence_level <- 0.95

# simulazione rendimenti
simulated_returns <- mvrnorm(n_simulations, mu = expected_returns, Sigma = cov_matrix)
head(simulated_returns)

# Calcolo del rendimento del portafoglio per ciascuna simulazione
portfolio_returns <- simulated_returns %*% portfolio_weights

# Calcolo del value at risk del portafoglio
VaR <- quantile(portfolio_returns, 1 - confidence_level) * portfolio_value

cat("Portfolio Value at Risk (VaR) at", confidence_level * 100, "% confidence level:\n")
cat("VaR =", round(-VaR, 2), "USD\n")

# istogramma dei rendimenti
hist(portfolio_returns, breaks = 50, main = "Monte Carlo Simulation of Portfolio Returns",
     xlab = "Portfolio Returns", col = "lightblue", border = "darkblue")
abline(v = VaR / portfolio_value, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("VaR"), col = c("red"), lty = c(2), lwd = c(2))

VaR
