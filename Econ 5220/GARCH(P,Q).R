############################################################
# GARCH(P, Q) – REAL-WORLD ONLY VERSION
# Clean employer-ready script
############################################################

rm(list = ls())
graphics.off()

# Packages
library(quantmod)
library(rugarch)
library(PerformanceAnalytics)

set.seed(123)

ticker        <- "AAPL"
start_date    <- "2020-01-01"
end_date      <- Sys.Date()

p_order <- 1
q_order <- 1
innovation_dist <- "std"

compute_log_returns <- function(price_series) {
  returns <- diff(log(price_series))
  returns <- na.omit(returns)
  return(returns)
}

print_return_summary <- function(returns, label = "Returns") {
  cat("\n", paste(rep("=", 55), collapse = ""), "\n", sep = "")
  cat(label, "Summary\n")
  cat(paste(rep("=", 55), collapse = ""), "\n", sep = "")
  cat("Observations:", length(returns), "\n")
  cat("Mean       :", round(mean(returns), 6), "\n")
  cat("Std. Dev.  :", round(sd(returns), 6), "\n")
  cat("Min        :", round(min(returns), 6), "\n")
  cat("Max        :", round(max(returns), 6), "\n")
  cat("Skewness   :", round(PerformanceAnalytics::skewness(returns), 6), "\n")
  cat("Kurtosis   :", round(PerformanceAnalytics::kurtosis(returns), 6), "\n")
}

build_garch_spec <- function(p = 1, q = 1, dist = "std") {
  ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
    mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = dist
  )
}

cat("\nDownloading real-world market data...\n")

asset_prices <- getSymbols(
  Symbols = ticker,
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

adj_close <- Ad(asset_prices)
colnames(adj_close) <- "Adjusted"

# RETURNS → numeric (critical fix)
real_returns_xts <- compute_log_returns(adj_close)
real_returns <- as.numeric(real_returns_xts)

print_return_summary(real_returns, paste("Real-world", ticker, "log returns"))

par(mfrow = c(2, 1))
plot(adj_close, main = paste(ticker, "Adjusted Close Price"), major.ticks = "years")
plot(real_returns, type = "l", main = paste(ticker, "Daily Log Returns"))
par(mfrow = c(1, 1))

cat("\nFitting GARCH(", p_order, ",", q_order, ") model...\n", sep="")

real_spec <- build_garch_spec(p = p_order, q = q_order, dist = innovation_dist)

real_fit <- ugarchfit(
  spec = real_spec,
  data = real_returns,
  solver = "hybrid"
)

cat("\nModel Fit:\n")
show(real_fit)

cat("\nInformation Criteria:\n")
print(infocriteria(real_fit))

cat("\nGenerating diagnostic plots...\n")

titles <- c(
  "Series with Conditional SD",
  "VaR Limits",
  "Conditional SD vs Returns",
  "ACF of Returns",
  "ACF of Squared Returns",
  "ACF of Absolute Returns",
  "Cross-Correlation",
  "Density of Residuals",
  "QQ Plot",
  "ACF Standardized Residuals",
  "ACF Squared Standardized Residuals",
  "News Impact Curve"
)

par(mfrow = c(1, 1))

for (i in 1:12) {
  plot(real_fit, which = i)
}

cat("\nForecasting volatility...\n")

real_fcst <- ugarchforecast(real_fit, n.ahead = 20)

print(sigma(real_fcst))

pdf("GARCH_forecast.pdf", width = 8, height = 6)
plot(real_fcst, which = 3)
dev.off()

cat("\nComparing GARCH models...\n")

candidate_orders <- list(
  c(1, 1),
  c(1, 2),
  c(2, 1),
  c(2, 2)
)

model_results <- data.frame(
  Model = character(),
  AIC   = numeric(),
  BIC   = numeric()
)

for (ord in candidate_orders) {
  
  temp_spec <- build_garch_spec(p = ord[1], q = ord[2], dist = innovation_dist)
  
  temp_fit <- tryCatch(
    ugarchfit(spec = temp_spec, data = real_returns),
    error = function(e) NULL
  )
  
  if (!is.null(temp_fit)) {
    ic <- infocriteria(temp_fit)
    
    model_results <- rbind(
      model_results,
      data.frame(
        Model = paste0("GARCH(", ord[1], ",", ord[2], ")"),
        AIC = ic[1],
        BIC = ic[2]
      )
    )
  }
}

print(model_results[order(model_results$AIC), ])


cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
cat("PROJECT SUMMARY\n")
cat(paste(rep("=", 60), collapse = ""), "\n", sep = "")
cat("1. Pulled real-world financial data (Yahoo Finance).\n")
cat("2. Constructed log returns.\n")
cat("3. Estimated GARCH(p, q) volatility model.\n")
cat("4. Performed full residual diagnostics.\n")
cat("5. Generated volatility forecasts.\n")
cat("6. Compared multiple GARCH specifications.\n")
cat(paste(rep("=", 60), collapse = ""), "\n", sep = "")