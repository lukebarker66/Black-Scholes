### SHINY APP ###
rm(list = ls())

#load packages
library(tidyverse)
library(stargazer)
library(dplyr)
library(ggplot2)
library(shiny)

### Black-Scholes Option Pricer ###

# --- Function: Black-Scholes with Greeks ---
bs_with_greeks <- function(S, K, T, r, sigma, type = "call") {
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  price <- if (type == "call") {
    S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  } else {
    K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  }
  
  delta <- if (type == "call") pnorm(d1) else -pnorm(-d1)
  gamma <- dnorm(d1) / (S * sigma * sqrt(T))
  vega <- S * dnorm(d1) * sqrt(T)
  theta <- if (type == "call") {
    - (S * dnorm(d1) * sigma) / (2 * sqrt(T)) - r * K * exp(-r * T) * pnorm(d2)
  } else {
    - (S * dnorm(d1) * sigma) / (2 * sqrt(T)) + r * K * exp(-r * T) * pnorm(-d2)
  }
  
  return(list(price = price, delta = delta, gamma = gamma, vega = vega, theta = theta))
}

# --- Function: GBM Simulation ---
simulate_gbm <- function(S0, mu, sigma, T, steps) {
  dt <- T / steps
  time <- seq(0, T, by = dt)
  n <- length(time)
  Wt <- cumsum(c(0, sqrt(dt) * rnorm(n - 1)))  # Brownian motion
  S <- S0 * exp((mu - 0.5 * sigma^2) * time + sigma * Wt)
  return(data.frame(time = time, S = S))
}

# --- Function: PnL & Greeks over time ---
calc_pnl <- function(asset_path, K, r, sigma, T, type = "call", market_price_spread = 0.02) {
  n <- nrow(asset_path)
  results <- data.frame(
    time = asset_path$time,
    S = asset_path$S,
    price = numeric(n),
    pnl = numeric(n),
    delta = numeric(n),
    gamma = numeric(n),
    vega = numeric(n),
    theta = numeric(n)
  )
  
  init <- bs_with_greeks(S = asset_path$S[1], K, T, r, sigma, type)
  market_price <- init$price * (1 + market_price_spread)
  
  for (i in 1:n) {
    t_remaining <- T - asset_path$time[i]
    if (t_remaining <= 0) {
      results$price[i] <- max(ifelse(type == "call", asset_path$S[i] - K, K - asset_path$S[i]), 0)
    } else {
      bs <- bs_with_greeks(asset_path$S[i], K, t_remaining, r, sigma, type)
      results$price[i] <- bs$price
      results$delta[i] <- bs$delta
      results$gamma[i] <- bs$gamma
      results$vega[i] <- bs$vega
      results$theta[i] <- bs$theta
    }
    results$pnl[i] <- results$price[i] - market_price
  }
  
  return(results)
}


# --- Launch App ---
shinyApp(ui = ui, server = server)


### Step-by-Step Plan

#Simulate an asset path using Geometric Brownian Motion (GBM)
#Calculate option prices + Greeks at each point using Black-Scholes
#Compute PnL from a given trade (e.g., buying the option at market price)
#Plot the results in Shiny
  
