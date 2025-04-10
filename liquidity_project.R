rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(tibble)
library(ggplot2)
library(gridExtra)
df <- read_csv("liquidity.csv")


# 50 random stocks
set.seed(123)  # For reproducibility
selected_stocks <- df %>% distinct(PERMNO) %>% sample_n(50)
df <- df %>% filter(PERMNO %in% selected_stocks$PERMNO)

# Compute liquidity measures
liquidity_measures <- df %>%
  arrange(PERMNO, date) %>%
  group_by(PERMNO) %>%
  mutate(
    # Amihud Illiquidity Measure
    Amihud = (abs(RET) / (VOL * PRC))*1e6,
    
    # Abda and Ranaldo Spread
    eta = (log(ASKHI) + log(BIDLO)) / 2,
    eta_next = lead(eta),
    #lag_eta = lag(eta),
    #lag_logPRC = lag(log(PRC)),
    Abda_Ranaldo = sqrt(pmax(0, 4 * (log(PRC) - eta) * (log(PRC)  - eta_next))),
    
    # Corwin-Schultz Spread (with lag)
    Beta = log(ASKHI / BIDLO)^2 + log(lag(ASKHI) / lag(BIDLO))^2,
    Gamma = log(ASKHI / lag(BIDLO))^2,
    Alpha = (sqrt(2 * Beta) - sqrt(Beta)) / (3 - 2 * sqrt(2)),
    Corwin_Schultz = 2 * (exp(Alpha) - 1),
    
    # Chung-Zhang Spread
    Chung_Zhang = 2 * (ASK - BID) / (ASK + BID)
  ) %>%
  ungroup()

# Apply Z-score normalization to all liquidity measures
standardized_measures <- liquidity_measures %>%
  mutate(
    log_Amihud = log1p(Amihud),  # Log transform to handle small values
    z_Amihud = (log_Amihud - mean(log_Amihud, na.rm = TRUE)) / sd(log_Amihud, na.rm = TRUE),
    across(c(Abda_Ranaldo, Corwin_Schultz, Chung_Zhang),
           ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE), .names = "z_{.col}")
  )

daily_correlations <- standardized_measures %>%
  group_by(date) %>%
  filter(!is.na(z_Amihud) & !is.na(z_Abda_Ranaldo) & !is.na(z_Corwin_Schultz) & !is.na(z_Chung_Zhang)) %>%
  summarise(
    cor_Amihud_Abda_Ranaldo = ifelse(n() > 1, cor(z_Amihud, z_Abda_Ranaldo, use = "complete.obs"), NA),
    cor_Amihud_Corwin_Schultz = ifelse(n() > 1, cor(z_Amihud, z_Corwin_Schultz, use = "complete.obs"), NA),
    cor_Amihud_Chung_Zhang = ifelse(n() > 1, cor(z_Amihud, z_Chung_Zhang, use = "complete.obs"), NA),
    cor_Abda_Ranaldo_Corwin_Schultz = ifelse(n() > 1, cor(z_Abda_Ranaldo, z_Corwin_Schultz, use = "complete.obs"), NA),
    cor_Abda_Ranaldo_Chung_Zhang = ifelse(n() > 1, cor(z_Abda_Ranaldo, z_Chung_Zhang, use = "complete.obs"), NA),
    cor_Corwin_Schultz_Chung_Zhang = ifelse(n() > 1, cor(z_Corwin_Schultz, z_Chung_Zhang, use = "complete.obs"), NA)
  ) %>%
  ungroup()

avg_correlations_overtime <- daily_correlations %>%
  summarise(
    avg_Amihud_Abda_Ranaldo = mean(cor_Amihud_Abda_Ranaldo, na.rm = TRUE),
    avg_Amihud_Corwin_Schultz = mean(cor_Amihud_Corwin_Schultz, na.rm = TRUE),
    avg_Amihud_Chung_Zhang = mean(cor_Amihud_Chung_Zhang, na.rm = TRUE),
    avg_Abda_Ranaldo_Corwin_Schultz = mean(cor_Abda_Ranaldo_Corwin_Schultz, na.rm = TRUE),
    avg_Abda_Ranaldo_Chung_Zhang = mean(cor_Abda_Ranaldo_Chung_Zhang, na.rm = TRUE),
    avg_Corwin_Schultz_Chung_Zhang = mean(cor_Corwin_Schultz_Chung_Zhang, na.rm = TRUE)
  )

# Compute correlation across stocks
cor_across_stocks <- standardized_measures %>%
  group_by(PERMNO) %>%
  summarize(
    cor_Amihud_Abda_Ranaldo = cor(z_Amihud, z_Abda_Ranaldo, use = "complete.obs"),
    cor_Amihud_Corwin_Schultz = cor(z_Amihud, z_Corwin_Schultz, use = "complete.obs"),
    cor_Amihud_Chung_Zhang = cor(z_Amihud, z_Chung_Zhang, use = "complete.obs"),
    cor_Abda_Ranaldo_Corwin_Schultz = cor(z_Abda_Ranaldo, z_Corwin_Schultz, use = "complete.obs"),
    cor_Abda_Ranaldo_Chung_Zhang = cor(z_Abda_Ranaldo, z_Chung_Zhang, use = "complete.obs"),
    cor_Corwin_Schultz_Chung_Zhang = cor(z_Corwin_Schultz, z_Chung_Zhang, use = "complete.obs")
  )

cor_matrix_df <- cor_across_stocks %>%
  select(-PERMNO) %>%
  as.data.frame()


avg_correlation_matrix <- as.data.frame(matrix(NA, nrow = 4, ncol = 3))
colnames(avg_correlation_matrix) <- c("Amihud", "Corwin_Schultz", "Chung_Zhang")
rownames(avg_correlation_matrix) <- c("Amihud", "Corwin_Schultz", "Chung_Zhang", 
                                      "Abda_Ranaldo")

# Compute the average for each pair across all stocks
avg_correlation_matrix[4, 1] <- mean(cor_matrix_df$cor_Amihud_Abda_Ranaldo, na.rm = TRUE)
avg_correlation_matrix[2, 1] <- mean(cor_matrix_df$cor_Amihud_Corwin_Schultz, na.rm = TRUE)
avg_correlation_matrix[3, 1] <- mean(cor_matrix_df$cor_Amihud_Chung_Zhang, na.rm = TRUE)
avg_correlation_matrix[4, 2] <- mean(cor_matrix_df$cor_Abda_Ranaldo_Corwin_Schultz, na.rm = TRUE)
avg_correlation_matrix[4, 3] <- mean(cor_matrix_df$cor_Abda_Ranaldo_Chung_Zhang, na.rm = TRUE)
avg_correlation_matrix[3, 2] <- mean(cor_matrix_df$cor_Corwin_Schultz_Chung_Zhang, na.rm = TRUE)



# Aggregate liquidity measures by date (Market-wide average)
market_liquidity <- standardized_measures %>%
  group_by(date) %>%
  summarise(
    avg_Amihud = mean(z_Amihud, na.rm = TRUE),
    avg_Abda_Ranaldo = mean(z_Abda_Ranaldo, na.rm = TRUE),
    avg_Corwin_Schultz = mean(z_Corwin_Schultz, na.rm = TRUE),
    avg_Chung_Zhang = mean(z_Chung_Zhang, na.rm = TRUE)
  )

# Create individual plots
p1 <- ggplot(market_liquidity, aes(x = date, y = avg_Amihud)) +
  geom_line(color = "blue") +
  ggtitle("Amihud Illiquidity Measure") +
  theme_minimal()

p2 <- ggplot(market_liquidity, aes(x = date, y = avg_Abda_Ranaldo)) +
  geom_line(color = "red") +
  ggtitle("Abda & Ranaldo Spread") +
  theme_minimal()

p3 <- ggplot(market_liquidity, aes(x = date, y = avg_Corwin_Schultz)) +
  geom_line(color = "green") +
  ggtitle("Corwin-Schultz Spread") +
  theme_minimal()

p4 <- ggplot(market_liquidity, aes(x = date, y = avg_Chung_Zhang)) +
  geom_line(color = "purple") +
  ggtitle("Chung-Zhang Spread") +
  theme_minimal()

# Arrange the plots in the same window
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

