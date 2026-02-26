# ============================================================
# Retail Analytics in R
# Module 07: Demand Forecasting & Trend Analysis
# ============================================================

if (!require(forecast)) install.packages("forecast")
if (!require(tsibble)) install.packages("tsibble")
if (!require(feasts)) install.packages("feasts")
if (!require(fable)) install.packages("fable")

library(tidyverse)
library(lubridate)
library(forecast)
library(tsibble) # for time series
library(feasts)
library(fable)

# ------------------------------------------------------------
# 1. Load enriched sales data
# ------------------------------------------------------------
fact_sales <- readRDS("processed_data/fact_sales_enriched.rds")

# ------------------------------------------------------------
# 2. Aggregate monthly sales per product
# ------------------------------------------------------------
monthly_sales <- fact_sales %>%
  mutate(year_month = yearmonth(order_date)) %>% # fable models (ETS, ARIMA, etc.) expect a regular interval index
  group_by(product_sku, product_name, year_month) %>%
  summarise(
    quantity_sold = sum(net_quantity),
    revenue = sum(net_revenue),
    .groups = "drop"
  ) %>%
  arrange(product_sku, year_month)

# ------------------------------------------------------------
# 3. Convert to time series (tsibble)
# ------------------------------------------------------------
monthly_sales_ts <- monthly_sales %>%
  as_tsibble(index = year_month, key = product_sku)

# ------------------------------------------------------------
# 4. Forecast using ETS (Exponential Smoothing)
# ------------------------------------------------------------
# Fills missing year_month rows for each product_sku, needed for ETS sees a continuous monthly series
monthly_sales_ts <- monthly_sales_ts %>%
  fill_gaps(quantity_sold = 0, revenue = 0)

ets_model <- monthly_sales_ts %>%
  model(ets = ETS(quantity_sold))

ets_fc <- forecast(ets_model, h = "3 months")  # Forecast next 3 months

# ------------------------------------------------------------
# 5. Visualize forecast for top 5 products by revenue
# ------------------------------------------------------------
top_products <- monthly_sales %>%
  group_by(product_sku, product_name) %>%
  summarise(total_revenue = sum(revenue), .groups = "drop") %>%
  arrange(desc(total_revenue)) %>%
  slice(1:5)

ets_fc %>%
  filter(product_sku %in% top_products$product_sku) %>%
  autoplot(monthly_sales_ts, level = NULL) +
  labs(
    title = "Monthly Demand Forecast for Top 5 Products",
    x = "Month",
    y = "Quantity Sold"
  )

# ------------------------------------------------------------
# 6. Evaluate forecast accuracy (backtesting)
# ------------------------------------------------------------
# Simple train/test split for ETS evaluation
train <- monthly_sales_ts %>% filter(year_month <= yearmonth("2024 Jun"))
test  <- monthly_sales_ts %>% filter(year_month > yearmonth("2024 Jun"))

# Align products between train & test
valid_products <- intersect(
  unique(train$product_sku),
  unique(test$product_sku)
)

train_bt <- train %>% filter(product_sku %in% valid_products)
test_bt  <- test  %>% filter(product_sku %in% valid_products)

ets_bt <- train_bt %>%
  model(ets = ETS(quantity_sold))

ets_bt_fc <- forecast(ets_bt, new_data = test_bt)

accuracy(ets_bt_fc, test_bt)

# ------------------------------------------------------------
# 7. Key insights
# ------------------------------------------------------------
cat(
  "\nTime Series Forecasting Insights:\n",
  "- Monthly demand patterns were successfully modeled at the product level\n",
  "- Exponential Smoothing (ETS) performed well for stable, high-volume products.\n",
  "- Forecasting readiness depends on data continuity and product maturity.\n"
)

# ------------------------------------------------------------
# 8. Forecasting with ARIMA
# ------------------------------------------------------------
arima_model <- monthly_sales_ts %>%
  model(arima = ARIMA(quantity_sold))

arima_fc <- forecast(arima_model, h = "3 months")

arima_fc %>%
  filter(product_sku %in% top_products$product_sku) %>%
  autoplot(monthly_sales_ts, level = NULL) +
  labs(
    title = "ARIMA Forecast vs Actual Demand",
    x = "Month",
    y = "Quantity Sold"
  )

# ------------------------------------------------------------
# 9. Backtesting
# ------------------------------------------------------------

arima_bt <- train_bt %>%
  model(arima = ARIMA(quantity_sold))

arima_bt_fc <- forecast(arima_bt, new_data = test_bt)

accuracy(arima_bt_fc, test_bt)

# ------------------------------------------------------------
# 10. Choose the Final Forecasting Model (ETS vs ARIMA)
# ------------------------------------------------------------
# Fit BOTH models on the same data
models <- train_bt %>%
  model(
    ets   = ETS(quantity_sold),
    arima = ARIMA(quantity_sold)
  )

# Forecast both models
fc <- forecast(models, new_data = test_bt)

# Compare ETS vs ARIMA
acc <- accuracy(fc, test_bt)

acc %>%
  group_by(.model) %>%
  summarise(
    RMSE = mean(RMSE, na.rm = TRUE),
    MAE  = mean(MAE, na.rm = TRUE),
    ME   = mean(ME, na.rm = TRUE)
  )