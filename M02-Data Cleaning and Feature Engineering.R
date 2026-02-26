# ============================================================
# Retail Analytics in R
# Module 02: Data Cleaning & Feature Engineering
# ============================================================

library(tidyverse)
library(lubridate)

# ------------------------------------------------------------
# 1. Load processed data
# ------------------------------------------------------------
retail_data <- readRDS("processed_data/retail_data.rds")

products   <- retail_data$products
customers  <- retail_data$customers
retailers  <- retail_data$retailers
warehouses <- retail_data$warehouses
orders     <- retail_data$orders
shippings  <- retail_data$shippings
returns    <- retail_data$returns
calendar   <- retail_data$calendar

# ------------------------------------------------------------
# 2. Aggregate returns at order-product level
# ------------------------------------------------------------
returns_summary <- returns %>%
  group_by(order_id, product_sku) %>%
  summarise(
    return_quantity = sum(return_quantity, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 3. Build core fact table
# ------------------------------------------------------------
fact_sales <- orders %>%
  left_join(returns_summary, by = c("order_id", "product_sku")) %>%
  mutate(
    return_quantity = replace_na(return_quantity, 0),
    net_quantity = order_quantity - return_quantity
  ) %>%
  left_join(shippings %>% select(order_id, shipping_cost, delivery_status, shipping_method),by = "order_id") %>%
  left_join(products, by = "product_sku") %>%
  left_join(customers, by = "customer_id") %>%
  left_join(retailers, by = "retailer_id") %>%
  left_join(calendar, by = c("order_date" = "date"))

# ------------------------------------------------------------
# 4. Financial feature engineering
# ------------------------------------------------------------
fact_sales <- fact_sales %>%
  mutate(
    gross_revenue = product_price * order_quantity,
    net_revenue   = gross_revenue * (1 - random_discount),
    cost_of_goods = product_cost * order_quantity,
    gross_profit  = net_revenue - cost_of_goods,
    net_profit    = gross_profit - shipping_cost,
    gross_margin  = gross_profit / net_revenue,
    markup        = (product_price - product_cost) / product_cost
  )

# ------------------------------------------------------------
# 5. Data quality flags, important
# ------------------------------------------------------------
fact_sales <- fact_sales %>%
  mutate(
    loss_leader_flag = net_profit < 0,
    full_return_flag = net_quantity <= 0,
    delayed_delivery = delivery_status == "delayed"
  )

# ------------------------------------------------------------
# 6. Time-based features
# ------------------------------------------------------------
fact_sales <- fact_sales %>%
  mutate(
    year_month = floor_date(order_date, "month"),
    week       = isoweek(order_date),
    weekday    = wday(order_date, label = TRUE),
    order_age_days = as.numeric(Sys.Date() - order_date)
  )

# ------------------------------------------------------------
# 7. Final sanity checks
# ------------------------------------------------------------
summary(select(
  fact_sales,
  gross_revenue,
  net_revenue,
  net_profit,
  gross_margin,
  markup
))

mean(fact_sales$loss_leader_flag)
mean(fact_sales$full_return_flag)

# Check impossible values
fact_sales %>%
  filter(gross_margin > 1 | gross_margin < -1) %>% 
  select(c(order_id, gross_margin))

fact_sales %>%
  filter(markup > 5 | markup < -1) %>% 
  select(c(order_id, markup))

# ------------------------------------------------------------
# 8. Save analysis-ready dataset
# ------------------------------------------------------------
saveRDS(
  fact_sales,
  "processed_data/fact_sales_enriched.rds"
)