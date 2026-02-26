# ============================================================
# Retail Analytics in R
# Module 06: Promotions & Pricing Impact Analysis
# ============================================================

library(tidyverse)
library(scales)
library(lubridate)

# ------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------
fact_sales <- readRDS("processed_data/fact_sales_enriched.rds")
promotions <- readRDS("processed_data/retail_data.rds")$promotions

# ------------------------------------------------------------
# 2. Join promotion info to orders
# ------------------------------------------------------------
fact_sales <- fact_sales %>%
  left_join(
    promotions,
    by = "product_sku",
    relationship = "many-to-many"
  ) %>%
  mutate(
    # Check if the order is during a promotion
    on_promo = !is.na(discount_percentage) & order_date >= start_date & order_date <= end_date,
   
    # Calculate the discount in dollars
    promo_discount_amt = if_else(on_promo, product_price * order_quantity * discount_percentage, 0),
    random_discount_amt = product_price * order_quantity * random_discount,
    
    # Pick the **higher discount** between promo and random
    product_discount = round(pmax(promo_discount_amt, random_discount_amt), 2)
  ) %>%
  select(-start_date, -end_date, -discount_percentage, -promo_discount_amt, -random_discount_amt, -random_discount)

# ------------------------------------------------------------
# 3. Promotion and pricing impact analysis
# ------------------------------------------------------------
# Are promotions driving incremental revenue?
promo_analysis <- fact_sales %>%
  group_by(on_promo) %>%
  summarise(
    orders = n_distinct(order_id),
    revenue = sum(net_revenue, na.rm = TRUE),
    profit = sum(net_profit, na.rm = TRUE),
    avg_margin = mean(gross_margin, na.rm = TRUE),
    AOV = mean(net_revenue, na.rm = TRUE),
    .groups = "drop"
  )

promo_analysis
print("If revenue up but profit down, then you're buying sales at the expense of margin.")

# At what discount level does profit collapse?
fact_sales %>%
  mutate(discount_bucket = ntile(product_discount, 5)) %>%
  group_by(discount_bucket) %>%
  summarise(
    avg_revenue = mean(net_revenue),
    avg_profit = mean(net_profit)
  )

# Did promotions increase quantity?
fact_sales %>%
  group_by(on_promo) %>%
  summarise(
    total_quantity = sum(net_quantity),
    avg_quantity = mean(net_quantity)
  )

# Which categories benefit more from promotions?
fact_sales %>%
  group_by(product_category, on_promo) %>%
  summarise(
    total_revenue = sum(net_revenue),
    total_profit = sum(net_profit)
  )
print("Promotions may work for seasonal items but not premium goods.")

# Time-Series Outside vs During Promotion
fact_sales %>%
  group_by(year_month, on_promo) %>%
  summarise(
    total_revenue = sum(net_revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = year_month,
             y = total_revenue,
             color = on_promo)) +
  geom_line(linewidth = 1.2) +
  geom_smooth(se = FALSE, linewidth = 1) +
  scale_y_continuous(labels = dollar) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title = "Revenue Trend: Promotion vs Non-Promotion Periods",
    x = "Month",
    y = "Total Revenue",
    color = "Promotion Status"
  ) +
  theme_minimal()

cat("- If promotion line is consistently higher-> promotion drives revenue lift.\n",
    "- If promotion spikes only temporarily → demand may be shifted forward.\n",
    "- If non-promo revenue drops during promo → possible cannibalization."
    )