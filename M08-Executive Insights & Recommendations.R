# ============================================================
# Retail Analytics in R
# Module 08: Executive Insights & Recommendations
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. Load analysis datasets
# ------------------------------------------------------------
fact_sales <- readRDS("processed_data/fact_sales_enriched.rds")

fact_sales <- fact_sales %>%
  left_join(
    promotions,
    by = "product_sku",
    relationship = "many-to-many"
  ) %>%
  mutate(
    # Check if the order is during a promotion
    on_promo = !is.na(discount_percentage) & order_date >= start_date & order_date <= end_date
  )

# ------------------------------------------------------------
# 2. Key Performance Indicator - KPIs
# ------------------------------------------------------------
kpis <- fact_sales %>%
  summarise(
    total_revenue = sum(net_revenue),
    total_profit  = sum(net_profit),
    avg_margin    = mean(gross_margin, na.rm = TRUE),
    loss_leader_pct = mean(loss_leader_flag)
  )

kpis

# ------------------------------------------------------------
# 3. Profit concentration, Pareto 80/20 (Product-level)
# ------------------------------------------------------------
product_profit <- fact_sales %>%
  group_by(product_sku, product_name) %>%
  summarise(
    profit = sum(net_profit),
    .groups = "drop"
  ) %>%
  arrange(desc(profit)) %>%
  mutate(
    cumulative_profit = cumsum(profit),
    cumulative_pct = cumulative_profit / sum(profit)
  )

top_profit_products <- product_profit %>%
  filter(cumulative_pct <= 0.8)

# ------------------------------------------------------------
# 4. Customer concentration
# ------------------------------------------------------------
customer_profit <- fact_sales %>%
  group_by(customer_id) %>%
  summarise(
    profit = sum(net_profit),
    .groups = "drop"
  ) %>%
  arrange(desc(profit)) %>%
  mutate(
    cumulative_profit = cumsum(profit),
    cumulative_pct = cumulative_profit / sum(profit)
  )

top_customers <- customer_profit %>%
  filter(cumulative_pct <= 0.8)


# ------------------------------------------------------------
# 5. Promotion effectiveness snapshot
# ------------------------------------------------------------
promo_compare <- fact_sales %>%
  group_by(on_promo) %>%
  summarise(
    avg_revenue = mean(net_revenue),
    avg_profit  = mean(net_profit),
    avg_margin  = mean(gross_margin, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = on_promo,
    values_from = c(avg_revenue, avg_profit, avg_margin)
  )

# Error may occur due to data was created randomly
promo_lift <- promo_compare %>%
  mutate(
    revenue_lift = (avg_revenue_TRUE - avg_revenue_FALSE) / avg_revenue_FALSE,
    profit_lift = (avg_profit_TRUE - avg_profit_FALSE) / avg_profit_FALSE,
    margin_change = (avg_margin_TRUE - avg_margin_FALSE) / avg_margin_FALSE
  )

promo_lift %>%
  select(revenue_lift_pct, profit_lift_pct, margin_change_pp) %>%
  mutate(across(everything(), round, 2))

# ------------------------------------------------------------
# 6. Operational risk indicators
# ------------------------------------------------------------
ops_risk <- fact_sales %>%
  summarise(
    delayed_delivery_pct = mean(delayed_delivery),
    return_rate = sum(return_quantity) / sum(order_quantity)
  )

ops_risk

# Check Profit Impact of Delays
delay_impact <- fact_sales %>%
  group_by(delayed_delivery) %>%
  summarise(
    avg_profit = mean(net_profit),
    avg_margin = mean(gross_margin, na.rm = TRUE),
    .groups = "drop"
  )

# Check Quantify Financial Impact of Returns
returns_impact <- fact_sales %>%
  summarise(
    total_return_loss = sum(return_quantity * product_price),
    return_rate = sum(return_quantity) / sum(order_quantity)
  )

# ------------------------------------------------------------
# 7. Executive recommendations (printed)
# ------------------------------------------------------------
cat(
  "\nEXECUTIVE SUMMARY & RECOMMENDATIONS\n",
  "----------------------------------\n",
  "1. Revenue & Profit Concentration:\n",
  "   - A small subset of products and customers drive ~80% of profit.\n",
  "   - Focus retention and inventory planning on these segments.\n\n",
  
  "2. Product Strategy:\n",
  "   - Loss-leading products should be reviewed for repricing or discontinuation.\n",
  "   - High-revenue, low-margin products present margin optimization opportunities.\n\n",
  
  "3. Customer Strategy:\n",
  "   - Prioritize Champions & Loyal Customers with targeted promotions.\n",
  "   - Develop retention campaigns for At-Risk customers.\n\n",
  
  "4. Promotions & Pricing:\n",
  "   - Promotions may increase volume but must be controlled to protect margin.\n",
  "   - Moderate discounts outperform deep discounts in profitability.\n\n",
  
  "5. Operations:\n",
  "   - Delivery delays and returns impact profitability.\n",
  "   - Address logistics inefficiencies to improve customer experience and margins.\n"
)
