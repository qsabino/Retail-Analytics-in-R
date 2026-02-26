# ============================================================
# Retail Analytics in R
# Module 03: Exploratory Sales & Profit Analysis
# ============================================================

library(tidyverse)
library(lubridate)
library(scales)

# ------------------------------------------------------------
# 1. Load enriched fact table
# ------------------------------------------------------------
fact_sales <- readRDS("processed_data/fact_sales_enriched.rds")

# ------------------------------------------------------------
# 2. Overall business KPIs
# ------------------------------------------------------------
kpi_summary <- fact_sales %>%
  summarise(
    total_revenue = sum(net_revenue, na.rm = TRUE),
    total_profit  = sum(net_profit, na.rm = TRUE),
    avg_margin    = mean(gross_margin, na.rm = TRUE),
    total_orders  = n_distinct(order_id),
    total_customers = n_distinct(customer_id)
  )

kpi_summary

# ------------------------------------------------------------
# 3. Revenue & profit trends over time
# ------------------------------------------------------------
monthly_performance <- fact_sales %>%
  group_by(year_month) %>%
  summarise(
    revenue = sum(net_revenue),
    profit  = sum(net_profit),
    .groups = "drop"
  )

monthly_performance_long <- monthly_performance %>%
  pivot_longer(
    cols = c(revenue, profit),
    names_to = "metric",
    values_to = "value"
  )

ggplot(monthly_performance_long, aes(year_month, value, color = metric)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title = "Monthly Revenue vs Profit",
    x = "Month",
    y = "Amount",
    color = "Metric"
  )

# ------------------------------------------------------------
# 4. Category performance
# ------------------------------------------------------------
category_performance <- fact_sales %>%
  group_by(product_category) %>%
  summarise(
    revenue = sum(net_revenue),
    profit  = sum(net_profit),
    margin  = mean(gross_margin, na.rm = TRUE),
    .groups = "drop"
  )

category_performance_long <- category_performance %>%
  pivot_longer(
    cols = c(revenue, profit),
    names_to = "metric",
    values_to = "value"
  )

ggplot(category_performance_long, aes(reorder(product_category, value), value, fill = metric)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Revenue vs Profit by Product Category",
    x = "Category",
    y = "Amount",
    fill = "Metric"
  )

# plot for category margin
ggplot(category_performance,
       aes(reorder(product_category, margin), margin)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Average Profit Margin by Product Category",
    x = "Product Category",
    y = "Margin"
  )

# ------------------------------------------------------------
# 5. Regional performance
# ------------------------------------------------------------
region_performance <- fact_sales %>%
  group_by(region.y) %>%   # retailer region
  summarise(
    revenue = sum(net_revenue, na.rm = TRUE),
    profit  = sum(net_profit, na.rm = TRUE),
    .groups = "drop"
  )

region_performance_long <- region_performance %>%
  pivot_longer(
    cols = c(revenue, profit),
    names_to = "metric",
    values_to = "value"
  )

ggplot(region_performance_long, aes(reorder(region.y, value), value, fill = metric)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Revenue vs Profit by Retailer Region",
    x = "Region",
    y = "Amount",
    fill = "Metric"
  )
# ------------------------------------------------------------
# 6. Channel analysis
# ------------------------------------------------------------
channel_performance <- fact_sales %>%
  group_by(retailer_channel) %>%
  summarise(
    revenue = sum(net_revenue, na.rm = TRUE),
    profit  = sum(net_profit, na.rm = TRUE),
    avg_shipping_cost = mean(shipping_cost, na.rm = TRUE),
    .groups = "drop"
  )

channel_performance_long <- channel_performance %>%
  pivot_longer(
    cols = c(revenue, profit),
    names_to = "metric",
    values_to = "value"
  )

ggplot(channel_performance_long, aes(reorder(retailer_channel, value), value, fill = metric)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Revenue vs Profit by Retailer Channel",
    x = "Channel",
    y = "Amount",
    fill = "Metric"
  )

# Shipping Cost Visualization
ggplot(channel_performance, aes(reorder(retailer_channel, avg_shipping_cost), avg_shipping_cost)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Average Shipping Cost by Channel",
    x = "Channel",
    y = "Average Shipping Cost"
  )
# ------------------------------------------------------------
# 7. Loss leader analysis
# ------------------------------------------------------------
fact_sales %>%
  filter(loss_leader_flag) %>%
  summarise(
    loss_order_pct = n() / nrow(fact_sales),
    total_loss = sum(net_profit)
  )

# Average order value (AOV) for loss leader orders vs normal orders
fact_sales %>%
  group_by(loss_leader_flag) %>%
  summarise(
    avg_order_value = mean(product_price * order_quantity, na.rm = TRUE),
    total_revenue = sum(net_revenue)
  )

# Does profit from other products in the same order offset the loss?
fact_sales %>%
  group_by(order_id) %>%
  summarise(
    order_profit = sum(net_profit),
    loss_leader_used = any(loss_leader_flag)
  ) %>%
  group_by(loss_leader_used) %>%
  summarise(mean_order_profit = mean(order_profit))
print("Orders with loss leaders do not generate positive profit -> strategy isn't working.")

# Worse 20% loss_leaders
fact_sales %>%
  filter(loss_leader_flag) %>%
  group_by(product_sku) %>%
  summarise(total_loss = sum(net_profit)) %>%
  mutate(loss_tier = ntile(total_loss, 5)) %>% 
  filter(loss_tier == 1)

# ------------------------------------------------------------
# 8. Key takeaways (printed for report)
# ------------------------------------------------------------
cat(
  "\nKey Insights:\n",
  "- Revenue and profit trends show seasonality and volatility\n",
  "- Certain categories drive revenue\n",
  "- Loss-leading transactions are present and measurable\n",
  "- Channel-level shipping costs impact profitability\n"
)