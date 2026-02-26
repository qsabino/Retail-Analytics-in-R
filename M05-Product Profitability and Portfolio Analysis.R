# ============================================================
# Retail Analytics in R
# Module 05: Product Profitability & Portfolio Analysis
# ============================================================

library(tidyverse)
library(scales)

# ------------------------------------------------------------
# 1. Load enriched fact table
# ------------------------------------------------------------
fact_sales <- readRDS("processed_data/fact_sales_enriched.rds")

# ------------------------------------------------------------
# 2. Aggregate by product
# ------------------------------------------------------------
product_perf <- fact_sales %>%
  group_by(product_sku, product_name, product_category) %>%
  summarise(
    total_revenue = sum(net_revenue, na.rm = TRUE),
    total_profit  = sum(net_profit, na.rm = TRUE),
    avg_margin    = mean(gross_margin, na.rm = TRUE),
    total_quantity = sum(net_quantity),
    loss_orders = sum(loss_leader_flag),
    .groups = "drop"
  ) %>%
  arrange(desc(total_profit))

product_perf

# ------------------------------------------------------------
# 3. Top and bottom 10 products by profit
# ------------------------------------------------------------
product_perf %>% top_n(10, total_profit)

product_perf %>% top_n(-10, total_profit)

# ------------------------------------------------------------
# 4. Revenue vs Profit scatter (identify risky products)
# ------------------------------------------------------------
ggplot(product_perf, aes(total_revenue, total_profit, color = product_category)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype="dashed", color="red") +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Revenue vs Profit by Product",
    x = "Revenue",
    y = "Profit",
    color = "Category"
  )

# ------------------------------------------------------------
# 5. Pareto analysis: 80/20 profit
# ------------------------------------------------------------
product_perf <- product_perf %>%
  arrange(desc(total_profit)) %>%
  mutate(
    cumulative_profit = cumsum(total_profit),
    cumulative_pct = cumulative_profit / sum(total_profit)
  )

pareto_products <- product_perf %>%
  filter(cumulative_pct <= 0.8)

# % of products driving 80% profit
nrow(pareto_products) / nrow(product_perf) 

# ------------------------------------------------------------
# 6. Category-level summary
# ------------------------------------------------------------
category_perf <- fact_sales %>%
  group_by(product_category) %>%
  summarise(
    revenue = sum(net_revenue),
    profit  = sum(net_profit),
    margin  = mean(gross_margin, na.rm = TRUE)
  )

# For every $1 of revenue, how much profit do we make in each category?
category_perf %>% 
  mutate(weighted_margin = profit/revenue)

# % of products in each category are actually profitable
product_perf %>%
  group_by(product_category) %>%
  summarise(
    pct_profitable_products = mean(total_profit > 0)
  )

category_perf <- category_perf %>%
  mutate(
    profit_share = profit / sum(profit),
    revenue_share = revenue / sum(revenue)
  )

category_perf
print("If a category’s profit share > revenue share -> It punches above its weight.")

# ------------------------------------------------------------
# 7. Key insights
# ------------------------------------------------------------
cat(
  "\nProduct Insights:\n",
  "- Profit is highly concentrated: a small subset of products drives the majority of total profit (Pareto 80/20)\n",
  "- High revenue does not guarantee profitability; several products show weak or negative margins\n",
  "- Unprofitable products exist and may act as loss leaders or indicate pricing and cost issues\n",
  "- Category performance varies: some categories contribute more profit despite overall margin pressure\n"
)