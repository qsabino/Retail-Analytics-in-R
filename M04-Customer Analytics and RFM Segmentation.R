# ============================================================
# Retail Analytics in R
# Module 04: Customer Analytics & RFM Segmentation
# ============================================================

library(tidyverse)
library(lubridate)

# ------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------
fact_sales <- readRDS("processed_data/fact_sales_enriched.rds")

analysis_date <- max(fact_sales$order_date) + 1

# ------------------------------------------------------------
# 2. RFM metrics
# ------------------------------------------------------------
rfm_table <- fact_sales %>%
  group_by(customer_id) %>%
  summarise(
    recency = as.numeric(analysis_date - max(order_date)),
    frequency = n_distinct(order_id),
    monetary = sum(net_revenue),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 3. RFM scoring, quintiles
# ------------------------------------------------------------
rfm_scores <- rfm_table %>%
  mutate(
    r_score = ntile(desc(recency), 5),
    f_score = ntile(frequency, 5),
    m_score = ntile(monetary, 5),
    rfm_score = paste0(r_score, f_score, m_score)
  )

# ------------------------------------------------------------
# 4. Customer segmentation
# ------------------------------------------------------------
rfm_segments <- rfm_scores %>%
  mutate(
    segment = case_when(
      r_score >= 4 & f_score >= 4 & m_score >= 4 ~ "Champions",
      r_score >= 3 & f_score >= 3 ~ "Loyal Customers",
      r_score >= 4 & f_score <= 2 ~ "Potential Loyalists",
      r_score <= 2 & f_score <= 2 ~ "At Risk",
      TRUE ~ "Others"
    )
  )
head(rfm_segments[, c("customer_id", "segment")])

# ------------------------------------------------------------
# 5. Segment performance
# ------------------------------------------------------------
rfm_segments %>%
  group_by(segment) %>%
  summarise(
    customers = n(),
    avg_revenue = mean(monetary),
    total_revenue = sum(monetary),
    revenue_share = total_revenue / sum(rfm_segments$monetary),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue))

# ------------------------------------------------------------
# 6. Pareto (80/20) analysis
# “Roughly 80% of effects come from 20% of causes.”
# ------------------------------------------------------------
pareto <- rfm_segments %>%
  arrange(desc(monetary)) %>%
  mutate(
    cumulative_revenue = cumsum(monetary),
    cumulative_pct = cumulative_revenue / sum(monetary)
  )

pareto %>%
  filter(cumulative_pct <= 0.8) %>%
  summarise(top_customers = n())

library(ggplot2)
library(scales)

ggplot(pareto, aes(x = seq_along(customer_id), y = cumulative_pct)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  geom_vline(xintercept = which(pareto$cumulative_pct >= 0.8)[1], linetype = "dashed", color = "steelblue") +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    title = "Pareto Analysis: Customers Contribution to Revenue",
    x = "Customers (sorted by revenue)",
    y = "Cumulative Revenue %"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 7. Key insights
# ------------------------------------------------------------
cat(
  "\nCustomer Insights:\n",
  "- Revenue is highly concentrated among a small % of customers\n",
  "- Champions and Loyal Customers drive majority of sales\n",
  "- At Risk segment requires retention focus\n"
)