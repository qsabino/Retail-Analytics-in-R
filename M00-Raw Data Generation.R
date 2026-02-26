library(dplyr)
library(lubridate)
library(stringi)

set.seed(123)

# ============================================================
# 1. products
# ============================================================
n_products <- 50
products <- tibble(
  product_sku = 1:n_products,
  product_full_description = paste("product", 1:n_products, "full desc"),
  product_gender = sample(c("male", "female", "unisex"), n_products, replace = TRUE),
  product_category = sample(c("furniture", "office supplies", "sports", "electronics", "clothing"), n_products, replace = TRUE),
  product_name = paste("product", 1:n_products),
  product_size = sample(c("s","m","l","xl"), n_products, replace = TRUE),
  product_color = sample(c("black","white","blue","green","red"), n_products, replace = TRUE)
)

# ============================================================
# 2. warehouses
# ============================================================
n_warehouses <- 5
warehouses <- tibble(
  warehouse_id = 1:n_warehouses,
  warehouse_location = paste("warehouse", 1:n_warehouses),
  region = sample(c("africa", "north america", "south america", "europe", "asia"), n_warehouses, replace = TRUE),
  capacity = sample(1000:5000, n_warehouses, replace = TRUE)
)

# ============================================================
# 3. retailers
# ============================================================
n_retailers <- 30
retailers <- tibble(
  retailer_id = 1:n_retailers,
  retailer_channel = sample(c("online", "in-store", "distributor"), n_retailers, replace = TRUE),
  retailer_name = paste("retailer", 1:n_retailers),
  city = paste("city", sample(1:20, n_retailers, replace = TRUE)),
  region = sample(c("africa", "north america", "south america", "europe", "asia"), n_retailers, replace = TRUE),
  area = sample(c("urban","suburban","rural"), n_retailers, replace = TRUE),
  country = sample(c("usa","uk","india","brazil","germany"), n_retailers, replace = TRUE),
  distance_from_warehouse = round(runif(n_retailers, 10, 500), 1)
)

# ============================================================
# 4. customers
# ============================================================
n_customers <- 200
customers <- tibble(
  customer_id = 1:n_customers,
  customer_name = paste("customer", 1:n_customers),
  age_group = sample(c("18-25","26-35","36-45","46-60","60+"), n_customers, replace = TRUE),
  gender = sample(c("male","female","unisex"), n_customers, replace = TRUE),
  region = sample(c("africa", "north america", "south america", "europe", "asia"), n_customers, replace = TRUE),
  channel = sample(c("online","in-store"), n_customers, replace = TRUE),
  loyalty_score = round(runif(n_customers, 0, 100),1)
)

# ============================================================
# 5. orders
# ============================================================
n_orders <- 500
orders <- tibble(
  order_id = 1001:(1000 + n_orders),
  order_date = sample(seq(as.Date('2021-01-01'), as.Date('2024-12-31'), by="day"), n_orders, replace = TRUE),
  retailer_id = sample(retailers$retailer_id, n_orders, replace = TRUE),
  product_sku = sample(products$product_sku, n_orders, replace = TRUE),
  product_price = round(runif(n_orders, 20, 1000),2),
  product_cost = round(runif(n_orders, 10, 800),2),
  order_quantity = sample(1:10, n_orders, replace = TRUE),
  customer_id = sample(customers$customer_id, n_orders, replace = TRUE),
  # generate random discount 0-30% for 30% of orders
  random_discount = ifelse(runif(n_orders) < 0.3, runif(n_orders, 0, 0.3), 0)
)

# ============================================================
# 6. promotions
# ============================================================
n_promotions <- 30
promotions <- tibble(
  promotion_id = 1:n_promotions,
  product_sku = sample(products$product_sku, n_promotions, replace = TRUE),
  start_date = sample(seq(as.Date('2021-01-01'), as.Date('2024-12-01'), by="day"), n_promotions, replace = TRUE),
  promo_length = sample(7:14, n_promotions, replace = TRUE),
  end_date = start_date + promo_length,
  discount_percentage = round(runif(n_promotions, 5, 50), 1),
  campaign_name = paste("promo", 1:n_promotions)
) %>%
  select(-promo_length)  # remove helper column

# ============================================================
# 7. shippings
# ============================================================
shippings <- orders %>%
  select(order_id, order_date) %>%
  mutate(
    ship_date = order_date + sample(1:5, nrow(.), replace = TRUE),
    delivery_duration = sample(2:10, nrow(.), replace = TRUE),
    delivery_date = ship_date + delivery_duration,
    shipping_method = sample(c("standard", "express", "same-day"), nrow(.), replace = TRUE),
    carrier = sample(c("fedex", "ups", "dhl", "usps"), nrow(.), replace = TRUE),
    shipping_cost = round(runif(nrow(.), 5, 50), 2),
    delivery_status = sample(c("on-time", "delayed", "returned"), nrow(.), replace = TRUE, prob = c(0.7, 0.25, 0.05)),
    warehouse_id = sample(warehouses$warehouse_id, nrow(.), replace = TRUE)
  )

# ============================================================
# 8. returns
# ============================================================
n_returns <- 50
returns <- tibble(
  order_id = sample(orders$order_id, n_returns, replace = TRUE),
  returndate = sample(seq(as.POSIXct('2021-01-01'), as.POSIXct('2024-12-31 23:59:59'), by="min"), n_returns, replace = TRUE)
  ) %>%
  left_join(orders %>% select(order_id, product_sku, order_quantity), by = "order_id") %>%
  rowwise() %>% # Each row is sampled independently, needed for per-order operations
  mutate(return_quantity = sample(1:order_quantity, 1)) %>%
  select(order_id, product_sku, return_quantity, returndate)

# ============================================================
# 9. calendar
# ============================================================
all_dates <- seq(as.Date("2021-01-01"), as.Date("2024-12-31"), by="day")
calendar <- tibble(
  date = all_dates,
  year = year(all_dates),
  month = month(all_dates),
  quarter = quarter(all_dates),
  weekday = weekdays(all_dates),
  is_holiday = sample(c(TRUE, FALSE), length(all_dates), replace = TRUE, prob = c(0.1, 0.9))
)

# Write CSVs files
dir.create("raw_data", showWarnings = FALSE)

write.csv(products, "raw_data/dim_products.csv", row.names = FALSE)
write.csv(warehouses, "raw_data/dim_warehouses.csv", row.names = FALSE)
write.csv(retailers, "raw_data/dim_retailers.csv", row.names = FALSE)
write.csv(customers, "raw_data/dim_customers.csv", row.names = FALSE)
write.csv(orders, "raw_data/fact_orders.csv", row.names = FALSE)
write.csv(promotions, "raw_data/dim_promotions.csv", row.names = FALSE)
write.csv(shippings, "raw_data/fact_shippings.csv", row.names = FALSE)
write.csv(returns, "raw_data/fact_returns.csv", row.names = FALSE)
write.csv(calendar, "raw_data/dim_date.csv", row.names = FALSE)