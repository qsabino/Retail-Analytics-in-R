# ============================================================
# Retail Analytics in R
# Module 01: Data Ingestion & Validation
# ============================================================

library(tidyverse)
library(lubridate)
if (!require(janitor)) install.packages("janitor")
library(janitor)

# ------------------------------------------------------------
# 1. Load raw data
# ------------------------------------------------------------
products   <- read_csv("raw_data/dim_products.csv", show_col_types = FALSE)   %>%  clean_names()
warehouses <- read_csv("raw_data/dim_warehouses.csv", show_col_types = FALSE) %>%  clean_names()
retailers  <- read_csv("raw_data/dim_retailers.csv", show_col_types = FALSE)  %>%  clean_names()
customers  <- read_csv("raw_data/dim_customers.csv", show_col_types = FALSE)  %>%  clean_names()
orders     <- read_csv("raw_data/fact_orders.csv", show_col_types = FALSE)    %>%  clean_names()
promotions <- read_csv("raw_data/dim_promotions.csv", show_col_types = FALSE) %>%  clean_names()
shippings  <- read_csv("raw_data/fact_shippings.csv", show_col_types = FALSE) %>%  clean_names()
returns    <- read_csv("raw_data/fact_returns.csv", show_col_types = FALSE)   %>%  clean_names()
calendar   <- read_csv("raw_data/dim_date.csv", show_col_types = FALSE)       %>%  clean_names()

# ------------------------------------------------------------
# 2. Basic validation checks
# ------------------------------------------------------------

# Row counts
data.frame(
  table = c("products","warehouses","retailers","customers",
            "orders","promotions","shippings","returns","calendar"),
  rows = c(
    nrow(products),
    nrow(warehouses),
    nrow(retailers),
    nrow(customers),
    nrow(orders),
    nrow(promotions),
    nrow(shippings),
    nrow(returns),
    nrow(calendar)
  )
)

# Primary key uniqueness checks
stopifnot(nrow(products)   == n_distinct(products$product_sku))
stopifnot(nrow(customers)  == n_distinct(customers$customer_id))
stopifnot(nrow(retailers)  == n_distinct(retailers$retailer_id))
stopifnot(nrow(warehouses) == n_distinct(warehouses$warehouse_id))
stopifnot(nrow(orders)     == n_distinct(orders$order_id))

# ------------------------------------------------------------
# 3. Date sanity checks
# ------------------------------------------------------------
range(orders$order_date)
range(calendar$date)

# Shipping dates must be >= order date
shippings %>%
  filter(ship_date < order_date)

# ------------------------------------------------------------
# 4. Financial sanity checks
# ------------------------------------------------------------

orders %>%
  summarise(
    avg_price = mean(product_price),
    avg_cost  = mean(product_cost),
    pct_loss_leaders = mean(product_cost > product_price)
  )

# ------------------------------------------------------------
# 5. Save cleaned objects
# ------------------------------------------------------------
dir.create("processed_data", showWarnings = FALSE)

saveRDS(
  list(
    products   = products,
    warehouses = warehouses,
    retailers  = retailers,
    customers  = customers,
    orders     = orders,
    promotions = promotions,
    shippings  = shippings,
    returns    = returns,
    calendar   = calendar
  ),
  "processed_data/retail_data.rds"
)