#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(arrow)
#
#
#
#| cache: true
crypto <- open_dataset("data/daily_prices.parquet") |>
  collect() |>
  left_join(
    open_dataset("data/coin_metadata.parquet") |>
      collect() |>
      mutate(
        launch_date = as.Date(parse_date_time(
          launch_date,
          orders = c("Ymd", "mdy")
        ))
      ),
    by = "coin_id"
  ) |>
  left_join(
    open_dataset("data/categories.parquet") |>
      collect() |>
      select(category_id, category_name, category_description = description),
    by = "category_id"
  ) |>
  mutate(date = as.Date(date_raw))
#
#
#
crypto |>
  group_by(date) |>
  summarise(total_market_cap_usd = sum(market_cap_usd, na.rm = TRUE), .groups = "drop") |>
  mutate(total_market_cap_trillions = total_market_cap_usd / 1e12) |>
  ggplot(aes(x = date, y = total_market_cap_trillions)) +
  geom_line() +
  labs(
    title = "Total crypto market cap over time",
    x = "Date",
    y = "Market cap (USD trillions)"
  ) +
  theme_minimal()
#
#
#
crypto |>
  group_by(date, category_name) |>
  summarise(category_market_cap_usd = sum(market_cap_usd, na.rm = TRUE), .groups = "drop") |>
  group_by(date) |>
  mutate(
    total_market_cap_usd = sum(category_market_cap_usd, na.rm = TRUE),
    share = category_market_cap_usd / total_market_cap_usd
  ) |>
  ungroup() |>
  mutate(
    category_name = fct_reorder(category_name, -category_market_cap_usd, .fun = median)
  ) |>
  ggplot(aes(x = date, y = share, fill = category_name)) +
  geom_area(position = "stack", alpha = 0.9, linewidth = 0.2) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Crypto market-cap share by category",
    subtitle = "Each stacked band shows the share of total crypto market cap contributed by one category on a given day.",
    x = "Date",
    y = "Share of total market cap (%)",
    fill = "Category",
    caption = "Source: aggregated daily market-cap data for the coin categories in this dataset."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey40")
  )
#
#
#
#
#
