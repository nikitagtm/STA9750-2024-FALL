# Load required libraries
library(nycflights13)
library(dplyr)

# Step 1: Calculate the average departure delay for each origin airport
avg_delay <- flights |>
  group_by(origin) |>
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) |>
  arrange(desc(avg_dep_delay))

# Step 2: Join with the airports dataset to get the airport names
result <- avg_delay |>
  inner_join(airports, by = c("origin" = "faa")) |>
  select(name, avg_dep_delay) |>
  arrange(desc(avg_dep_delay))

# Step 3: Display the airport with the longest average departure delay
print(result)

