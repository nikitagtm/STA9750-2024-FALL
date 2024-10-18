library(ggplot2)

# Scatter plot of price vs carat, faceted by cut
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.5) +  # Add transparency to handle overplotting
  facet_wrap(~ cut) +  # Facet by cut
  labs(title = "Price vs Carat, Faceted by Cut", x = "Carat", y = "Price") +
  theme_minimal()


# Scatter plot of price vs carat with geom_smooth, colored by color
ggplot(diamonds, aes(x = carat, y = price, color = color)) +
  geom_point(alpha = 0.5) +  # Add transparency to points
  geom_smooth(se = FALSE) +  # Add smooth lines without confidence intervals
  labs(title = "Price vs Carat by Diamond Color", x = "Carat", y = "Price") +
  theme_minimal()

# Frequency polygon of price, broken out by cut
ggplot(diamonds, aes(x = price, color = cut)) +
  geom_freqpoly(binwidth = 500) +  # Define bin width
  labs(title = "Frequency Polygon of Diamond Price by Cut", x = "Price", y = "Count") +
  theme_minimal()

# Scatter plot of color by clarity
ggplot(diamonds, aes(x = color, y = clarity)) +
  geom_jitter(alpha = 0.5) +  # Use jitter to avoid overplotting
  labs(title = "Scatter Plot of Diamond Color by Clarity", x = "Color", y = "Clarity") +
  theme_minimal()



if (!require("ggmosaic")) install.packages("ggmosaic")
# Load the ggmosaic library
library(ggmosaic)

# Mosaic plot of color by clarity
ggplot(data = diamonds) +
  geom_mosaic(aes(x = product(color), fill = clarity), na.rm = TRUE) +
  labs(title = "Mosaic Plot of Diamond Color by Clarity", x = "Color", y = "Proportion") +
  theme_minimal()

install.packages("CVXR")
library(CVXR)
library(tidyverse)
data(cdiac)
glimpse(cdiac)

#Plot the estimated annual global mean temperature (GMT) anomaly from 1850 to 2015.
cdiac <- cdiac %>%
  mutate(date = as.Date(paste0(year, "-01-01")))

# Plot the estimated annual global mean temperature (GMT) anomaly from 1850 to 2015
ggplot(cdiac, aes(x = date, y = annual)) +
  geom_line(color = "pink") +  # Line plot
  geom_point(color = "black", size = 0.5) +  # Adding points
  labs(title = "Annual Global Mean Temperature Anomaly (1850 - 2015)",
       x = "Year", y = "Temperature Anomaly (°C)") +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years") +  # Use scale_x_date to improve x-axis
  theme_minimal()

#Plot the GMT anomaly for each month on the same plot (as different lines).
# Reshape the data so each month is a row
cdiac_long <- cdiac %>%
  pivot_longer(cols = jan:dec, names_to = "month", values_to = "temperature_anomaly")

# Plot the monthly GMT anomaly series
ggplot(cdiac_long, aes(x = date, y = temperature_anomaly, color = month)) +
  geom_line() +
  labs(title = "Monthly Global Mean Temperature Anomalies (1850 - 2015)",
       x = "Year", y = "Temperature Anomaly (°C)") +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years") +  # Improve x-axis
  theme_minimal()

#
# 3- Plot all monthly data as one continuous line
ggplot(cdiac_long, aes(x = date, y = temperature_anomaly)) +
  geom_line() +
  geom_point(size = 0.5) +
  labs(title = "Monthly GMT Anomaly Series as One Continuous Line",
       x = "Year", y = "Temperature Anomaly (°C)") +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years") + 
  theme_minimal()

#4
# Filter for July data
july_data <- cdiac_long %>%
  filter(month == "jul")

# Plot July GMT anomaly and add a 5-year running median
ggplot(july_data, aes(x = date, y = temperature_anomaly)) +
  geom_line(color = "brown") +
  geom_point(size = 0.5, color = "blue") +
  geom_line(aes(y = runmed(temperature_anomaly, 5)), color = "pink") +  # 5-year running median
  labs(title = "July GMT Anomaly with 5-Year Running Median",
       x = "Year", y = "Temperature Anomaly (°C)") +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years") +
  theme_minimal()

# extra
# Find the warmest month for each year
warmest_months <- cdiac_long %>%
  group_by(year) %>%
  slice(which.max(temperature_anomaly)) %>%
  ungroup()

# Plot a histogram of the warmest months
ggplot(warmest_months, aes(x = factor(month, levels = month.abb))) +  # Ensuring chronological order
  geom_bar(fill = "skyblue") +
  labs(title = "Histogram of the Warmest Month by Year",
       x = "Month", y = "Frequency") +
  theme_minimal()

