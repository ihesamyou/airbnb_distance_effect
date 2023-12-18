library(tidyverse)
library(readr)
library(lubridate)
library(janitor)
library(scales)
library(here)
library(geosphere)
library(leaflet)
library(dplyr)
library(mapview)
webshot::install_phantomjs()


# ------------------------ Data Cleaning --------------------------------


# get the directory of the script
script_directory <- here()

# Set the working directory to the script directory
setwd(script_directory)

# load the data
airbnb <- readr::read_csv("./airbnb.csv")

# remove duplicates
airbnb <- airbnb %>%
  distinct()

# check
# sum(duplicated(airbnb))

# select the columns that are needed for data analysis
selected_columns <- c("id", "host_id", "neighbourhood", "latitude", "longitude", "room_type", "price")
airbnb_cleaned <- airbnb[, (names(airbnb) %in% selected_columns)]

# remove rows with missing values
airbnb_cleaned <- airbnb_cleaned %>% 
  drop_na()

# check
# sum(is.na(airbnb_cleaned))


calculate_distance <- function(dataframe, center_longitude, center_latitude) {
  # this function calculates the distance between each record in a dataframe
  # to a central location. it will add a column named "distance_to_center"
  # to the dataframe. library geosphere is used to calculate the distances.
  if (!all(c("latitude", "longitude") %in% names(dataframe))) {
    stop("The dataframe does not have coordinates.")
  }

  distances <- distVincentySphere(
    c(center_longitude, center_latitude),
    cbind(dataframe$longitude, dataframe$latitude)
  )

  # convert meters to kilometers and round to 2 decimal points
  distances <- round(distances / 1000, 2)
  
  dataframe$distance_to_center <- distances
  return(dataframe)
}


# we consider the Duomo as the center of the city of Milan
duomo_longitude <- 9.190560
duomo_latitude <- 45.464186

# calculate_distance for each row in airbnb_cleaned
airbnb_cleaned <- calculate_distance(airbnb_cleaned, duomo_longitude, duomo_latitude)


assign_zones <- function(df, distance_col = "distance_to_center") {
  # this function assigns zones to each record in a dataframe based on the distance.
  # it will add a column named zone to the dataframe.
  df$zone <- case_when(
    df[[distance_col]] <= 1.5 ~ 1,
    df[[distance_col]] > 1.5 & df[[distance_col]] <= 3 ~ 2,
    df[[distance_col]] > 3 & df[[distance_col]] <= 4.5 ~ 3,
    df[[distance_col]] > 4.5 & df[[distance_col]] <= 6 ~ 4,
    df[[distance_col]] > 6 & df[[distance_col]] <= 7.5 ~ 5,
    df[[distance_col]] > 7.5 ~ 6
  )
  return(df)
}


# assign_zones to each row in airbnb_cleaned
airbnb_cleaned <- airbnb_cleaned %>%
  assign_zones(distance_col = "distance_to_center")

# preview data
head(airbnb_cleaned[, c("id", "neighbourhood", "price", "latitude", "longitude", "distance_to_center", "zone")], 10)

# total number of rows
print(paste("total rows: ", nrow(airbnb_cleaned)))

# save airbnb_cleaned in the current directory
write_csv(airbnb_cleaned, "airbnb_cleaned.csv")


# ------------------------ Data Analysis --------------------------------


# calculate average values for price and distance_to_center
average_values <- airbnb_cleaned %>%
  summarise(avg_price = mean(price),
            avg_distance_to_center = mean(distance_to_center))

cat("Average Daily Price: €", round(average_values$avg_price, 2), "\n")
cat("Average Distance to Center: ", round(average_values$avg_distance_to_center, 2), " km\n")



# function to show the entries of airbnb_cleaned on the map
add_entries_to_map <- function(map, longitude, latitude) {
  map <- map %>%
    addCircleMarkers(
      lng = longitude,
      lat = latitude,
      radius = 0.02,
      color = "purple",
      fillOpacity = 1,
      weight = 0.02
    )
  return(map)
}

# create airbnb_map
airbnb_map <- leaflet() %>%
  setView(lng = duomo_longitude, lat = duomo_latitude, zoom = 12) %>%
  addTiles()

# put the entries in airbnb_cleaned on the map based on coordinates
airbnb_map <- add_entries_to_map(airbnb_map, airbnb_cleaned$longitude, airbnb_cleaned$latitude)
mapview::mapshot(airbnb_map, file = "airbnb_map.png")



# zone data
zone_categories <- rev(c("Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5", "Zone 6"))
radius <- rev(c(1500, 3000, 4500, 6000, 7500, 10000))
zone_colors <- rev(c("#FF0000", "#FFA500", "#FFFF00", "#00FF00", "#00FFFF", "#0000FF"))
zone_opacities <- rev(c(0.15, 0.30, 0.40, 0.40, 0.25, 0.12))

# this function draws zones on the Milan map for visualizing zones
draw_zone <- function(map, center, radius, colors) {
  for (i in (1:length(radius))) {
    map <- addCircles(
      map = map,
      lng = center$longitude,
      lat = center$latitude,
      radius = radius[i],
      color = colors[i],
      fillOpacity = zone_opacities[i],
      fill = TRUE,
      weight = 1,
      popup = paste("Zone:", length(radius)-i+1)
    )
  }
  return(map)
}

# create zone_map
zone_map <- leaflet() %>%
  setView(lng = duomo_longitude, lat = duomo_latitude, zoom = 12) %>%
  addTiles()

# call the function draw_zone to add zones to the zone_map
center <- data.frame(longitude = duomo_longitude, latitude = duomo_latitude)
zone_map <- draw_zone(zone_map, center, radius, zone_colors)
mapview::mapshot(zone_map, file = "zone_map.png")



# create airbnb_zones to show the zones and the entries
# in airbnb_cleaned on the same map together
airbnb_zones <- leaflet() %>%
  setView(lng = duomo_longitude, lat = duomo_latitude, zoom = 12) %>%
  addTiles()

# put the entries of airbnb_cleaned on the map based on coordinates
airbnb_zones <- add_entries_to_map(airbnb_zones, airbnb_cleaned$longitude, airbnb_cleaned$latitude)

# add zones to the airbnb_zones
for (i in 1:length(radius)) {
  airbnb_zones <- addCircles(
    map = airbnb_zones,
    lng = duomo_longitude,
    lat = duomo_latitude,
    radius = radius[i],
    color = zone_colors[i],
    fillOpacity = zone_opacities[i],
    fill = TRUE,
    weight = 1,
    popup = paste("Zone:", length(radius)-i+1)
  )
}
mapview::mapshot(airbnb_zones, file = "airbnb_zones.png")



# count the number of records for each zone
zone_record_counts <- airbnb_cleaned %>%
  group_by(zone) %>%
  summarise(record_count = n())

# calculate percentage of records for each zone
zone_record_counts <- zone_record_counts %>%
  mutate(percentage = record_count / sum(record_count) * 100)

# pie chart for visualizing the number of records and percentage for each zone
ggplot(zone_record_counts, aes(x = "", y = percentage, fill = factor(zone), label = sprintf("Z %s\n%d\n%.1f%%", zone, record_count, percentage))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = rev(zone_colors)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(title = "Percentage of Records by Zones",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position="none",
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )
ggsave("zone_record_counts.png", width = 8, height = 6, dpi = 300)



# calculate average price for each zone
average_zone_prices <- airbnb_cleaned %>%
  group_by(zone) %>%
  summarise(avg_price = mean(price))

# bar chart for visualizing average price for each zone
ggplot(average_zone_prices, aes(x = factor(zone, levels = levels(factor(zone))), y = avg_price, fill = factor(zone))) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_hline(yintercept = average_values$avg_price, color = "purple", size = 1) +
  geom_text(aes(x = length(levels(factor(zone))) + 0.5, y = average_values$avg_price,
                label = paste("Average Price", round(average_values$avg_price, 2))),
            color = "red", hjust = 1, vjust = 1, size = 3) +
  scale_fill_manual(values = rev(zone_colors)) + 
  labs(title = "Average Prices by Zone",
       x = "Zone",
       y = "Average Price") +
  theme_minimal() + 
  theme(
    legend.position="none",
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )
ggsave("average_zone_prices.png", width = 8, height = 6, dpi = 300)



# calculate average price for each neighborhood
average_neighborhood_prices <- airbnb_cleaned %>%
  group_by(neighbourhood) %>%
  summarise(avg_price = mean(price))

# sort neighborhoods by average price in descending order
average_neighborhood_prices <- average_neighborhood_prices %>%
  arrange(desc(avg_price))

# bar chart for visualizing average price for each neighborhood
neighborhood_chart <- ggplot(average_neighborhood_prices, aes(x = fct_reorder(neighbourhood, avg_price), y = avg_price, fill = avg_price)) +
  geom_col(alpha = 0.7) +
  scale_fill_viridis_c() +
  labs(title = "Average Prices by Neighborhood",
       x = "Neighborhood",
       y = "Average Price") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"
  )

neighborhood_chart <- neighborhood_chart +
  geom_hline(yintercept = average_values$avg_price, color = "purple", size = 1) +
  geom_text(
    aes(x = Inf, y = average_values$avg_price,
        label = paste("Average Price", round(average_values$avg_price, 2))),
    color = "red", hjust = 1, vjust = 1, size = 3
  )
ggsave("average_neighborhood_prices.png", plot = neighborhood_chart, width = 14, height = 8, dpi = 300)



# calculate average prices for each room_type
average_room_type_prices <- airbnb_cleaned %>%
  group_by(room_type) %>%
  summarise(avg_price = mean(price))

room_type_colors <- c("#FF0000", "#FFA500", "#FFFF00", "#00FF00")

# bar chart for visualizing average price for each room_type
ggplot(average_room_type_prices, aes(x = factor(room_type, levels = levels(factor(room_type))), y = avg_price, fill = factor(room_type))) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_manual(values = room_type_colors) + 
  labs(title = "Average Prices by Room Type",
       x = "Room Type",
       y = "Average Price") +
  theme_minimal() + 
  theme(
    legend.position="none",
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )
ggsave("average_room_type_prices.png", width = 8, height = 6, dpi = 300)



# box plot for the price variable in airbnb_cleaned
ggplot(airbnb_cleaned, aes(y = price)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7, width = 0.2) +
  labs(title = "Box Plot of Airbnb Prices",
       y = "Price") +
  theme_minimal() + 
  theme(
    legend.position="none",
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )
ggsave("boxplot_airbnb_cleaned.png", width = 8, height = 6, dpi = 300)



# scatter plot of airbnb_cleaned for distance_to_center and price
ggplot(airbnb_cleaned, aes(x = distance_to_center, y = price)) +
  geom_point(color = "orange", size = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  labs(title = "Relationship between Distance to Center and Price",
       x = "Distance to Center (km)",
       y = "Price") +
  theme_minimal() + 
  theme(
    legend.position="none",
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )
ggsave("scatter_plot_airbnb_cleaned.png", width = 8, height = 6, dpi = 300)



# calculate correlation coefficient
correlation_coefficient <- cor(airbnb_cleaned$distance_to_center, airbnb_cleaned$price)
cat("Correlation Coefficient:", round(correlation_coefficient, 5), "\n")


# calculate the interquartile range (IQR) for price
price_iqr <- IQR(airbnb_cleaned$price)

# define the lower and upper quartile
lower_quartile <- quantile(airbnb_cleaned$price, 0.25) - 1.5 * price_iqr
upper_quartile <- quantile(airbnb_cleaned$price, 0.75) + 1.5 * price_iqr

# filter out outliers
airbnb_no_outliers <- airbnb_cleaned %>%
  filter(price >= lower_quartile, price <= upper_quartile)



cat("Number of rows before removing outliers:", nrow(airbnb_cleaned), "\n")
cat("Number of rows after removing outliers:", nrow(airbnb_no_outliers), "\n")



# box plot for the price variable in airbnb_no_outliers
ggplot(airbnb_no_outliers, aes(y = price)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7, width = 0.2) +
  labs(title = "Box Plot of Airbnb Prices (No Outliers)",
       y = "Price") +
  theme_minimal() + 
  theme(
    legend.position="none",
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )
ggsave("boxplot_airbnb_no_outliers.png", width = 8, height = 6, dpi = 300)



# scatter plot of airbnb_no_outliers for distance_to_center and price
ggplot(airbnb_no_outliers, aes(x = distance_to_center, y = price)) +
  geom_point(color = "orange", size = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  labs(title = "Relationship between Distance to Center and Price (No Outliers)",
       x = "Distance to Center (km)",
       y = "Price") +
  theme_minimal() + 
  theme(
    legend.position="none",
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )
ggsave("scatter_plot_airbnb_no_outliers.png", width = 8, height = 6, dpi = 300)



# calculate correlation coefficient without outliers
correlation_coefficient_no_outliers <- cor(airbnb_no_outliers$distance_to_center, airbnb_no_outliers$price)
cat("Correlation Coefficient (No Outliers):", round(correlation_coefficient_no_outliers, 5), "\n")
