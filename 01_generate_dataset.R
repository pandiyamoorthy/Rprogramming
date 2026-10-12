# ==============================================================================
# 01_generate_dataset.R
# Food Delivery Time Analysis and Delay Prediction Using R
# Author : Pandiyamoorthy S  |  Course: 23CAP317 - R Programming
#
# Purpose: builds the raw dataset "food_delivery_raw.csv" (5,000 orders).
# The data is SIMULATED with a fixed seed so that the whole project is
# reproducible without any download. Delivery time is generated from realistic
# business rules (distance, vehicle speed, traffic, weather, time of day,
# kitchen preparation time, courier experience) plus random noise.
# Data-quality problems (missing values, duplicate rows, impossible values)
# are injected on purpose so that the cleaning stage has real work to do.
# ==============================================================================

set.seed(2026)
n <- 5000

# ---- 1. Order / restaurant / courier attributes ------------------------------
Order_ID            <- sprintf("FD%05d", 1:n)
Distance_km         <- round(pmax(0.5, rgamma(n, shape = 4, scale = 1.6)), 1)
Prep_Time_min       <- round(pmax(3, rnorm(n, mean = 15, sd = 5)))
Courier_Experience  <- round(pmin(12, rexp(n, rate = 1 / 3)), 1)           # years
Items_Count         <- 1 + rpois(n, lambda = 2)
Order_Value         <- round(Items_Count * runif(n, 120, 300))             # INR
Restaurant_Rating   <- round(pmin(5, pmax(2.5, rnorm(n, 4.1, 0.5))), 1)

Traffic     <- sample(c("Low", "Medium", "High"), n, TRUE, prob = c(0.30, 0.45, 0.25))
Weather     <- sample(c("Clear", "Rainy", "Foggy", "Stormy"), n, TRUE,
                      prob = c(0.55, 0.25, 0.12, 0.08))
Time_of_Day <- sample(c("Morning", "Afternoon", "Evening", "Night"), n, TRUE,
                      prob = c(0.20, 0.30, 0.35, 0.15))
Vehicle     <- sample(c("Bike", "Scooter", "Car"), n, TRUE, prob = c(0.45, 0.40, 0.15))

# ---- 2. Business rules that generate delivery time ---------------------------
min_per_km     <- c(Bike = 3.0, Scooter = 2.4, Car = 2.8)[Vehicle]
traffic_factor <- c(Low = 1.00, Medium = 1.25, High = 1.60)[Traffic]
weather_factor <- c(Clear = 1.00, Rainy = 1.15, Foggy = 1.10, Stormy = 1.35)[Weather]
tod_factor     <- c(Morning = 1.00, Afternoon = 1.05, Evening = 1.12, Night = 0.95)[Time_of_Day]

travel_time <- Distance_km * min_per_km * traffic_factor * weather_factor * tod_factor

Delivery_Time_min <- 0.8 * Prep_Time_min +            # pickup waits on the kitchen
  travel_time +                                       # riding time
  5 +                                                 # handover / parking time
  0.4 * Items_Count -                                 # bigger orders take longer
  0.6 * pmin(Courier_Experience, 8) -                 # experienced couriers are faster
  1.0 * (Restaurant_Rating - 4) +                     # well-run kitchens dispatch faster
  rnorm(n, 0, 4)                                      # unexplained randomness
Delivery_Time_min <- round(pmax(8, Delivery_Time_min), 1)

# ---- 3. Promised time and delay flag -----------------------------------------
Promised_Time_min <- round(30 + 2.5 * Distance_km)    # shown to the customer
Delayed           <- as.integer(Delivery_Time_min > Promised_Time_min)

food <- data.frame(Order_ID, Distance_km, Prep_Time_min, Courier_Experience,
                   Items_Count, Order_Value, Restaurant_Rating, Traffic, Weather,
                   Time_of_Day, Vehicle, Promised_Time_min, Delivery_Time_min,
                   Delayed, stringsAsFactors = FALSE)

# ---- 4. Inject realistic data-quality problems -------------------------------
# (a) ~2% missing values in three columns
for (col in c("Courier_Experience", "Restaurant_Rating", "Weather")) {
  food[sample(n, round(0.02 * n)), col] <- NA
}
# (b) 25 duplicated order rows (e.g. double submission)
food <- rbind(food, food[sample(n, 25), ])
# (c) 10 impossible delivery times (sensor / entry errors)
bad <- sample(nrow(food), 10)
food$Delivery_Time_min[bad] <- sample(c(-5, 0, 240, 300, 480), 10, TRUE)

food <- food[sample(nrow(food)), ]          # shuffle row order
rownames(food) <- NULL
write.csv(food, "food_delivery_raw.csv", row.names = FALSE, na = "")
cat("Raw dataset written:", nrow(food), "rows x", ncol(food), "columns\n")
