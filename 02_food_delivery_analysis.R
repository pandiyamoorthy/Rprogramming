# ==============================================================================
# 02_food_delivery_analysis.R
# Food Delivery Time Analysis and Delay Prediction Using R
# Author : Pandiyamoorthy S  |  Course: 23CAP317 - R Programming (MCA, II/III/A)
#
# Run order:  Rscript 01_generate_dataset.R   ->   Rscript 02_food_delivery_analysis.R
# Outputs  :  plots/*.png , results/*.csv , results/*.rds
#
# Stages
#   1  Data structures & dataset overview          (Q1)
#   2  Control structures, functions, lists, data frames   (Q2)
#   3  Data cleaning & visualisation
#   4  Statistical problems / hypothesis tests     (Q3)
#   5  Data splitting
#   6  Regression models: LM, multiple LM, decision tree, random forest, logistic (Q4)
#   7  Model comparison, prediction on new orders  (Q5)
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(corrplot)
  library(caret);   library(ranger); library(rpart); library(reshape2)
})
set.seed(2026)
dir.create("plots",   showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

# Project colour palette (used in every plot)
col_teal <- "#1F5C6B"; col_orange <- "#E07B1F"; col_grey <- "#8AA2A9"
theme_set(theme_minimal(base_size = 13) +
          theme(plot.title = element_text(face = "bold", colour = col_teal)))
save_plot <- function(p, name, w = 7, h = 4.5)
  ggsave(file.path("plots", name), p, width = w, height = h, dpi = 150, bg = "white")

section <- function(txt) cat("\n### ", txt, "\n", sep = "")

# ==============================================================================
# STAGE 1 : DATA STRUCTURES AND DATASET OVERVIEW  (Q1)
# ==============================================================================
section("STAGE1_DATA_STRUCTURES")
# The five core R data structures, each shown with a food-delivery example
vec  <- c(Bike = 3.0, Scooter = 2.4, Car = 2.8)                      # atomic vector
mat  <- matrix(c(1.00, 1.25, 1.60, 1.00, 1.15, 1.35), nrow = 3,
               dimnames = list(c("Low", "Medium", "High"), c("Clear", "Rainy")))  # matrix
arr  <- array(1:24, dim = c(3, 4, 2))                                # array
fac  <- factor(c("Low", "High", "Medium", "Low"),
               levels = c("Low", "Medium", "High"), ordered = TRUE)  # (ordered) factor
lst  <- list(restaurant = "Spice Hub", rating = 4.3, items = c("Biryani", "Raita"))  # list
cat("Vector  (minutes per km by vehicle):\n");  print(vec)
cat("\nMatrix  (traffic x weather multiplier):\n"); print(mat)
cat("\nArray dimensions:", dim(arr), "\n")
cat("\nOrdered factor:\n"); print(fac)
cat("\nList structure:\n"); str(lst)

section("STAGE1_LOAD_DATASET")
food_raw <- read.csv("food_delivery_raw.csv", na.strings = c("", "NA"),
                     stringsAsFactors = FALSE)       # data frame
print(head(food_raw))
cat("\nDimensions:", dim(food_raw), "\n")
cat("\nColumn names:\n"); print(names(food_raw))
cat("\n"); str(food_raw)
cat("\n"); print(summary(food_raw))

# ==============================================================================
# STAGE 2 : CONTROL STRUCTURES, FUNCTIONS, LISTS, DATA FRAMES  (Q2)
# ==============================================================================
section("STAGE2_CONTROL_STRUCTURES")

# ---- user-defined functions --------------------------------------------------
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
mae  <- function(actual, predicted) mean(abs(actual - predicted))
r2   <- function(actual, predicted) 1 - sum((actual - predicted)^2) /
                                         sum((actual - mean(actual))^2)
get_mode <- function(x) { x <- x[!is.na(x)]; names(sort(table(x), decreasing = TRUE))[1] }

# function with if / else if / else  -> delay category from minutes late
delay_category <- function(minutes_late) {
  if (is.na(minutes_late))      "Unknown"
  else if (minutes_late <= 0)   "On time"
  else if (minutes_late <= 10)  "Slightly late"
  else                          "Very late"
}

# ---- for loop + switch: summarise each vehicle type --------------------------
valid <- food_raw[food_raw$Delivery_Time_min > 0 & food_raw$Delivery_Time_min < 150, ]
for (v in c("Bike", "Scooter", "Car")) {
  note <- switch(v,
                 Bike    = "cheap, slower, affected most by traffic",
                 Scooter = "fastest in city traffic",
                 Car     = "good for bad weather, slow to park")
  cat(sprintf("%-8s mean delivery = %5.1f min | %s\n", v,
              mean(valid$Delivery_Time_min[valid$Vehicle == v]), note))
}

# ---- while loop: how many extra km before the average delivery passes 60 min? -
dist <- mean(valid$Distance_km); est <- mean(valid$Delivery_Time_min)
while (est < 60) { dist <- dist + 0.5; est <- est + 0.5 * 2.6 * 1.25 }
cat(sprintf("\nwhile-loop estimate: average order would need ~%.1f km to take 60 min\n", dist))

# ---- repeat loop with break: simulate courier shifts until 30 delays ---------
delays <- 0; orders_checked <- 0
repeat {
  orders_checked <- orders_checked + 1
  delays <- delays + valid$Delayed[orders_checked]
  if (delays >= 30) break
}
cat("repeat-loop: needed", orders_checked, "orders to see 30 delays\n")

# ---- apply family ------------------------------------------------------------
num_cols <- c("Distance_km", "Prep_Time_min", "Courier_Experience",
              "Items_Count", "Order_Value", "Restaurant_Rating", "Delivery_Time_min")
cat("\nsapply  - column means:\n");  print(round(sapply(valid[num_cols], mean, na.rm = TRUE), 2))
cat("\nlapply  - first two column ranges:\n"); print(lapply(valid[num_cols[1:2]], range))
cat("\ntapply  - mean delivery time by traffic:\n"); print(round(tapply(valid$Delivery_Time_min, valid$Traffic, mean), 2))
cat("\nsapply + user function - delay category counts for the first 200 orders:\n")
print(table(sapply(valid$Delivery_Time_min[1:200] - valid$Promised_Time_min[1:200], delay_category)))

# ---- list: a "model report card" that holds mixed objects --------------------
report_card <- list(
  project   = "Food Delivery Time Analysis",
  n_orders  = nrow(valid),
  vehicles  = unique(valid$Vehicle),
  averages  = sapply(valid[c("Distance_km", "Delivery_Time_min")], mean),
  func_used = c("rmse", "mae", "r2", "get_mode", "delay_category")
)
cat("\nList of mixed objects:\n"); str(report_card)

# ---- data-frame operations (dplyr) -------------------------------------------
cat("\nData-frame manipulation - top 5 slowest weather/traffic combinations:\n")
combo <- valid %>% filter(!is.na(Weather)) %>%
  group_by(Weather, Traffic) %>%
  summarise(orders = n(), mean_time = round(mean(Delivery_Time_min), 1),
            delay_rate = round(mean(Delayed), 3), .groups = "drop") %>%
  arrange(desc(mean_time)) %>% head(5)
print(as.data.frame(combo))

# ==============================================================================
# STAGE 3 : DATA CLEANING AND VISUALISATION
# ==============================================================================
section("STAGE3_CLEANING")
food <- food_raw
cat("Missing values per column:\n"); print(colSums(is.na(food)))
cat("\nDuplicate rows:", sum(duplicated(food)), "\n")
food <- food[!duplicated(food), ]
cat("Rows after removing duplicates:", nrow(food), "\n")

# validity rule: delivery between 5 and 150 minutes, distance <= 40 km
invalid <- food$Delivery_Time_min < 5 | food$Delivery_Time_min > 150 | food$Distance_km > 40
cat("Rows with impossible delivery time / distance:", sum(invalid), "\n")
food <- food[!invalid, ]

# impute: median for numeric, mode for categorical
food$Courier_Experience[is.na(food$Courier_Experience)] <- median(food$Courier_Experience, na.rm = TRUE)
food$Restaurant_Rating[is.na(food$Restaurant_Rating)]   <- median(food$Restaurant_Rating,   na.rm = TRUE)
food$Weather[is.na(food$Weather)]                       <- get_mode(food$Weather)
cat("Missing values left:", sum(is.na(food)), "\n")

# convert categoricals to factors, create features
food$Traffic     <- factor(food$Traffic,     levels = c("Low", "Medium", "High"))
food$Weather     <- factor(food$Weather,     levels = c("Clear", "Foggy", "Rainy", "Stormy"))
food$Time_of_Day <- factor(food$Time_of_Day, levels = c("Morning", "Afternoon", "Evening", "Night"))
food$Vehicle     <- factor(food$Vehicle,     levels = c("Bike", "Scooter", "Car"))
food$Is_Peak     <- factor(ifelse(food$Time_of_Day %in% c("Afternoon", "Evening"), "Peak", "Off-peak"))
food$Minutes_Late <- food$Delivery_Time_min - food$Promised_Time_min
food$Delay_Category <- factor(sapply(food$Minutes_Late, delay_category),
                              levels = c("On time", "Slightly late", "Very late"))
cat("\nClean dataset:", nrow(food), "rows x", ncol(food), "columns\n")
cat("\nDelay category counts:\n"); print(table(food$Delay_Category))
cat("\nDelayed share:", round(mean(food$Delayed), 3), "\n")
write.csv(food, "food_delivery_clean.csv", row.names = FALSE)

section("STAGE3_VISUALISATION")
# 1 distribution of target
p1 <- ggplot(food, aes(Delivery_Time_min)) +
  geom_histogram(bins = 40, fill = col_teal, colour = "white") +
  geom_vline(xintercept = mean(food$Delivery_Time_min), colour = col_orange, linewidth = 1) +
  labs(title = "Distribution of Delivery Time", x = "Delivery time (min)", y = "Orders")
save_plot(p1, "01_delivery_time_hist.png")

# 2 delay share
p2 <- food %>% count(Delay_Category) %>% mutate(pct = n / sum(n)) %>%
  ggplot(aes(Delay_Category, pct, fill = Delay_Category)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), vjust = -0.5) +
  scale_fill_manual(values = c(col_teal, col_grey, col_orange)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.8)) +
  labs(title = "How Late Are Orders?", x = NULL, y = "Share of orders")
save_plot(p2, "02_delay_categories.png")

# 3 traffic boxplot
p3 <- ggplot(food, aes(Traffic, Delivery_Time_min, fill = Traffic)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.3) +
  scale_fill_manual(values = c(col_grey, "#E9B872", col_orange)) +
  labs(title = "Delivery Time by Traffic Level", y = "Delivery time (min)")
save_plot(p3, "03_time_by_traffic.png")

# 4 weather boxplot
p4 <- ggplot(food, aes(Weather, Delivery_Time_min, fill = Weather)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.3) +
  scale_fill_manual(values = c(col_grey, "#B7C9CE", col_teal, "#0F3540")) +
  labs(title = "Delivery Time by Weather", y = "Delivery time (min)")
save_plot(p4, "04_time_by_weather.png")

# 5 distance scatter
p5 <- ggplot(food, aes(Distance_km, Delivery_Time_min)) +
  geom_point(alpha = 0.15, colour = col_teal) +
  geom_smooth(method = "lm", colour = col_orange, se = FALSE) +
  labs(title = "Distance vs Delivery Time", x = "Distance (km)", y = "Delivery time (min)")
save_plot(p5, "05_distance_scatter.png")

# 6 correlation matrix
num_vars <- food[, sapply(food, is.numeric) & !names(food) %in% c("Minutes_Late")]
cor_mat  <- cor(num_vars, use = "pairwise.complete.obs")
png("plots/06_correlation_matrix.png", width = 1100, height = 1000, res = 130, bg = "white")
corrplot(cor_mat, method = "color", type = "upper", tl.cex = 0.8, tl.col = "black",
         addCoef.col = "black", number.cex = 0.6, tl.srt = 45)
dev.off()
cat("Correlation of every numeric variable with Delivery_Time_min:\n")
print(round(sort(cor_mat[, "Delivery_Time_min"], decreasing = TRUE), 3))

# 7 outlier boxplots
long <- melt(food[, c("Distance_km", "Prep_Time_min", "Courier_Experience",
                      "Items_Count", "Delivery_Time_min")], id.vars = NULL)
p7 <- ggplot(long, aes(variable, value)) + geom_boxplot(fill = col_grey, outlier.colour = col_orange) +
  facet_wrap(~variable, scales = "free", nrow = 1) +
  labs(title = "Boxplots for Checking Outliers", x = NULL, y = NULL) +
  theme(axis.text.x = element_blank())
save_plot(p7, "07_outlier_boxplots.png", w = 9, h = 4)

# 8 time of day x vehicle
p8 <- food %>% group_by(Time_of_Day, Vehicle) %>% summarise(m = mean(Delivery_Time_min), .groups = "drop") %>%
  ggplot(aes(Time_of_Day, m, fill = Vehicle)) + geom_col(position = "dodge") +
  scale_fill_manual(values = c(col_teal, col_orange, col_grey)) +
  labs(title = "Mean Delivery Time by Time of Day and Vehicle", x = NULL, y = "Mean time (min)")
save_plot(p8, "08_time_of_day_vehicle.png")

# 9 delay rate by traffic & weather heat-map
p9 <- food %>% group_by(Traffic, Weather) %>% summarise(rate = mean(Delayed), .groups = "drop") %>%
  ggplot(aes(Weather, Traffic, fill = rate)) + geom_tile(colour = "white") +
  geom_text(aes(label = scales::percent(rate, accuracy = 1)), colour = "white", fontface = "bold") +
  scale_fill_gradient(low = col_grey, high = col_orange, labels = scales::percent) +
  labs(title = "Delay Rate by Traffic and Weather", fill = "Delay rate")
save_plot(p9, "09_delay_heatmap.png")

# ==============================================================================
# STAGE 4 : STATISTICAL PROBLEMS  (Q3)
# ==============================================================================
section("STAGE4_STATISTICS")
cat("--- 4.1 Descriptive statistics & skewness of Delivery_Time_min ---\n")
x <- food$Delivery_Time_min
skew <- mean((x - mean(x))^3) / sd(x)^3
cat(sprintf("n=%d mean=%.2f median=%.2f sd=%.2f skewness=%.3f\n", length(x), mean(x), median(x), sd(x), skew))
ci <- t.test(x)$conf.int
cat(sprintf("95%% CI for mean delivery time: [%.2f, %.2f]\n", ci[1], ci[2]))

cat("\n--- 4.2 Normality test (Shapiro-Wilk on random sample of 4000) ---\n")
print(shapiro.test(sample(x, 4000)))

cat("\n--- 4.3 Pearson correlation test: Distance vs Delivery time ---\n")
print(cor.test(food$Distance_km, food$Delivery_Time_min))

cat("\n--- 4.4 Two-sample t-test: Clear vs Rainy weather (Welch) ---\n")
print(t.test(Delivery_Time_min ~ Weather, data = droplevels(subset(food, Weather %in% c("Clear", "Rainy")))))

cat("\n--- 4.5 One-way ANOVA: Delivery time ~ Traffic ---\n")
aov_fit <- aov(Delivery_Time_min ~ Traffic, data = food)
print(summary(aov_fit))
cat("\nTukey HSD post-hoc:\n"); print(TukeyHSD(aov_fit))

cat("\n--- 4.6 Kruskal-Wallis (non-parametric): Delivery time ~ Vehicle ---\n")
print(kruskal.test(Delivery_Time_min ~ Vehicle, data = food))

cat("\n--- 4.7 Chi-square test of independence: Traffic vs Delayed ---\n")
tab <- table(food$Traffic, food$Delayed)
print(tab); print(chisq.test(tab))

cat("\n--- 4.8 Multicollinearity (VIF from auxiliary regressions) ---\n")
preds <- c("Distance_km", "Prep_Time_min", "Courier_Experience", "Items_Count", "Order_Value", "Restaurant_Rating")
vif <- sapply(preds, function(v) {
  f <- as.formula(paste(v, "~", paste(setdiff(preds, v), collapse = "+")))
  1 / (1 - summary(lm(f, data = food))$r.squared)
})
print(round(vif, 3))
cat("(VIF > 5 would signal a multicollinearity problem)\n")

# ==============================================================================
# STAGE 5 : DATA SPLITTING
# ==============================================================================
section("STAGE5_SPLIT")
features <- c("Distance_km", "Prep_Time_min", "Courier_Experience", "Items_Count",
              "Order_Value", "Restaurant_Rating", "Traffic", "Weather", "Time_of_Day", "Vehicle")
model_df <- food[, c(features, "Delivery_Time_min", "Delayed")]
idx   <- createDataPartition(model_df$Delivery_Time_min, p = 0.8, list = FALSE)
train <- model_df[idx, ];  test <- model_df[-idx, ]
cat("Train rows:", nrow(train), "| Test rows:", nrow(test), "\n")
cat("Delayed share  train:", round(mean(train$Delayed), 3), " test:", round(mean(test$Delayed), 3), "\n")

# regression data (no Delayed) and classification data (no Delivery_Time = no leakage)
tr_reg <- train[, c(features, "Delivery_Time_min")];  te_reg <- test[, c(features, "Delivery_Time_min")]
tr_cl  <- train[, c(features, "Delayed")];            te_cl  <- test[, c(features, "Delayed")]

# ==============================================================================
# STAGE 6 : REGRESSION AND CLASSIFICATION MODELS  (Q4)
# ==============================================================================
metrics <- data.frame()
add_metric <- function(model, type, actual, pred)
  data.frame(Model = model, Type = type, RMSE = rmse(actual, pred),
             MAE = mae(actual, pred), R2 = r2(actual, pred))

section("STAGE6_LM_SIMPLE")
m_simple <- lm(Delivery_Time_min ~ Distance_km, data = tr_reg)
print(summary(m_simple))
p_simple <- predict(m_simple, te_reg)
metrics  <- rbind(metrics, add_metric("Simple Linear (Distance)", "Regression", te_reg$Delivery_Time_min, p_simple))

section("STAGE6_LM_MULTIPLE")
m_multi <- lm(Delivery_Time_min ~ ., data = tr_reg)
print(summary(m_multi))
p_multi <- predict(m_multi, te_reg)
metrics <- rbind(metrics, add_metric("Multiple Linear", "Regression", te_reg$Delivery_Time_min, p_multi))
cat("\nAIC multiple LM:", round(AIC(m_multi), 1), " | AIC simple LM:", round(AIC(m_simple), 1), "\n")

section("STAGE6_LM_INTERACTION")
m_int <- lm(Delivery_Time_min ~ . + Distance_km:Traffic + Distance_km:Weather + Distance_km:Vehicle,
            data = tr_reg)
cat("Adjusted R2 (interaction model):", round(summary(m_int)$adj.r.squared, 4), "\n")
cat("ANOVA: does the interaction model beat the multiple model?\n"); print(anova(m_multi, m_int))
p_int   <- predict(m_int, te_reg)
metrics <- rbind(metrics, add_metric("Linear + Interactions", "Regression", te_reg$Delivery_Time_min, p_int))

section("STAGE6_5FOLD_CV")
ctrl <- trainControl(method = "cv", number = 5)
cv_lm <- train(Delivery_Time_min ~ ., data = tr_reg, method = "lm", trControl = ctrl)
cat("5-fold CV (multiple LM):  RMSE =", round(cv_lm$results$RMSE, 3), " R2 =", round(cv_lm$results$Rsquared, 4), "\n")

section("STAGE6_TREE")
m_tree <- rpart(Delivery_Time_min ~ ., data = tr_reg, control = rpart.control(cp = 0.002, maxdepth = 6))
p_tree <- predict(m_tree, te_reg)
metrics <- rbind(metrics, add_metric("Decision Tree", "Regression", te_reg$Delivery_Time_min, p_tree))
cat("Decision tree used", sum(m_tree$frame$var == "<leaf>"), "leaf nodes\n")

section("STAGE6_RF")
m_rf <- ranger(Delivery_Time_min ~ ., data = tr_reg, num.trees = 300,
               importance = "impurity", seed = 2026)
p_rf <- predict(m_rf, te_reg)$predictions
metrics <- rbind(metrics, add_metric("Random Forest", "Regression", te_reg$Delivery_Time_min, p_rf))
cat("Random forest OOB R2:", round(m_rf$r.squared, 4), "\n")
imp <- sort(m_rf$variable.importance, decreasing = TRUE)
cat("\nVariable importance (impurity):\n"); print(round(imp, 0))

section("STAGE6_LOGISTIC")
m_glm <- glm(Delayed ~ ., data = tr_cl, family = binomial)
print(summary(m_glm))
cat("\nOdds ratios:\n"); print(round(exp(coef(m_glm)), 3))
glm_prob <- predict(m_glm, te_cl, type = "response")
glm_cls  <- ifelse(glm_prob > 0.5, 1, 0)
conf <- confusionMatrix(factor(glm_cls, levels = c(0, 1)), factor(te_cl$Delayed, levels = c(0, 1)), positive = "1")
print(conf)

auc <- function(y, p) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
                        (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
glm_auc <- auc(te_cl$Delayed, glm_prob)
cat("\nLogistic regression AUC:", round(glm_auc, 4), "\n")

# random-forest classifier for comparison
m_rfc <- ranger(factor(Delayed) ~ ., data = tr_cl, num.trees = 300, probability = TRUE, seed = 2026)
rfc_prob <- predict(m_rfc, te_cl)$predictions[, "1"]
rfc_cls  <- ifelse(rfc_prob > 0.5, 1, 0)
conf_rf  <- confusionMatrix(factor(rfc_cls, levels = c(0, 1)), factor(te_cl$Delayed, levels = c(0, 1)), positive = "1")
rfc_auc  <- auc(te_cl$Delayed, rfc_prob)
cat("Random-forest classifier  accuracy:", round(conf_rf$overall["Accuracy"], 4), " AUC:", round(rfc_auc, 4), "\n")

cls_metrics <- data.frame(
  Model = c("Logistic Regression", "Random Forest Classifier"),
  Accuracy    = c(conf$overall["Accuracy"], conf_rf$overall["Accuracy"]),
  Sensitivity = c(conf$byClass["Sensitivity"], conf_rf$byClass["Sensitivity"]),
  Specificity = c(conf$byClass["Specificity"], conf_rf$byClass["Specificity"]),
  Precision   = c(conf$byClass["Pos Pred Value"], conf_rf$byClass["Pos Pred Value"]),
  AUC         = c(glm_auc, rfc_auc))

# ==============================================================================
# STAGE 7 : COMPARISON, PLOTS AND PREDICTION  (Q5)
# ==============================================================================
section("STAGE7_COMPARISON")
metrics[, c("RMSE", "MAE", "R2")] <- round(metrics[, c("RMSE", "MAE", "R2")], 4)
cls_metrics[, -1] <- round(cls_metrics[, -1], 4)
cat("Regression models (target: Delivery_Time_min)\n"); print(metrics, row.names = FALSE)
cat("\nClassification models (target: Delayed)\n");       print(cls_metrics, row.names = FALSE)
write.csv(metrics,     "results/regression_metrics.csv",     row.names = FALSE)
write.csv(cls_metrics, "results/classification_metrics.csv", row.names = FALSE)
write.csv(as.data.frame(conf$table), "results/confusion_matrix_logistic.csv", row.names = FALSE)
write.csv(data.frame(Feature = names(imp), Importance = as.numeric(imp)),
          "results/rf_variable_importance.csv", row.names = FALSE)
write.csv(data.frame(Variable = names(coef(m_multi)), Coefficient = round(coef(m_multi), 4)),
          "results/multiple_lm_coefficients.csv", row.names = FALSE)

pred_plot <- function(actual, pred, title, fname) {
  d <- data.frame(Observed = actual, Predicted = pred)
  p <- ggplot(d, aes(Observed, Predicted)) + geom_point(alpha = 0.25, colour = col_teal) +
    geom_abline(colour = col_orange, linewidth = 1) + labs(title = title, x = "Observed (min)", y = "Predicted (min)")
  save_plot(p, fname, w = 6, h = 5)
}
pred_plot(te_reg$Delivery_Time_min, p_multi, "Multiple Linear: Predicted vs Actual", "10_lm_pred_vs_actual.png")
pred_plot(te_reg$Delivery_Time_min, p_rf,    "Random Forest: Predicted vs Actual",   "11_rf_pred_vs_actual.png")

res <- data.frame(Fitted = fitted(m_multi), Residual = resid(m_multi))
save_plot(ggplot(res, aes(Fitted, Residual)) + geom_point(alpha = 0.2, colour = col_teal) +
            geom_hline(yintercept = 0, colour = col_orange) +
            labs(title = "Residuals vs Fitted (Multiple Linear)", x = "Fitted (min)"),
          "12_lm_residuals.png")

imp_df <- data.frame(Feature = names(imp), Importance = imp)
save_plot(ggplot(imp_df, aes(reorder(Feature, Importance), Importance)) + geom_col(fill = col_teal) +
            coord_flip() + labs(title = "Random Forest Variable Importance", x = NULL),
          "13_rf_importance.png")

# ROC curve
roc_points <- function(y, p) { th <- sort(unique(c(0, p, 1)), decreasing = TRUE)
  data.frame(FPR = sapply(th, function(t) mean(p[y == 0] >= t)),
             TPR = sapply(th, function(t) mean(p[y == 1] >= t))) }
roc_df <- rbind(cbind(roc_points(te_cl$Delayed, glm_prob), Model = "Logistic"),
                cbind(roc_points(te_cl$Delayed, rfc_prob), Model = "Random Forest"))
save_plot(ggplot(roc_df, aes(FPR, TPR, colour = Model)) + geom_line(linewidth = 1) +
            geom_abline(linetype = "dashed", colour = "grey50") +
            scale_colour_manual(values = c(col_teal, col_orange)) +
            labs(title = sprintf("ROC Curves (AUC: logistic %.3f, RF %.3f)", glm_auc, rfc_auc),
                 x = "False positive rate", y = "True positive rate"),
          "14_roc_curves.png", w = 6.5, h = 5)

# confusion matrix heat-map
cm_df <- as.data.frame(conf$table)
save_plot(ggplot(cm_df, aes(Reference, Prediction, fill = Freq)) + geom_tile(colour = "white") +
            geom_text(aes(label = Freq), colour = "white", size = 6, fontface = "bold") +
            scale_fill_gradient(low = col_grey, high = col_teal, guide = "none") +
            scale_x_discrete(labels = c("On time", "Delayed")) + scale_y_discrete(labels = c("On time", "Delayed")) +
            labs(title = "Confusion Matrix - Logistic Regression", x = "Actual", y = "Predicted"),
          "15_confusion_matrix.png", w = 5.5, h = 4.5)

# model comparison
cmp <- metrics
save_plot(ggplot(cmp, aes(reorder(Model, -RMSE), RMSE, fill = Model)) + geom_col(show.legend = FALSE, width = 0.6) +
            geom_text(aes(label = round(RMSE, 2)), vjust = -0.4) +
            scale_fill_manual(values = c(col_grey, "#4C8896", col_teal, col_orange, "#0F3540")) +
            labs(title = "Test RMSE by Model (lower is better)", x = NULL, y = "RMSE (min)") +
            theme(axis.text.x = element_text(angle = 15, hjust = 1)),
          "16_model_comparison.png", w = 7.5, h = 4.8)

# ---- prediction on NEW orders ------------------------------------------------
section("STAGE7_PREDICTION")
new_orders <- data.frame(
  Scenario           = c("Short trip, clear, low traffic", "Evening rush, rain", "Long trip, storm, night",
                         "Medium trip, light rain, scooter", "Heavy order, high traffic"),
  Distance_km        = c(2.0, 6.5, 12.0, 7.5, 8.0),
  Prep_Time_min      = c(10, 18, 20, 15, 25),
  Courier_Experience = c(4, 1, 0.5, 3, 2),
  Items_Count        = c(1, 3, 2, 2, 6),
  Order_Value        = c(250, 640, 420, 380, 1500),
  Restaurant_Rating  = c(4.5, 3.8, 4.0, 4.6, 3.5),
  Traffic            = c("Low", "High", "High", "Medium", "High"),
  Weather            = c("Clear", "Rainy", "Stormy", "Rainy", "Foggy"),
  Time_of_Day        = c("Afternoon", "Evening", "Night", "Afternoon", "Evening"),
  Vehicle            = c("Scooter", "Bike", "Car", "Scooter", "Bike"),
  stringsAsFactors = FALSE)
for (f in c("Traffic", "Weather", "Time_of_Day", "Vehicle"))
  new_orders[[f]] <- factor(new_orders[[f]], levels = levels(train[[f]]))

best_reg <- m_int                        # lowest test RMSE (see Stage 7 comparison)
new_orders$Predicted_Time_min  <- round(predict(best_reg, new_orders), 1)
new_orders$RF_Time_min         <- round(predict(m_rf, new_orders)$predictions, 1)
new_orders$Promised_Time_min   <- round(30 + 2.5 * new_orders$Distance_km)
new_orders$Delay_Probability   <- round(predict(m_glm, new_orders, type = "response"), 3)
new_orders$Delay_Prediction    <- ifelse(new_orders$Delay_Probability > 0.5, "DELAYED", "On time")
print(new_orders[, c("Scenario", "Predicted_Time_min", "RF_Time_min", "Promised_Time_min",
                     "Delay_Probability", "Delay_Prediction")], row.names = FALSE)
write.csv(new_orders, "results/new_order_predictions.csv", row.names = FALSE)

saveRDS(m_multi, "results/model_multiple_lm.rds"); saveRDS(m_int, "results/model_linear_interaction.rds")
saveRDS(m_rf,    "results/model_random_forest.rds")
saveRDS(m_glm,   "results/model_logistic.rds")

section("STAGE7_CONCLUSION")
best <- metrics[which.min(metrics$RMSE), ]
cat("Best regression model on test data:", best$Model, "| RMSE", best$RMSE, "| R2", best$R2, "\n")
cat("Logistic regression AUC:", round(glm_auc, 3), "\n")
cat("\nAnalysis finished. Plots in ./plots, tables in ./results\n")
