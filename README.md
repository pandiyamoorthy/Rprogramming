# Food Delivery Time Analysis and Delay Prediction Using R

Mini project for **23CAP317 – R Programming** (MCA, II Year / III Semester / Section A, Karpagam College of Engineering, Autonomous).
Author: **Pandiyamoorthy S**

## What this project does

Predicts how long a food delivery will take and whether it will arrive later than promised, using R.

| Stage | What happens |
|---|---|
| Q1 | R data structures, dataset description and objectives |
| Q2 | Control structures, functions, lists and data frames used in the model |
| Cleaning + EDA | Missing values, duplicates, invalid entries, 9 visualisations |
| Q3 | Shapiro-Wilk, Pearson, Welch t-test, ANOVA + Tukey, Kruskal-Wallis, chi-square, VIF |
| Q4 | Simple / multiple / interaction linear regression, decision tree, random forest, logistic regression |
| Q5 | Model comparison, predictions for new orders |

## Results (996 unseen test orders)

| Model | R² / Accuracy | RMSE (min) |
|---|---|---|
| Simple linear (distance) | 0.664 | 8.349 |
| Multiple linear | 0.877 | 5.051 |
| **Linear + interactions (best)** | **0.904** | **4.470** |
| Decision tree | 0.755 | 7.131 |
| Random forest | 0.861 | 5.374 |
| Logistic regression (delay) | accuracy 0.868, AUC 0.914 | – |

## Dataset

`food_delivery_raw.csv` (5,025 rows × 14 columns) is **simulated** by `01_generate_dataset.R` with `set.seed(2026)` from realistic delivery rules
(distance, vehicle speed, traffic, weather, time of day, preparation time, courier experience) plus noise. Missing values, duplicates and impossible
values are injected on purpose to exercise the cleaning stage. To use real order logs, replace the CSV with a file that has the same columns.

## Files

```
01_generate_dataset.R          builds food_delivery_raw.csv
02_food_delivery_analysis.R    cleaning, EDA, statistics, models, prediction
food_delivery_raw.csv          raw data
food_delivery_clean.csv        cleaned data (4,990 rows x 17 columns)
plots/                         16 figures
results/                       metric tables, predictions, saved models (.rds)
results_console.txt            full console output
```

## How to run

Requires R 4.3+ and the packages `ggplot2`, `dplyr`, `corrplot`, `caret`, `ranger`, `rpart`, `reshape2`.

```r
install.packages(c("ggplot2", "dplyr", "corrplot", "caret", "ranger", "rpart", "reshape2"))
```

```bash
Rscript 01_generate_dataset.R
Rscript 02_food_delivery_analysis.R
```
