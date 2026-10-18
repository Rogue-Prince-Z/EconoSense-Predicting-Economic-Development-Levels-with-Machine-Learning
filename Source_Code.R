# ============================================================
# IDS Final Project
# Topic: GDP Per Capita Level Classification Using World Bank Data
# Method: R-based Web Scraping / Web Data Extraction
# Source: World Bank Public Web Endpoints
# Models: Decision Tree and Random Forest
# ============================================================

# -------------------------------
# Step 1: Install and load packages
# -------------------------------

options(timeout = 300)

packages <- c(
  "rvest",
  "httr",
  "jsonlite",
  "dplyr",
  "tidyr",
  "ggplot2",
  "rpart",
  "rpart.plot",
  "randomForest"
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

library(rvest)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)

# -------------------------------
# Step 2: Create folders
# -------------------------------

if (!dir.exists("dataset")) {
  dir.create("dataset")
}

if (!dir.exists("plots")) {
  dir.create("plots")
}

# -------------------------------
# Step 3: Read source webpage using rvest
# -------------------------------

source_url <- "https://data.worldbank.org/indicator/NY.GDP.PCAP.CD"

page_title <- tryCatch(
  {
    page <- rvest::read_html(source_url)
    page %>%
      rvest::html_node("title") %>%
      rvest::html_text()
  },
  error = function(e) {
    "World Bank GDP per capita source page"
  }
)

print(page_title)

source_info <- data.frame(
  Source_Name = "World Bank Data",
  Source_URL = source_url,
  Page_Title = page_title,
  Collection_Method = "R-based web scraping and web data extraction using rvest, httr, and jsonlite",
  stringsAsFactors = FALSE
)

write.csv(
  source_info,
  "dataset/source_information.csv",
  row.names = FALSE
)

# -------------------------------
# Step 4: Define World Bank indicators
# -------------------------------
indicators <- c(
  gdp_per_capita = "NY.GDP.PCAP.CD",
  population = "SP.POP.TOTL",
  life_expectancy = "SP.DYN.LE00.IN",
  internet_users = "IT.NET.USER.ZS",
  urban_population_pct = "SP.URB.TOTL.IN.ZS",
  unemployment_rate = "SL.UEM.TOTL.ZS"
)

start_year <- 2018
end_year <- 2022

# -------------------------------
# Step 5: Helper functions
# -------------------------------

get_col <- function(df, col_name, default_value = NA) {
  if (col_name %in% names(df)) {
    return(df[[col_name]])
  } else {
    return(rep(default_value, nrow(df)))
  }
}

get_first_available_col <- function(df, col_names, default_value = NA) {
  for (col_name in col_names) {
    if (col_name %in% names(df)) {
      return(df[[col_name]])
    }
  }
  return(rep(default_value, nrow(df)))
}

safe_median_impute <- function(x) {
  if (all(is.na(x))) {
    return(rep(0, length(x)))
  }
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
}

# -------------------------------
# Step 6: Scrape one indicator from World Bank public web endpoint
# -------------------------------

scrape_indicator <- function(indicator_code, variable_name, start_year, end_year) {

  url <- paste0(
    "https://api.worldbank.org/v2/country/all/indicator/",
    indicator_code,
    "?format=json&date=",
    start_year,
    ":",
    end_year,
    "&per_page=20000"
  )

  print(paste("Collecting:", variable_name))
  print(url)

  response <- tryCatch(
    {
      httr::GET(
        url,
        httr::user_agent("Mozilla/5.0 Data Science Student Project")
      )
    },
    error = function(e) {
      warning(paste("Request failed for:", variable_name))
      return(NULL)
    }
  )

  if (is.null(response)) {
    return(data.frame())
  }

  if (httr::status_code(response) != 200) {
    warning(paste("Failed to collect:", variable_name))
    return(data.frame())
  }

  text_data <- httr::content(
    response,
    as = "text",
    encoding = "UTF-8"
  )

  parsed_data <- tryCatch(
    {
      jsonlite::fromJSON(text_data, flatten = TRUE)
    },
    error = function(e) {
      warning(paste("JSON parsing failed for:", variable_name))
      return(NULL)
    }
  )

  if (is.null(parsed_data) || length(parsed_data) < 2 || is.null(parsed_data[[2]])) {
    warning(paste("No data found for:", variable_name))
    return(data.frame())
  }

  df <- as.data.frame(parsed_data[[2]])

  if (nrow(df) == 0) {
    warning(paste("Empty data for:", variable_name))
    return(data.frame())
  }

  result <- data.frame(
    country = as.character(get_col(df, "country.value", "")),
    iso2c = as.character(get_col(df, "country.id", "")),
    iso3c = as.character(get_col(df, "countryiso3code", "")),
    year = as.integer(get_col(df, "date", NA)),
    variable = variable_name,
    value = as.numeric(get_col(df, "value", NA)),
    stringsAsFactors = FALSE
  )

  result <- result %>%
    dplyr::filter(
      country != "",
      iso2c != "",
      !is.na(year)
    )

  Sys.sleep(0.5)

  return(result)
}

# -------------------------------
# Step 7: Collect all indicators
# -------------------------------

all_data <- list()

for (indicator_name in names(indicators)) {
  all_data[[indicator_name]] <- scrape_indicator(
    indicator_code = indicators[[indicator_name]],
    variable_name = indicator_name,
    start_year = start_year,
    end_year = end_year
  )
}

long_data <- dplyr::bind_rows(all_data)
if (nrow(long_data) == 0) {
  stop("No data was collected. Check internet connection or World Bank server.")
}

# -------------------------------
# Step 8: Convert long data to wide format
# -------------------------------

world_data <- long_data %>%
  dplyr::group_by(country, iso2c, iso3c, year, variable) %>%
  dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = variable,
    values_from = value
  )

# -------------------------------
# Step 9: Scrape country metadata
# -------------------------------

scrape_country_metadata <- function() {

  url <- "https://api.worldbank.org/v2/country/all?format=json&per_page=400"

  print("Collecting country metadata")
  print(url)

  response <- tryCatch(
    {
      httr::GET(
        url,
        httr::user_agent("Mozilla/5.0 Data Science Student Project")
      )
    },
    error = function(e) {
      warning("Country metadata request failed.")
      return(NULL)
    }
  )

  if (is.null(response)) {
    return(data.frame())
  }

  if (httr::status_code(response) != 200) {
    warning("Country metadata could not be collected.")
    return(data.frame())
  }

  text_data <- httr::content(
    response,
    as = "text",
    encoding = "UTF-8"
  )

  parsed_data <- tryCatch(
    {
      jsonlite::fromJSON(text_data, flatten = TRUE)
    },
    error = function(e) {
      warning("Country metadata JSON parsing failed.")
      return(NULL)
    }
  )

  if (is.null(parsed_data) || length(parsed_data) < 2 || is.null(parsed_data[[2]])) {
    warning("No country metadata found.")
    return(data.frame())
  }

  df <- as.data.frame(parsed_data[[2]])

  metadata <- data.frame(
    iso2c = as.character(get_first_available_col(df, c("iso2Code", "id"), "")),
    country_name_meta = as.character(get_first_available_col(df, c("name"), "")),
    region = as.character(get_first_available_col(df, c("region.value"), "")),
    income = as.character(get_first_available_col(df, c("incomeLevel.value"), "")),
    capital = as.character(get_first_available_col(df, c("capitalCity"), "")),
    longitude = as.character(get_first_available_col(df, c("longitude"), "")),
    latitude = as.character(get_first_available_col(df, c("latitude"), "")),
    stringsAsFactors = FALSE
  )

  return(metadata)
}

country_metadata <- scrape_country_metadata()

# -------------------------------
# Step 10: Merge indicator data with country metadata
# -------------------------------

if (nrow(country_metadata) > 0) {
  world_data <- world_data %>%
    dplyr::left_join(country_metadata, by = "iso2c") %>%
    dplyr::mutate(
      country = ifelse(
        !is.na(country_name_meta) & country_name_meta != "",
        country_name_meta,
        country
      )
    )
} else {
  world_data$region <- NA
  world_data$income <- NA
}

# -------------------------------
# Step 11: Remove aggregate regions
# -------------------------------

if ("region" %in% names(world_data)) {
  world_data <- world_data %>%
    dplyr::filter(region != "Aggregates")
}

# -------------------------------
# Step 12: Select useful columns
# -------------------------------

world_data <- world_data %>%
  dplyr::select(
    country,
    iso2c,
    year,
    region,
    income,
    gdp_per_capita,
    population,
    life_expectancy,
    internet_users,
    urban_population_pct,
    unemployment_rate
  )

# -------------------------------
# Step 13: View raw collected data
# -------------------------------

print(head(world_data))
print(dim(world_data))
str(world_data)

# -------------------------------
# Step 14: Check missing values
# -------------------------------

missing_values <- colSums(is.na(world_data))
print(missing_values)
missing_values_df <- data.frame(
  Column = names(missing_values),
  Missing_Count = as.numeric(missing_values)
)

write.csv(
  missing_values_df,
  "dataset/missing_values_before_cleaning.csv",
  row.names = FALSE
)

# -------------------------------
# Step 15: Clean dataset
# -------------------------------

world_data_clean <- world_data %>%
  dplyr::filter(
    !is.na(gdp_per_capita),
    !is.na(population)
  )

# -------------------------------
# Step 16: Handle missing values
# -------------------------------

world_data_clean$life_expectancy <- safe_median_impute(world_data_clean$life_expectancy)
world_data_clean$internet_users <- safe_median_impute(world_data_clean$internet_users)
world_data_clean$urban_population_pct <- safe_median_impute(world_data_clean$urban_population_pct)
world_data_clean$unemployment_rate <- safe_median_impute(world_data_clean$unemployment_rate)

world_data_clean$region[is.na(world_data_clean$region) | world_data_clean$region == ""] <- "Unknown"
world_data_clean$income[is.na(world_data_clean$income) | world_data_clean$income == ""] <- "Unknown"

# -------------------------------
# Step 17: Feature engineering
# -------------------------------

world_data_clean$log_population <- log10(world_data_clean$population + 1)

# -------------------------------
# Step 18: Create target variable
# -------------------------------

median_gdp <- median(world_data_clean$gdp_per_capita, na.rm = TRUE)

world_data_clean$high_gdp_per_capita <- ifelse(
  world_data_clean$gdp_per_capita >= median_gdp,
  1,
  0
)

# -------------------------------
# Step 19: Final dataset check
# -------------------------------

print(head(world_data_clean))
print(dim(world_data_clean))
str(world_data_clean)
print(summary(world_data_clean))
print(table(world_data_clean$high_gdp_per_capita))

if (interactive()) {
  View(world_data_clean)
}

# -------------------------------
# Step 20: Save final dataset as CSV
# -------------------------------

write.csv(
  world_data_clean,
  "dataset/world_bank_gdp_project_dataset.csv",
  row.names = FALSE
)
# ============================================================
# Exploratory Data Analysis
# ============================================================

plot1 <- ggplot(world_data_clean, aes(x = factor(high_gdp_per_capita))) +
  geom_bar() +
  labs(
    title = "High vs Low GDP Per Capita",
    x = "High GDP Per Capita",
    y = "Count"
  )

print(plot1)

ggsave(
  "plots/01_high_low_gdp_count.png",
  plot1,
  width = 7,
  height = 5
)

plot2 <- ggplot(world_data_clean, aes(x = gdp_per_capita)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of GDP Per Capita",
    x = "GDP Per Capita",
    y = "Frequency"
  )

print(plot2)

ggsave(
  "plots/02_gdp_distribution.png",
  plot2,
  width = 7,
  height = 5
)

plot3 <- ggplot(world_data_clean, aes(x = life_expectancy, y = gdp_per_capita)) +
  geom_point() +
  labs(
    title = "Life Expectancy vs GDP Per Capita",
    x = "Life Expectancy",
    y = "GDP Per Capita"
  )

print(plot3)

ggsave(
  "plots/03_life_expectancy_vs_gdp.png",
  plot3,
  width = 7,
  height = 5
)

plot4 <- ggplot(world_data_clean, aes(x = internet_users, y = gdp_per_capita)) +
  geom_point() +
  labs(
    title = "Internet Users vs GDP Per Capita",
    x = "Internet Users Percentage",
    y = "GDP Per Capita"
  )

print(plot4)

ggsave(
  "plots/04_internet_users_vs_gdp.png",
  plot4,
  width = 7,
  height = 5
)

avg_gdp_year <- world_data_clean %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(avg_gdp = mean(gdp_per_capita, na.rm = TRUE), .groups = "drop")
plot5 <- ggplot(avg_gdp_year, aes(x = year, y = avg_gdp)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average GDP Per Capita by Year",
    x = "Year",
    y = "Average GDP Per Capita"
  )

print(plot5)

ggsave(
  "plots/05_average_gdp_by_year.png",
  plot5,
  width = 7,
  height = 5
)

top_gdp_countries <- world_data_clean %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(avg_gdp = mean(gdp_per_capita, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(desc(avg_gdp)) %>%
  dplyr::slice_head(n = 10)

plot6 <- ggplot(top_gdp_countries, aes(x = reorder(country, avg_gdp), y = avg_gdp)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Average GDP Per Capita",
    x = "Country",
    y = "Average GDP Per Capita"
  )

print(plot6)

ggsave(
  "plots/06_top_10_gdp_countries.png",
  plot6,
  width = 8,
  height = 5
)

# ============================================================
# Model 1: Decision Tree Classification
# Model 2: Random Forest Classification
# ============================================================

# -------------------------------
# Step 21: Decision Tree model dataset
# -------------------------------

tree_model_data <- world_data_clean %>%
  dplyr::select(
    year,
    log_population,
    life_expectancy,
    internet_users,
    urban_population_pct,
    unemployment_rate,
    high_gdp_per_capita
  )

tree_model_data$high_gdp_per_capita <- as.factor(tree_model_data$high_gdp_per_capita)

if (interactive()) {
  View(tree_model_data)
}

# -------------------------------
# Step 22: Train-test split
# -------------------------------

set.seed(123)
train_index <- sample(
  1:nrow(tree_model_data),
  size = floor(0.7 * nrow(tree_model_data))
)

tree_train_data <- tree_model_data[train_index, ]
tree_test_data <- tree_model_data[-train_index, ]

# -------------------------------
# Step 23: Train Decision Tree model
# -------------------------------

tree_model <- rpart(
  high_gdp_per_capita ~ .,
  data = tree_train_data,
  method = "class"
)

# -------------------------------
# Step 24: Plot Decision Tree
# -------------------------------

rpart.plot(tree_model)

png(
  "plots/07_decision_tree_model.png",
  width = 1200,
  height = 800
)

rpart.plot(
  tree_model,
  main = "Decision Tree Model"
)

dev.off()

# -------------------------------
# Step 25: Decision Tree prediction and evaluation
# -------------------------------

tree_predictions <- predict(
  tree_model,
  tree_test_data,
  type = "class"
)

tree_confusion_matrix <- table(
  Actual = tree_test_data$high_gdp_per_capita,
  Predicted = tree_predictions
)

print(tree_confusion_matrix)

tree_accuracy <- sum(diag(tree_confusion_matrix)) / sum(tree_confusion_matrix)

print(
  paste("Decision Tree Accuracy:", round(tree_accuracy, 4))
)

# ============================================================
# Random Forest Classification
# ============================================================

# -------------------------------
# Step 26: Random Forest model dataset
# region and income are included to improve accuracy.
# gdp_per_capita is NOT used as a feature.
# -------------------------------

rf_model_data <- world_data_clean %>%
  dplyr::select(
    year,
    region,
    income,
    log_population,
    life_expectancy,
    internet_users,
    urban_population_pct,
    unemployment_rate,
    high_gdp_per_capita
  )

rf_model_data$region <- as.factor(rf_model_data$region)
rf_model_data$income <- as.factor(rf_model_data$income)
rf_model_data$high_gdp_per_capita <- as.factor(rf_model_data$high_gdp_per_capita)

if (interactive()) {
  View(rf_model_data)
}

# -------------------------------
# Step 27: Use same train-test split
# -------------------------------

rf_train_data <- rf_model_data[train_index, ]
rf_test_data <- rf_model_data[-train_index, ]

# -------------------------------
# Step 28: Train Random Forest model
# -------------------------------

set.seed(123)

rf_model <- randomForest(
  high_gdp_per_capita ~ .,
  data = rf_train_data,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

print(rf_model)

# -------------------------------
# Step 29: Random Forest prediction and evaluation
# -------------------------------

rf_predictions <- predict(
  rf_model,
  newdata = rf_test_data,
  type = "class"
)

rf_confusion_matrix <- table(
  Actual = rf_test_data$high_gdp_per_capita,
  Predicted = rf_predictions
)

print(rf_confusion_matrix)

rf_accuracy <- sum(diag(rf_confusion_matrix)) / sum(rf_confusion_matrix)

print(
  paste("Random Forest Accuracy:", round(rf_accuracy, 4))
)

# -------------------------------
# Step 30: Random Forest feature importance
# -------------------------------

rf_importance <- importance(rf_model)

print(rf_importance)

rf_importance_df <- data.frame(
  Feature = rownames(rf_importance),
  MeanDecreaseGini = rf_importance[, "MeanDecreaseGini"],
  row.names = NULL
)
rf_importance_df <- rf_importance_df %>%
  dplyr::arrange(desc(MeanDecreaseGini))

write.csv(
  rf_importance_df,
  "dataset/random_forest_feature_importance.csv",
  row.names = FALSE
)

png(
  "plots/08_random_forest_feature_importance.png",
  width = 1000,
  height = 700
)

varImpPlot(
  rf_model,
  main = "Random Forest Feature Importance"
)

dev.off()

# ============================================================
# Evaluation Metrics
# ============================================================

calculate_metrics <- function(conf_matrix) {

  conf_matrix <- as.matrix(conf_matrix)

  if (!all(c("0", "1") %in% rownames(conf_matrix)) || !all(c("0", "1") %in% colnames(conf_matrix))) {
    stop("Confusion matrix must contain both classes 0 and 1.")
  }

  TP <- conf_matrix["1", "1"]
  TN <- conf_matrix["0", "0"]
  FP <- conf_matrix["0", "1"]
  FN <- conf_matrix["1", "0"]

  accuracy <- (TP + TN) / sum(conf_matrix)
  precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
  recall <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  f1_score <- ifelse((precision + recall) == 0, 0, 2 * ((precision * recall) / (precision + recall)))

  return(
    c(
      Accuracy = accuracy,
      Precision = precision,
      Recall = recall,
      F1_Score = f1_score
    )
  )
}

tree_metrics <- calculate_metrics(tree_confusion_matrix)
rf_metrics <- calculate_metrics(rf_confusion_matrix)

model_results <- data.frame(
  Model = c("Decision Tree", "Random Forest"),
  Accuracy = c(tree_metrics["Accuracy"], rf_metrics["Accuracy"]),
  Accuracy_Percentage = c(
    paste0(round(tree_metrics["Accuracy"] * 100, 2), "%"),
    paste0(round(rf_metrics["Accuracy"] * 100, 2), "%")
  ),
  Precision = c(tree_metrics["Precision"], rf_metrics["Precision"]),
  Recall = c(tree_metrics["Recall"], rf_metrics["Recall"]),
  F1_Score = c(tree_metrics["F1_Score"], rf_metrics["F1_Score"])
)

print(model_results)

# ============================================================
# Save model results
# ============================================================
write.csv(
  model_results,
  "dataset/world_bank_model_results.csv",
  row.names = FALSE
)

write.csv(
  as.data.frame(tree_confusion_matrix),
  "dataset/decision_tree_confusion_matrix.csv",
  row.names = FALSE
)

write.csv(
  as.data.frame(rf_confusion_matrix),
  "dataset/random_forest_confusion_matrix.csv",
  row.names = FALSE
)

# ============================================================
# Final Output
# ============================================================

print("Project completed successfully using R-based web scraping/data extraction.")
print("Dataset saved as: dataset/world_bank_gdp_project_dataset.csv")
print("Model results saved as: dataset/world_bank_model_results.csv")
print("Decision Tree confusion matrix saved.")
print("Random Forest confusion matrix saved.")
print("Random Forest feature importance saved.")
print("Graphs saved inside: plots folder")
