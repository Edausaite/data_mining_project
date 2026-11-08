library(dplyr)
library(tidyr)
library(ggplot2)
dir.create("output", showWarnings = FALSE)


# 1. Read the five files into one table ---------------------------------
file_names <- paste0("FOOD-DATA-GROUP", 1:5, ".csv")
all_files <- list()

for (name in file_names) {
  one_file <- read.csv(file.path("data", name), check.names = FALSE)
  bad_columns <- names(one_file) == "" | grepl("^Unnamed", names(one_file))
  one_file <- one_file[, !bad_columns]                         # drop index column
  one_file$group_file <- sub(".csv", "", name, fixed = TRUE)   # source file (for food categories later)
  all_files[[name]] <- one_file
}

food <- bind_rows(all_files)
food$id <- seq_len(nrow(food))   # unique food ID
food$`Nutrition Density` <- NULL   # not used
cat("Number of foods:", nrow(food), "\n")  

macros <- c("Fat", "Saturated Fats", "Monounsaturated Fats",
            "Polyunsaturated Fats", "Carbohydrates", "Sugars",
            "Protein", "Dietary Fiber", "Water")
minerals <- c("Calcium", "Copper", "Iron", "Magnesium", "Manganese",
              "Phosphorus", "Potassium", "Selenium", "Zinc")


# 2. Values per 100 g? (Figure 1) --------------------------------
# If yes, water + fat + carbohydrates + protein would be about 100 g
food$portion_g <- food$Water + food$Fat + food$Carbohydrates + food$Protein

cat("Share of foods with sum 90-110 g:",
    round(mean(food$portion_g >= 90 & food$portion_g <= 110), 3), "\n")   # ~0.04

ggplot(filter(food, portion_g > 0), aes(x = portion_g)) +
  geom_histogram(bins = 60, fill = "#4472C4") +
  geom_vline(xintercept = 100, linetype = "dashed", colour = "red") +
  scale_x_log10() +
  labs(x = "Water + fat + carbohydrates + protein (g, log scale)",
       y = "Number of foods",
       title = "Sum of main components per food (red line = 100 g)") +
  theme_minimal()
ggsave("output/fig1_portion_check.png", width = 8, height = 5, dpi = 300)


# 3. Convert to 100 g and clean (Table 1) --------------------------------
# Remove foods under 5 g (scaling up multiplies rounding errors)
step1 <- filter(food, portion_g >= 5)

# Convert: value / reference weight * 100
nutrient_columns <- setdiff(names(food)[sapply(food, is.numeric)], c("portion_g", "id"))
for (col in nutrient_columns) {
  step1[[col]] <- step1[[col]] / step1$portion_g * 100
}

# Remove impossible foods: a macronutrient above 100 g per 100 g
step2 <- step1[apply(step1[macros], 1, max) <= 100, ]

# Remove impossible foods: more sugar than carbohydrates (+0.1 for rounding)
step3 <- filter(step2, Sugars <= Carbohydrates + 0.1)

food100 <- step3

cleaning <- data.frame(
  step = c("Original dataset", "Reference weight below 5 g",
           "Nutrient above 100 g per 100 g", "Sugars exceed total carbohydrates"),
  removed = c(NA, nrow(food) - nrow(step1), nrow(step1) - nrow(step2),
              nrow(step2) - nrow(step3)),
  remaining = c(nrow(food), nrow(step1), nrow(step2), nrow(step3))
)
print(cleaning)
write.csv(cleaning, "output/table1_cleaning_steps.csv", row.names = FALSE)


# 4. Share of zeros (targets need < 25% zeros) ---------------------------
vitamins <- grep("^Vitamin", names(food100), value = TRUE)
share_zero <- sapply(food100[c(minerals, vitamins)], function(x) mean(x == 0))
print(round(sort(share_zero), 3))


# 5. Mineral distributions(Figure 2) -----------------
minerals_long <- pivot_longer(food100[minerals], cols = everything(),
                              names_to = "mineral", values_to = "value")

ggplot(minerals_long, aes(x = value)) +
  geom_histogram(bins = 40, fill = "#4472C4") +
  facet_wrap(~ mineral, scales = "free") +
  labs(x = "Value per 100 g", y = "Number of foods", title = "Minerals: raw scale") +
  theme_minimal()
ggsave("output/fig2_minerals_raw.png", width = 10, height = 7, dpi = 300)

# log1p(x) = log(1 + x)
ggplot(minerals_long, aes(x = log1p(value))) +
  geom_histogram(bins = 40, fill = "#4472C4") +
  facet_wrap(~ mineral, scales = "free") +
  labs(x = "log(1 + value per 100 g)", y = "Number of foods", title = "Minerals: log scale") +
  theme_minimal()
ggsave("output/fig2_minerals_log.png", width = 10, height = 7, dpi = 300)


# 6. Spearman correlations --------
cor_matrix <- cor(food100[macros], food100[minerals], method = "spearman")
cor_table <- as.data.frame(as.table(cor_matrix))
names(cor_table) <- c("macronutrient", "mineral", "rho")

ggplot(cor_table, aes(x = mineral, y = macronutrient, fill = rho)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(rho, 2)), size = 2.6) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       limits = c(-1, 1)) +
  labs(x = NULL, y = NULL, fill = "Spearman\nrho",
       title = "Correlation between macronutrients and minerals (per 100 g)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/fig3_correlations.png", width = 10, height = 6, dpi = 300)


# 7. Save the cleaned dataset - for modelling  --------------
write.csv(food100, "output/food_per100g_clean.csv", row.names = FALSE)
cat("Done:", nrow(food100), "foods saved\n")   # 2198
