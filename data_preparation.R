packages <- c('caret', 'rpart', 'corrplot', 'rpart.plot', 'car',
              'VGAM', 'dplyr', 'caTools', 'class', 'ggplot2',
              'kernlab', 'glmnet', 'tidyr', 'reticulate', 'proxy')

invisible(lapply(packages, library, character.only = TRUE))

dir.create("output", showWarnings = FALSE)

#setwd('C:/Users/vomba/Desktop/Studijos/Data Mining/Group Project/Nutrient Project/Git')

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
food$id <- seq_len(nrow(food))   # adding unique food ID
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

# Remove foods with mineral values shifted by one column
# (calcium -> copper, magnesium -> manganese, phosphorus -> potassium, potassium -> selenium)
step4 <- filter(step3, !(Copper > 5 & Calcium < 1))

food100 <- step4

cleaning <- data.frame(
  step = c("Original dataset", "Reference weight below 5 g",
           "Nutrient above 100 g per 100 g", "Sugars exceed total carbohydrates","Mineral values shifted by one column"),
  removed = c(NA, nrow(food) - nrow(step1), nrow(step1) - nrow(step2),
              nrow(step2) - nrow(step3), nrow(step3) - nrow(step4)),
  remaining = c(nrow(food), nrow(step1), nrow(step2), nrow(step3), nrow(step4))
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

##########

df <- step4

### Target

targets <- minerals

### Test/Train splits 70:30

set.seed(321)

tr_df_filtered <- sample(1:nrow(df), size = 0.7 * nrow(df))
train_df_filtered <- df[tr_df_filtered, ]
test_df_filtered <- df[-tr_df_filtered, ]

names(train_df_filtered) <- make.names(names(train_df_filtered))
names(test_df_filtered) <- make.names(names(test_df_filtered))  

macros <- make.names(macros)

### kfolds value

k_folds <- 8


### Linear model

lm_models <- list()
performance <- list()

for (target in targets){
  
  tr <- train_df_filtered
  te <- test_df_filtered
  tr[[target]] <- log1p(tr[[target]])
  te[[target]] <- log1p(te[[target]])
  
  predictors <- setdiff(macros, target)
  
  set.seed(321)
  fold <- createFolds(tr[[target]], k = k_folds, returnTrain = TRUE)
  ctr1 <- trainControl(method = 'cv', index = fold)
  
  lmCTRL <- train(reformulate(predictors, response = target), data = tr,method = 'lm',
    preProcess = c('zv', 'YeoJohnson'), trControl = ctr1)
  
  lm_models[[target]] <- lmCTRL
  
  ### Error metrics
  tr_perf <- getTrainPerf(lmCTRL)[, c('TrainRMSE', 'TrainRsquared', 'TrainMAE')]
  
  ### Log scale
  te_perf <- defaultSummary(data.frame(obs  = te[[target]],pred = predict(lmCTRL, te)))
  names(te_perf) <- paste0('Test', names(te_perf))
  
  ### Back to original scale
  te_orig <- defaultSummary(data.frame(obs  = expm1(te[[target]]), pred = expm1(predict(lmCTRL, te))))
  names(te_orig) <- paste0('TestOrig', names(te_orig))
  performance[[target]] <- cbind(tr_perf, t(te_perf), t(te_orig))
  }

performance_table <- do.call(rbind, performance)
round(performance_table,3)

### Lasso

lasso_models <- list()
performance_lasso <- list()

for (target in targets){
  
  tr <- train_df_filtered
  te <- test_df_filtered
  tr[[target]] <- log1p(tr[[target]])
  te[[target]] <- log1p(te[[target]])
  
  predictors <- setdiff(macros, target)
  
  set.seed(321)
  fold <- createFolds(tr[[target]], k = k_folds, returnTrain = TRUE)
  ctr1 <- trainControl(method = 'cv', index = fold)
  
  lassoGrid = expand.grid(alpha = 1, lambda = 10^seq(-3, 1, length = 30)) ### Check params later
  
  LglmnetCTRL <- train(reformulate(predictors, response = target), data = tr,
                       method = 'glmnet', preProcess = c('YeoJohnson','zv'),
                       trControl = ctr1, tuneGrid = lassoGrid)
  
  lasso_models[[target]] <- LglmnetCTRL
  
  ### Error metrics
  tr_perf <- getTrainPerf(LglmnetCTRL)[, c('TrainRMSE', 'TrainRsquared', 'TrainMAE')]
  
  ### Log scale
  te_perf <- defaultSummary(data.frame(obs  = te[[target]],pred = predict(LglmnetCTRL, te)))
  names(te_perf) <- paste0('Test', names(te_perf))
  
  ### Back to original scale
  te_orig <- defaultSummary(data.frame(obs  = expm1(te[[target]]), pred = expm1(predict(LglmnetCTRL, te))))
  names(te_orig) <- paste0('TestOrig', names(te_orig))
  
  performance_lasso[[target]] <- cbind(tr_perf, t(te_perf), t(te_orig))
  }

performance_table_lasso <- do.call(rbind, performance_lasso)
round(performance_table_lasso,3)

### Ridge

ridgeGrid <- data.frame(lambda = seq(0, 1, length = 21))

ridge_models <- list()
performance_ridge <- list()

for (target in targets){
  
  tr <- train_df_filtered
  te <- test_df_filtered
  tr[[target]] <- log1p(tr[[target]])
  te[[target]] <- log1p(te[[target]])
  
  predictors <- setdiff(macros, target)
  
  set.seed(321)
  fold <- createFolds(tr[[target]], k = k_folds, returnTrain = TRUE)
  ctr1 <- trainControl(method = 'cv', index = fold)
  
  RidgeCTRL <- train(reformulate(predictors, response = target), data = tr,
                       method = 'ridge', preProcess = c('YeoJohnson','zv'),
                       trControl = ctr1, tuneGrid = ridgeGrid)
  
  ridge_models[[target]] <- RidgeCTRL
  
  ### Error metrics
  tr_perf <- getTrainPerf(RidgeCTRL)[, c('TrainRMSE', 'TrainRsquared', 'TrainMAE')]
  
  ### Log scale
  pred_log <- predict(RidgeCTRL, te)
  te_perf <- defaultSummary(data.frame(obs = te[[target]], pred = pred_log))
  names(te_perf) <- paste0('Test', names(te_perf))
  
  ### Back to original scale
  te_orig <- defaultSummary(data.frame(obs = expm1(te[[target]]), pred = expm1(pred_log)))
  names(te_orig) <- paste0('TestOrig', names(te_orig))
  
  performance_ridge[[target]] <- cbind(tr_perf, t(te_perf), t(te_orig),
                                       lambda = RidgeCTRL$bestTune$lambda)
  }

performance_table_ridge <- do.call(rbind, performance_ridge)
round(performance_table_ridge, 3)

### Elastic Net

Enet_models <- list()
performance_enet <- list()
enetGrid <- expand.grid(alpha = seq(0.1, 0.9, by = 0.1),
                        lambda = 10^seq(-3, 1, length = 30))

for (target in targets){
  
  tr <- train_df_filtered
  te <- test_df_filtered
  tr[[target]] <- log1p(tr[[target]])
  te[[target]] <- log1p(te[[target]])
  
  predictors <- setdiff(macros, target)
  
  set.seed(321)
  fold <- createFolds(tr[[target]], k = k_folds, returnTrain = TRUE)
  ctr1 <- trainControl(method = 'cv', index = fold)
  
  ENglmnetCTRL <- train(reformulate(predictors, response = target), data = tr,
                        method = 'glmnet', preProcess = c('YeoJohnson','zv'),
                        trControl = ctr1, tuneGrid = enetGrid)
  Enet_models[[target]] <- ENglmnetCTRL
  
  ### Error metrics
  tr_perf <- getTrainPerf(ENglmnetCTRL)[, c('TrainRMSE', 'TrainRsquared', 'TrainMAE')]
  
  ### Log scale
  te_perf <- defaultSummary(data.frame(obs  = te[[target]],pred = predict(ENglmnetCTRL, te)))
  names(te_perf) <- paste0('Test', names(te_perf))
  
  ### Back to original scale
  te_orig <- defaultSummary(data.frame(obs  = expm1(te[[target]]), pred = expm1(predict(ENglmnetCTRL, te))))
  names(te_orig) <- paste0('TestOrig', names(te_orig))
  
  performance_enet[[target]] <- cbind(tr_perf, t(te_perf), t(te_orig))
}

performance_table_enet <- do.call(rbind, performance_enet)
round(performance_table_enet,3)

### KNN

knnGrid <- data.frame(k = seq(3, 31, by = 2))

KNN_models <- list()
performance_KNN <- list()

for (target in targets){
  
  tr <- train_df_filtered
  te <- test_df_filtered
  tr[[target]] <- log1p(tr[[target]])
  te[[target]] <- log1p(te[[target]])
  
  predictors <- setdiff(macros, target)
  
  set.seed(321)
  fold <- createFolds(tr[[target]], k = k_folds, returnTrain = TRUE)
  ctr1 <- trainControl(method = 'cv', index = fold)
  
  knnTune <- train(reformulate(predictors, response = target), data = tr,
                   method = 'knn', tuneGrid = knnGrid,
                   preProcess = c('YeoJohnson', 'zv', 'center', 'scale'),
                   trControl = ctr1)
 
  KNN_models[[target]] <- knnTune  
  tr_perf <- getTrainPerf(knnTune)[, c('TrainRMSE', 'TrainRsquared', 'TrainMAE')]
  
  # Predict log scale
  
  pred_log <- predict(knnTune, te)
  te_perf <- defaultSummary(data.frame(obs = te[[target]], pred = pred_log))
  names(te_perf) <- paste0('Test', names(te_perf))
  
  # Predict orignal scale
  
  te_orig <- defaultSummary(data.frame(obs = expm1(te[[target]]), pred = expm1(pred_log)))
  names(te_orig) <- paste0('TestOrig', names(te_orig))
  
  performance_KNN[[target]] <- cbind(tr_perf, t(te_perf), t(te_orig),K = knnTune$bestTune$k)
  }
performance_table_KNN <- do.call(rbind, performance_KNN)
round(performance_table_KNN,3)

### Support vector machines radial

SVMr_models <- list()
performance_SVMr <- list()

for (target in targets){
  
  tr <- train_df_filtered
  te <- test_df_filtered
  tr[[target]] <- log1p(tr[[target]])
  te[[target]] <- log1p(te[[target]])
  
  predictors <- setdiff(macros, target)
  
  set.seed(321)
  fold <- createFolds(tr[[target]], k = k_folds, returnTrain = TRUE)
  ctr1 <- trainControl(method = 'cv', index = fold)
  
  SVMRadial <- train(reformulate(predictors, response = target), data = tr,
                     method = 'svmRadial', tuneLength = 8,
                     preProcess = c('YeoJohnson','zv', 'center', 'scale'),
                     trControl = ctr1)
  
  SVMr_models[[target]] <- SVMRadial
  tr_perf <- getTrainPerf(SVMRadial)[, c('TrainRMSE', 'TrainRsquared', 'TrainMAE')]
  
  # Log predict
  
  pred_log <- predict(SVMRadial, te)
  te_perf <- defaultSummary(data.frame(obs = te[[target]], pred = pred_log))
  names(te_perf) <- paste0('Test', names(te_perf))
  
  # Original scale
  
  te_orig <- defaultSummary(data.frame(obs = expm1(te[[target]]), pred = expm1(pred_log)))
  names(te_orig) <- paste0('TestOrig', names(te_orig))
  
  performance_SVMr[[target]] <- cbind(tr_perf, t(te_perf), t(te_orig), C = SVMRadial$bestTune$C, sigma = SVMRadial$bestTune$sigma)
}

performance_table_SVMr <- do.call(rbind, performance_SVMr)
round(performance_table_SVMr,3)

### Random Forest

RF_models <- list()
performance_RF <- list()

for (target in targets){
  
  tr <- train_df_filtered
  te <- test_df_filtered
  tr[[target]] <- log1p(tr[[target]])
  te[[target]] <- log1p(te[[target]])
  
  predictors <- setdiff(macros, target)
  
  set.seed(321)
  fold <- createFolds(tr[[target]], k = k_folds, returnTrain = TRUE)
  ctr1 <- trainControl(method = 'cv', index = fold)
  
  p <- length(predictors)
  tunegrid <- expand.grid(mtry = sort(unique(c(2, floor(p / 2), p))))
  
  
  RF <- train(reformulate(predictors, response = target), data = tr,
                     method = 'rf', ntree = 50, #increase ntree???
                     preProcess = c('zv'),
                     trControl = ctr1, tuneGrid = tunegrid)
  
  RF_models[[target]] <- RF  
  tr_perf <- getTrainPerf(RF)[, c('TrainRMSE', 'TrainRsquared', 'TrainMAE')] 
  
  # Log predict
  
  pred_log <- predict(RF, te)
  te_perf <- defaultSummary(data.frame(obs = te[[target]], pred = pred_log))
  names(te_perf) <- paste0('Test', names(te_perf))
  
  # Original scale
  
  te_orig <- defaultSummary(data.frame(obs = expm1(te[[target]]), pred = expm1(pred_log)))
  names(te_orig) <- paste0('TestOrig', names(te_orig))
  
  performance_RF[[target]] <- cbind(tr_perf, t(te_perf), t(te_orig), mtry = RF$bestTune$mtry)
    }

performance_table_RF <- do.call(rbind, performance_RF)
round(performance_table_RF,3)

### Category addition

#approach (sentence-transformer embeddings + hierarchical clustering)

py_install("sentence-transformers", pip = TRUE)

st <- import("sentence_transformers")
model <- st$SentenceTransformer('all-mpnet-base-v2')

embeddings <- model$encode(df$food)

dist_matrix <- proxy::dist(embeddings, method = "cosine")

hc <- hclust(dist_matrix, method = "average")

cluster_labels <- cutree(hc, h = 0.70)

df$group <- cluster_labels

df$group <- as.factor(df$group)

# Food categories from keywords in the food name --------------------------
# Rules are checked from top to bottom; the first match wins.
df$category <- case_when(
  # 1. Fast food chains
  grepl("mcdonalds|burger king|taco bell|wendys|kfc|subway|pizza hut|dominos|starbucks|dunkin|arbys|dairy queen", df$food) ~ "Fast food",
  
  # 2. Specific cases that would otherwise match a wrong rule
  grepl("applesauce|apple sauce", df$food) ~ "Fruit",
  grepl("butternut", df$food) ~ "Vegetables",
  grepl("goose fat|tallow|shortening|\\blard\\b", df$food) ~ "Fats and oils",
  grepl("arroz con leche|dulce de leche|honey|ladyfinger|sucker|licorice|doughnut|donut", df$food) ~ "Sweets and desserts",
  grepl("peanut butter|almond butter|nut butter|hummus|tahini|tempeh|meatless|vegetarian|meat extender|hanfprotein|lupin|pecan|trail mix|studentenfutter", df$food) ~ "Legumes, nuts and seeds",
  grepl("ice cream|chocolate", df$food) ~ "Sweets and desserts",
  grepl("coconut milk|almond milk|soy milk|oat milk|rice milk", df$food) ~ "Beverages",
  grepl("sesame butter|cashew butter|sunflower butter|acorn", df$food) ~ "Legumes, nuts and seeds",
  grepl("water chestnut|pickled cucumber|pickles", df$food) ~ "Vegetables",
  grepl("dressing", df$food) ~ "Fats and oils",
  grepl("\\btea\\b|juice|lemonade|limeade|coconut water", df$food) ~ "Beverages",
  grepl("pancake|hotcake|bagel", df$food) ~ "Cereals and grains",
  grepl("gravy|sauce", df$food) ~ "Sauces and condiments",
  
  # 3. Soups and mixed dishes
  grepl("soup|broth|consomme|stew|chowder|bisque|gazpacho", df$food) ~ "Soups and stews",
  grepl("lasagna|ravioli|spaghetti|pizza|taco|burrito|tamale|pupusa|tostada|nachos|enchilada|quesadilla|casserole|sandwich|burger|salad|succotash|loaf|arroz|wonton", df$food) ~ "Mixed dishes",
  grepl("hotdog|hot dog|frankfurter|pepperoni", df$food) ~ "Meat",
  
  # 4. Sweets (before dairy and cereals: cheesecake, cinnamon rolls)
  grepl("candy|cookie|cake|\\bpie\\b|sugar|syrup|\\bjam\\b|dessert|pastry|marshmallow|fudge|halavah|praline|mints|gum drop|skittles|caramel|jell|fruit leather|clif bar|granola bar|frosting|fondant|sweetener|splenda|saccharin|aspartame|pudding|brownie|eclair|cinnamon roll|wafer|molasses|grenadine|pectin|tapioca", df$food) ~ "Sweets and desserts",
  
  # 5. Animal products
  grepl("cheese|milk|yogurt|cream|\\bbutter\\b|kefir", df$food) ~ "Dairy",
  grepl("beef|pork|chicken|turkey|lamb|veal|\\bham\\b|bacon|sausage|salami|duck|liver|steak|mutton|chuck|mortadella|bologna|wurst|ostrich|\\bemu\\b|bison|caribou|venison|\\belk\\b|moose|buffalo|rabbit|goose|quail|pheasant|squab|turtle|spleen|tongue|tripe", df$food) ~ "Meat",
  grepl("fish|salmon|tuna|\\bcod\\b|trout|sardine|shrimp|crab|lobster|oyster|clam|mussel|pike|haddock|burbot|pollock|smelt|walleye|carp|snapper|\\broe\\b|anchovy|herring|tilapia|whiting|shad|shark|mullet|cisco|pompano|perch|flounder|mackerel|croaker|scup|sheepshead|bass|halibut|turbot|surimi|sturgeon|\\bspot\\b|\\bdrum\\b|\\bpout\\b|catfish|\\bsole\\b|\\beel\\b|squid|octopus|scallop|caviar|cusk|yellowtail|\\bling\\b|grouper|abalone|whelk|conch", df$food) ~ "Fish and seafood",
  grepl("^eggs?\\b|scrambled egg|egg yolk|egg white|omelet", df$food) ~ "Eggs",
  
  # 6. Sauces and herbs (before legumes and vegetables: soy sauce, nutmeg)
  grepl("sauce|gravy|vinegar|catsup|ketchup|salsa|relish|sofrito|adobo|au jus|mustard", df$food) ~ "Sauces and condiments",
  grepl("thyme|\\bmace\\b|curry|coriander|wasabi|spice|cinnamon|oregano|basil|paprika|nutmeg|clove|cumin|sage|rosemary|\\bdill\\b|parsley|turmeric|saffron|tarragon|marjoram", df$food) ~ "Herbs and spices",
  
  # 7. Plant foods
  grepl("bean|lentil|\\bpeas?\\b|chickpea|tofu|soy|nut|almond|walnut|cashew|seed|sesame", df$food) ~ "Legumes, nuts and seeds",
  grepl("bread|rice|pasta|cereal|\\boats?\\b|oatmeal|wheat|flour|cracker|noodle|bagel|muffin|amaranth|triticale|barley|bulgur|quinoa|millet|buckwheat|couscous|teff|kamut|spelt|sorghum|\\brye\\b|semolina|bran\\b|kelloggs|granola|macaroni|waffle|tortilla|crouton|\\broll|focaccia|arepa|biscuit|pretzel|keikitos|toast|popover", df$food) ~ "Cereals and grains",
  grepl("apple|banana|orange|berr|grape|pear|peach|plum|cherr|mango|melon|juice|kiwi|rowan|tangerine|prune|\\bfigs?\\b|currant|plantain|rose hip|lemon|\\blimes?\\b|abiyuch|persimmon|carambola|starfruit|feijoa|jeijoa|rambutan|apricot|papaya|pineapple|guava|nectarine|grapefruit|lychee|\\bdates?\\b|raisin|sapote|loquat|naranjilla|lulo|sapodilla|pummelo|soursop", df$food) ~ "Fruit",
  grepl("potato|tomato|carrot|onion|lettuce|spinach|broccoli|cabbage|pepper|corn|squash|cattail|chayote|gourd|borage|dandelion|cress|fireweed|greens|kale|celery|cucumber|zucchini|eggplant|asparagus|mushroom|radish|beet|turnip|okra|artichoke|leek|garlic|cauliflower|sprout|fries|jute|romanesco|nopales|rutabaga|pickle|tomatillo|taro|purslane|drumstick leaves|pumpkin|salsify|burdock|sesbania|\\byam\\b|arrowhead|vegetable chips|seaweed", df$food) ~ "Vegetables",
  
  # 8. Fats and beverages
  grepl("\\boils?\\b|margarine|dressing|mayonnaise|ghee", df$food) ~ "Fats and oils",
  grepl("soda|coffee|\\btea\\b|beer|bier|wine|drink|\\bwater\\b|whiskey|tequila|daiquiri|vodka|\\brum\\b|\\bgin\\b|liqueur|cocktail|cola|sprite|ginger ale|espresso|cappuccino|cocoa|gatorade|powerade|energy|throttle|beverage|nectar|horchata|punch|glucerna", df$food) ~ "Beverages",
  
  TRUE ~ "Other"
)
df$category <- as.factor(df$category)
table(df$category)