library(caret)
library(xgboost)


epacorr <- joinedmotions %>%
  right_join(centroid_changes_by_play, by = c("gameId", "playId"))

epacorr <- epacorr %>%
  mutate(
    motion_type = ifelse(is.na(motion_type), "NO MOTION", motion_type)
  )
epaclean <- epacorr %>%
  filter(isDropback == TRUE)

epamotion <- epaclean %>%
  filter(motion_type != "NO MOTION")

epaoverall <- epacorr %>%
  filter(motion_type == "NO MOTION")

modeldata <- epamotion %>%
  select(down, gameId, playId,nflId, possessionTeam, yardsToGo, offenseFormation, receiverAlignment, motion_type, pff_manZone, total_distance, centroid_change) %>%
  filter(!is.na(centroid_change))


modeldata <- modeldata %>%
  mutate(
    down = as.factor(down),
    receiverAlignment = as.factor(receiverAlignment),
    offenseFormation = as.factor(offenseFormation),
    motion_type = as.factor(motion_type),
    pff_manZone = as.factor(pff_manZone)

  )

set.seed(123)

games <- read.csv("nfl-big-data-bowl-2025/games.csv")

modeldata <- games %>%
  left_join(modeldata, by = "gameId")

#weeks 1 through 6 for training
train_data <- modeldata %>%
  filter(week <= 6) %>%
  select(-gameId, -playId, -possessionTeam)

#7 through 9 for training
test_data <- modeldata %>%
  filter(week > 6)

train_data <- train_data %>%
  filter( !is.na(offenseFormation) &
            !is.na(receiverAlignment)  & !is.na(motion_type) & !is.na(down) & !is.na(pff_manZone) & !is.na(yardsToGo) & !is.na(total_distance) 
  )
test_data <- test_data %>%
  filter(!is.na(offenseFormation) &
            !is.na(receiverAlignment)  & !is.na(motion_type) & !is.na(down) & !is.na(pff_manZone) & !is.na(yardsToGo) & !is.na(total_distance) 

  )


train_dummies <- model.matrix(~  pff_manZone + offenseFormation + receiverAlignment + down + motion_type, data = train_data)
test_dummies <- model.matrix(~  pff_manZone +offenseFormation + receiverAlignment + down + motion_type, data = test_data)

train_x <- as.matrix(cbind(train_dummies, train_data %>% select(yardsToGo, total_distance)))
test_x <- as.matrix(cbind(test_dummies, test_data %>% select(yardsToGo, total_distance)))

train_y <- train_data$centroid_change
test_y <- test_data$centroid_change

dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest <- xgb.DMatrix(data = test_x, label = test_y)

# Set parameters for XGBoost
params <- list(
  objective = "reg:squarederror", # Default for regression tasks
  booster = "gbtree", # Default booster
  eta = 0.1,                     # Default learning rate                    # No minimum loss reduction required
  max_depth = 6,                 # Default maximum depth of a tree
  min_child_weight = 2,          # Default minimum sum of instance weight (hessian) needed in a child
  subsample = 1,                 # Default: use all data without subsampling
  colsample_bytree = 1,          # Default: use all features
  lambda = 1,                    # Default L2 regularization term
  alpha = 2                     # Default L1 regularization term
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,          # Increased max rounds since we reduced learning rate
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 20,  # Stop if no improvement for 50 rounds
  print_every_n = 50          # Reduce printing frequency
)


importance_matrix <- xgb.importance(
  feature_names = colnames(train_x),
  model = xgb_model
)
print(importance_matrix)

# Plot feature importance
xgb.plot.importance(importance_matrix[1:10])


predictions <- predict(xgb_model, newdata = dtest)


ggplot(test_data, aes(x = test_y, y = predictions)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Predicted vs Actual Net Change",
    x = "Actual Net Change",
    y = "Predicted Net Change"
  ) +
  theme_minimal()

metrics <- postResample(pred = predictions, obs = test_y)
print(metrics)


testpreds <- cbind(test_data,predictions)

results <- testpreds %>%
  mutate(
    coe = centroid_change - predictions
  )


summary(epamotion$centroid_change)

