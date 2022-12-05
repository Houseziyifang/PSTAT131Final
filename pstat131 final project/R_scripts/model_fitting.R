logreg <- logistic_reg(
  mode = "classification", 
  engine = "glmnet", 
  penalty = tune(), mixture = 0)

# our workflow based on our recipe and model
w_log <- workflow() %>%
  add_recipe(rec_obj) %>%
  add_model(logreg)
rf_model <- rand_forest(
  mode = "classification", 
  engine = "ranger",
  trees = 100,
  mtry = tune(), 
  min_n = tune())

w_rf <- workflow() %>%
  add_recipe(rec_obj) %>%
  add_model(rf_model)
bt_tune_model <- boost_tree(mode = "classification", engine = "xgboost", 
                            mtry = 5,
                            min_n = tune(),
                            tree_depth = tune(),
                            learn_rate = tune(),
                            loss_reduction = tune(),
                            sample_size = tune(),
                            trees = 100)
w_tune_bt <- workflow() %>%
  add_recipe(rec_obj) %>%
  add_model(bt_tune_model)
knn_model <- nearest_neighbor(mode = "classification",
                              neighbors = tune(),
                              engine = "kknn")
w_knn <- workflow() %>%
  add_recipe(rec_obj) %>%
  add_model(knn_model)

#metrics we will use for finetuning, for this task I chose sensitivity, specificity and area under the ROC curve 
met <- metric_set(sensitivity, specificity, roc_auc)

# tuning parameters from 0.001 to 1 
grid <- tibble(penalty = 10^seq(-3, 0, length.out = 10))

# traing our model on cv folds
lr_res <- w_log %>% 
  tune_grid(resamples = folds, grid = grid, metrics = met)

autoplot(lr_res)

rf_grid <- expand.grid(
  mtry = c(2, 5, 10), 
  min_n = c(20, 100, 200, 300))

rf_res <- w_rf %>% 
  tune_grid(resamples = folds, grid = rf_grid, metrics = met)

rf_res %>% autoplot()

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  learn_rate(),
  size = 15
)
xgb_res2 <- tune_grid(
  w_tune_bt,
  folds,
  xgb_grid,
  control_grid(save_pred = TRUE)
)
xgb_res2 %>% collect_metrics()

knn_res <- tune_grid(
  w_knn,
  folds,
  grid = tibble(neighbors = c(40,80,150)),
  control_grid(save_pred = TRUE)
)

knn_res %>% collect_metrics()

