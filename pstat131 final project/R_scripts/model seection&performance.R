logreg <- logistic_reg(
  mode = "classification", 
  engine = "glmnet", 
  penalty = 0.01, mixture = 0)

w_log <- workflow() %>%
  add_recipe(rec_obj) %>%
  add_model(logreg)

final_log_model <- w_log %>% 
  fit(train_data)

rf_model <- rand_forest(
  mode = "classification", 
  engine = "ranger",
  trees = 100,
  mtry = 5, 
  min_n = 200)

w_rf <- workflow() %>%
  add_recipe(rec_obj) %>%
  add_model(rf_model)

final_rf_model <- w_rf %>% 
  fit(train_data)

bt_model <- boost_tree(mode = "classification", engine = "xgboost", 
                       trees = 100,
                       mtry = 10,
                       learn_rate = 0.174,
                       tree_depth = 11,
                       loss_reduction = 26.8,
                       sample_size = 0.949,
                       min_n = 3)
w_bt <- workflow() %>%
  add_recipe(rec_obj) %>%
  add_model(bt_model)

final_bt_model <- w_bt %>% 
  fit(train_data)

knn_model <- nearest_neighbor(mode = "classification",
                              neighbors = 150,
                              engine = "kknn")
w_knn <- workflow() %>%
  add_recipe(rec_obj) %>%
  add_model(knn_model)


final_knn_model <- w_knn %>% 
  fit(train_data)


pred <- augment(final_log_model, test_data)
test_data %>%
  mutate(prediction = pred$.pred_class) %>% 
  conf_mat(churn, prediction) %>% 
  autoplot(type = "heatmap")

TP<-1220
FP<-115
TN<-293
FN<-373
accuracy<-(TP+TN)/(TP+FP+TN+FN)
round(accuracy,4)


pred <- augment(final_rf_model, test_data)
test_data %>%
  mutate(prediction = pred$.pred_class) %>% 
  conf_mat(churn, prediction) %>% 
  autoplot(type = "heatmap")

TP<-1371
FP<-132
TN<-276
FN<-222
accuracy<-(TP+TN)/(TP+FP+TN+FN)
round(accuracy,4)

pred <- augment(final_bt_model, test_data)
test_data %>%
  mutate(prediction = pred$.pred_class) %>% 
  conf_mat(churn, prediction) %>% 
  autoplot(type = "heatmap")

TP<-1486
FP<-190
TN<-218
FN<-107
accuracy<-(TP+TN)/(TP+FP+TN+FN)
round(accuracy,4)


pred <- augment(final_knn_model, test_data)
test_data %>%
  mutate(prediction = pred$.pred_class) %>% 
  conf_mat(churn, prediction) %>% 
  autoplot(type = "heatmap")

TP<-1225
FP<-112
TN<-296
FN<-386
accuracy<-(TP+TN)/(TP+FP+TN+FN)
round(accuracy,4)

