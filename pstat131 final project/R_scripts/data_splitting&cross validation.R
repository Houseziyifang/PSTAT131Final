train_data$tenurre<-NULL
set.seed(421)
recipe(churn ~ ., train_data) %>%
  # encode factors with one hot encoder
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
  # normalize all predictor
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  # upscale churn variable with smote algorithm 
  step_smote(all_outcomes()) -> rec_obj
folds <- vfold_cv(train_data, v = 5)
