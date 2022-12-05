data <- read.csv("Bank Customer Churn Prediction.csv")
str(data)
data %>% skimr::skim() %>% 
  skimr::yank("numeric") %>% as_tibble() %>% 
  select(-p25, -p75) %>% knitr::kable()
data %>% skimr::skim() %>% skimr::yank("character")
data %>% 
  select(-customer_id) %>% 
  mutate(products_number = as.factor(products_number)) %>%
  mutate(credit_card = as.factor(credit_card)) %>% 
  mutate(active_member = as.factor(active_member)) %>% 
  mutate(churn = as.factor(churn)) ->  data
set.seed(420)
data_split <- data %>% 
  initial_split(prop = 0.8, strata = "churn")

train_data <- training(data_split)
test_data <- testing(data_split)

dim(train_data)
dim(test_data)