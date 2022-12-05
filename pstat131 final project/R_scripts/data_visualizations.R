train_data %>% 
  count(gender) %>% 
  ggplot()+
  aes(gender, n)+
  geom_col()+
  labs(x = "", y  ="")+
  theme_light()+
  coord_flip()
train_data%>% 
  group_by(gender, country) %>% 
  count() %>% 
  ggplot(aes(gender, n, fill = gender)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~country, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Distribution of Gender by country",
    y = "Gender"
  )
train_data %>% 
  count(country) %>% 
  ggplot()+
  aes(country, n)+
  geom_col()+
  labs(x = "", y  ="")+
  theme_light()+
  coord_flip()
ggplot(train_data, aes(x=balance)) + 
  geom_histogram()+ggtitle("Distribution of balance")+xlab("Balance")+ylab("Frequency")
ggplot(train_data, aes(x=balance,fill=gender)) + 
  geom_histogram()+ggtitle("Distribution of balance by gender")+xlab("Balance")+ylab("Frequency")+ 
  facet_wrap(~gender, scales = "free_y")
ggplot(train_data, aes(x=balance,fill=country)) + 
  geom_histogram()+ggtitle("Distribution of balance by country")+xlab("Balance")+ylab("Frequency")+ 
  facet_wrap(~country, scales = "free_y")
ggplot(train_data, aes(reorder(products_number, balance), balance,fill=products_number)) +
  geom_boxplot(varwidth = TRUE) + 
  coord_flip() +
  labs(
    title = "Distribution of balance per product type",
    x = "Products number(type)"
  )
ggplot(train_data, aes(reorder(gender, balance), balance,fill=gender)) +
  geom_boxplot(varwidth = TRUE) + 
  coord_flip() +
  labs(
    title = "Distribution of balance per gender",
    x = "Gender"
  )
ggplot(train_data, aes(reorder(churn, balance), balance,fill=churn)) +
  geom_boxplot(varwidth = TRUE) + 
  coord_flip() +
  labs(
    title = "Distribution of balance per churn",
    x = "Churn"
  )
ggplot(train_data, aes(x=estimated_salary)) + 
  geom_histogram()+ggtitle("Distribution of estimated salary")+xlab("Balance")+ylab("Frequency")
ggplot(train_data, aes(x=estimated_salary,fill=gender)) + 
  geom_histogram()+ggtitle("Distribution of estimated salary by gender")+xlab("Estimated salary")+ylab("Frequency")+ 
  facet_wrap(~gender, scales = "free_y")
ggplot(train_data, aes(x=estimated_salary,fill=country)) + 
  geom_histogram()+ggtitle("Distribution of estimated salary by country")+xlab("Estimated salary")+ylab("Frequency")+ 
  facet_wrap(~country, scales = "free_y")
ggplot(train_data, aes(reorder(products_number,estimated_salary), balance,fill=products_number)) +
  geom_boxplot(varwidth = TRUE) + 
  coord_flip() +
  labs(
    title = "Distribution of estimated salary per product type",
    x = "Products number(type)"
  )
ggplot(train_data, aes(reorder(gender, estimated_salary), estimated_salary,fill=gender)) +
  geom_boxplot(varwidth = TRUE) + 
  coord_flip() +
  labs(
    title = "Distribution of estimated salary per gender",
    x = "Gender"
  )
ggplot(train_data, aes(reorder(churn, estimated_salary), estimated_salary,fill=churn)) +
  geom_boxplot(varwidth = TRUE) + 
  coord_flip() +
  labs(
    title = "Distribution of estimated salary per churn",
    x = "Churn"
  )
train_data %>% 
  ggplot(aes(age,estimated_salary)) +
  geom_point(alpha = 0.1) +
  stat_summary(fun.y=mean, colour="red", geom="line", size = 1)+
  facet_wrap(~products_number, scales = "free") +
  labs(
    title = "Age Vs Estimated salary by product number"
  )

train_data %>% 
  ggplot(aes(age,estimated_salary)) +
  geom_point(alpha = 0.1) +
  stat_summary(fun.y=mean, colour="red", geom="line", size = 1)+
  facet_wrap(~country, scales = "free") +
  labs(
    title = "Age Vs Estimated salary by country"
  )
train_data %>% 
  ggplot(aes(age,estimated_salary)) +
  geom_point(alpha = 0.1) +
  stat_summary(fun.y=mean, colour="red", geom="line", size = 1)+
  facet_wrap(~churn, scales = "free") +
  labs(
    title = "Age Vs Estimated salary by churn"
  )
train_data %>% 
  ggplot(aes(age,balance)) +
  geom_point(alpha = 0.1) +
  stat_summary(fun.y=mean, colour="red", geom="line", size = 1)+
  facet_wrap(~products_number, scales = "free") +
  labs(
    title = "Age Vs balance by product number"
  )
train_data %>% 
  ggplot(aes(age,estimated_salary)) +
  geom_point(alpha = 0.1) +
  stat_summary(fun.y=mean, colour="red", geom="line", size = 1)+
  facet_wrap(~country, scales = "free") +
  labs(
    title = "Age Vs balance by country"
  )

train_data %>% 
  ggplot(aes(age,estimated_salary)) +
  geom_point(alpha = 0.1) +
  stat_summary(fun.y=mean, colour="red", geom="line", size = 1)+
  facet_wrap(~churn, scales = "free") +
  labs(
    title = "Age Vs balance by churn"
  )
train_data %>% 
  mutate(churn = if_else(churn == "1", TRUE, FALSE)) %>% 
  count(country, churn) %>% 
  ggplot()+
  aes(country, n, fill = churn)+
  geom_col(position = "dodge")+
  labs(x = "", y  ="")+
  theme_light()+
  coord_flip()

train_data %>% 
  mutate(churn = if_else(churn == "1", TRUE, FALSE)) %>% 
  count(products_number, churn) %>% 
  ggplot()+
  aes(products_number, n, fill = churn)+
  geom_col(position = "dodge")+
  labs(x = "", y  ="")+
  theme_light()+
  coord_flip()

train_data %>% 
  mutate(churn = if_else(churn == "1", TRUE, FALSE)) %>% 
  count(active_member, churn) %>% 
  ggplot()+
  aes(active_member, n, fill = churn)+
  geom_col(position = "dodge")+
  labs(x = "", y  ="")+
  theme_light()+
  coord_flip()

train_data %>% 
  mutate(churn = if_else(churn == "1", TRUE, FALSE)) %>% 
  count(gender, churn) %>% 
  ggplot()+
  aes(gender, n, fill = churn)+
  geom_col(position = "dodge")+
  labs(x = "", y  ="")+
  theme_light()+
  coord_flip()

train_data %>% 
  mutate(churn = if_else(churn == "1", TRUE, FALSE)) %>% 
  ggplot()+
  aes(age, fill = churn)+
  geom_density(alpha = 0.5)+
  theme_light()+
  labs(x = "Age", y  ="")

train_data %>% 
  mutate(churn = if_else(churn == "1", TRUE, FALSE)) %>% 
  ggplot()+
  aes(balance, churn, fill = churn)+
  geom_boxplot()+
  # facet_wrap(~country)+
  theme_light()+ 
  coord_flip()

train_data %>% 
  mutate(churn = if_else(churn == "1", TRUE, FALSE)) %>% 
  filter(balance != 0) %>%
  ggplot()+
  aes(balance, churn, fill = churn)+
  geom_boxplot()+
  # facet_wrap(~country)+
  theme_light()+ 
  coord_flip()

train_data %>% 
  mutate(churn = if_else(churn == "1", TRUE, FALSE)) %>% 
  filter(balance != 0) %>%
  ggplot()+
  aes(balance, churn, fill = churn)+
  geom_boxplot()+
  facet_wrap(~country)+
  theme_light()+ 
  coord_flip()

train_data %>% 
  mutate(churn = if_else(churn == "1", TRUE, FALSE)) %>% 
  filter(balance != 0) %>%
  ggplot()+
  aes(balance, churn, fill = churn)+
  geom_boxplot()+
  facet_wrap(~gender)+
  theme_light()+ 
  coord_flip()

train_data %>% 
  mutate(churn = if_else(churn == "1", TRUE, FALSE)) %>% 
  mutate(credit_card = if_else(credit_card == "1", "Yes", "No")) %>% 
  count(credit_card, churn) %>% 
  ggplot()+
  aes(credit_card, n, fill = churn)+
  geom_col(position = "fill")+
  labs(y = "percentage", x = "having a credit card")

train_data %>% 
  mutate(churn = if_else(churn == "1", TRUE, FALSE)) %>% 
  ggplot()+
  aes(estimated_salary, churn, fill = churn)+
  geom_boxplot()+
  theme_light()+ 
  coord_flip()

