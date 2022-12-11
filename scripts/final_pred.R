source("../scripts/recipes.R")
final_st <- readRDS("../stores/final_st.Rds")


# final_pred <- predict(final_st, test) %>%
#     bind_cols(test) %>%
#     select(city, year, weekofyear,.pred, total_cases) 
# #%>%
# #     rename(total_cases = .pred)
# 
# final_pred <- final_pred %>% mutate(.pred = round(.pred))
# colnames(final_pred)[colnames(final_pred) == ".pred"] = "Predicciones"
# final_pred <- final_pred %>% mutate(weekofyear = as.factor(weekofyear))
# write.csv(final_pred, "../stores/predictions_final.csv", row.names = FALSE)

final_pred <- predict(final_st, df_final_test) %>%
  bind_cols(df_final_test) %>%
  select(.pred, city, year, weekofyear) %>%
  rename(total_cases = .pred) 

#k <- final_pred %>% group_by(city, year, weekofyear) %>% summarise_each(funs(mean, sd))

sample.mean <- mean(final_pred$total_cases)
sample.n <- length(final_pred$total_cases)
sample.sd <- sd(final_pred$total_cases)
sample.se <- sample.sd/sqrt(sample.n)

alpha = 0.05
degrees.freedom = sample.n - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
margin.error <- t.score * sample.se
lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error
print(c(lower.bound,upper.bound))

final_pred$media <- mean(final_pred$total_cases)
final_pred$sd <- sd(final_pred$total_cases)
final_pred$lower_bound <- final_pred$total_cases - margin.error
final_pred$upper_bound <- final_pred$total_cases + margin.error




form <- read_csv("../stores/dengue_features_test.csv")
form <- form %>% mutate(weekofyear = as.factor(weekofyear))
form <- form %>%
  select(city, year, weekofyear) %>%
  left_join(final_pred, by = c("city", "year", "weekofyear"))
form <- form %>% mutate(total_cases = round(total_cases))

write.csv(form, "../stores/predictions.csv", row.names = FALSE)