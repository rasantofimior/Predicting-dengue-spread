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


form <- read_csv("../stores/dengue_features_test.csv")
form <- form %>% mutate(weekofyear = as.factor(weekofyear))
form <- form %>%
  select(city, year, weekofyear) %>%
  left_join(final_pred, by = c("city", "year", "weekofyear"))
form <- form %>% mutate(total_cases = round(total_cases))

write.csv(form, "../stores/predictions.csv", row.names = FALSE)