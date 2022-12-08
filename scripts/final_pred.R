source("../scripts/recipes.R")
final_st <- readRDS("../stores/final_st.Rds")


final_pred <- predict(final_st, df_final_test) %>%
    bind_cols(df_final_test) %>%
    select(.pred, city, year, weekofyear) %>%
    rename(total_cases = .pred)


form <- read_csv("../stores/dengue_features_test.csv")
form <- form %>% mutate(weekofyear = as.factor(weekofyear))
form <- form %>%
    select(city, year, weekofyear) %>%
    left_join(final_pred, by = c("city", "year", "weekofyear"))
form <- form %>% mutate(total_cases = round(total_cases))

write.csv(form, "../stores/predictions.csv", row.names = FALSE)
