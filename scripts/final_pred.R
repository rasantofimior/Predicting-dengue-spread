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



 df_confidence <- final_pred %>%  group_by(city, year) %>% 
  summarise(mean = mean(total_cases), sample.n = length(total_cases), sample.sd = sd(total_cases))
 df_confidence$sample.se <-  (df_confidence$sample.sd/sqrt(df_confidence$sample.n))

alpha = 0.05
df_confidence$degrees.freedom =  df_confidence$sample.n - 1
df_confidence$t.score = qt(p=alpha/2, df= df_confidence$degrees.freedom,lower.tail=F)
df_confidence$margin.error <-  df_confidence$t.score *  df_confidence$sample.se
df_confidence$lower.bound <-  df_confidence$mean -  df_confidence$margin.error
df_confidence$upper.bound <-  df_confidence$mean +  df_confidence$margin.error
print(c(lower.bound,upper.bound))
write.csv(df_confidence, "../stores/df_confidence_group.csv", row.names = FALSE)



form <- read_csv("../stores/dengue_features_test.csv")
form <- form %>% mutate(weekofyear = as.factor(weekofyear))
form <- form %>%
  select(city, year, weekofyear) %>%
  left_join(final_pred, by = c("city", "year", "weekofyear"))
form <- form %>% mutate(total_cases = round(total_cases))

write.csv(form, "../stores/predictions.csv", row.names = FALSE)