# https://www.drivendata.org/competitions/44/dengai-predicting-disease-spread/page/82/
library("tidyverse")
library("tidymodels")
library("factoextra")

df <- readRDS("../stores/df.Rds")
df_final_test <- readRDS("../stores/df_final_test.Rds")

predictors <- df %>%
    select(-c(city, weekofyear, total_cases)) %>%
    names()

# worst_vars <- readRDS("../stores/worst_vars4.Rds")
# worst_vars <- readRDS("../stores/worst_xgb4.Rds")
# estrato <- stringr::str_detect(worst_vars, "estrato")
# worst_vars <- worst_vars[!estrato]

# Create train and test samples
set.seed(10)
data_split <- df %>% initial_split(prop = 0.85)
train <- data_split %>% training()
test <- data_split %>% testing()

# Distuinguish between validation and training sets
train$id <- seq(nrow(train))
set.seed(10)
train_temp <- train %>% dplyr::sample_frac(0.70)
validation <- dplyr::anti_join(train, train_temp, by = "id") %>% select(-id)
train <- train_temp %>% select(-id)

rm(train_temp)


# Recipe to prepare data for regression
rec_maker <- function(data,
                      interact = FALSE,
                      poly = FALSE,
                      percentage = TRUE,
                      pca = FALSE) {
    rec_reg <- recipe(total_cases ~ ., data = data) %>%
        step_rm(year) %>%
        update_role(total_cases, new_role = "outcome") %>%
        update_role(contains(c("city", "week")), new_role = "cat_pred") %>%
        update_role(all_of(!!predictors), new_role = "num_pred") %>%
        step_dummy(
            city,
            weekofyear,
            one_hot = TRUE
        ) %>%
        step_impute_mean(has_role("num_pred")) %>%
        step_normalize(has_role("num_pred"))

    if (interact) {
        rec_reg <- rec_reg %>%
            step_interact(terms = ~ has_role("num_pred"):contains("city"))
    }

    if (poly) {
        rec_reg <- rec_reg %>%
            recipes::step_poly(
                recipes::has_role("num_pred"),
                degree = 2
            )
    }

    if (!percentage) {
        rec_reg <- rec_reg %>%
            recipes::step_rm(contains("pct"))
    }

    if (pca) {
        rec_reg <- rec_reg %>%
            recipes::step_pca(contains("pct"), contains("poly"), contains("_x_"), num_comp = pca)
    }

    rec_reg
}

rec_caller <- function(data,
                       interact = FALSE,
                       poly = FALSE,
                       percentage = TRUE,
                       pca = FALSE) {
    if (pca) {
        rec <- rec_maker(
            data,
            interact = interact,
            poly = poly,
            percentage = percentage
        ) %>%
            recipes::step_rm(city_iq, city_sj, contains("week")) %>%
            recipes::prep() %>%
            recipes::bake(new_data = NULL)

        rec <- bind_cols(rec, data %>% select(city, weekofyear))


        prcomp_ana <- prcomp(~ . - city - weekofyear - total_cases, data = rec)




        num_comp <- factoextra::get_eigenvalue(prcomp_ana) %>%
            filter(eigenvalue > 1) %>%
            nrow()

        return_rec <- rec_maker(
            data,
            interact = interact,
            poly = poly,
            percentage = percentage,
            pca = num_comp
        )
        return(return_rec)
    } else {
        return_rec <- rec_maker(
            data,
            interact = interact,
            poly = poly,
            percentage = percentage
        )
        return(return_rec)
    }
}

set.seed(10)
validation_split <- vfold_cv(validation, v = 5)

rec <- rec_caller(
    validation,
    interact = TRUE,
    poly = TRUE,
    percentage = TRUE,
    pca = TRUE
)

# %>%
# step_rm(all_of(!!worst_vars))
