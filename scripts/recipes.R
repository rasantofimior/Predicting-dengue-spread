# https://www.drivendata.org/competitions/44/dengai-predicting-disease-spread/page/82/

library("tidyverse")
library("tidymodels")
library("factoextra")

feats <- read_csv("../stores/dengue_features_train.csv")
labs <- read_csv("../stores/dengue_labels_train.csv")

df_import <- feats %>% left_join(labs, by = c("city", "year", "weekofyear"))

data_prepare <- function(df) {
    df <- df %>% select(-precipitation_amt_mm)
    df <- df %>% mutate(
        across(
            c(
                city,
                weekofyear
            ),
            as.factor
        )
    )

    df_iq <- df %>%
        filter(city == "iq") %>%
        arrange(year, weekofyear)
    df_sj <- df %>%
        filter(city == "sj") %>%
        arrange(year, weekofyear)

    pct_ch_1 <- function(x) {
        x / lag(x) - 1
    }
    pct_ch_2 <- function(x) {
        x / lag(x, n = 2) - 1
    }
    pct_ch_3 <- function(x) {
        x / lag(x, n = 3) - 1
    }
    pct_ch_4 <- function(x) {
        x / lag(x, n = 4) - 1
    }

    var_creator <- function(df_ready) {
        df_pct_ch <- df_ready %>%
            select(-c(total_cases, city, weekofyear, week_start_date, year)) %>%
            mutate(
                across(
                    everything(),
                    .fns = list(
                        pct_ch_1 = pct_ch_1,
                        pct_ch_2 = pct_ch_2,
                        pct_ch_3 = pct_ch_3,
                        pct_ch_4 = pct_ch_4
                    ),
                    .names = "{.col}_{.fn}"
                )
            ) %>%
            na_if(Inf)
        df_pct_ch
    }

    df_sj_pct_ch <- var_creator(df_sj)
    df_iq_pct_ch <- var_creator(df_iq)

    df_sj <- df_sj %>%
        select(c(total_cases, city, weekofyear, week_start_date, year)) %>%
        cbind(df_sj_pct_ch) %>%
        as_tibble()

    df_iq <- df_iq %>%
        select(c(total_cases, city, weekofyear, week_start_date, year)) %>%
        bind_cols(df_iq_pct_ch)

    processed_df <- bind_rows(df_sj, df_iq) %>%
        select(-c(week_start_date, year))

    processed_df
}

df <- data_prepare(df_import)



# outliers <- which(abs(df$price) > 3)
# df <- df[-outliers]


# # Selector de muestra!!!
# set.seed(10)
# df <- df %>%
#     slice_sample(prop = 0.5)


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


set.seed(10)
validation_split <- vfold_cv(train, v = 5)

# Recipe to prepare data for regression
rec_maker <- function(data,
                      interact = FALSE,
                      poly = FALSE,
                      percentage = TRUE,
                      pca = FALSE) {
    rec_reg <- recipe(total_cases ~ ., data = data) %>%
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
            step_poly(
                has_role("num_pred"),
                degree = 2
            )
    }

    if (!percentage) {
        rec_reg <- rec_reg %>%
            step_rm(contains("pct"))
    }

    if (pca) {
        rec_reg <- rec_reg %>%
            step_pca(contains("pct"), contains("poly"), contains("_x_"), num_comp = pca)
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
            step_rm(city_iq, city_sj, contains("week")) %>%
            prep() %>%
            bake(new_data = NULL)

        rec <- bind_cols(rec, data %>% select(city, weekofyear))

        prcomp_ana <- prcomp(~ . - city - weekofyear - total_cases, data = rec)

        num_comp <- get_eigenvalue(prcomp_ana) %>%
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

rec <- rec_caller(
    validation,
    interact = TRUE,
    poly = TRUE,
    percentage = TRUE,
    pca = TRUE
)



# %>%
# step_rm(all_of(!!worst_vars))
