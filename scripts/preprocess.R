library("tidyverse")

feats <- read_csv("../stores/dengue_features_train.csv")
labs <- read_csv("../stores/dengue_labels_train.csv")
df_final_test <- read_csv("../stores/dengue_features_test.csv")

df_train <- feats %>% left_join(labs, by = c("city", "year", "weekofyear"))

data_prepare <- function(df, train = TRUE) {
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
        if (train) {
            df_pct_ch <- df_ready %>%
                select(-c(total_cases, city, weekofyear, week_start_date, year))
        } else {
            df_pct_ch <- df_ready %>%
                select(-c(city, weekofyear, week_start_date, year))
        }
        df_pct_ch <- df_pct_ch %>%
            mutate(
                .keep = "none",
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

    df_sj <- df_sj %>% bind_cols(df_sj_pct_ch)

    df_iq <- df_iq %>% bind_cols(df_iq_pct_ch)

    processed_df <- bind_rows(df_sj, df_iq) %>%
        select(-c(week_start_date)) %>%
        mutate(
            across(
                !c(
                    city,
                    weekofyear,
                    year
                ),
                as.numeric
            )
        )

    processed_df
}

df <- data_prepare(df_train)
df_final_test <- data_prepare(df_final_test, train = FALSE)

saveRDS(df, "../stores/df.Rds")
saveRDS(df_final_test, "../stores/df_final_test.Rds")
