library("tidymodels")
library("ranger")
library("xgboost")
library("brulee")

specs <- function(model) {
    # Null model
    if (model == "null") {
        spec <- null_model() %>%
            set_engine("parsnip") %>%
            set_mode("regression") %>%
            translate()
    }

    # Linear - Regression
    if (model == "lm") {
        spec <- linear_reg() %>%
            set_engine("lm")
    }

    # Elastic - Regression
    if (model == "elastic") {
        spec <- linear_reg(
            penalty = tune(),
            mixture = tune()
        ) %>%
            set_engine("glmnet")
    }

    # Ridge - Regression
    if (model == "ridge") {
        spec <- linear_reg(
            penalty = tune(),
            mixture = 0
        ) %>%
            set_engine("glmnet")
    }

    # Lasso - Regression
    if (model == "lasso") {
        spec <- linear_reg(
            penalty = tune(),
            mixture = 1
        ) %>%
            set_engine("glmnet")
    }


    # RF - Regression
    if (model == "rf") {
        spec <- rand_forest(
            trees = tune(),
            mtry = tune(),
            min_n = tune(),
        ) %>%
            set_engine(
                "ranger",
                importance = "impurity",
                verbose = TRUE,
                num.threads = 3
            ) %>%
            set_mode("regression")
    }

    # XGB - Regression
    if (model == "xgb") {
        spec <- boost_tree(
            trees = tune(),
            mtry = tune(),
            min_n = tune(),
            sample_size = tune(),
            stop_iter = tune(),
            tree_depth = tune(),
            learn_rate = tune(),
            loss_reduction = tune()
        ) %>%
            set_engine("xgboost", nthread = 3) %>%
            set_mode("regression")
    }

    # MLP - Regression
    if (model == "mlp") {
        spec <- mlp(
            epochs = tune(),
            hidden_units = tune(),
            # activation = "linear",
            penalty = tune(),
            learn_rate = tune()
            # ,
            # mixture = tune(),
            # stop_iter = tune()
        ) %>%
            set_engine("brulee") %>%
            set_mode("regression") %>%
            translate()
    }

    # if (model == "mlp") {
    #     spec <- brulee_mlp(
    #         x = rec_reg,
    #         data = validation,
    #         epochs = tune(),
    #         hidden_units = c(16, 8),
    #         activation = c("relu", "linear"),
    #         penalty = tune(),
    #         learn_rate = tune(),
    #         mixture = tune(),
    #         dropout = tune(),
    #         stop_iter = tune(),
    #         verbose = TRUE
    #     )
    # }


    spec
}
