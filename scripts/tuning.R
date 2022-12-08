library("yardstick")
library("tune")
library("stacks")
library("finetune")

ctrl_grid <- stacks::control_stack_grid()
ctrl_grid_race <- finetune::control_race(save_pred = TRUE, save_workflow = TRUE)

size <- 3

# This variable assumes that recipes.R has been called before by the model
# runner (e.g.: lasso_reg.R). It is meant to establish the max number of
# variables in the first iteration when tuning tree models
max_number_of_vars <- rec %>%
    prep() %>%
    bake(new_data = NULL) %>%
    ncol()

tuning <- function(object,
                   resamples,
                   model,
                   ...) {
    if (model == "null") {
        tune <- tune::tune_grid(
            object = object,
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }

    if (model == "lasso") {
        # tune <- finetune::tune_race_anova(
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(),
                size = size
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid_race
        )
    }

    if (model == "ridge") {
        # tune <- finetune::tune_race_anova(
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(),
                size = size
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }

    if (model == "elastic") {
        # tune <- finetune::tune_race_anova(
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(),
                mixture(),
                size = size
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    if (model == "rf") {
        # tune <- finetune::tune_race_anova(
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                mtry(c(1, max_number_of_vars)), # Starting range depends on max_number_of_vars
                min_n(),
                trees(),
                size = size
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    if (model == "xgb") {
        # tune <- finetune::tune_race_anova(
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                mtry(c(1, max_number_of_vars)), # Starting range depends on max_number_of_vars
                min_n(),
                sample_prop(),
                tree_depth(),
                learn_rate(),
                loss_reduction(),
                trees(),
                stop_iter(),
                size = size
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }

    if (model == "mlp") {
        # tune <- finetune::tune_race_anova(
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                epochs(),
                penalty(),
                learn_rate(),
                hidden_units(),
                size = size
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }

    tune
}
