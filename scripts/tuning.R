library("yardstick")
library("tune")
library("stacks")

ctrl_grid <- stacks::control_stack_grid()

tuning <- function(object,
                   #    grid,
                   #    params,
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
    if (model == "lm") {
        tune <- tune::tune_grid(
            object = object,
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    if (model == "ridge") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(),
                size = 50
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    if (model == "lasso") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(),
                size = 50
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    if (model == "elastic") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                penalty(),
                mixture(),
                size = 50
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    if (model == "rf") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                mtry(c(1, 70)),
                min_n(),
                trees(),
                size = 50
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }
    if (model == "xgb") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                mtry(c(1, 70)),
                min_n(),
                sample_prop(),
                tree_depth(),
                learn_rate(),
                loss_reduction(),
                trees(),
                stop_iter(),
                size = 20
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }

    if (model == "mlp") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid_latin_hypercube(
                epochs(),
                penalty(),
                learn_rate(),
                hidden_units(),
                size = 20
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }

    tune
}
