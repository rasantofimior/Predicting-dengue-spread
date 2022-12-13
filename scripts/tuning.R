library("yardstick")
library("tune")
library("stacks")
library("finetune")

ctrl_grid <- stacks::control_stack_grid()
ctrl_grid_race <- finetune::control_race(save_pred = TRUE, save_workflow = TRUE)

size <- 5

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
                penalty(c(-0.5,-0.0001)),
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
                penalty(c(-1,-0.001)),
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
                penalty(c(-0.01,-0.001)),
                mixture(c(0.8,1)),
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
                mtry(c(2, 4)), # Starting range depends on max_number_of_vars
                min_n(c(25,40)),
                trees(c(60,100)),
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
                mtry(c(1, 40)), # Starting range depends on max_number_of_vars
                min_n(c(10,30)),
                sample_prop(c(0.25,1)),
                tree_depth(c(2,10)),
                learn_rate(c(-3.2,-2.9)),
                loss_reduction(c(-7,-4)),
                trees(c(700,1300)),
                stop_iter(c(10,20)),
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
                epochs(c(600,900)),
                penalty(c(-2,-0.5)),
                learn_rate(c(-1.3,-1)),
                hidden_units(c(8,9)),
                size = size
            ),
            metrics = yardstick::metric_set(mae),
            resamples = resamples,
            control = ctrl_grid
        )
    }

    tune
}
