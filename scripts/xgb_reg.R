tictoc::tic("XGBoost")
source("../scripts/recipes.R")
source("../scripts/workflows.R")
source("../scripts/tuning.R")

wf <- workflows("xgb")

cl <- parallel::makeCluster(3)
result <- wf %>% tuning(
    resamples = validation_split,
    model = "xgb"
)
parallel::stopCluster(cl)

cat("-------------------------\n---------- XGB ----------\n-------------------------\n")
result %>%
    collect_metrics() %>%
    arrange(mean) %>%
    print()

# Select best model
best <- select_best(result, metric = "mae")
# Finalize the workflow with those parameter values
final_wf <- wf %>% finalize_workflow(best)

# See variables arranged by importance
# final_wf %>%
#     fit(train) %>%
#     extract_fit_parsnip() %>%
#     vip::vi() %>%
#     dplyr::arrange(desc(Importance)) %>%
#     print(n = Inf)

# Save workflow
saveRDS(final_wf, "../stores/bestwf_xgb.Rds")

# Save result
saveRDS(result, "../stores/result_xgb.Rds")

tictoc::toc()
