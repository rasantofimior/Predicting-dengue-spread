tictoc::tic("Neural Network")
source("../scripts/recipes.R")
source("../scripts/workflows.R")
source("../scripts/tuning.R")

wf <- workflows("mlp")
cl <- parallel::makeCluster(3)
result <- wf %>% tuning(
    resamples = validation_split,
    model = "mlp"
)
parallel::stopCluster(cl)
cat("-------------------------\n---------- MLP ----------\n-------------------------\n")
result %>%
    collect_metrics() %>%
    arrange(mean) %>%
    print()

# Select best model
best <- select_best(result, metric = "mae")
# Finalize the workflow with those parameter values
final_wf <- wf %>% finalize_workflow(best)


# Save workflow
saveRDS(final_wf, "../stores/bestwf_mlp.Rds")

# Save result
saveRDS(result, "../stores/result_mlp.Rds")

tictoc::toc()
