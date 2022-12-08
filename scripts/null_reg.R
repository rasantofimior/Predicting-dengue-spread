tictoc::tic("Null Model")
source("../scripts/recipes.R")
source("../scripts/workflows.R")
source("../scripts/tuning.R")

wf <- workflows("null")
result <- wf %>% tuning(
    resamples = validation_split,
    model = "null"
)

cat("--------------------------------\n---------- NULL MODEL ----------\n--------------------------------\n")
result %>%
    collect_metrics() %>%
    arrange(mean) %>%
    print()

# Select best model
best <- select_best(result, metric = "mae")
# Finalize the workflow with those parameter values
# final_wf <- wf %>% finalize_workflow(best)


# Check coefficients
# final_wf %>%
#     fit(validation) %>%
#     tidy() %>%
#     print(n = Inf)

# Save workflow
# saveRDS(final_wf, "../stores/bestwf_null.Rds")

# Save result
saveRDS(result, "../stores/result_null.Rds")

tictoc::toc()
