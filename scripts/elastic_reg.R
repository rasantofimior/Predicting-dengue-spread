tictoc::tic("Elastic Net")
source("../scripts/recipes.R")
source("../scripts/workflows.R")
source("../scripts/tuning.R")

wf <- workflows("elastic")

cl <- parallel::makeCluster(3)
result <- wf %>% tuning(
    resamples = validation_split,
    model = "elastic"
)
parallel::stopCluster(cl)

cat("-----------------------------\n---------- ELASTIC ----------\n-----------------------------\n")
result %>%
    collect_metrics() %>%
    arrange(mean) %>%
    print()

# Select best model
best <- select_best(result, metric = "mae")
# Finalize the workflow with those parameter values
final_wf <- wf %>% finalize_workflow(best)

# Check coefficients
# final_wf %>%
#     fit(validation) %>%
#     tidy() %>%
#     arrange(desc(estimate)) %>%
#     print(n = Inf)

# Save workflow
saveRDS(final_wf, "../stores/bestwf_elastic.Rds")

# Save result
saveRDS(result, "../stores/result_elastic.Rds")

tictoc::toc()
