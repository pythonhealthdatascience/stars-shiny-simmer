source("model.R")
source("output_analysis.R")

run_simulation <- function(){
  # Run the model function with Shiny inputs
  # 1st create the experiment using Shiny 
  exp = create_experiment(log_level=0)
  
  # run multiple replications of the model...
  df_model_reps = multiple_replications(exp = exp,
                                        n_reps=5,
                                        random_seed=42)
  
  # return a replications table
  results <- replication_results_table(df_model_reps, exp,
                                       DEFAULT_RESULTS_COLLECTION_PERIOD)
  
  return(results)
  
}

replications_table <- run_simulation()
print(head(replications_table))

#—— CREATE SUMMARY TABLE ——#
summary_table <- create_summary_table(replications_table)

print(summary_table)



