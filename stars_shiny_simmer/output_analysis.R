library(simmer)
library(simmer.bricks)
library(tibble)
library(ggplot2)
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

options(dplyr.summarise.inform = FALSE)

## 9. Results analysis

get_resource_counts <- function(exp) {
  resource = c("triage_bay", 
               "registration_clerk", 
               "examination_room",
               "trauma_room",
               "trauma_treat_cubicle",
               "nontrauma_treat_cubicle")
  
  resource_counts = c(exp$n_triage_bays,
                      exp$n_reg_clerks,
                      exp$n_exam_rooms,
                      exp$n_trauma_rooms,
                      exp$n_trauma_cubicles,
                      exp$n_non_trauma_cubicles)
  
  df_resource <- data.frame(resource)
  df_resource$count <- resource_counts
  return(df_resource)
}


# assumes df is monitored arrivals
waiting_time <- function(df){
  df$waiting_time <-df$end_time - df$start_time - df$activity_time  
  return(df)
}

resource_waiting_times_by_replication <- function(reps) {
  # - WAITING TIMES FOR RESOURCES - #
  
  cols <- c("resource", "replication")
  waiting_times_wide <- get_mon_arrivals(reps, per_resource=TRUE) %>%
    # waiting time = end time - start time - activity time
    waiting_time() %>% 
    # mean waiting time in each replication
    group_by(across(all_of(cols))) %>%
    # mean for each replication
    summarise(rep_waiting_time=mean(waiting_time)) %>% 
    # recode kpi names
    mutate(resource=recode(resource,
                           'triage_bay'='01a_triage_wait',
                           'registration_clerk'='02a_registration_wait',
                           'examination_room'='03a_examination_wait',
                           'nontrauma_treat_cubicle'='04a_treatment_wait(non_trauma)',
                           'trauma_room'='06a_stabilisation_wait',
                           'trauma_treat_cubicle'='07a_treatment_wait(trauma)')) %>%
    # organise
    arrange(resource) %>% 
    # long to wide format ...
    spread(resource, rep_waiting_time)
  
  return(waiting_times_wide)
}


### 9.2. Resource utilisation KPIs

get_resource_counts <- function(exp) {
  resource = c("triage_bay", 
               "registration_clerk", 
               "examination_room",
               "trauma_room",
               "trauma_treat_cubicle",
               "nontrauma_treat_cubicle")
  
  resource_counts = c(exp$n_triage_bays,
                      exp$n_reg_clerks,
                      exp$n_exam_rooms,
                      exp$n_trauma_rooms,
                      exp$n_trauma_cubicles,
                      exp$n_non_trauma_cubicles)
  
  df_resource <- data.frame(resource)
  df_resource$count <- resource_counts
  return(df_resource)
}


# simple calculation of total busy time / total scheduled resource time.
resource_utilisation <- function(df, scheduled_time){
  df$util = df$in_use / (scheduled_time * df$count)  
  return(df)
}

# calculate resource utilisation and return table (rows = reps and cols = resources)
resource_utilisation_by_replication <- function(reps, exp, results_collection_period){
  
  # get results dataframe broken down by resource and replication.
  cols <- c("resource", "replication")
  
  # utilisation calculation:
  # simple calculation of total busy time / total scheduled resource time.
  # where total scheduled time = n_resource * results collection period.
  util_wide <- get_mon_arrivals(reps, per_resource=TRUE) %>%
    # total activity time in each replication per resource (long format)
    group_by(across(all_of(cols))) %>%
    summarise(in_use=sum(activity_time)) %>% 
    # merge with the number of resources available
    merge(get_resource_counts(exp), by="resource", all=TRUE) %>% 
    # calculate the utilisation using scheduled resource availability
    resource_utilisation(results_collection_period) %>% 
    # drop total activity time and count of resources
    subset(select = c(replication, resource, util)) %>% 
    # recode names
    mutate(resource=recode(resource,
                           'triage_bay'='01b_triage_util',
                           'registration_clerk'='02b_registration_util',
                           'examination_room'='03b_examination_util',
                           'nontrauma_treat_cubicle'='04b_treatment_util(non_trauma)',
                           'trauma_room'='06b_stabilisation_util',
                           'trauma_treat_cubicle'='07b_treatment_util(trauma)')) %>%
    arrange(resource) %>% 
    # long to wide format...
    spread(resource, util)
  
  return(util_wide)
}


### 9.3. Patient arrival numbers output

# number of arrivals in each replication
arrivals_by_replication <- function(envs){
  results <- vector()
  for(env in envs){
    results <- c(results, get_n_generated(env, "Patient"))
  }
  
  results <- data.frame(replication = c(1:length(results)), 
                        arrivals = results)
  colnames(results) <- c("replication", "00_arrivals")
  return(results)
}


### 9.4 System level KPIs
# mean time in the system and throughput
system_kpi_for_rep_i <- function(reps, rep_i){
  
  # get attributes
  att <- get_mon_attributes(reps)
  
  # for speed - limit to replication number.
  data_wide <- subset(att[att$replication == rep_i,], select = c(name, key, value)) %>% 
    spread(key, value)
  
  # Patient type 1: trauma
  # take the mean and ignore patients still in pathway
  mean_time_1 = mean(data_wide[data_wide$patient_type == 1,]$total_time, na.rm = TRUE)
  
  # Patient type 2: non_trauma
  # take the mean and ignore patients still in pathway
  mean_time_2 = mean(data_wide[data_wide$patient_type == 2,]$total_time, na.rm = TRUE)
  
  # Throughput - discharges during opening hours.
  throughput <- sum(data_wide$departed, na.rm=TRUE)
  
  # store and return data.frame of results
  rep_results <- data.frame("replication" = rep_i,
                            "05_total_time(non-trauma)" = mean_time_2,
                            "08_total_time(trauma)" = mean_time_1, 
                            "09_throughput"= throughput)
  
  colnames(rep_results) = c("replication",
                            "05_total_time(non-trauma)",
                            "08_total_time(trauma)", 
                            "09_throughput")
  return(rep_results)
}


system_kpi_by_replication <- function(reps){
  # calcs total time by patient type and total throughput
  
  # empty dataframe for attribute calculations.
  att_results <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(att_results) <- c("replication", 
                             "05_total_time(non-trauma)", 
                             "08_total_time(trauma)", 
                             "_09_throughput")
  
  # add each rep separately as this works faster with pivot
  for(rep_i in 1:length(reps)){
    att_results <- rbind(att_results, system_kpi_for_rep_i(reps, rep_i))
  }
  
  # return the KPIs by replications
  return(att_results)
}


### 9.5. Function to create the replications table


replication_results_table <- function(reps, exp, results_collection_period){
  # generate and merge all results tables on the replication column
  results_table <- arrivals_by_replication(reps) %>% 
    merge(resource_waiting_times_by_replication(reps), by="replication", all=TRUE) %>% 
    merge(resource_utilisation_by_replication(reps, exp,
                                              results_collection_period),
           by="replication", all=TRUE) %>% 
    merge(system_kpi_by_replication(reps), by="replication", all=TRUE) %>% 
    # sort by column names to get "replication" followed by ordered 00_, 01a, 01b and so on...
    select(replication, sort(tidyselect::peek_vars()))
  
  results_table 
}


### 9.6 Histogram of replications

histogram_of_replications <- function(rep_table, column_name, unit_label, n_bins=10){
  
  # Divide the x range for selected column into n_bins
  binwidth <- diff(range(select(rep_table, all_of(column_name))))/n_bins
  
  g <- ggplot(rep_table, aes(.data[[column_name]])) +
    geom_histogram(binwidth = binwidth, fill="steelblue", colour = "black") + 
    xlab(paste(column_name, " (", unit_label, ")")) + 
    ylab("Replications")
  
  return(g)
}


### 9.7 Results summary table

# modified summary table function
# modified to remove passing the Experiment - not needed.
create_summary_table <- function(rep_table, dp=2){
  # mean of all columns, but ignore rep number
  mean_values <- data.frame(colMeans(rep_table[c(2:length(rep_table))]))
  colnames(mean_values) <- c("mean")
  return(round(mean_values, dp))
  
}
