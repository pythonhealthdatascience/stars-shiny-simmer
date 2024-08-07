library(simmer)
library(simmer.bricks)
library(tibble)
library(ggplot2)
suppressMessages(library(RCurl))
suppressMessages(library(Rlab))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

options(dplyr.summarise.inform = FALSE)


## 2. Default values and constants

### 2.1 Distribution parameters

#' Mean and Variance of the underlying Normal Distribution
#' 
#' @description
#' `normal_moments_from_lognormal` calculates the mu and sigma
#' of the normal distribution underlying a lognormal
#' mean and standard 
#'
#' @details
#' `rlnorm` from `stats` is designed to sample from the lognormal distribution. 
#' The parameters is expects are moments of the underlying normal distribution
#' Using sample mean and standard deviation this function calculates 
#' the mu and sigma of the normal distribution. source: https://blogs.sas.com/content/iml/2014/06/04/simulate-lognormal-data-with-specified-mean-and-variance.html
#' 
#' @param mean A number. Sample mean.
#' @param stdev A number. Sample standard deviation
#' @returns A list 
normal_moments_from_lognormal <- function(mean, std){
  phi <- sqrt(std^2 + mean^2)
  mu <- log(mean**2/phi)
  sigma <- sqrt(log(phi^2/mean^2))
  return(list("mu" = mu, "sigma" = sigma))
}


# sign-in/triage parameters
DEFAULT_TRIAGE_MEAN <- 3.0

# registration parameters (lognormal distribution)
DEFAULT_REG_PARAMS <- normal_moments_from_lognormal(5.0, sqrt(2.0))

# examination parameters
DEFAULT_EXAM_PARAMS = list(mean=16.0, var=3.0)

# trauma/stabilisation
DEFAULT_TRAUMA_MEAN <- 90.0

# Trauma treatment (lognormal distribution)
DEFAULT_TRAUMA_TREATMENT_PARAMS <- normal_moments_from_lognormal(30.0, sqrt(4.0))

# Non trauma treatment (lognormal distribution)
DEFAULT_NON_TRAUMA_TREATMENT_PARAMS <- normal_moments_from_lognormal(13.3, sqrt(2.0))

# prob patient requires treatment given trauma
DEFAULT_NON_TRAUMA_TREAT_P <- 0.60

# proportion of patients triaged as trauma
DEFAULT_PROB_TRAUMA <- 0.12

### 2.2 Time dependent arrival rate data

# data are held in the Github repo and loaded from there.
NSPP_PATH = 'https://raw.githubusercontent.com/TomMonks/open-science-for-sim/main/src/notebooks/01_foss_sim/data/ed_arrivals.csv'

# csv_data <- getURL(NSPP_PATH)
# df <- read.csv(text=csv_data)
# 
# # lock in order of time of day for bar chart display
# df$period <- factor(df$period, levels = df$period)
# 
# ggplot(data=df, aes(x=period, y=arrival_rate)) +
#   geom_bar(stat="identity", fill="steelblue") + 
#   theme(axis.text.x = element_text(angle = 90, 
#                                    vjust = 0.5, 
#                                    hjust=1)) +
#   xlab("Hour of day") + 
#   ylab("Mean arrivals (patients/hr)")

### 2.3 Resource Counts
DEFAULT_N_TRIAGE <- 1
DEFAULT_N_REG <- 1
DEFAULT_N_EXAM <- 3

# stabilisation rooms
DEFAULT_N_TRAUMA <- 1

# Non-trauma cubicles
DEFAULT_NON_TRAUMA_CUBICLES <- 1

# trauma pathway cubicles
DEFAULT_TRAUMA_CUBICLES <- 1

### 2.4 Simulation model run settings

# Random seed - this will be investigated for CRN
SEED <- 42

# default results collection period
DEFAULT_RESULTS_COLLECTION_PERIOD <- 60 * 19

# number of replications.
DEFAULT_N_REPS <- 5

# Show the a trace of simulated events
# 1 = show, 0 = do not show.
LOG_LEVEL <- 1

## 3. Functions

load_arrival_data <- function(path=NSPP_PATH){
  csv_data <- getURL(NSPP_PATH)
  df <- read.csv(text=csv_data)
  
  # arrivals per minute...
  df$arrival_rate2 <- df$arrival_rate/60.0
  
  # create 60 minute increments for period
  df$period = seq(0, (nrow(df)-1)*60, by=60)
  return(df)
}

#' Sample a patient type
#' 
#' @description
#' `sample_arrival_type` samples if a patient type is trauma or non-trauma
#' with a given probability.
#'
#' @details
#' The function uses the Bernouli distribution (Rlab) to sample
#' if a patient is Trauma or Non-Trauma.  The return values are 
#' 1 = Trauma, 2 = Non-trauma.
#' @param p A number: the probability a patient has trauma on arrival
sample_arrival_type <- function(p, n=1){
  ifelse(rbern(n, prob = DEFAULT_PROB_TRAUMA) == 1, 1, 2)
}

#' Sample a if a non-trauma patient requires treatment
#' 
#' @description
#' `sample_nt_trauma_treatment` samples if a non-trauma patient
#' requires cubicle treatment
#'
#' @details
#' The function uses the Bernouli distribution (Rlab) to sample
#' if a patient is requires treatment or not.  The return values are 
#' 1 = Treatment, 0 = No treatment
#' @param p A number: The probability the patient requires treatment
sample_nt_trauma_treatment <- function(p){
  ifelse(rbern(1, prob = p) == 1, 1, 0)
}

nspp_thinning <- function(simulation_time, data, debug=FALSE){
  
  # calc time interval: assumes intervals are of equal length
  interval <- data$period[2] - data$period[1]
  
  # maximum arrival rate (smallest time between arrivals)
  lambda_max <- max(data$arrival_rate2)
  
  while(TRUE){
    # get time bucket (row of dataframe to use)
    t <- floor(simulation_time / interval) %% nrow(data) + 1
    lambda_t <- data$arrival_rate2[t]
    
    # set to a large number so that at least 1 sample is taken
    u <- Inf
    rejects <- -1
    
    # running total of time until next arrival
    inter_arrival_time <- 0.0
    
    # reject proportionate to lambda_t / lambda_max
    ratio <- lambda_t / lambda_max
    while(u >= ratio){
      rejects <- rejects + 1
      # sample using max arrival rate
      inter_arrival_time <- inter_arrival_time + rexp(1, lambda_max)
      u <- runif(1, 0.0, 1.0)
    }
    
    if(debug){
      print({paste("Time:", simulation_time, 
                   " Rejections:", rejects, 
                   " t:", t, 
                   " lambda_t:", lambda_t, 
                   " IAT:", inter_arrival_time)})
    }
    
    return(inter_arrival_time)
  }
}

## 4. Model parameterisation

create_experiment <- function(n_triage_bays=DEFAULT_N_TRIAGE,
                              n_reg_clerks=DEFAULT_N_REG,
                              n_exam_rooms=DEFAULT_N_EXAM,
                              n_trauma_rooms=DEFAULT_N_TRAUMA,
                              n_non_trauma_cubicles=DEFAULT_NON_TRAUMA_CUBICLES,
                              n_trauma_cubicles=DEFAULT_TRAUMA_CUBICLES,
                              triage_mean=DEFAULT_TRIAGE_MEAN,
                              stabilisation_mean=DEFAULT_TRAUMA_MEAN,
                              trauma_treat_params=DEFAULT_TRAUMA_TREATMENT_PARAMS,
                              reg_params=DEFAULT_REG_PARAMS,
                              exam_params=DEFAULT_EXAM_PARAMS,
                              prob_non_trauma_treat=DEFAULT_NON_TRAUMA_TREAT_P,
                              nontrauma_treat_params=DEFAULT_NON_TRAUMA_TREATMENT_PARAMS,
                              prob_trauma=DEFAULT_PROB_TRAUMA,
                              arrival_data_path=NSPP_PATH,
                              log_level=LOG_LEVEL) {
  
  # load arrival data
  arrival_data <- load_arrival_data(path=arrival_data_path)
  
  # create list of parameters
  experiment <- list(n_triage_bays=n_triage_bays,
                     n_reg_clerks=n_reg_clerks,
                     n_exam_rooms=n_exam_rooms,
                     n_trauma_rooms=n_trauma_rooms,
                     n_non_trauma_cubicles=n_non_trauma_cubicles,
                     n_trauma_cubicles=n_trauma_cubicles,
                     triage_mean=triage_mean,
                     stabilisation_mean=stabilisation_mean,
                     trauma_treat_params=trauma_treat_params,
                     reg_params=reg_params,
                     exam_params=exam_params,
                     prob_non_trauma_treat=prob_non_trauma_treat,
                     nontrauma_treat_params=nontrauma_treat_params,
                     prob_trauma=prob_trauma,
                     arrival_data=arrival_data,
                     log_level=log_level)
  
  return(experiment)
}     

## 5. Patient Trajectories

### 5.1. Trauma Patients
create_trauma_pathway <- function(exp){
  
  trauma_pathway <- trajectory(name="trauma_pathway") %>%
    set_attribute("patient_type", 1) %>%
    # log patient arrival
    log_(function() {paste("**Trauma arrival")}, level=1) %>% 
    
    # triage 
    set_attribute("start_triage_wait", function() {now(exp$env)}) %>%
    visit("triage_bay", function() rexp(1, 1/exp$triage_mean)) %>%
    log_(function() {paste("(T) Triage wait time:",
                           now(exp$env) - get_attribute(exp$env, "start_triage_wait"))},
         level=1) %>%
    
    # request trauma room for stabilization
    set_attribute("start_trauma_room_wait", function() {now(exp$env)}) %>%
    visit("trauma_room", function() rexp(1, 1/exp$stabilisation_mean)) %>%
    log_(function() {paste("(T) Trauma room wait time:",
                           now(exp$env) - get_attribute(exp$env, "start_trauma_room_wait"))},
         level=1) %>%
    
    # request treatment cubicle
    set_attribute("start_trauma_treat_wait", function() {now(exp$env)}) %>%
    visit("trauma_treat_cubicle", function() rlnorm(1, exp$trauma_treat_params$mu,
                                                    exp$trauma_treat_params$sigma)) %>%
    log_(function() {paste("********************(T) Trauma treatment cubicle wait time:",
                           now(exp$env) - get_attribute(exp$env, "start_trauma_treat_wait"))},
         level=1) %>% 
    
    # store the total time in system 
    set_attribute("total_time", 
                  function() {now(exp$env) - get_attribute(exp$env, "start_triage_wait")})
  
  return(trauma_pathway)
}

### 5.2 Non-trauma patients

create_nt_cubicle_treatment <- function(exp){
  
  nt_cubicle_treatment <- trajectory() %>% 
    log_(function() {paste("NT patient requirement treatment")},
         level=1) %>% 
    seize(resource="nontrauma_treat_cubicle", amount=1) %>% 
    
    timeout(task = function() rlnorm(1, exp$nontrauma_treat_params$mu,                                                     exp$nontrauma_treat_params$sigma)) %>%
    release(resource = "nontrauma_treat_cubicle", amount = 1) %>% 
    log_(function() {paste("NT treatment complete")},
         level=1) %>% 
    return(nt_cubicle_treatment)
}

create_non_trauma_pathway <- function(exp){
  # log messages
  ARRIVAL_MSG = "**Non-Trauma arrival**"
  TRIAGE_MSG = "(NT) Triage wait time:"
  REG_MSG = "Reg wait time:"
  EXAM_MSG = "Exam wait time:"
  EXIT_MSG = "NT Total time in system:"
  
  # optional trajectory for proportion of patients that requirement treatment
  nt_cubicle_treatment <- create_nt_cubicle_treatment(exp)
  
  non_trauma_pathway <- trajectory(name="non_trauma_pathway") %>%
    set_attribute("patient_type", 2) %>%
    # log non_trauma arrival
    log_(function() {paste(ARRIVAL_MSG)}, level=1) %>% 
    
    # store start of waiting time for log calculations
    set_attribute("start_triage_wait", function() {now(exp$env)}) %>%
    # queue and use triage bay
    visit("triage_bay", function() rexp(1, 1/exp$triage_mean)) %>%
    log_(function() {paste(TRIAGE_MSG, now(exp$env) - get_attribute(exp$env, "start_triage_wait"))},
         level=1) %>%
    
    # queue and use registration clerk
    set_attribute("start_reg_wait", function() {now(exp$env)}) %>%
    visit("registration_clerk", function() rlnorm(1, exp$reg_params$mu, 
                                                  exp$reg_params$sigma)) %>%
    log_(function() {paste(REG_MSG, now(exp$env) - get_attribute(exp$env, "start_reg_wait"))},
         level=1) %>%
    
    # queue and use examination room
    set_attribute("start_exam_wait", function() {now(exp$env)}) %>%
    visit("examination_room",  function() rnorm(1, exp$exam_params$mean, 
                                                sqrt(exp$exam_params$var))) %>%
    log_(function() {paste(EXAM_MSG, now(exp$env) - get_attribute(exp$env, "start_exam_wait"))},
         level=1) %>%
    
    # a Proportion of patients require treatment in a cubicle
    branch (
      function() sample_nt_trauma_treatment(exp$prob_non_trauma_treat), continue=T,
      nt_cubicle_treatment
    ) %>% 
    log_(function() {paste(EXIT_MSG, now(exp$env) - get_attribute(exp$env, "start_triage_wait"))},
         level=1) %>% 
    # store the total time in system 
    set_attribute("total_time", 
                  function() {now(exp$env) - get_attribute(exp$env, "start_triage_wait")})
  
  return(non_trauma_pathway)
}

## 6. Modelling patient arrivals

create_arrival_generator <- function(exp){
  
  DEPART_MSG <- "A patient has departed the UTC"
  
  # create and parameterise the trauma pathway trajectory
  trauma_pathway <- create_trauma_pathway(exp)
  
  # create and parameterise the non-trauma pathway trajectory
  non_trauma_pathway <- create_non_trauma_pathway(exp)
  
  patient_arrival <- trajectory() %>%
    branch(
      function() sample_arrival_type(exp$prob_trauma), continue=T,
      trauma_pathway,
      non_trauma_pathway
    ) %>%
    log_(function() {paste(DEPART_MSG)},level=1) %>% 
    set_attribute("departed", 1)
  
  return(patient_arrival)
}

## 7. Single run of the model

single_run <- function(env, exp, 
                       rep_number=1, 
                       run_length=DEFAULT_RESULTS_COLLECTION_PERIOD, 
                       debug_arrivals=FALSE){
  # add the simmer environment to the experiment list.
  exp <- c(exp, env=env) 
  
  # Create the arrivals generator
  arrival_gen <- create_arrival_generator(exp)
  
  # create model and run.
  env %>% 
    add_resource("triage_bay", exp$n_triage_bays) %>%
    add_resource("registration_clerk", exp$n_reg_clerks) %>%
    add_resource("examination_room", exp$n_exam_rooms) %>%
    add_resource("trauma_room", exp$n_trauma_rooms) %>%
    add_resource("trauma_treat_cubicle", exp$n_trauma_cubicles) %>%
    add_resource("nontrauma_treat_cubicle", exp$n_non_trauma_cubicles) %>%
    add_generator("Patient", arrival_gen, 
                  function() nspp_thinning(now(env), exp$arrival_data, 
                                           debug=debug_arrivals),
                  mon=2) %>% 
    run(until=run_length)
  
  # return environment and all of its results.
  return(env)
}


multiple_replications <- function(exp, n_reps=5, random_seed=0){
  
  set.seed(random_seed)
  
  # note unlike in simmer documentation we use a traditional for loop
  # instead of lapply. This allows us to separate env creation
  # from run and preserve the environment interaction between NSPP 
  # and current sim time.
  # TO DO: look again -> can treat_sim be created inside single_run()
  print("running replications...")
  reps = vector()
  for(rep in 1:n_reps){
    treat_sim <- simmer("TreatSimmer", log_level=exp$log_level)
    treat_sim <- single_run(treat_sim, exp)
    # store the latest simulation environment and its results.
    reps <- c(reps, treat_sim)
  }
  print("Complete.")
  return(reps)
}





