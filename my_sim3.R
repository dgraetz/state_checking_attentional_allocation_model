library(tidyverse)

sim_sidetask_vec <- function(RT_A_only,
                             RT_A_B_check,
                             RT_A_B_response,
                             ITI, 
                             Win_A,
                             Loss_A,
                             Win_B,
                             Loss_B,
                             p_go_on,
                             p_go_off,
                             Time,
                             cr,
                             reps = 10){
  
  
  #FOR DEBUGGING
  # RT_A_only <- 1
  # RT_A_B_check <- 1.1
  # RT_A_B_response <- 1.5
  # 
  # ITI <- 0.2
  # 
  # Win_A <- 1
  # Loss_A <- 1
  # 
  # Win_B <- 1
  # Loss_B <- 10
  # 
  # p_go_on <- 0.9
  # p_go_off <- 0.5
  # 
  # Time <- 50
  # 
  # cr <- 0.05
  # 
  # reps <- 100
  
  ############
  
  RT_min <- min(c(RT_A_only, RT_A_B_check, RT_A_B_check)) #only to determine the trials to simulate
  min_trials <- ceiling(Time/RT_min)
  
  total_n <- reps*min_trials
  
  checks <- rbinom(n = min_trials*reps, size = 1, prob = cr)
  state_B <- integer(total_n)
  rand_vec <- runif(total_n)
  
  for (i in 1:total_n) {
    # Every 'min_trials', we force a reset (start of new simulation)
    if (i %% min_trials == 1) {
      current_state <- 0
    } else {
      if (current_state == 0) {
        # Check transition 0 -> 1
        if (rand_vec[i] < p_go_on) {
          current_state <- 1
        }
      } else {
        # Check transition 1 -> 0
        if (rand_vec[i] < p_go_off) {
          current_state <- 0
        }
      }
    }
    
    # if (checks[i] == 1){ #
    #   current_state <- 0 #
    # }                    #
    state_B[i] <- current_state
  }
  
  
  
  
  result <- data.frame(
    rep = rep(1:reps, each = min_trials),
    trial = rep(1:min_trials, reps),
    state_B = state_B,
    checks = checks) %>%
    group_by(rep) %>%
    mutate(checks = replace(checks, 1, 0),
           state_on_first = ifelse(lag(state_B) == 0 & state_B == 1, 1, 0),
           state_id = cumsum(state_on_first),
           state_id = ifelse(state_B == 0, 0, state_id)) %>%
    group_by(rep, state_id) %>%
    mutate(state_checked = cumsum(checks),
           state_checked = ifelse(state_checked > 0, 1, 0),
           state_checked = ifelse(state_B == 0, 0, state_checked),
           on_missed_tmp = ifelse(state_checked[n()] == 0, 1, 0),
           on_missed_tmp = ifelse(state_B == 0, 0, on_missed_tmp),
           on_missed = 0,
           on_missed = replace(on_missed, n(), on_missed_tmp[1])) %>%
    group_by(rep) %>%
    mutate(Rew = case_when(checks == 0 & on_missed == 1 ~ Win_A - abs(Loss_B),
                           checks == 1 & state_B == 1 & state_checked == 0 ~ Win_A + Win_B, 
                           .default = Win_A),
           Rew_total = cumsum(Rew),
           RT = case_when(checks == 0 ~ RT_A_only + ITI,
                          checks == 1 & state_checked == 0 ~ RT_A_B_response + ITI,
                          checks == 1 & state_checked == 1 ~ RT_A_B_check + ITI),
           timeout = ifelse(cumsum(RT) > Time, 1, 0)) %>%
    filter(timeout == 0)
  
  return(result)
}



