library(expm)
library(tidyverse)
library(rio)

source("state3_after_n_trans.R")

#get a random run length
get_run_length <- function(cr, attemps = 1000){
  
  rand <- runif(attemps)
  return(which(rand <= cr)[1])
  
}




simulated_run_lengths <- lapply(1:1000, function(x){
  get_run_length(cr = 0.1)
}) %>% unlist()


get_geometric_run_weights <- function(cr, runs = 1000){
  r <- 1:runs
  w <- cr * (1 - cr)^(r - 1)
  #w <- w / sum(w)
  return(w)
}

weights <- get_geometric_run_weights(0.1) %>%
  as.data.frame() %>%
  rename(w = ".") %>%
  mutate(run_lengths = 1:1000)

ggplot()+
  geom_histogram(aes(x = simulated_run_lengths), bins = 50)

ggplot(weights %>% filter(run_lengths < 50))+
  geom_bar(aes(x = run_lengths, y = w*1000), stat = "identity")


gen_trial_seq <- function(N = 100, 
                          p_A2B = 0.1,
                          p_B2A = 0.1,
                          start = "A_detected"){
  
  ran <- runif(N)
  sequence <- character(N)
  sequence[1] <- start
  
  current_state <- sequence[1]
  
  for(i in 2:N){
    
    if (current_state == "A"|current_state == "A_detected"){
      if (ran[i] <= p_A2B){
        current_state <- "B"
      }
    } else if (current_state == "B"){
      if (ran[i] <= p_B2A){
        current_state <- "A"
      }
    }
    sequence[i] <- current_state
  }
  
  return(sequence)
}





sequences <- list()

for (i in 1:1000){
  sequences[[i]] <- data.frame(sequence = gen_trial_seq(p_A2B = 0.1, 
                                                        p_B2A = 0.1, 
                                                        N = 100, 
                                                        start = "A_detected")) %>%
    mutate(iteration = i)
}


sequences <- bind_rows(sequences)

sequences_agg <- sequences %>%
  group_by(iteration) %>%
  mutate(trial = 1:n()) %>%
  group_by(trial) %>%
  summarize(sim_p_A = mean(sequence == "A"),
            sim_p_A_detected = mean(sequence == "A_detected"),
            sim_p_B = mean(sequence == "B"))


calculated <- data.frame(trial = 1:100) %>%
  rowwise() %>%
  mutate(calc_p_A = state3_after_n_trans(p_A2B = 0.1, 
                                         p_B2A = 0.1, 
                                         p_initial_B = 0, 
                                         p_initial_A_detected = 1, 
                                         p_initial_A = 0, 
                                         n = trial)$p_state_A,
         calc_p_A_detected = state3_after_n_trans(p_A2B = 0.1, 
                                                  p_B2A = 0.1, 
                                                  p_initial_B = 0, 
                                                  p_initial_A_detected = 1, 
                                                  p_initial_A = 0, 
                                                  n = trial)$p_state_A_detected,
         calc_p_B = state3_after_n_trans(p_A2B = 0.1, 
                                         p_B2A = 0.1, 
                                         p_initial_B = 0, 
                                         p_initial_A_detected = 1, 
                                         p_initial_A = 0, 
                                         n = trial)$p_state_B)

sequences_agg <- left_join(sequences_agg, calculated)

ggplot(sequences_agg, aes(x = trial))+
  geom_line(aes(y = calc_p_A), color = "red", lwd = 2, alpha = .2)+
  geom_line(aes(y = calc_p_A_detected), color = "green", lwd = 2, alpha = .2)+
  geom_line(aes(y = calc_p_B), color = "blue", lwd = 2, alpha = .2)+
  geom_line(aes(y = sim_p_A), color = "red")+
  geom_line(aes(y = sim_p_A_detected), color = "green")+
  geom_line(aes(y = sim_p_B), color = "blue")+
  theme_classic()




simulate_monster <- function(p_A2B = 0.1,
                             p_B2A = 0.1,
                             Win = 1,
                             Loss = 10,
                             RT_nc = 1,
                             RT_c = 1.5,
                             ITI = 0.1, 
                             TTime = 90, 
                             start = "A",
                             Trials = 1:20,
                             N_sim = 1000
                             ){
  
  
  #for debugging
  # p_A2B = 0.1
  # p_B2A = 0.1
  # Win = 1
  # Loss = 10
  # RT_nc = 1
  # RT_c = 1.5
  # ITI = 0.1
  # TTime = 90
  # N_sim <- 1000
  # N_trials <- 20
  # start = "A_detected"
  # p_initial_A = 0
  # p_initial_A_detected = 0
  # p_initial_B = 0
  
  ######################################
  run_lengths <- 1:1000
  Simulated_trials <- N_trials + max(run_lengths) 
  
  p_initial_A = 0
  p_initial_A_detected = 0
  p_initial_B = 0
  if (start == "A_detected"){
    p_initial_A_detected = 1
  } else  if (start == "A"){
    p_initial_A = 1
  } else  if (start == "B"){
    p_initial_B = 1
  }
  
  
  sequences <- list()
  
  for (i in 1:N_sim){
    sequences[[i]] <- data.frame(sequence = gen_trial_seq(N = Simulated_trials, 
                                                          p_A2B = p_A2B, 
                                                          p_B2A = p_B2A, 
                                                          start = start)) %>%
      mutate(iteration = i)
  }
  
  
  sequences <- bind_rows(sequences) %>%
    group_by(iteration) %>%
    mutate(trial = 1:n(),
           monster_id = 0,
           monster_id = ifelse(lag(sequence, 1) == "B" & sequence == "A", 1, monster_id),
           monster_id = case_when(trial == 1 & sequence == "A" ~ 1, .default = monster_id),
           monster_id = cumsum(monster_id),
           monster_id = ifelse(sequence == "B", 0, monster_id),
           #rand = runif(n())
    )
  
  sims <- list()
  
  for (t in 1:N_trials){
    
    
    x <- sequences %>%
      filter(trial >= t) 
    
    #sims[[iter]] <- x %>%
    x <- x %>%
      group_by(iteration) %>%
      mutate(rl = 1:n(),
             trial_time = RT_nc + ITI,
             cum_time = cumsum(trial_time),
             Win = Win,
             cum_Win = cumsum(Win)) %>% 
      filter(rl <= max(run_lengths)) %>%
      group_by(iteration, monster_id) %>%
      mutate(monster_missed = ifelse(sequence != "A_detected" & monster_id != 0 & row_number() == n(), 1, 0)) %>%
      group_by(iteration) %>%
      mutate(monsters_missed = cumsum(monster_missed),
             rl_monster_missed = case_when(monster_missed == 1 ~ monsters_missed - 1, .default = monsters_missed), #if this is the current run length, then we wanna make sure that we remove one monster to "kill" it
             rl_time = cum_time + (RT_c - RT_nc), # add cue check time because we check on the last trial
             rl_avg_time = rl_time/rl,
             rl_Rew_abs = cum_Win - (rl_monster_missed*Loss),
             rl_avg_payoff_by_r = rl_Rew_abs/rl)
    
    payoffs_by_rl <- x %>%
      group_by(rl) %>%
      summarize(expected_payoff = mean(rl_avg_payoff_by_r), .groups = "drop")
    
    #now get the optimality curve
    probs <- seq(0.01, 1, by = 0.01)
    earnings <- numeric(length(probs))
    
    for (i in seq_along(probs)) {
      
      repeats <- 1000
      
      runs <- integer(repeats)
      
      for (rpt in 1:repeats){
        runs[rpt] <- get_run_length(cr = probs[i])
      }
      
      runs <- runs %>%
        table() %>%
        as.data.frame() %>%
        mutate(w = Freq/repeats) %>%
        rename(rl = ".") %>%
        mutate(rl = as.integer(as.character(rl)))
      
      denom_with_p <- TTime / (RT_nc + ITI + probs[i] * (RT_c - RT_nc))
      
      # current_p_results <- x %>%
      #   left_join(runs, by = "rl") %>%
      #   mutate(w = ifelse(is.na(w), 0, w)) %>%
      #   group_by(iteration) %>% 
      #   summarize(avg_payoff_p = sum(w * rl_avg_payoff_by_r),
      #             earnings = avg_payoff_p * denom_with_p)
      current_p_results <- payoffs_by_rl %>%
        left_join(runs, by = "rl") %>%
        mutate(w = ifelse(is.na(w), 0, w)) %>%
        summarize(avg_payoff_p = sum(w * expected_payoff),
                  earnings = avg_payoff_p * denom_with_p)
      
      
      earnings[i] <- mean(current_p_results$earnings)
      
    }
    
    sims[[t]] = data.frame(
      trial = rep(t, length.out = length(earnings)),
      earnings       = earnings,
      probabilities  = probs
    )
    
  }
  
  
  return(list(sims = bind_rows(sims),
              sequences = sequences))
  
}


p_A2B = 0.1
p_B2A = 0.1
Win = 1
Loss = 10
RT_nc = 1
RT_c = 1.5
ITI = 0.1 
TTime = 90 
start = "A_detected"

####################
#Code below automatically figures out the starting probablity based on start vector
p_initial_A = 0
p_initial_A_detected = 0
p_initial_B = 0
if (start == "A_detected"){
  p_initial_A_detected = 1
} else  if (start == "A"){
  p_initial_A = 1
} else  if (start == "B"){
  p_initial_B = 1
}
#####################

sim_out <- simulate_monster(p_A2B = p_A2B,
                            p_B2A = p_B2A,
                            Win = Win,
                            Loss = Loss,
                            RT_nc = RT_nc,
                            RT_c = RT_c,
                            ITI = ITI, 
                            TTime = TTime, 
                            start = start,
                            Trials = 20,
                            N_sim = 1000)


curves <- sim_out$sims
sequences <- sim_out$sequences

ggplot(curves, aes(x = probabilities, y = earnings, group = trial, color = trial))+
  geom_line()+
  scale_color_viridis_c()+
  theme_classic()


# plot the simulated sequence
sequences_agg <- sequences %>%
  group_by(iteration) %>%
  mutate(trial = 1:n()) %>%
  group_by(trial) %>%
  summarize(sim_p_A = mean(sequence == "A"),
            sim_p_A_detected = mean(sequence == "A_detected"),
            sim_p_B = mean(sequence == "B"))


calculated <- data.frame(trial = 1:100) %>%
  rowwise() %>%
  mutate(calc_p_A = state3_after_n_trans(p_A2B = p_A2B,
                                         p_B2A = p_B2A,
                                         p_initial_B = p_initial_B,
                                         p_initial_A_detected = p_initial_A_detected,
                                         p_initial_A = p_initial_A,
                                         n = trial)$p_state_A,
         calc_p_A_detected = state3_after_n_trans(p_A2B = 0.1,
                                                  p_B2A = 0.1,
                                                  p_initial_B = p_initial_B,
                                                  p_initial_A_detected = p_initial_A_detected,
                                                  p_initial_A = p_initial_A,
                                                  n = trial)$p_state_A_detected,
         calc_p_B = state3_after_n_trans(p_A2B = 0.1,
                                         p_B2A = 0.1,
                                         p_initial_B = p_initial_B,
                                         p_initial_A_detected = p_initial_A_detected,
                                         p_initial_A = p_initial_A,
                                         n = trial)$p_state_B)

sequences_agg <- left_join(sequences_agg, calculated)

ggplot(sequences_agg %>% filter(trial < 30), aes(x = trial))+
  geom_line(aes(x = trial + 1, y = calc_p_A), color = "red", lwd = 2, alpha = .2)+
  geom_line(aes(x = trial + 1, y = calc_p_A_detected), color = "green", lwd = 2, alpha = .2)+
  geom_line(aes(x = trial + 1, y = calc_p_B), color = "blue", lwd = 2, alpha = .2)+
  geom_line(aes(y = sim_p_A), color = "red")+
  geom_line(aes(y = sim_p_A_detected), color = "green")+
  geom_line(aes(y = sim_p_B), color = "blue")+
  theme_classic()
