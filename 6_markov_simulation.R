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



simulate <- function(
    Win = 1,
    Loss = 10,
    RT_nc = 0.6,
    RT_c = 1.7,
    ITI = 0.1, 
    TTime = 90, 
    N_sim = 1000,
    p_A2B = 0.1,
    p_B2A = 0.1,
    Total_time = 90,
    start = "B"
){
  
  # Win = 1
  # Loss = 12
  # RT_nc = 0.7
  # RT_c = 1.7
  
  # Win = 1
  # Loss = 10
  # RT_nc = 1
  # RT_c = 1.5
  # ITI = 0.1
  # TTime = 90
  # N_sim = 1000
  # Total_time = 90
  # ITI = 0.1
  # p_A2B = 0.1
  # p_B2A = 0.1
  # start = "B"
  
  #Trials to simulate:
  N_trials <- (TTime / (min(RT_nc, RT_c))) + 3 #(buffer)
  
  sequences <- list()
  
  for (i in 1:N_sim){
    sequences[[i]] <- data.frame(sequence = gen_trial_seq(p_A2B = p_A2B, 
                                                          p_B2A = p_B2A, 
                                                          N = N_trials, 
                                                          start = start)) %>%
      mutate(iteration = i)
  }
  
  
  sequences <- bind_rows(sequences) %>%
    mutate(rand = runif(n()))
  
  cr <- seq(0.01, 1, by = 0.05)  
  
  results <- list()
  
  for(i in 1:length(cr)){
    #i = 10
    
    trials <- sequences %>% 
      mutate(Check = ifelse(rand < cr[i], 1 , 0),
             RT = ifelse(Check == 1, RT_c, RT_nc) + ITI) %>%
      group_by(iteration) %>%
      mutate(Trial = 1:n(),
             RT_cum = cumsum(RT),
             monster_id = ifelse(sequence == "A" & lag(sequence, 1) == "B", 1, 0),
             monster_id = cumsum(monster_id),
             monster_id = ifelse(sequence == "B", 0, monster_id)) %>%
      group_by(iteration, monster_id) %>%
      mutate(n_checks = cumsum(Check)) %>%
      group_by(iteration) %>%
      mutate(Rew = ifelse(sequence == "A" & lead(sequence) == "B" & n_checks == 0, Win - Loss, Win),
             Rew_cum = cumsum(Rew))
    
    
    iteration_summary <- trials %>%
      filter(RT_cum < TTime) %>% 
      summarize(Rew_cum = Rew_cum[n()],
                RT_M = mean(RT),
                N_trials = Trial[n()]) %>%
      mutate(CR = cr[i])
    
    trial_summary <- trials %>%
      group_by(Trial) %>%
      summarize(Rew = mean(Rew),
                RT_M = mean(RT)) %>%
      mutate(CR = cr[i])
    
    results[[i]] <- list(iteration_summary = iteration_summary,
                        trial_summary = trial_summary)
    
  }
  
  return(results)
  
}

results <- simulate(Win = 1, 
                    Loss = 10, 
                    RT_nc = 1,
                    RT_c = 1.5, 
                    start = "B",  
                    N_sim = 10000, 
                    p_A2B = .5, 
                    p_B2A = .5)


iteration_summaries <- lapply(results, function(x){
  x$iteration_summary
})

iteration_summaries <- bind_rows(iteration_summaries)

iteration_summaries %>% 
  group_by(CR) %>% 
  summarize(Rew_cum = mean(Rew_cum)) %>%
  ggplot(aes(x = CR, y = Rew_cum))+
  geom_line()+
  theme_classic()


trial_summaries <- lapply(results, function(x){
  x$trial_summary
})
trial_summaries <- bind_rows(trial_summaries)

trial_summaries %>% 
  group_by(CR, Trial) %>% 
  summarize(Rew_per_s = Rew/RT_M) %>%
  ggplot(aes(x = CR, y = Rew_per_s, group = Trial, color = Trial))+
  geom_line()+
  scale_color_viridis_c()+
  theme_classic()




