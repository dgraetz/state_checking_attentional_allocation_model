library(expm)
library(tidyverse)
library(rio)
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
                             p_initial_A = 0,
                             p_initial_A_detected = 0,
                             p_initial_B = 1,
                             Win = 1,
                             Loss = 10,
                             RT_nc = 1,
                             RT_c = 1.5,
                             ITI = 0.1, 
                             TTime = 90){
  
  
  
  
  
  #for debugging
  p_A2B = 0.1
  p_B2A = 0.1
  p_initial_A = 0
  p_initial_A_detected = 0
  p_initial_B = 1
  Win = 1
  Loss = 5
  RT_nc = 1
  RT_c = 1.5
  ITI = 0.1
  TTime = 90
  N_sim <- 1000
  start = "B"
  
  
  sequences <- list()
  
  for (i in 1:N_sim){
    sequences[[i]] <- data.frame(sequence = gen_trial_seq(N = 1000, 
                                                          p_A2B = p_A2B, 
                                                          p_B2A = p_B2A, 
                                                          start = start)) %>%
      mutate(iteration = i)
  }
  
  
  sequences <- bind_rows(sequences) %>%
    group_by(iteration) %>%
    mutate(trial = 1:n(),
           monster_id = 0,
           monster_id = ifelse(lag(sequence, 1, default = "A") == "B", 1, monster_id),
           monster_id = cumsum(monster_id),
           monster_id = ifelse(sequence == "B", 0, monster_id),
           #rand = runif(n())
    )
  
  
  sims <- list()
  
  runlength <- 1:20
  iter <- 0
  for (t in 1:50){
    
    for (rl in 1:length(runlength)){
      iter <- iter + 1
      x <- sequences %>%
        filter(trial >= t & trial <= t + rl) 
      
      sims[[iter]] <- x %>%
        group_by(iteration) %>%
        mutate(check = ifelse(row_number() == n(), 1, 0),
               trial_time = ifelse(check == 1, RT_c, RT_nc) + ITI )%>%
        group_by(iteration, monster_id) %>%
        mutate(monster_missed = ifelse(sequence != "A_detected" & monster_id != 0 & row_number() == n() & sum(check) == 0, 1, 0),
               Rew = ifelse(monster_missed == 1, Win - Loss, Win)) %>% 
        group_by(iteration) %>%
        summarize(Rew = sum(Rew),
                  run_dur = sum(trial_time),
                  Rew_per_s = Rew/run_dur) %>%
        ungroup() %>%
        summarize(trial = t,
                  checkrate = 1/runlength[rl],
                  expected_earnings = mean(Rew_per_s) * TTime,
                  .groups = "drop")
      
    }
  }
  
  
  
  
  sims1 <- bind_rows(sims)
  
  sims_agg <- sims1 %>%
    group_by(checkrate, trial) %>%
    summarize(
      Rew = mean(expected_earnings),
      .groups = "drop")
  
  
  
  
  # Plot sim_earnings instead of Rew
  ggplot(sims_agg %>% filter(trial < 30), aes(x = checkrate, y = Rew, group = trial, color = trial))+
    geom_line()+
    scale_color_viridis_c(end = 0.9, name = "Start trial of run")+
    labs(title = "Monte Carlo Simulated Earnings",
         x = "checkrate",
         y = "sim_earnings (per 90s block)")+
    theme_classic()
  
}

























