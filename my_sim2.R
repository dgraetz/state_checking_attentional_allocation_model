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
  # cr <- seq(0, 1, by = 0.1)
  # 
  # reps <- 100
  # 
  ############
  
  RT_min <- min(c(RT_A_only, RT_A_B_check, RT_A_B_check)) #only to determine the trials to simulate
  min_trials <- ceiling(Time/RT_min)
  
  Blocks <- reps * length(cr)
  
  exp <- expand.grid(
    Trial = 1:min_trials,
    rep = 1:reps,
    cr = cr
  )
  
  exp <- exp %>% 
    #select(-Trial) %>%
    mutate(Block = rep(1:Blocks, each = min_trials)) %>%
    relocate(Block, cr, rep) %>%
    group_by(Block)%>%
    mutate(checks = c(1, sample(c(0, 1), n() - 1, replace = TRUE, prob = c(1-cr[1], cr[1]))))
  

  state_B <- integer(nrow(exp))
  rand_vec <- runif(nrow(exp))
  
  for (i in 1:nrow(exp)) {
    # Every 'min_trials', we force a reset (start of new simulation)
    if (exp$Trial[i] == 1) {
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
    state_B[i] <- current_state
  }
  
  exp$state_B <- state_B
  
  
  result <- exp %>%
    group_by(Block) %>%
    mutate(state_on_first = ifelse(lag(state_B) == 0 & state_B == 1, 1, 0),
           state_id = cumsum(state_on_first),
           state_id = ifelse(state_B == 0, 0, state_id)) %>%
    group_by(Block, state_id) %>%
    mutate(state_checked = cumsum(checks),
           state_checked = ifelse(state_checked > 0, 1, 0),
           state_checked = ifelse(state_B == 0, 0, state_checked),
           on_missed_tmp = ifelse(state_checked[n()] == 0, 1, 0),
           on_missed_tmp = ifelse(state_B == 0, 0, on_missed_tmp),
           on_missed = 0,
           on_missed = replace(on_missed, n(), on_missed_tmp[1])) %>%
    group_by(Block) %>%
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


sim_out <- sim_sidetask_vec(RT_A_only = 0.6,
                            RT_A_B_check = 1.7,
                            RT_A_B_response = 1.7,
                            ITI = 0.2,
                            Win_A = 1,
                            Loss_A = 0,
                            Win_B = 0,
                            Loss_B = 20,
                            p_go_on = 0.1,
                            p_go_off = 0.1,
                            Time = 100,
                            cr = seq(0, 1, by = 0.01),
                            reps = 100)

  



sim_out_summary <- sim_out %>%
  group_by(rep, cr) %>%
  summarize(Rew = Rew_total[n()],
            n_states = max(state_id),
            n_trials = max(Trial)) %>%
  group_by(cr) %>%
  summarize(Rew = mean(Rew),
            n_states = mean(n_states),
            n_trials = mean(n_trials),
            p_states = n_states/n_trials) %>%
  ungroup() %>%
  mutate(rel_reward = Rew/max(Rew))

ggplot(mapping = aes(x = cr, y = rel_reward)) + 
  geom_line(data = sim_out_summary, color = "red")+
  geom_hline(yintercept = 0)+
  coord_cartesian(ylim = c(0, 1))+
  theme_classic()
