markov_state <- function(mat_initial_state,
                         mat_transition,
                         n) {
  
  mat_initial_state %*% (mat_transition %^% n) %>%
    as.data.frame() %>%
    #rename(p_state_A = V1, p_state_A_detected = V2, p_state_B = V3) %>%
    return()
  
}

build_mats <- function(p_A2B = 0.1,
                       p_B2A = 0.1, 
                       cr_a = 0.2,
                       cr_b = 0.2,
                       start = "B_mB"){
  
  trans_mat <- matrix(0, 5, 5)
  
  # p_A2B <- 0.1
  # p_B2A <- 0.1
  # cr_a <- 0.1
  # cr_b <- 0.01
  
  names <- c("Au_mA", "Au_mB", "Ad_mA", "B_mA", "B_mB")
  
  colnames(trans_mat) <- names
  rownames(trans_mat) <- names
  
  trans_mat["Au_mA", "Au_mA"] <- (1 - p_A2B) * (1 - cr_a) #for it to remain undetected, I can't check
  trans_mat["Au_mA", "Au_mB"] <- 0 #memory can't update from A to B without 
  trans_mat["Au_mA", "Ad_mA"] <- (1 - p_A2B) * cr_a #for state to update from detected to undetected, you need to check
  trans_mat["Au_mA", "B_mA"] <- p_A2B * (1 - cr_a) # state changes from A to B, but your memory remains the same (you don't check)
  trans_mat["Au_mA", "B_mB"] <- p_A2B * cr_a #state changes from A to B, you check to update memory
  
  trans_mat["Au_mB", "Au_mA"] <- 0 #memory update impossible without checking 
  trans_mat["Au_mB", "Au_mB"] <- (1 - p_A2B) * (1 - cr_b) #state is not allowed to change, and you dont check
  trans_mat["Au_mB", "Ad_mA"] <- (1 - p_A2B) * cr_b # A state remains, you check
  trans_mat["Au_mB", "B_mA"] <- 0 # impossible transition (can't update memory)
  trans_mat["Au_mB", "B_mB"] <- p_A2B # it has to change to B, whether you check or not doesn't matter, the memory remains the same
  
  trans_mat["Ad_mA", "Au_mA"] <- 0 #impossible with just one transition
  trans_mat["Ad_mA", "Au_mB"] <- 0 #if I were to check, the memory wouldnt update to B in this case
  trans_mat["Ad_mA", "Ad_mA"] <- 1 - p_A2B #no matter whether you check or not, neither state nor memory change
  trans_mat["Ad_mA", "B_mA"] <- p_A2B * (1 - cr_a) # state changes without noticing (no checking)
  trans_mat["Ad_mA", "B_mB"] <- p_A2B * cr_a #(state chanegs and I update memory)
  
  trans_mat["B_mA", "Au_mA"] <- p_B2A * (1-cr_a) # state changes but you dont check (because otherwise state would be Ad)
  trans_mat["B_mA", "Au_mB"] <- 0 #impossible memory transition
  trans_mat["B_mA", "Ad_mA"] <- p_B2A * cr_a
  trans_mat["B_mA", "B_mA"] <- (1 - p_B2A) * (1 - cr_a)
  trans_mat["B_mA", "B_mB"] <- (1 - p_B2A) * cr_a
  
  trans_mat["B_mB", "Au_mA"] <- 0 #impossible to update memory from B to A if not also changing A undetected state to detected state
  trans_mat["B_mB", "Au_mB"] <- p_B2A * (1 - cr_b) # state changes, memory doesnt get updated, therefore no check allowed
  trans_mat["B_mB", "Ad_mA"] <- p_B2A * cr_b # state transitions AND I also check
  trans_mat["B_mB", "B_mA"] <- 0 #impossible to update memory from B to A if state doesnt change
  trans_mat["B_mB", "B_mB"] <- 1 - p_B2A #state doesnt change, regardless of whether i check or not
  
  initial_mat <- matrix(0, nrow = 1, ncol = 5)
  colnames(initial_mat) <- names
  
  initial_mat[1, "Au_mA"] <- 0
  initial_mat[1, "Au_mB"] <- 0
  initial_mat[1, "Ad_mA"] <- 0
  initial_mat[1, "B_mA"] <- 0
  initial_mat[1, "B_mB"] <- 0
  initial_mat[1, start] <- 1
  
  return(list(trans_mat = trans_mat,
              initial_mat = initial_mat))
}



get_states <- function(p_A2B = 0.1, 
                         p_B2A = 0.1, 
                         cr_a = seq(0, 1, by = 0.01),
                         cr_b = seq(0, 1, by = 0.01),
                         N_Trials = 90){
  
  
  # p_A2B = 0.1
  # p_B2A = 0.1
  # cr_a = seq(0, 1, by = 0.01)
  # cr_b = seq(0, 1, by = 0.01)
  # RT_nc = 0.5
  # RT_c = 1.5
  # ITI = 0.1
  # Win = 1
  # Loss = 10
  # TTime = 90
  #N_Trials = ceiling(TTime/RT_nc)

  data.frame(setup = 1) %>%
    mutate(trial = list(1:N_Trials),
           p_A2B = p_A2B, 
           p_B2A = p_B2A, 
           cr_a = list(cr_a = cr_a),
           cr_b = list(cr_b = cr_b),
           ) %>%
    select(-setup) %>%
    unnest(cr_a) %>%
    unnest(cr_b) %>%
    rowwise() %>%
    mutate(mats = list(build_mats(p_A2B = p_A2B, 
                                  p_B2A = p_B2A, 
                                  cr_a = cr_a, 
                                  cr_b = cr_b, 
                                  start = "B_mB"))) %>%
    unnest(trial) %>%
    rowwise() %>%
    arrange(cr_a, cr_b, trial) %>%
    mutate(states = list(markov_state(mat_initial_state = mats$initial_mat,
                                      mat_transition = mats$trans_mat, 
                                      n = trial))) %>%
    ungroup() %>%
    select(-mats) %>%
    #mutate(states = as.data.frame(states))
    unnest(states) %>%
    mutate(A_total = Au_mA + Au_mB + Ad_mA,
           Au_total = Au_mA + Au_mB,
           inst_miss = Au_total * p_A2B,
           B_total = B_mA + B_mB,
           Checks = (Au_mA + Ad_mA + B_mA) * cr_a + (Au_mB + B_mB) * cr_b)
  
}


get_payout <- function(states,
                       RT_c,
                       RT_nc,
                       ITI,
                       Win,
                       Loss,
                       TTime){
  
  
  # states <- x$states[1][[1]]
  # RT_c = 1
  # RT_nc= 1
  # ITI =1
  # Win= 1
  # Loss =1
  # TTime =1
  
  states %>%
    mutate(expected_RT = RT_nc + Checks * (RT_c - RT_nc) + ITI,
           expected_Rew = Win - (inst_miss * Loss)) %>%
    group_by(cr_a, cr_b) %>%
    mutate(cum_RT = cumsum(expected_RT),
           cum_Rew = cumsum(expected_Rew)) %>%
    filter(cum_RT > TTime) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(RT_correction = cum_RT - expected_RT, # subtract current RT
           Time_left = TTime - RT_correction, #how much time was left from previous trial to block end
           p_Trial_completed = Time_left/expected_RT, #this is the proportion of the last trial that was completed
           Rew_corrected = cum_Rew - ((1 - p_Trial_completed) * expected_Rew),#to correct, I am subtracting the reward associated with the incomplete portion of the trial 
           is_max = ifelse(cum_Rew == max(cum_Rew), 1, 0),
           is_max_same = ifelse(cr_a == cr_b & cum_Rew == max(cum_Rew[cr_a == cr_b]), 1, 0)) %>%
    select(cr_a, cr_b, Rew_corrected, is_max, is_max_same) %>%
    return()
  
}




get_states_crgrid <- function(p_A2B = 0.1, 
                       p_B2A = 0.1, 
                       cr_grid,
                       N_Trials = 90){
  
  
  # p_A2B = 0.1
  # p_B2A = 0.1
  # cr_a = seq(0, 1, by = 0.01)
  # cr_b = seq(0, 1, by = 0.01)
  # RT_nc = 0.5
  # RT_c = 1.5
  # ITI = 0.1
  # Win = 1
  # Loss = 10
  # TTime = 90
  #N_Trials = ceiling(TTime/RT_nc)
  
  data.frame(setup = 1) %>%
    mutate(trial = list(1:N_Trials),
           p_A2B = p_A2B, 
           p_B2A = p_B2A, 
           cr_a = list(cr_a = cr_grid$cr_a),
           cr_b = list(cr_b = cr_grid$cr_b),
    ) %>%
    select(-setup) %>%
    unnest(cr_a) %>%
    unnest(cr_b) %>%
    rowwise() %>%
    mutate(mats = list(build_mats(p_A2B = p_A2B, 
                                  p_B2A = p_B2A, 
                                  cr_a = cr_a, 
                                  cr_b = cr_b, 
                                  start = "B_mB"))) %>%
    unnest(trial) %>%
    rowwise() %>%
    arrange(cr_a, cr_b, trial) %>%
    mutate(states = list(markov_state(mat_initial_state = mats$initial_mat,
                                      mat_transition = mats$trans_mat, 
                                      n = trial))) %>%
    ungroup() %>%
    select(-mats) %>%
    #mutate(states = as.data.frame(states))
    unnest(states) %>%
    mutate(A_total = Au_mA + Au_mB + Ad_mA,
           Au_total = Au_mA + Au_mB,
           inst_miss = Au_total * p_A2B,
           B_total = B_mA + B_mB,
           Checks = (Au_mA + Ad_mA + B_mA) * cr_a + (Au_mB + B_mB) * cr_b)
  
}


get_payout <- function(states,
                       RT_c,
                       RT_nc,
                       ITI,
                       Win,
                       Loss,
                       TTime){
  
  
  # states <- x$states[1][[1]]
  # RT_c = 1
  # RT_nc= 1
  # ITI =1
  # Win= 1
  # Loss =1
  # TTime =1
  
  states %>%
    mutate(expected_RT = RT_nc + Checks * (RT_c - RT_nc) + ITI,
           expected_Rew = Win - (inst_miss * Loss)) %>%
    group_by(cr_a, cr_b) %>%
    mutate(cum_RT = cumsum(expected_RT),
           cum_Rew = cumsum(expected_Rew)) %>%
    filter(cum_RT > TTime) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(RT_correction = cum_RT - expected_RT, # subtract current RT
           Time_left = TTime - RT_correction, #how much time was left from previous trial to block end
           p_Trial_completed = Time_left/expected_RT, #this is the proportion of the last trial that was completed
           Rew_corrected = cum_Rew - ((1 - p_Trial_completed) * expected_Rew),#to correct, I am subtracting the reward associated with the incomplete portion of the trial 
           is_max = ifelse(cum_Rew == max(cum_Rew), 1, 0),
           is_max_same = ifelse(cr_a == cr_b & cum_Rew == max(cum_Rew[cr_a == cr_b]), 1, 0)) %>%
    select(cr_a, cr_b, Rew_corrected, is_max, is_max_same) %>%
    return()
  
}