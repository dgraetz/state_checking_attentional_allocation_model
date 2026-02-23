library(tidyverse)
library(expm)
options(scipen = 99)

state_after_n_trans <- function(p_A2B, 
                                p_B2A,
                                p_initial_A = 0, 
                                p_initial_B = 1,
                                n) {
  
  #really nice set of videos: https://www.youtube.com/watch?v=IYaOMor9qvE
  #initial state matrix: p(inital state being on), p(inital state being off)
  initial_state <- matrix(c(p_initial_A, p_initial_B), nrow = 1)
  
  
  p_A2A <- 1-p_A2B
  p_B2B <- 1-p_B2A
  
  #transition matrix: upper left: p(remaining in on state), upper right: p(from on to off)
  #                   lower left: p(remaining off to on), lower right: p(remaining in off state)
  transition <- matrix(c(p_A2A, p_A2B,
                         p_B2A, p_B2B), nrow = 2, byrow = TRUE)
  
  
  initial_state %*% (transition %^% n) %>% 
    as.data.frame() %>%
    rename(p_state_A = V1, p_state_B = V2) %>%
    return()
  
}

state_after_n_trans(p_A2B = 0.4, 
                    p_B2A = 0.15, 
                    p_initial_A = 0.1, 
                    p_initial_B = 0.9, 
                    n = 4)


###############

p_A2B <- 0.1
p_B2A <- 0.1
p_initial_A <- 0
p_initial_B <- 1

#let's say A is the ON state, B is OFF
df <- data.frame(n = 0L:10L,
                 states = NA,
                 p_A2B = p_A2B, #from on to off
                 p_B2A = p_B2A, #from off to on
                 p_initial_A = p_initial_A, #we're never starting with the on state
                 p_initial_B = p_initial_B) %>% #we're always starting with the off state %>%
  rowwise() %>%
  mutate(states = list(state_after_n_trans(p_A2B = p_A2B, #from on to off
                                           p_B2A = p_B2A, #from off to on
                                           p_initial_A = p_initial_A, #we're never starting with the on state
                                           p_initial_B = p_initial_B, #we're always starting with the off state
                                           n = n) %>% 
                         as.data.frame()
  )
  ) %>%
  unnest(states)


ggplot(df, aes(x = n))+
  geom_line(aes(y = p_state_A, color = "A"))+
  geom_line(aes(y = p_state_B, color = "B"))+
  #geom_line(aes(y = p_state_A/0.5, color = "uncertainty"))+
  scale_color_manual(values = c("A" = "green",
                                "B" = "red",
                                "uncertainty" = "blue"))+
  scale_y_continuous(limits = c(0,1))+
  labs(x = "Trial Number",
       y = "state probability")+
  theme_classic()


#########BASED ON STARTING WITH OFF STATE
#probability that an event starts, when starting from 0
df <- df %>%
  mutate(p_A_starts = lag(p_state_B, 1, default = 0) * p_B2A,
         p_A_ends = lag(p_state_A, 1, default = 0) * p_A2B,
         cum_A_ends = cumsum(p_A_ends))


ggplot(df, aes(x = n))+
  geom_line(aes(y = p_state_A, color = "A"))+
  geom_line(aes(y = p_state_B, color = "B"))+
  geom_line(aes(y = p_A_starts, color = "A_starts"))+
  geom_line(aes(y = p_A_ends, color = "A_ends"))+
  geom_line(aes(y = cum_A_ends, color = "cum_A_ends"))+
  scale_color_manual(values = c("A" = "green",
                                "B" = "red",
                                "A_starts" = "blue",
                                "A_ends" = "black",
                                "cum_A_ends" = "grey"), 
                     labels = c("A" = "p(A)",
                                "B" = "p(B)",
                                "A_starts" = "p(A onset)",
                                "A_ends" = "p(A offset)",
                                "cum_A_ends" = "N missed on states"))+
  scale_y_continuous(limits = c(0,1))+
  labs(x = "Trial Number",
       y = "state probability")+
  theme_classic()


state_after_n_trans2 <- function(p_A2B, 
                                p_B2A,
                                p_initial_A = 0, 
                                p_initial_B = 1,
                                cr,
                                n) {
  
  #really nice set of videos: https://www.youtube.com/watch?v=IYaOMor9qvE
  #initial state matrix: p(inital state being on), p(inital state being off)
  initial_state <- matrix(c(p_initial_A, p_initial_B, 0), nrow = 1)
  
  
  p_A2A <- 1-p_A2B
  p_B2B <- 1-p_B2A
  
  #transition matrix: rows: FROM, columns: TO; (A BEING MONSTER IS ON)
  #upper:  A & checked
  #middle: B
  #lower:  A & not checked
  transition <- matrix(c(1-p_A2B         ,     p_A2B        , 0                       ,
                         p_B2A * cr      , 1 - p_B2A        , p_B2A * (1 - cr)        , 
                         (1 - p_A2B) * cr,     p_A2B        , (1 - p_A2B) * (1 - cr)) ,
                       
                       
                       nrow = 3        , byrow = TRUE)
  
  
  #1-p_A2B: This is equal to the probability of staying state remaining ON (i. e., not transitioning)
  #p_A2B: Probability of transitioning from ON to OFF,
  #0: you can't go from state ON checked to state ON unchecked
  
  #p_B2A * cr: this is when transitioning from OFF state to ON state that is instantly detected
  #1-p_B2A: this is from OFF state to remaining OFF
  #p_B2A * (1-cr): this is from OFF state to ON state that is NOT detected instantly
  
  #(1-p_A2B) * cr: this is coming from a non detected ON state and detecting it next.
  #p_A2B: this is from an ON state, non detected to an OFF state
  #this is being in an ON state not detected, and still not detecting the on state in the next trial
  
  initial_state %*% (transition %^% n) %>% 
    as.data.frame() %>%
    rename(p_state_A_detected = V1, p_state_B = V2, p_state_A_unchecked = V3) %>%
    return()
  
}

df2 <- data.frame(n = 0L:100L,
             states = NA,
             p_A2B = p_A2B, #from on to off
             p_B2A = p_B2A, #from off to on
             p_initial_A = 0, #we're never starting with the on state
             p_initial_B = 1) %>% #we're always starting with the off state %>%
  rowwise() %>%
  mutate(states = list(state_after_n_trans2(p_A2B = p_A2B, #from on to off
                                           p_B2A = p_B2A, #from off to on
                                           p_initial_A = p_initial_A, #we're never starting with the on state
                                           p_initial_B = p_initial_B, #we're always starting with the off state
                                           cr = 0,
                                           n = n) %>% 
                         as.data.frame()
  )
  ) %>%
  unnest(states)


df2 <- df2 %>%
  mutate(p_missed = lag(p_state_A_unchecked, 1, default = 0) * p_A2B,
         count = cumsum(p_missed))

ggplot(df2, aes(x = n))+
  geom_line(aes(y = p_state_A_detected, color = "p_state_A_detected"))+
  geom_line(aes(y = p_state_A_unchecked, color = "p_state_A_unchecked"))+
  geom_line(aes(y = p_state_B, color = "p_state_B"))+
  geom_line(aes(y = p_missed, color = "p_missed"))+
  geom_line(aes(y = count, color = "count"))+
  scale_color_manual(values = c("p_state_A_detected" = "green",
                                "p_state_A_unchecked" = "red",
                                "p_state_B" = "blue",
                                "p_missed" = "yellow",
                                "count" = "grey"),
                     labels = c("p_state_A_detected" = "p(A detected)",
                                "p_state_A_unchecked" = "p(A not detected)",
                                "p_state_B" = "p(B)",
                                "p_missed" = "p(missed)",
                                "count" = "N missed"))+
  scale_x_continuous(limits = c(0, 30))+
  scale_y_continuous(limits = c(0, 1))+
  theme_classic()

#not quite sure if this accounts for the fact that you have a knowledge reset at every check - i. e., the markov process begins again


RT_c = 1.5
RT_nc = 1
ttime = 50
rew = 1
penalty = 10


s <- df2 %>%
  group_by(cr) %>%
  mutate(time = cr*RT_c + (1-cr)*RT_nc,
         cum_time = cumsum(time),
         rew = rew,
         cum_rew = cumsum(rew)) %>%
  filter((ttime - cum_time) > 0) %>% 
  filter(min(ttime - cum_time) == (ttime - cum_time))%>%
  summarize(reward = cum_rew - count * penalty)

s
ggplot(s, aes(x = cr, y = reward))+
  geom_line()




#gemini: calculate expeced misses:

calc_expected_misses <- function(n_trials, 
                                 cr, 
                                 p_A2B = 0.1, 
                                 p_B2A = 0.1, 
                                 p_initial_A = 0, 
                                 p_initial_B = 1) {
  
  # 1. Initialize State Vector [Detected, Empty, Hidden]
  # We assume Hidden starts at 0 because you either "know" the state (A or B) or you don't.
  current_vec <- c(p_initial_A, p_initial_B, 0)
  
  # 2. Define Transition Matrix
  # Row 1: Detected -> [Stays, Leaves, Impossible]
  # Row 2: Empty    -> [Caught, Stays, Missed_Spawn]
  # Row 3: Hidden   -> [Caught, Leaves(Miss), Ignored]
  trans_mat <- matrix(c(
    1 - p_A2B,           p_A2B,        0,
    p_B2A * cr,          1 - p_B2A,    p_B2A * (1 - cr),
    (1 - p_A2B) * cr,    p_A2B,        (1 - p_A2B) * (1 - cr)
  ), nrow = 3, byrow = TRUE)
  
  total_misses <- 0
  
  # 3. Run Simulation Loop
  if (n_trials > 0) {
    for (i in 1:n_trials) {
      
      # A. Calculate the Leak (The Miss)
      # The misses that happen *during* this transition come from 
      # the Hidden monsters (State 3) present at the previous step.
      prob_hidden_prev <- current_vec[3]
      miss_flow <- prob_hidden_prev * p_A2B
      
      # Accumulate
      total_misses <- total_misses + miss_flow
      
      # B. Update the State Probabilities for the next step
      current_vec <- current_vec %*% trans_mat
    }
  }
  
  return(as.numeric(total_misses))
}

RT_c = 1.5
RT_nc = 1
ttime = 50
rew = 1
penalty = 10

cr = seq(0, 1, 0.01)
final_reward = vector()
for (i in 1:length(cr)){
  expected_trials = ttime/(cr[i] * RT_c + (1-cr[i]) * RT_nc)
  
  misses <- calc_expected_misses(n_trials = expected_trials,
                                 cr = cr[i], p_A2B = 0.2, p_B2A = 0.1, p_initial_A = 0, p_initial_B = 1)
  
  final_reward[i] = (expected_trials * rew) - misses * penalty
}

ggplot()+
  geom_line(aes(x = cr, y = final_reward/max(final_reward)))

############################
#FROM GEMINI:

calc_sidetask <- function(RT_A_only, RT_A_B_check, RT_A_B_response, ITI, 
                          Win_A, Loss_A, Win_B, Loss_B, 
                          p_go_on, p_go_off, Time, cr){
  
  Win_A <- abs(Win_A)
  Loss_A <- abs(Loss_A)
  Win_B <- abs(Win_B)
  Loss_B <- abs(Loss_B)
  # 1. State Probabilities (Steady State Markov)
  # Pi_off: Proportion of time world is in "OFF" state
  # Pi_on:  Proportion of time world is in "ON" state
  Pi_off <- p_go_off / (p_go_on + p_go_off)
  Pi_on  <- 1 - Pi_off
  
  # 2. Event Statistics
  # Lambda: Probability of a new event STARTING on any given trial
  lambda_event <- Pi_off * p_go_on
  
  # 3. Probability of Missing an Event
  # A miss occurs if we fail to check (1-cr) for every trial the monster is alive.
  # The duration of the monster follows a Geometric distribution (p = p_go_off).
  # P(Miss) = Sum[ P(Length=k) * (1-cr)^k ] for k=1 to infinity.
  # This resolves to a geometric series:
  q <- 1 - cr
  P_miss_event <- (p_go_off * q) / (1 - (1 - p_go_off) * q)
  
  # Handle edge case where cr = 1 (P_miss must be 0)
  if(cr == 1) P_miss_event <- 0
  
  P_catch_event <- 1 - P_miss_event
  
  # 4. Expected Reward Per Trial
  # Base Reward (A) + (Prob of Catch * Bonus) - (Prob of Miss * Penalty)
  # Note: Win_A happens every trial. Bonus/Penalty happen once per event.
  E_Reward_Trial <- Win_A + 
    (lambda_event * P_catch_event * Win_B) - 
    (lambda_event * P_miss_event * Loss_B)
  
  # 5. Expected Time Per Trial
  # We calculate the weighted average of time spent.
  # T_base:  Time when NOT checking
  # T_check: Time when Checking (standard check cost)
  # T_extra: The extra cost of responding (RT_response - RT_check) happens 
  #          exactly once per CAUGHT event.
  
  T_base  <- (1 - cr) * (RT_A_only + ITI)
  T_check <- cr * (RT_A_B_check + ITI)
  
  # Correction: If we catch a monster, one of those checks was actually a response.
  # We add the difference between Response Time and Check Time.
  T_response_adjustment <- lambda_event * P_catch_event * (RT_A_B_response - RT_A_B_check)
  
  E_Time_Trial <- T_base + T_check + T_response_adjustment
  
  # 6. Total Calculation
  # How many trials fit in the total Time?
  Expected_Trials <- Time / E_Time_Trial
  Total_Reward <- Expected_Trials * E_Reward_Trial
  
  return(Total_Reward)
}


opt <- data.frame(cr = seq(0, 1, 0.001)) %>%
  mutate(Loss_B = list(seq(0, 80, by = 10))) %>%
  #mutate(Loss_B = list(10)) %>%
  unnest(Loss_B) %>%
  rowwise()%>%
  mutate(rew = calc_sidetask(RT_A_only = .7,
                             RT_A_B_check = 2,
                             RT_A_B_response = 2,
                             ITI = 0.2,
                             Win_A = 2,
                             Loss_A = 0,
                             Win_B = 0,
                             Loss_B = Loss_B,
                             p_go_on = 0.1,
                             p_go_off = 0.2,
                             Time = 200,
                             cr = cr)) %>%
  group_by(Loss_B) %>%
  mutate(rel_reward = rew/max(rew))

ggplot(data = opt, aes(x = cr, y = rel_reward, color = Loss_B, group = Loss_B))+
  geom_line()+
  geom_point(data = opt[opt$rel_reward == 1,])+
  coord_cartesian(ylim = c(0, 1))+
  theme_classic()



##################################################

opt <- data.frame(cr = seq(0, 1, 0.001)) %>%
  mutate(Loss_B = 10) %>%
  unnest(Loss_B) %>%
  rowwise()%>%
  mutate(rew = calc_sidetask(RT_A_only = .6,
                             RT_A_B_check = 1.7,
                             RT_A_B_response = 1.7,
                             ITI = 0.2,
                             Win_A = 1,
                             Loss_A = 0,
                             Win_B = 0,
                             Loss_B = Loss_B,
                             p_go_on = 0.1,
                             p_go_off = 0.1,
                             Time = 200,
                             cr = cr)) %>%
  group_by(Loss_B) %>%
  mutate(rel_reward = rew/max(rew))

ggplot(data = opt, aes(x = cr, y = rel_reward, color = Loss_B, group = Loss_B))+
  geom_line()+
  geom_point(data = opt[opt$rel_reward == 1,])+
  coord_cartesian(ylim = c(0, 1))+
  theme_classic()

