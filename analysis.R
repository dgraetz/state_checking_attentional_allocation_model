library(tidyverse)
library(rio)

files <- list.files("../data/", pattern = "txt", full.names = TRUE)

data <- lapply(files, function(x){
  import(x) %>% 
    mutate(ID = str_extract(x, "[[:digit:]]+(?=_monsters)") %>% factor(), 
           .before = Block)
}) %>%
  bind_rows()

data <- data %>%
  filter(Is_practice == 0) %>%
  filter(!is.nan(RT_task)) %>%
  filter(Trial > 1) %>%
  group_by(ID, Block) %>%
  mutate(Task_rew = Correct * A_Win,
         missed_factor = ifelse(last_trial_on_state == 1 & Detected_current_state == 0, 1, 0),
         Monster_loss =  missed_factor * B_Loss,
         current_reward = Task_rew - Monster_loss,
         current_reward = cumsum(current_reward)) %>%
  ungroup() %>%
  mutate(ITI = ifelse(ID == 100, 0.1, ITI),
         Placeh = ifelse(ID == 100, 0, Placeh),
         Placeh = factor(Placeh),
         ITI = factor(ITI),
         B_Loss = factor(B_Loss),
         ID = factor(ID))


agg_ID <- data %>%
  group_by(ID, Block, B_Loss, ITI, Placeh) %>%
  summarize(RT_task_checked = mean(RT_task[State_checked == 1]),
            RT_task_nc = mean(RT_task[State_checked == 0]),
            Check = mean(State_checked)) %>%
  group_by(ID, B_Loss, ITI, Placeh) %>%
  summarize(RT_task_checked_M = mean(RT_task_checked),
            RT_task_checked_SD= sd(RT_task_checked),
            RT_task_nc_M = mean(RT_task_nc),
            RT_task_nc_SD = sd(RT_task_nc),
            Check = mean(Check),
            abs_cost = RT_task_checked_M - RT_task_nc_M)


agg <- agg_ID %>%
  group_by(B_Loss, ITI, Placeh) %>%
  summarize(RT_task_checked_M = mean(RT_task_checked_M),
            RT_task_nc_M = mean(RT_task_nc_M),
            Check = mean(Check))

# In Loss = 2, you should not check; 

# At Loss = 10
# cr_a  cr_b is_max is_max_same
# <dbl> <dbl>  <dbl>       <dbl>
#   1 0.1   0.225      1           0
#   2 0.175 0.175      0           1


# At Loss = 30
# cr_a  cr_b is_max is_max_same
# <dbl> <dbl>  <dbl>       <dbl>
#   1  0.25 0.575      1           0
#   2  0.45 0.45       0           1



ggplot(agg_ID, aes(x = B_Loss, y = Check, group = interaction(ID, Placeh), color = ID, linetype = Placeh))+
  geom_line()+
  geom_line(data = agg, aes(x = B_Loss, y = Check, group = interaction(Placeh), linetype = Placeh), inherit.aes = FALSE, lwd = 1.5)+
  facet_wrap(~ITI)+
  theme_classic()



agg_ID_ph_loss <- data %>%
  group_by(ID, Placeh, B_Loss) %>%
  summarize(Check = mean(State_checked))


ggplot(agg_ID_ph_loss, aes(x = B_Loss, y = Check, group = interaction(ID, Placeh), color =ID, linetype = factor(Placeh)))+
  geom_line()+
  theme_classic()


agg_ID_iti <- data %>%
  group_by(ID, ITI) %>%
  summarize(Check = mean(State_checked))


ggplot(agg_ID_iti, aes(x = ITI, y = Check, group = ID, color = ID))+
  geom_line()+
  theme_classic()




# Let's look at trial sequences

data %>%
  group_by(ID, Trial) %>%
  summarize(p_On = mean(StateOn)) %>%
  group_by(Trial) %>%
  summarize(p_On = mean(p_On)) %>%
  ggplot(aes(x = Trial))+
  geom_line(aes(y = p_On))+
  geom_line(aes(y = 1-p_On))+
  scale_y_continuous(limits = c(0, 1))+
  theme_classic()



check_by_state_ID <- data %>%
  group_by(ID, Block) %>%
  mutate(check_group = cumsum(State_checked)) %>%
  group_by(ID, Block, check_group) %>%
  mutate(state_at_check = StateOn[1]) %>%
  filter(check_group != 0) %>%
  group_by(ID, Block, B_Loss, check_group, state_at_check) %>%  
  summarize(avg_run_length = n()) %>%
  group_by(ID, Block, B_Loss, state_at_check) %>%
  summarize(avg_run_length = mean(avg_run_length)) %>%
  group_by(ID, B_Loss, state_at_check) %>%
  summarize(avg_run_length = mean(avg_run_length),
            cc = 1/avg_run_length) %>%
  mutate(state_at_check = factor(state_at_check))


check_by_state_agg <- check_by_state_ID %>%
  group_by(B_Loss, state_at_check) %>%
  summarize(avg_run_length = mean(avg_run_length),
            cc = 1/avg_run_length)


ggplot(check_by_state_ID, aes(x = state_at_check, y = cc, group = interaction(ID, B_Loss), color = B_Loss))+
  geom_line()+
  geom_line(data = check_by_state_agg, aes(group = B_Loss), lwd = 2)+
  theme_classic()


