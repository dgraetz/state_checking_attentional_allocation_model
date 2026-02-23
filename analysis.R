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
         current_reward = cumsum(current_reward))


agg <- data %>%
  group_by(ID, Block, B_Loss) %>%
  summarize(RT_task_checked = mean(RT_task[State_checked == 1]),
            RT_task_checked_response = mean(RT_task[State_checked == 1 & Response_state != ""]),
            RT_task_not_checked = mean(RT_task[State_checked == 0]),
            Check = mean(State_checked)) %>%
  group_by(ID, B_Loss) %>%
  summarize(RT_task_checked = mean(RT_task_checked),
            RT_task_checked_response = mean(RT_task_checked_response),
            RT_task_not_checked = mean(RT_task_not_checked),
            Check = mean(Check))

agg

ggplot(agg, aes(x = B_Loss, y = Check, group = 1))+
  geom_line()+
  theme_classic()



cost_agg <- data %>%
  group_by(ID, Block, B_Loss) %>%
  summarize(RT_task_checked = mean(RT_task[State_checked == 1]),
            RT_task_not_checked = mean(RT_task[State_checked == 0]),
            Check = mean(State_checked),
            Cost = (RT_task_checked - RT_task_not_checked)/RT_task_not_checked,
            earnings = current_reward[n()])

ggplot(cost_agg, aes(x = Cost, y = Check, color = B_Loss))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()


source("my_sim3.R")


# 1. Setup the cluster
# Detect cores and leave one free for the OS
library(foreach)
library(doParallel)
n_cores <- parallel::detectCores() - 1 
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# 2. Execute the parallel loop
# We store the output of the loop into a list called 'results_list'
results_list <- foreach(i = 1:nrow(cost_agg), 
                        .packages = c('tidyverse'), 
                        .export = c('sim_sidetask_vec')) %dopar% {
                          
                          crs <- seq(0, 1, 0.01)
                          opt <- list()
                          
                          # Inner loop runs sequentially within each worker (usually faster this way)
                          for (j in 1:length(crs)){
                            
                            # Note: Accessing cost_agg values directly by index i
                            opt[[j]] <- sim_sidetask_vec(RT_A_only = cost_agg$RT_task_not_checked[i],
                                                         RT_A_B_check = cost_agg$RT_task_checked[i], 
                                                         RT_A_B_response = cost_agg$RT_task_checked[i], 
                                                         ITI = 0.2, 
                                                         Win_A = 1, 
                                                         Loss_A = 0, 
                                                         Win_B = 0, 
                                                         Loss_B = cost_agg$B_Loss[i] %>% as.character() %>% as.numeric(), 
                                                         p_go_on = 0.1, 
                                                         p_go_off = 0.1, 
                                                         Time = 90, 
                                                         cr = crs[j], 
                                                         reps = 500) %>%
                              mutate(cr = crs[j])
                          }
                          
                          # Process the data for this specific row (i)
                          # The result of this block is what gets returned to 'results_list'
                          opt %>%
                            bind_rows() %>%
                            group_by(rep, cr) %>%
                            summarize(Rew_total = Rew_total[n()], .groups = "drop_last") %>%
                            group_by(cr) %>%
                            summarize(Rew_total = mean(Rew_total), .groups = "drop")
                        }

stopCluster(cl)

# 4. Assign the results back to your dataframe
# Since foreach returns a list, we can assign it directly to the list-column
cost_agg$optimality <- results_list


cost_agg2 <- cost_agg %>%
  unnest(optimality) %>%
  group_by(ID, Block) %>%
  mutate(opt_check = cr[which.max(Rew_total)],
         opt_earnings = max(Rew_total),
         predicted_earnings = Rew_total[which.min(abs(Check - cr))],
         rel_Rew_total = Rew_total/max(Rew_total))

ggplot(cost_agg2, aes(x = cr, y = Rew_total, group = Block, color = factor(B_Loss)))+
  geom_line()+
  geom_point(data = cost_agg2, aes(x = opt_check, opt_earnings), pch = 5, size = 4)+
  geom_point(data = cost_agg2, aes(x = Check, earnings), pch = 1, size = 3)+
  facet_wrap(~B_Loss, ncol = 1)+
  scale_color_discrete(name = "Loss cond")+
  theme_classic()

ggplot(cost_agg2, aes(x = cr, y = rel_Rew_total, group = Block))+
  geom_line()+
  facet_wrap(~B_Loss, ncol = 1)+
  coord_cartesian(ylim = c(0, 1))+
  theme_classic()

cost_agg_summary <- cost_agg2B_Losscost_agg_summary <- cost_agg2 %>%
  group_by(ID, Block) %>%
  slice(1)


ggplot(cost_agg_summary, aes(x = predicted_earnings, y = earnings))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()


cor(cost_agg_summary$predicted_earnings, cost_agg_summary$earnings)
