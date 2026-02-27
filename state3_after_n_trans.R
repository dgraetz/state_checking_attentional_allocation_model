state3_after_n_trans <- function(p_A2B, 
                                 p_B2A,
                                 p_initial_B = 1, 
                                 p_initial_A_detected = 0,
                                 p_initial_A = 0,
                                 n) {
  
  
  # p_A2B = 0.1
  # p_B2A = 0.1
  # p_initial_B = 1
  # p_initial_A_detected = 0
  # p_initial_A_undetected = 0
  # n = 2
  
  p_A2A <- 1-p_A2B
  p_B2B <- 1-p_B2A
  
  
  #really nice set of videos: https://www.youtube.com/watch?v=IYaOMor9qvE
  #initial state matrix: p(inital state being on), p(inital state being off)
  initial_state <- matrix(c(p_initial_A, p_initial_A_detected, p_initial_B), nrow = 1)
  
  
  
  #transition matrix: 
  #   TO A A_d B       
  #FROM
  #A
  #A_d
  #B
  
  transition <- matrix(c(p_A2A,     0, p_A2B,
                         0    , p_A2A, p_A2B,
                         p_B2A,     0, p_B2B), nrow = 3, byrow = TRUE)
  
  
  initial_state %*% (transition %^% n) %>% 
    as.data.frame() %>%
    rename(p_state_A = V1, p_state_A_detected = V2, p_state_B = V3) %>%
    return()
  
}