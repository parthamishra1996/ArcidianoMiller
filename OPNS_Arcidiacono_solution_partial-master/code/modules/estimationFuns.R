#####Notes
#simData:
  #Use %T>% to save intermediary result
  #Use sample_n to sample by likelihood: calculate the likelihood only once
  #If you simulate each person individually, your code will take hours.
  #Sum across mileage levels to express the data in most parsimonious fashion
##########

sim_data <- function(l, num.obs = 10^6) { #Create simulation data
  l %>% 
    calc_value_function %T>% 
    saveRDS('../variables/value_fn.rds') %>%   
    calc_csv(l) %>% 
    calc_log_ccp %>%
    calc_log_duration_prob %>% 
    inner_join(l$intercepts) %>% 
    mutate(prob = pi.s * exp(log.like)) %>% 
    sample_n(
      size = num.obs, 
      weight = prob, 
      replace = TRUE
    ) %>% 
    count(s, x)
}

ccp_preestimate <- function(sim_data){
  sim_data %>% 
    group_by(s) %>% mutate(
      sum_ = sum(n), 
      p_1 = n/sum_,
      p_0 = 1 - p_1,
      log_ccp_1 = p_1 %>% log - (p_0 %>% log %>% lag(default = 0) %>% cumsum),
      V = log_ccp_1[1] - log_ccp_1
    ) %>% ungroup %>%
    select(s, x, log_ccp_1, V)
}

log_like <- function(theta, l){
  sim_data %>% ccp_preestimate %>% select(s, x, V)%>% 
    calc_csv(l) %>% 
    calc_log_ccp %>%
    calc_log_duration_prob %>% 
    summarise_at(
      vars(log.like), 
      list(name = sum)
    )
}

mle <- function(theta, l){
  obj <- optim(par = unlist(theta),
               fn = log_like,
               l = l,
               control = list(fnscale = -1), 
               method = 'L-BFGS-B',
               lower = c(0,0)
  )
  print(obj$par)
}
