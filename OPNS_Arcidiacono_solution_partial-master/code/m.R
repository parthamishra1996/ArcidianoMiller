source('header.R')

theta = c(10, 0.01)

l = list(
      theta = theta,
      intercepts = 
        tibble(
          s = c(1, 2),
          pi.s = c(.25, .75)
        ),
      beta = .99
    )  %>%
    sim_data %T>%
    saveRDS('../variables/sim_data.rds') %>% 
    plot_sim_data
  
sim_data <- readRDS('../variables/sim_data.rds')

l %>% 
  mle(theta = theta)
  