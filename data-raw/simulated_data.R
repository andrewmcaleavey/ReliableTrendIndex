## Simulating data to have as a test object

ids <- 1:500  
id_variable <- unlist(lapply(ids, function(x) rep(x, each = sample(2:20, length(ids), replace = TRUE))))
id_data <- id_variable %>% 
  tibble() %>% 
  set_names("id") %>% 
  group_by(id)

num_obs <- id_data %>% 
  summarise(num_obs = n()) 

# True scores
BL_true = rnorm(length(ids))
# final score has a mean of .5, so most people will be increasing
Final_true = rnorm(length(ids), mean = .5)
diff_true = Final_true - BL_true

pre_post <- num_obs %>% 
  mutate(BL_true = BL_true, 
         Final_true = Final_true, 
         diff_true = diff_true)

simulated_data <- left_join(id_data, pre_post, 
                            by = "id") %>% 
  mutate(index = row_number()-1) %>% 
  ungroup() %>% 
  mutate(increment = diff_true / (num_obs-1),
         true_score = BL_true + (index * increment), 
         true_change = case_when(diff_true > 0 ~ "True Increase", 
                                 diff_true < 0 ~ "True Decrease", 
                                 TRUE ~ "True No Change"), 
         # adding measurement error to each observation
         # SD of error is .2
         obs_score = true_score + rnorm(nrow(.), 
                                        mean = 0, 
                                        sd = .2))

usethis::use_data(simulated_data, overwrite = TRUE)
