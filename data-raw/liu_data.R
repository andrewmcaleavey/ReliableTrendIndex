## code to prepare `liu_data` 

# from @article{liu2019treatment,
# title={Is treatment working? Detecting real change in the treatment of child and adolescent depression},
# author={Liu, Freda F and Adrian, Molly C},
# journal={Journal of the American Academy of Child \& Adolescent Psychiatry},
# volume={58},
# number={12},
# pages={1157--1164},
# year={2019},
# publisher={Elsevier}
# }

liu_2019_phq9a <- c(18, 21, 14, 17, 14, 13, 14, 11, 13, 8, 5, 6)
liu_data <- data.frame(obs = liu_2019_phq9a, time = 1:length(liu_2019_phq9a))

usethis::use_data(liu_data, overwrite = TRUE)
