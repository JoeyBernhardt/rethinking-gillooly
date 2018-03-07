

library(tidyverse)
library(readxl)
library(janitor)


sor <- read_xlsx("data-raw/soerensen.xlsx") %>% 
	clean_names() %>% 
	mutate(u_max = as.numeric(u_max))


sor %>% 
	filter(taxgrp == "fish") %>% 
	ggplot(aes(x = t_opt_c, y = u_max)) + geom_point() +
	geom_smooth(method = "lm")
