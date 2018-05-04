

library(tidyverse)
library(cowplot)
library(janitor)
library(viridis)
library(broom)


chen <- clean_names(read_csv("data-raw/Chen-metabolic-rate.csv")) %>% 
	rename(temperature = temperature_oc) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15))))

names(chen)

chen %>% 
	ggplot(aes(x = inverse_temp, y = log(rmr_mg_o2_kg_1_h_1), color = population, fill = population)) + geom_point(size = 2) +
	scale_color_viridis_d() +
	scale_fill_viridis_d() +
	scale_x_reverse() + geom_smooth(method = "lm") + facet_wrap( ~ population) +
	ylab("ln(RMR) mgO2/g^0.88") + xlab("Temperature (1/kT)")
ggsave("figures/chen-redband-trout.pdf", width = 8, height = 6)


chen %>% 
	group_by(population) %>% 
	do(tidy(lm(log(rmr_mg_o2_g_0_88_h_1) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View
