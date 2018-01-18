
library(tidyverse)
library(cowplot)
library(viridis)
library(broom)
library(patchwork)

unicells_raw <- read_csv("data-raw/gillooly_unicells.csv")


unicells <- unicells_raw %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) 


mr_plot <- unicells %>% 
	ggplot(aes(x = inverse_temp, y = log(metabolic_rate), color = species)) + geom_point() +
	geom_smooth(method = "lm", color = "black") +
	geom_smooth(method = "lm") +
	scale_x_reverse(sec.axis = sec_axis(~((1/(.*8.62 * 10^(-5)))-273.15))) +
	xlab("Inverse temperature (1/kT)") +
	ylab("Log metabolic rate (watts/gram)") +
	scale_color_viridis(discrete = TRUE) 

all_cells_params <- unicells %>% 
	do(tidy(lm(log(metabolic_rate) ~ inverse_temp, data = .), conf.int = TRUE)) %>% 
	mutate(species = "all_species")

species_params <- unicells %>% 
	group_by(species) %>% 
	do(tidy(lm(log(metabolic_rate) ~ inverse_temp, data = .), conf.int = TRUE)) 

params <- bind_rows(all_cells_params, species_params)


params_plot <- params %>% 
	filter(!is.na(conf.low)) %>%
	filter(term == "inverse_temp") %>%
	ggplot(aes(x = species, y = estimate)) + geom_point() +
	geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width  = 0.1) +
	geom_hline(yintercept = -0.65) +
	geom_point(aes()) + ylab("Activation energy (eV)") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))


giant_plot <- mr_plot + params_plot + plot_layout(ncol = 2)
ggplot2::ggsave(plot = giant_plot, filename = "figures/unicells.png", width =16, height = 5)
	
