library(tidyverse)
library(janitor)
library(plotrix)

ehn <- clean_names(read_csv("data-raw/ehnes-2011.csv", skip = 2, col_names = TRUE))

ehn2 <- ehn %>% 
	filter(!is.na(group_2)) %>% 
	mutate(id = rownames(.)) %>%
	separate(group_2, into = c("ID", "group_2"), remove = TRUE, sep = " ", extra = "merge" ) %>% 
	mutate(group_2 = ifelse(is.na(group_2), ID, group_2)) %>% 
	select(-ID) %>% 
	select(-x10) %>% 
	mutate(species = as.factor(species))

str(ehn2)
unique(ehn2$species)

ehn2 %>% 
	ggplot(aes(x = temperature_c, y = log(j_h*(weight_mg^0.75)), color = species)) + geom_point()  +
	facet_wrap( ~ group_2)


unique(ehn2$temperature_c)

mult <- ehn2 %>% 
	unite(col = species_study, sep = "_", remove = FALSE, species, original_study) %>% 
	# group_by(original_study, species) %>%
	distinct(species_study, species, temperature) %>% 
	group_by(species) %>% 
	tally() %>% 
	filter(n > 1)

ms <- mult$species

ehn2 %>% 
	filter(species %in% ms) %>% 
	unite(col = species_study, sep = "_", remove = FALSE, species, original_study) %>% 
	ggplot(aes(x = temperature_c, y = log(j_h*(weight_mg^0.75)), color = species_study)) + geom_point()  +
	facet_wrap( ~ group_2) + geom_smooth(method = "lm") + ylab("Mass normalized metabolic rate (j/g^3/4)") + xlab("Temperature (°C)") +
	theme(legend.position = "none")


ehn2 %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature_c+273.15)))) %>% 
	filter(species %in% ms) %>% 
	unite(col = species_study, sep = "_", remove = FALSE, species, original_study) %>% 
	group_by(species_study, species) %>% 
	do(tidy(lm(log(j_h*(weight_mg^0.75)) ~ inverse_temp, data = .), conf.int = TRUE)) %>%
	filter(term == "inverse_temp") %>% 
	filter(!is.na(std.error)) %>%
	ungroup() %>% 
	ggplot(aes(x = reorder(species, estimate), y = estimate)) + geom_point() +
	# geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
	geom_hline(yintercept = -0.65) +
	geom_hline(yintercept = -0.711218 + 0.05936081, color = "blue") +
	geom_hline(yintercept = -0.711218, color = "pink") +
	geom_hline(yintercept = -0.711218 - 0.05936081, color = "blue") +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	ylab("Respiration activation energy (eV)")
ggsave("figures/ehnes_Ea.pdf", width = 10, height = 10)


ehn2 %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature_c+273.15)))) %>% 
	filter(species %in% ms) %>% 
	unite(col = species_study, sep = "_", remove = FALSE, species, original_study) %>% 
	group_by(species_study, species) %>% 
	do(tidy(lm(log(j_h*(weight_mg^0.75)) ~ inverse_temp, data = .), conf.int = TRUE)) %>% 
	filter(term == "inverse_temp") %>% 
	filter(!is.na(std.error)) %>% 
	ungroup() %>% 
	summarise_each(funs(mean, std.error), estimate) %>% View
	