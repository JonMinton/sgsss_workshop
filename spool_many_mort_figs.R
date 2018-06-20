
# Produce mortality figures as PNGs 

rm(list = ls())

require(tidyverse)

dta <- read_csv("data/mortality/counts.csv")
codes <- read_csv("data/mortality/country_codes__new.csv")

codes %>% 
  mutate(country_name = stringr::str_replace_all(`"long"`, "\"", "")) %>%
  select(country = short, country_name) %>% 
  right_join(dta) %>% 
  mutate(death_rate = 1000 * death_count / population_count) %>% 
  group_by(sex, country_name) %>% 
  nest() -> dta_nested


create_figures <- function(PLACE_NAME, SEX, DATA){
  
  LIMITS <- c(0.05, 300)
  
  TITLE <- paste0("Mortality, ", SEX, ", ", PLACE_NAME)
  DATA %>% 
    filter(age <= 90) %>% 
    mutate(death_rate = case_when(
      death_rate < LIMITS[1] ~ LIMITS[1],
      death_rate > LIMITS[2] ~ LIMITS[2],
      TRUE ~ death_rate
    )) -> DATA 

  
    DATA %>% 
    ggplot(aes(x = year, y = age, fill = death_rate)) + 
    geom_tile() + 
    coord_fixed() + 
    xlim(c(1850, 2015)) + 
    scale_fill_gradientn(
      "Death rate\n/1000",
      colours = scales::brewer_pal(palette = "Paired")(12), 
      trans = "log", 
      breaks = c(0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200),
      limits = c(0.05, 300)
    )  +
    geom_abline(slope = 1, intercept = seq(-2010, -1750, by = 10), linetype = "dashed", alpha = 0.5) + 
    theme_minimal() + 
    geom_vline(xintercept = seq(1850, 2010, by = 10), linetype = "dashed", alpha = 0.5) + 
    geom_hline(yintercept = seq(0, 90, by = 10), linetype = "dashed", alpha = 0.5) + 
    labs(
      x = "Year", 
      y = "Age in single years", 
      title = TITLE,
      caption = "Source: Human Mortality Database") -> fig
  

  return(fig)
}



dta_nested %>% 
  mutate(figs = pmap(list(PLACE_NAME = country_name, SEX = sex, DATA = data), create_figures)) -> dta_nested


pdf("figures/mortality_gridded.pdf", paper = "a4r", height = 18, width = 25)
lapply(X = dta_nested[["figs"]], FUN = plot)
dev.off()



create_figures_ungridded <- function(PLACE_NAME, SEX, DATA){
  
  LIMITS <- c(0.05, 300)
  
  TITLE <- paste0("Mortality, ", SEX, ", ", PLACE_NAME)
  DATA %>% 
    filter(age <= 90) %>% 
    mutate(death_rate = case_when(
      death_rate < LIMITS[1] ~ LIMITS[1],
      death_rate > LIMITS[2] ~ LIMITS[2],
      TRUE ~ death_rate
    )) -> DATA 
  
  
  DATA %>% 
    ggplot(aes(x = year, y = age, fill = death_rate)) + 
    geom_tile() + 
    coord_fixed() + 
    xlim(c(1850, 2015)) + 
    scale_fill_gradientn(
      "Death rate\n/1000",
      colours = scales::brewer_pal(palette = "Paired")(12), 
      trans = "log", 
      breaks = c(0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200),
      limits = c(0.05, 300)
    )  +
    theme_minimal() + 
    labs(
      x = "Year", 
      y = "Age in single years", 
      title = TITLE,
      caption = "Source: Human Mortality Database") -> fig
  
  
  return(fig)
}


dta_nested %>% 
  mutate(figs_ungridded = pmap(list(PLACE_NAME = country_name, SEX = sex, DATA = data), create_figures_ungridded)) -> dta_nested


pdf("figures/mortality_ungridded.pdf", paper = "a4r", height = 18, width = 25)
lapply(X = dta_nested[["figs_ungridded"]], FUN = plot)
dev.off()

