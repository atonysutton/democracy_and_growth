setwd('C:/Tony/git_workspace/democratic_erosion')

#declare arbitrary thresholds
dem_threshold = 0.5

#load libraries
library(tidyverse)

#load and shape data ----

##regime type data from V-Dem - one row per country year 
#vdem <- read_csv('./data/V-Dem-CY-Full+Others-v11.1.csv')

##trim to a manageable file size by selecting only relevant variables
#vdem <- vdem %>% select(country_name, country_text_id, year, COWcode,
#                        v2x_polyarchy, v2x_libdem, v2x_partipdem, v2x_delibdem, v2x_egaldem,
#                        v2xnp_client, v2elvotbuy, v2dlencmps, v2psprlnks, v2x_elecreg,
#                        v2smonex, v2smmefra, v2smgovdom, v2smpardom, v2smfordom, 
#                        v2cacamps, v2smpolsoc, v2smpolhate,
#                        e_migdpgro, e_migdppc,
#                        e_mipopula, e_miurbani,
#                        e_total_fuel_income_pc, e_total_resources_income_pc)

##write_csv(vdem, file = './data/vdem_trimmed.csv')

vdem <- read_csv('./data/vdem_trimmed.csv')

vdem <- vdem %>% filter(!is.na(v2x_polyarchy), !is.na(e_migdppc), !is.na(e_migdpgro), year >= 1946)


#create reference table of standardized growth at each (logged) GDP level ----
stg <- vdem %>%
  group_by(year, gdp_log = round(log(e_migdppc), digits = 0)) %>%
  summarize(count = n(), growth_refsd = sd(e_migdpgro))

##check missingness
sum(is.na(stg$growth_refsd))

##interpolate missing data by using nearby years at same logged gdp
for (i in seq_along(stg$growth_refsd)){
  if(!is.na(stg$growth_refsd[i])) next #skip rows where datum is present
  
  working_df <- stg %>% filter(gdp_log >= (stg$gdp_log[i] - 1),
                               gdp_log <= (stg$gdp_log[i] + 1),
                               year >= (stg$year[i] - 1),
                               year <= (stg$year[i] + 1))
  
  stg$growth_refsd[i] <- mean(working_df$growth_refsd, na.rm = TRUE)
}

##recheck missingness
stg %>% filter(is.na(growth_refsd))
##remove infinite case
stg <- stg %>% filter(!is.na(growth_refsd))
##remove count column
stg <- stg %>% select(-count)

#convert yearly economic growth into standard terms ----
vdem <- vdem %>% mutate(gdp_log = round(log(e_migdppc), digits = 0))
vdem <- vdem %>% filter(gdp_log > -Inf)
summary(vdem$gdp_log)
vdem <- vdem %>% left_join(stg, by = c('year', 'gdp_log'))

summary(vdem$growth_refsd)

vdem <- vdem %>% mutate(growth_std = e_migdpgro / growth_refsd)
summary(vdem$growth_std)
vdem <- vdem %>% filter(growth_std != Inf,
                        growth_std != -Inf)


#compare standardized economic growth by regime type

ggplot(data = vdem, 
       aes(x = growth_std, 
           fill = v2x_polyarchy >= dem_threshold,
           color = v2x_polyarchy >= dem_threshold))+
  geom_density(bw = .1, alpha = 0.1, size = 2.3)+
  coord_cartesian(xlim = c(-3, 3))+
  scale_color_manual(values = c('firebrick', 'dodgerblue'))+
  scale_fill_manual(values = c('firebrick', 'dodgerblue'))+
  theme_minimal()+
  labs(title = "The Democratic Edge in Economic Performance",
       subtitle = ' in 1946-2018, standardized by GDP per capita and year',
       y = '',
       x = 'GDP Growth (in Standard Deviations)')+
  annotate('text', label = 'Democracies', color = 'dodgerblue', size = 8, x = 2.2, y = 0.35)+
  annotate('text', label = 'Autocracies', color = 'firebrick', size = 8, x = -1.2, y = 0.35)+
  theme(title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(size = 18, face = 'plain'),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 18, margin = margin(t = 8)),
        legend.position = 'none')

ggsave(filename = "C:/Tony/Political Science MA/data/growth_distribution_by_dem.jpg",
       width = 10,
       height = 6,
       units = 'in')


#illustrative stat to explain scale ----
median_2018_growth <- 
  vdem %>% 
  filter(year == 2018) %>% 
  summarize(median_2018_growth = median(e_migdppc, na.rm = TRUE)) %>%
  pull(median_2018_growth)

stg %>% filter(year == 2018,
               gdp_log == round(log(median_2018_growth), digits = 0)) %>%
  pull(growth_refsd)
 ##one standard deviation in GDP growth at the median GDP per capita level in 2018 is 3.9 percentage points


#add example country-years to show folly of cherry-picking successful autocracy years
china1961 <- vdem %>% filter(country_name == 'China', year == 1961) %>% pull(growth_std)
china2001 <- vdem %>% filter(country_name == 'China', year == 2001) %>% pull(growth_std)

ggplot(data = vdem, 
       aes(x = growth_std, 
           fill = v2x_polyarchy >= dem_threshold,
           color = v2x_polyarchy >= dem_threshold))+
  geom_density(bw = .1, alpha = 0.1, size = 2.3)+
  coord_cartesian(xlim = c(-3, 3))+
  scale_color_manual(values = c('firebrick', 'dodgerblue'))+
  scale_fill_manual(values = c('firebrick', 'dodgerblue'))+
  theme_minimal()+
  labs(title = "The Democratic Edge in Economic Performance",
       subtitle = ' in 1946-2018, standardized by GDP per capita and year',
       y = '',
       x = 'GDP Growth (in Standard Deviations)')+
  annotate(geom = 'point', size = 5, color = 'firebrick', x = china1961, y = 0.28)+
  annotate('text', label = 'China 1961', color = 'firebrick', size = 7, x = china1961 + 0.7, y = 0.32)+
  annotate(geom = 'point', size = 5, color = 'firebrick', x = china2001, y = 0.28)+
  annotate('text', label = 'China 2001', color = 'firebrick', size = 7, x = china2001 + 0.7, y = 0.32)+
  theme(title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(size = 18, face = 'plain'),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 18, margin = margin(t = 8)),
        legend.position = 'none')

ggsave(filename = "C:/Tony/Political Science MA/data/growth_distribution_by_dem_plus_examples.jpg",
       width = 10,
       height = 6,
       units = 'in')
