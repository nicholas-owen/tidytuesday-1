library(tidyverse); library(maps); library(viridis)

url <- "https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv"
combined_data <- readr::read_csv(url)

data('state')
states_df <- data.frame(state_code = state.abb, state_name = state.name, stringsAsFactors = F) %>% 
  mutate(state_name = tolower(state_name))

county_data <- map_data("county") %>% 
  rename(county = subregion,
         state_name = region)

combined_data_clean <- combined_data %>%
  separate(location, c('col1','col2','col3','col4'), sep = ' ') %>% 
  mutate(col2 = ifelse(col2 == 'COUNTY', ' ', col2),
         col3 = ifelse(col3 == 'COUNTY', ' ', col3),
         col4 = ifelse(col4 == 'COUNTY', ' ', col4),
         col2 = ifelse(is.na(col2), ' ', col2),
         col3 = ifelse(is.na(col3), ' ', col3),
         col4 = ifelse(is.na(col4), ' ', col4)) %>% 
  unite(county, c('col1','col2','col3','col4'), sep = ' ') %>% 
  mutate(county = tolower(str_trim(county))) %>%
  mutate(county = ifelse(county=='st. croix', 'st croix', county)) %>% 
  rename(state_code = state) %>% 
  left_join(states_df, by=c('state_code'))

county_data_combined <- county_data %>% 
  left_join(combined_data_clean, by=c('state_name','county')) %>%
  filter(state_name=='wisconsin') %>% 
  select(long, lat, group, state_name, driver_race, county,stops_per_year, stop_rate, search_rate, arrest_rate)

county_data_white <- county_data_combined %>% 
  filter(driver_race=='White') %>% 
  select(-driver_race) %>% 
  rename(stops_per_year_white = stops_per_year,stop_rate_white = stop_rate, 
         search_rate_white = search_rate, arrest_rate_white = arrest_rate)

county_data_hispanic <- county_data_combined %>% 
  filter(driver_race=='Hispanic') %>% 
  select(-driver_race) %>% 
  rename(stops_per_year_hisp = stops_per_year,stop_rate_hisp = stop_rate, 
         search_rate_hisp = search_rate, arrest_rate_hisp = arrest_rate)

county_data_black <- county_data_combined %>% 
  filter(driver_race=='Black') %>% 
  select(-driver_race) %>% 
  rename(stops_per_year_black = stops_per_year,stop_rate_black = stop_rate, 
         search_rate_black = search_rate, arrest_rate_black = arrest_rate)

county_all <- county_data_black %>% 
  inner_join(county_data_hispanic, by=c('long', 'lat', 'group', 'state_name','county')) %>% 
  inner_join(county_data_white, by=c('long', 'lat', 'group', 'state_name','county')) %>% 
  group_by(state_name, county) %>% 
  mutate(arrest_rate_minority = (arrest_rate_black + arrest_rate_hisp) / 2,
         arrest_rate_delta_black = arrest_rate_black - arrest_rate_white,
         arrest_rate_delta_hisp = arrest_rate_hisp - arrest_rate_white,
         arrest_rate_delta_minority = arrest_rate_minority - arrest_rate_white,
         stop_rate_minority = (stop_rate_black + stop_rate_hisp) / 2,
         stop_rate_delta_black = stop_rate_black - stop_rate_white,
         stop_rate_delta_hisp = stop_rate_hisp - stop_rate_white,
         stop_rate_delta_minority = stop_rate_minority - stop_rate_white)

wisc_plot <- county_all %>% 
  ggplot(aes(x = long,y = lat, fill = arrest_rate_delta_minority, group = group)) +
  geom_polygon(size = 0.3, color = 'black') +
  coord_map(projection = 'albers', lat0 = 39, lat1 = 45) + 
  scale_fill_viridis(option = 'plasma') +
  theme_minimal() +
  theme(text = element_text(color = 'white', family = 'Andale Mono'),
        panel.background = element_rect(fill = 'black'),
        legend.background = element_rect(fill = 'black'),
        legend.position = 'bottom',
        plot.title = element_text(size = 18),
        plot.background = element_rect(color = 'black', fill = 'black'),
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank(), 
        axis.title = element_blank(), 
        panel.grid = element_blank()) +
  guides(fill = guide_colorbar(
    title = "Arrest Rate Difference",
    title.position = "top" ,
    title.hjust = 0.5,
    barwidth = 16,
    barheight = 0.5
  )) +
  labs(title = 'Disparity in Arrest Rates\nBetween Whites and\nMinorites in Wisconsin',
       caption = 'Data: Stanford Open Policing Project')
wisc_plot

png("~/Desktop/tidytuesday/open-policing/wisconsin.png",
    width = 200, height = 150, res = 500, units = 'mm')
print(wisc_plot)
dev.off()




