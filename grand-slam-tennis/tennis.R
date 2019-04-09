library(tidyverse); library(gganimate)

grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")


#create variable for tournament order, shorten goolagong's name for labeling ease
grand_slams_clean <- grand_slams %>% 
  mutate(tournament_order = case_when(grand_slam=='australian_open' ~ 1,
                                      grand_slam=='french_open' ~ 2,
                                      grand_slam=='wimbledon' ~ 3,
                                      grand_slam=='us_open' ~ 4),
         name = ifelse(name == 'Evonne Goolagong Cawley', 'Evonne Goolagong', name)) %>% 
  arrange(tournament_date)


init_df <- grand_slams_clean %>% 
  filter(year <= 1975) %>%
  group_by(name) %>% 
  filter(rolling_win_count==max(rolling_win_count)) %>% 
  ungroup() %>% 
  top_n(10, wt=rolling_win_count) %>% 
  arrange(desc(rolling_win_count)) %>% 
  select(name,gender, rolling_win_count) %>% 
  mutate(curr_year = 1975,
         ordering = as.double(rev(seq(10:1))) * 1.0)

for (i in 1976:2019) {
  for (j in 1:4) {
    tmp_df <- grand_slams_clean %>% 
      filter(year < i | (year==i & tournament_order <= j)) %>% 
      group_by(name) %>% 
      filter(rolling_win_count==max(rolling_win_count)) %>% 
      ungroup() %>% 
      top_n(10, wt=rolling_win_count) %>%
      select(name, gender, rolling_win_count) %>%
      arrange(desc(rolling_win_count)) %>%
      slice(1:10) %>% 
      mutate(curr_year = i,
             tournament_num = j,
             ordering = as.double(rev(seq(10:1))) * 1.0) 
    
    init_df <- init_df %>%
      bind_rows(tmp_df)
  }
}

#add group ids to use as transition states in gganimate
final_df <- init_df %>% 
  group_by(curr_year, tournament_num) %>% 
  mutate(num = group_indices()) %>% 
  ungroup()


#set font, theme
my_font <- 'Quicksand'
my_background <- 'antiquewhite'
my_theme <- my_theme <- theme(text = element_text(family = my_font),
                              rect = element_rect(fill = my_background),
                              plot.background = element_rect(fill = my_background, color = NA),
                              panel.background = element_rect(fill = my_background, color = NA),
                              panel.border = element_blank(),
                              plot.title = element_text(face = 'bold', size = 20),
                              plot.subtitle = element_text(size = 14),
                              panel.grid.major.y = element_blank(),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_line(color = 'grey75'),
                              panel.grid.minor.x = element_line(color = 'grey75'),
                              legend.position = 'none',
                              plot.caption = element_text(size = 8),
                              axis.ticks = element_blank(),
                              axis.text.y =  element_blank())

theme_set(theme_light() + my_theme)

#make plot
barplot_race <- ggplot(aes(ordering, group = name), data = final_df) +
  geom_tile(aes(y = rolling_win_count / 2, 
                height = rolling_win_count,
                width = 0.9, fill=gender), alpha = 0.9) +
  scale_fill_manual(values = c('#F8AFA8','#74A089')) +
  geom_text(aes(y = rolling_win_count, label = name), family=my_font, nudge_y = -2, size = 3) +
  geom_text(aes(y = rolling_win_count, label = as.character(rolling_win_count)), family=my_font, nudge_y = 0.5) +
  geom_text(aes(x=1,y=18.75, label=paste0(curr_year)), family=my_font, size=8, color = 'gray45') +
  coord_cartesian(clip = "off", expand = FALSE) +
  coord_flip() +
  labs(title = 'Most Grand Slam Singles Championships',
       subtitle = 'Open Era Only',
       caption = 'data source: Wikipedia | plot by @emilykuehler',
       x = '',
       y = '') +
  transition_states(num, 
                    transition_length = 4, state_length = 3) +
  ease_aes('cubic-in-out')

animate(barplot_race, nframes = 1000, fps = 30, width = 600, height = 400, res=80, detail = 3)

anim_save("barplot_race.gif")
















  