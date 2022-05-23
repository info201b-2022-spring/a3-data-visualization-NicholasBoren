library("stringr")
library("dplyr")
library("ggplot2")
library("usmap")

raw_df <- read.csv("data/incarceration_trends.csv")
year_current_df <- filter(raw_df, year == max(year, na.rm=T))


#### TOTAL STATS #####

total_jail_pop_by_year <- raw_df %>%
    group_by(year) %>%
    summarize(total_pop=sum(total_jail_pop, na.rm=T)) 

##### BLACK STATS #####

#Mean of the black jail population in the current year
mean_black_pop <- mean(year_current_df$black_jail_pop, na.rm=T)
 
#Min of the black jail pop in the current year
min_back_pop <- min(year_current_df$black_jail_pop, na.rm=T)

#Max of the black jail pop in the current year
min_back_pop <- max(year_current_df$black_jail_pop, na.rm=T)

# State with highest black jail population in the current year
highest_state_black <- year_current_df %>%
    filter(black_jail_pop == max(black_jail_pop, na.rm=T)) %>%
    pull(state, black_jail_pop)

# State with the lowest black jail population in the current year
lowest_state_black <- year_current_df %>%
    filter(black_jail_pop == min(black_jail_pop, na.rm=T)) %>%
    pull(state, black_jail_pop)

# Total black jail population in the current year
total_black_pop <- year_current_df %>%
    summarize(total=sum(black_jail_pop, na.rm=T)) %>%
    pull(total)

black_jail_pop_by_year <- raw_df %>%
    group_by(year) %>%
    summarize(total_pop_black=sum(black_jail_pop, na.rm=T)) 

black_jail_pop_rate_by_year <- raw_df %>%
    group_by(year) %>%
    filter(state == "CA") %>%
    summarize(total_pop_rate_black=sum(black_jail_pop_rate, na.rm=T)) 

##### WHITE STATS #####

#Mean of the white jail population in the current year
mean_white_pop <- mean(year_current_df$white_jail_pop, na.rm=T)
 
#Min of the white jail pop in the current year
min_white_pop <- min(year_current_df$white_jail_pop, na.rm=T)

#Max of the white jail pop in the current year
min_white_pop <- max(year_current_df$white_jail_pop, na.rm=T)

# State with highest white jail population in the current year
highest_state_white <- year_current_df %>%
    filter(white_jail_pop == max(white_jail_pop, na.rm=T)) %>%
    pull(state, white_jail_pop)

# State with the lowest white jail population in the current year
lowest_state_white <- year_current_df %>%
    filter(white_jail_pop == min(white_jail_pop, na.rm=T)) %>%
    pull(state, white_jail_pop)

# Total white jail population in the current year
total_white_pop <- year_current_df %>%
    summarize(total=sum(white_jail_pop, na.rm=T)) %>%
    pull(total)

white_jail_pop_by_year <- raw_df %>%
    group_by(year) %>%
    summarize(total_pop_white=sum(white_jail_pop, na.rm=T)) 

white_jail_pop_rate_by_year <- raw_df %>%
    group_by(year) %>%
    filter(state == "CA") %>%
    summarize(total_pop_rate_white=sum(white_jail_pop_rate, na.rm=T)) 

#### PLOTS ####

time_series <- ggplot() +
    geom_line(data = total_jail_pop_by_year, aes(x=year, y=total_pop, color="total_pop")) +
    geom_line(data = black_jail_pop_by_year, aes(x=year, y=total_pop_black, color="total_pop_black")) +
    geom_line(data = white_jail_pop_by_year, aes(x=year, y=total_pop_white, color="total_pop_white")) +
    labs(title = "Jail population by year", x="year", y="total jail population") + 
    scale_color_discrete(name = "Race", labels=c("Total population", "Total black population", "Total white population"))
    
time_vs_pop_rate <- ggplot() +
    geom_line(data = black_jail_pop_rate_by_year, aes(x=year, y=total_pop_rate_black, color="total_pop_rate_black")) + 
    geom_line(data = white_jail_pop_rate_by_year, aes(x=year, y=total_pop_rate_white, color="total_pop_rate_white")) +
    labs(title = "Jail population rate by year", x="year", y="total jail population rate") +
    scale_color_discrete(name = "Race", labels=c("Total black population rate", "Total white population rate"))

### MAP ###

black_pop_per_state <- year_current_df %>%
    group_by(state) %>%
    summarize(pop=sum(black_jail_pop, na.rm=T), total=max(total_pop),    
              mutate=sum(total_pop / black_jail_pop))
        
map <- plot_usmap(
    data=black_pop_per_state, values= "pop", color="black") +
    theme(legend.position = "right") + labs(title = "United States black jail population density in 2018")
                                            