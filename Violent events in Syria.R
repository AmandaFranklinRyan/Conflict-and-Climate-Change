library(tidyverse)
library(plotly)
library(sf)
library(magick) #for videos

syria_data <- read_csv("Raw Data\\Syria ACLED.csv")
syria_regions_2 <- sf::read_sf("Raw Data\\Admin 2 boundaries.json")

key_variables <- syria_data %>% 
  select(event_date,year,disorder_type:actor2,notes, admin1:longitude)

# 1. Exploratory Data Analysis --------------------------------------------

common_disorder <- table(key_variables$disorder_type)
# Most common event type is Political violence

common_event <- table(key_variables$event_type)
#Battles and explosions/remote violence are most common
# Violence against civilians is also common, but relatively few riots and protests

common_sub_event <- table(key_variables$sub_event_type)
#for analysis drop non fire related violence e.g. abduction/forced disappearance,
#arrests, change to group/activity,peaceful protest,sexual violence, non violent
# transfer of territory, agreement

# 2. Plot violent events timeseries --------------------------------------------------

peaceful_event_types <- c("Abduction/forced disappearance","Arrests","Change to group/activity",
                          "Non-violent transfer of territory","Peaceful protest","Agreement",
                          "Sexual violence")

violent_events <- key_variables %>% 
  filter(!sub_event_type %in% peaceful_event_types) %>% 
  mutate(date=lubridate::parse_date_time(event_date,c("%d %Om %Y")))

timeseries_data <- violent_events %>% 
  group_by(date, event_type) %>% 
  summarise(`Violent events`=n()) %>% 
  mutate(sub_event_type=as.factor(event_type)) %>% 
  ungroup()

time_series <- plot_ly(timeseries_data, x=~date, y=~`Violent events`,type="bar", color=~event_type) %>% 
  layout(title= "Violent Events in Syria")

# Entire time period has almost continuous shelling/missile attacks
# Air/drone strikes more common before 2020
# 2020 onwards has steady baseline of armed clashes
#Violent demonstrations seem to have been increasing since 2022

# 3. Geographical Distribution of Violence --------------------------------

syria_data %>% 
  select(admin2) %>% 
  distinct() %>% 
  count()
# 14 regions in admin1
# 62 regions in admin2
# 246 regions in admin3

#Join geographic and ACLED data (admin2)

geo_admin_2 <- syria_regions_2 %>% 
  select(Shape_Leng:ADM2_PCODE, geometry) %>% 
  mutate(ADM2_EN=str_replace_all(ADM2_EN,"-"," ")) %>% 
  mutate(ADM2_EN=str_replace_all(ADM2_EN,"'",""))

# Create data for violent events map
violent_df <- violent_events %>% 
  group_by(admin2,year) %>% 
  summarise(Count=n()) %>% 
  ungroup() 

geo_conflict_df_2 <- left_join(geo_admin_2,violent_df, by=c("ADM2_EN"="admin2")) %>%  #sf object first
  mutate(ADM2_EN=as.factor(ADM2_EN))

# Plot distribution of violent events in 2024

violent_df_2024 <- violent_df %>% 
  filter(year=="2024")

dist_events_2024 <- plot_ly(violent_df_2024, y=~fct_reorder(admin2,Count), 
                            x=~Count,type="bar", orientation="h") %>% 
  layout(title= "Number of Violent Events (2024)",
         yaxis=list(title=""),
         xaxis=list(title=""))


# Plot simple regional map
ggplot(geo_conflict_df_2) +
  geom_sf(aes(fill = Count), linewidth = 0.2,alpha=0.9) +
  theme_void()+
  scale_fill_distiller(
    palette = "YlOrRd",
    trans = c("reverse"), breaks = c(1, 50, 100, 250, 500, 1000),
    name = "Number of Violent Events",
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth = unit(12, units = "mm"),
      label.position = "bottom",
      title.position = "top",
      nrow = 1
    ))+
  labs(
    title = "Violent Conflict in Syria in 2024",
    subtitle = "Number of violent events per administrative region",
    caption = "Data: ACLED | Non-violent events and protests removed"
  )+
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(
      size = 18, hjust = 0.01, color = "#4e4d47",
      margin = margin(
        b = -0.1, t = 0.4, l = 1,
        unit = "cm"
      )
    ),
    plot.subtitle = element_text(
      size = 14, hjust = 0.01,
      color = "#4e4d47",
      margin = margin(
        b = -0.1, t = 0.33, l = 1,
        unit = "cm"
      )
    ),
    plot.caption = element_text(
      size = 10,
      color = "#4e4d47",
      margin = margin(
        b = 0.3, r = -99, t = 0.3,
        unit = "cm"
      )
    ),
    legend.position = c(0.7, 0.09)
  )

# 3. Create video from all years ------------------------------------------

#Create dataframe containing number of violent events per year
violent_years_df <- violent_events %>% 
  group_by(admin2,year) %>% 
  summarise(Count=n()) %>% 
  ungroup() 

#Create list of all years in dataset
years <- violent_years_df %>% 
  select(year) %>% 
  distinct() %>% 
  pull()

#Loop creating violence map for every year
for (current_year in years){ 
  
  current_df <- geo_conflict_df_2 %>% 
    filter(year==current_year)
  
  current_title <- paste("Violent Conflict in Syria in ", current_year,sep="")
  
  current_plot <-ggplot(current_df) +
    
    geom_sf(aes(fill = Count), linewidth = 0.2,alpha=0.9) +
    theme_void()+
    scale_fill_distiller(
      palette = "YlOrRd",
      trans = c("reverse"), breaks = c(100, 500, 1000, 2000, 3000, 5000),
      limits = c(5000, 1),
      name = "Number of Violent Events",
      guide = guide_legend(
        keyheight = unit(3, units = "mm"),
        keywidth = unit(9, units = "mm"),
        label.position = "bottom",
        title.position = "top",
        nrow = 1
      ))+
    labs(
      title = current_title,
      subtitle = "Number of violent events per administrative region",
      caption = "Data: ACLED | Non-violent events and protests removed"
    )+
    theme(
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.title = element_text(
        size = 16, hjust = 0.01, color = "#4e4d47",face="bold",
        margin = margin(
          b = -0.1, t = 0.4, l = 1,
          unit = "cm"
        )
      ),
      plot.subtitle = element_text(
        size = 12, hjust = 0.01,
        color = "#4e4d47",
        margin = margin(
          b = -0.1, t = 0.33, l = 1,
          unit = "cm"
        )
      ),
      plot.caption = element_text(
        size = 10,
        color = "#4e4d47",
        margin = margin(
          b = 0.3, r = -99, t = 0.3,
          unit = "cm"
        )
      ),
      legend.position = c(0.7, 0.09)
    )
  current_file <- paste("Visualisations\\Syria_Map",as.character(current_year),".png",sep="")
  
  ggsave(current_file, plot = current_plot, width = 10.42, height = 5.21, device="png")
  
}

#Collate still maps into an animated GIF showing evolution of violent events

syria_gif <- list.files(path = "Visualisations\\Map Animation", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_write("syria_violence.gif") 

