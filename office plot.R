library(tidyverse)
library(ggbeeswarm)
library(paletteer)
library(grid)

#Import the dataset
office <- read_csv('/User/shambhavisingh/Office_Episodes_IMDb_Ratings.csv')

#column that has the seasons as strings for axis ticks
office <- office %>% mutate(season_name =paste0('Season ', season))
summary(office)

#theme
theme_set(theme_minimal())
theme <- theme_update(text = element_text(size = 13),
                      plot.title = element_text(size = 40, color = "gray20"),
                      plot.title.position = "plot",
                      axis.text = element_text(size = 16),
                      axis.title.x = element_text(size = 20),
                      axis.line.x = element_line(color = "gray80"),
                      axis.line.y = element_line(color = "gray80"),
                      panel.grid.minor = element_blank(),
                      plot.margin = margin(15, 30, 15, 15))
# Base plot
base <- ggplot(data = community,aes(fct_rev(season_name),rating))+
  geom_blank() +
  labs(title = "Office Episodes", x = "", y = "IMDb Rating") + 
  #geom_vline(xintercept = 2.5) +
  geom_rect(data = community, xmin = 1.5,
            xmax = 2.5, ymin = 6, ymax = 10,fill = "#EBEBEB",alpha = 0.1) +
  geom_quasirandom(aes(fct_rev(season_name),rating,color = season_name),size=4,alpha=0.7,show.legend = FALSE) +
  scale_y_continuous(limits = c(6,10),expand = c(0,0)) +
  scale_color_paletteer_d("ggsci::default_jco") +
  coord_flip()
base

#Annotations
arrows1 <- tribble(~label,       ~y1,    ~y2,   ~x1,   ~x2,
                   "Michael",   9.85,   9.8,    3.4,    3)
arrows2 <- tribble(~label,       ~y1,    ~y2,   ~x1,   ~x2,
                   "Finale",   9.5,   9.8,    0.65,    1.05)
base +
  annotate("text", x = 2.1, y = 6.1, label = "Season just after \nlead \ncharacter Michael left", hjust = 0) +
  annotate("text", x = 0.65, y = 8.8, label = "Emotional Consequences of\nBroadcast Television", hjust = 0)+
  annotate("text", x = 3.8, y = 9.41, label = "Goodbye, Michael", hjust = 0)+
  annotate("text", x = 3.6, y = 9.41, label = "Michael Soctt's", hjust = 0)+
  annotate("text", x = 3.4, y = 9.41, label = "last appearance", hjust = 0)+
  geom_curve(data = arrows1, aes(x=x1, y=y1, xend=x2, yend=y2),
             arrow = arrow(length = unit(0.07, "inch")),
             curvature=-0.4, color="grey70")+
  geom_curve(data = arrows2, aes(x=x1, y=y1, xend=x2, yend=y2),
             arrow = arrow(length = unit(0.07, "inch")),
             curvature=0.4, color="grey70")
