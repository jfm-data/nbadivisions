library(ggplot2)
library(dplyr)
library(data.table)
library(scales)
library(lubridate)
library(RColorBrewer)
library(ggthemes)


standings<-read.csv("nbaatlantic.csv")
head(standings)



ggplot(data = standings, aes(x = Year, y = Div.Rank, group = Team)) +
  geom_line(aes(color = Team, alpha = 1), size = 2) +
  geom_point(aes(color = Team, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:5) +
  scale_x_continuous(breaks = 2010:2019, minor_breaks = 2010:2019, expand = c(.05, .05)) +
  geom_text(data = standings %>% filter(Year == "2010"),
            aes(label = Team, x = 2009.75) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = standings %>% filter(Year == "2019"),
            aes(label = Team, x = 2019.2) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,5)) + 
  labs(x = "Year",
       y = "Rank",
       title = "Regular Season Placement",
       subtitle = "(2010 - 2019)") +
  theme_economist_white()+
  scale_color_manual(values=c("darkgreen","black","darkorange2","blue","darkred"))+
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(size=0.1, color = "grey"),
    axis.ticks.x =element_blank())


bar1<-data.frame(standings %>% 
                 group_by(Team) %>% 
                 summarise(Wins = sum(Wins),Playoff = sum(Playoff)))
bar1

ggplot(bar1, aes(x=Team,y=Wins, fill=Team)) +
  geom_bar(position="dodge", stat="identity") +
  geom_point(aes(y=Playoff*40), size = 6, color = "white")+
  geom_point(aes(y=Playoff*40), size = 2.5, color = "gold3")+
  scale_fill_manual(values=c("darkgreen","black","darkorange2","blue","darkred")) +
  labs(x = "Teams",
       title = "Total Wins & Playoff Appearances",
       subtitle = "(2010 - 2019)") +
  theme_economist_white() +
  theme(legend.position = "none") +
  theme(panel.grid.major.y = element_blank()) +
  scale_y_continuous(name = "Total Wins", 
                     sec.axis = sec_axis(~./40, name = "Playoff Appearances"),
                     expand = c(0, 0), limits = c(0,485)) + 
  theme(
    axis.title.y = element_text(color = "black", face = "bold"),
    axis.title.y.right = element_text(color = "gold4", face = "bold", vjust = 2.5),
    axis.line.y = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.line.y.right = element_line(color = "gold4"),
    axis.ticks.y.right = element_line(color = "gold4"),
    axis.text.y.right = element_text(color = "gold4", hjust = 1.1),
    axis.ticks.x =element_blank())




