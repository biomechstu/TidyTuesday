#load in libraries
library(tidyverse) 
library(ggplot2)
library(ggthemes)

# read in rankings data
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

ggplot(data = rankings, aes(x = year, label=paste(title,artist, sep=", "))) +
  
  #Add geom_() objects
  geom_point(data = rankings %>% filter(year > 1979 & year < 1985), aes(x = 1980, y = points), color = 'dodgerblue', size = 2) +
  coord_cartesian(xlim = c(1978, 2020), ylim = c(0, 150)) +
  geom_point(data = rankings %>% filter(year > 1984 & year < 1990), aes(x = 1985, y = points), color = 'dodgerblue', size = 2) +
  geom_point(data = rankings %>% filter(year > 1989 & year < 1995), aes(x = 1990, y = points), color = 'dodgerblue', size = 2) +
  geom_point(data = rankings %>% filter(year > 1994 & year < 2000), aes(x = 1995, y = points), color = 'dodgerblue', size = 2) +
  geom_point(data = rankings %>% filter(year > 1999 & year < 2005), aes(x = 2000, y = points), color = 'dodgerblue', size = 2) +
  geom_point(data = rankings %>% filter(year > 2004 & year < 2010), aes(x = 2005, y = points), color = 'dodgerblue', size = 2) +
  geom_point(data = rankings %>% filter(year > 2009 & year < 2015), aes(x = 2010, y = points), color = 'dodgerblue', size = 2) +
  geom_point(data = rankings %>% filter(year > 2014 & year < 2020), aes(x = 2015, y = points), color = 'dodgerblue', size = 2) +
  geom_violin(data = rankings %>% filter(year > 1979 & year < 1985), aes(x = 1980, y = points),width = 4,fill = 'dodgerblue',alpha = 0.1) +
  geom_boxplot(data = rankings %>% filter(year > 1979 & year < 1985), aes(x = 1980, y = points),width = 2,fill = 'dodgerblue',alpha = 0.2) +
  geom_violin(data = rankings %>% filter(year > 1984 & year < 1990), aes(x = 1985, y = points),width = 4,fill = 'dodgerblue',alpha = 0.1) +
  geom_boxplot(data = rankings %>% filter(year > 1984 & year < 1990), aes(x = 1985, y = points),width = 2,fill = 'dodgerblue',alpha = 0.2) +
  geom_violin(data = rankings %>% filter(year > 1989 & year < 1995), aes(x = 1990, y = points),width = 4,fill = 'dodgerblue',alpha = 0.1) +
  geom_boxplot(data = rankings %>% filter(year > 1989 & year < 1995), aes(x = 1990, y = points),width = 2,fill = 'dodgerblue',alpha = 0.2) +
  geom_violin(data = rankings %>% filter(year > 1994 & year < 2000), aes(x = 1995, y = points),width = 4,fill = 'dodgerblue',alpha = 0.1) +
  geom_boxplot(data = rankings %>% filter(year > 1994 & year < 2000), aes(x = 1995, y = points),width = 2,fill = 'dodgerblue',alpha = 0.2) +
  geom_violin(data = rankings %>% filter(year > 1999 & year < 2005), aes(x = 2000, y = points),width = 4,fill = 'dodgerblue',alpha = 0.1) +
  geom_boxplot(data = rankings %>% filter(year > 1999 & year < 2005), aes(x = 2000, y = points),width = 2,fill = 'dodgerblue',alpha = 0.2) +
  geom_violin(data = rankings %>% filter(year > 2004 & year < 2010), aes(x = 2005, y = points),width = 4,fill = 'dodgerblue',alpha = 0.1) +
  geom_boxplot(data = rankings %>% filter(year > 2004 & year < 2010), aes(x = 2005, y = points),width = 2,fill = 'dodgerblue',alpha = 0.2) +
  geom_violin(data = rankings %>% filter(year > 2009 & year < 2015), aes(x = 2010, y = points),width = 4,fill = 'dodgerblue',alpha = 0.1) +
  geom_boxplot(data = rankings %>% filter(year > 2009 & year < 2015), aes(x = 2010, y = points),width = 2,fill = 'dodgerblue',alpha = 0.2) +
  geom_violin(data = rankings %>% filter(year > 2014 & year < 2020), aes(x = 2015, y = points),width = 4,fill = 'dodgerblue',alpha = 0.1) +
  geom_boxplot(data = rankings %>% filter(year > 2014 & year < 2020), aes(x = 2015, y = points),width = 2,fill = 'dodgerblue',alpha = 0.2) +
  geom_text(data = slice(rankings %>% filter(year > 1979 & year < 1985), 1L),aes(x = 1980+7,y = points+3),size=2.5) +
  geom_text(data = slice(rankings %>% filter(year > 1984 & year < 1990), 1L),aes(x = 1985+4,y = points+3),size=2.5) +
  geom_text(data = slice(rankings %>% filter(year > 1989 & year < 1995), 1L),aes(x = 1990+4,y = points+3),size=2.5) +
  geom_text(data = slice(rankings %>% filter(year > 1994 & year < 2000), 1L),aes(x = 1995+4,y = points+3),size=2.5) +
  geom_text(data = slice(rankings %>% filter(year > 1999 & year < 2005), 1L),aes(x = 2000+2,y = points+3),size=2.5) +
  geom_text(data = slice(rankings %>% filter(year > 2004 & year < 2010), 1L),aes(x = 2005+6,y = points+3),size=2.5) +
  geom_text(data = slice(rankings %>% filter(year > 2009 & year < 2015), 1L),aes(x = 2010+4,y = points+3),size=2.5) +
  geom_text(data = slice(rankings %>% filter(year > 2014 & year < 2020), 1L),aes(x = 2015+3,y = points+3),size=2.5) +
  theme_classic() +
  ggtitle("Critics Choice: Greatest Rap Songs") +
  xlab("5 year period beginning") +
  ylab("BBC critic ranking points") +
  theme(plot.title = element_text(size = rel(1.5),hjust = 0.5),axis.text.x= element_text(face="bold"),axis.text.y= element_text(face="bold"),axis.title.x= element_text(face="bold"),axis.title.y= element_text(face="bold")) +
  scale_x_continuous(breaks = round(seq(1980, 2015, by = 5),1)) +
  ggsave(filename="rap.jpg", plot = last_plot())