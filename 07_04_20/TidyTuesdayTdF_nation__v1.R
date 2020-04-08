# Load libraries
library(tidyverse)
library(kableExtra)
library(magick)

# Read data
tdf_winners <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

for(i in 1:106) {
tdf_winners %>%
  filter(edition <=i) %>%
  count(nationality, name = 'num_of_wins') %>%
  mutate(rank = rank(-num_of_wins)) %>%
  filter(rank <=20) %>%
  ggplot(aes(reorder(winner_name, num_of_wins), num_of_wins),main=edition) +
  geom_col(fill = "blue",
           width = 0.6,
           alpha = 0.8) +
  labs(x = NULL, y = paste("Tour de France wins - ",i,sep="")) +
  coord_flip()
  if (i<10) {
    ggsave(filename=paste("00",i,".png",sep=""))
  } else if ( i<100) {
    ggsave(filename=paste("0",i,".png",sep=""))
  } else {
    ggsave(filename=paste(i,".png",sep=""))
  }
}

list.files(pattern = "*.png") %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("TdF.gif") # write to current dir