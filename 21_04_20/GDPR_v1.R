library(rvest)
library(tidyverse)
require(scales)
library(ggplot2)
library(magick)

temp <- read_html("https://www.privacyaffairs.com/gdpr-fines/") %>% html_nodes("script") %>% .[9] %>% 
  rvest::html_text() 

ends <- str_locate_all(temp, "\\]")
starts <- str_locate_all(temp, "\\[")
table1 <- temp %>% stringi::stri_sub(from = starts[[1]][1,2], to = ends[[1]][1,1]) %>% 
  str_remove_all("\n") %>% 
  str_remove_all("\r") %>%
  jsonlite::fromJSON()

# sort data into categories
data <- table1 %>% count(type, name = 'Count') %>% arrange(desc(Count))
a <- table1 %>% filter(type == toString(data[1,1]) & substring(date,7,10)>2017) %>% arrange(desc(price))
b <- table1 %>% filter(type == toString(data[2,1]) & substring(date,7,10)>2017) %>% arrange(desc(price))
c <- table1 %>% filter(type == toString(data[3,1]) & substring(date,7,10)>2017) %>% arrange(desc(price))
d <- table1 %>% filter(type == toString(data[4,1]) & substring(date,7,10)>2017) %>% arrange(desc(price))
e <- table1 %>% filter(type == toString(data[5,1]) & substring(date,7,10)>2017) %>% arrange(desc(price))
other <- table1 %>% filter(type != toString(data[1,1]) & type != toString(data[2,1]) & type != toString(data[3,1]) & type != toString(data[4,1]) & type != toString(data[5,1]) & substring(date,7,10)>2017) %>% arrange(desc(price))

#annotation
wrap40 <- wrap_format(80)
annot <- wrap40(other[1,11])  
annot <- gsub('<p>','',annot)
annot <- gsub('</p>','',annot)

# plot chart 1
# x axis
wrap20 <- wrap_format(20)
xtext <- wrap20(a[1,9])
# plot
ggplot(data = table1, aes(y = price, label=controller)) +
  coord_cartesian(xlim = c(0.5, 6.5)) +
  geom_jitter(data = a, aes(x = 1, y = price), size = 2,height=0,width=0.1,colour='dodgerblue') +
  geom_violin(data = a, aes(x = 1, y = price),width = 0.8,fill = 'dodgerblue',alpha = 0.1) +
  geom_boxplot(data = a, aes(x = 1, y = price),width = 0.4,fill = 'dodgerblue',alpha = 0.2,outlier.size=0) +
  geom_text(data = slice(a, 1L),aes(x = 1,y = price),size=5,position = position_nudge(y = +0.15)) +
  scale_y_continuous(labels = comma,trans='log10', breaks = c(500,5000,50000,500000,5000000,50000000)) +
  scale_x_continuous(breaks=1, labels=(xtext)) +
  theme_classic() +
  ggtitle("Biggest GDPR Fines") +
  xlab("Type of Violation") +
  ylab("Fine (€)") +
  theme(plot.title = element_text(size = 40,hjust = 0.5),axis.text.x= element_text(face="bold",size=15,lineheight=.5),axis.text.y= element_text(face="bold",size=15),axis.title.x= element_text(face="bold",size=20),axis.title.y= element_text(face="bold",size=20)) +
  ggsave(filename="1.png",width = 176, height = 109,units = "mm")

# plot chart 2
# x axis
wrap20 <- wrap_format(20)
xtext <- wrap20(c(a[1,9], b[1,9]))
# plot
ggplot(data = table1, aes(y = price, label=controller)) +
  coord_cartesian(xlim = c(0.5, 6.5)) +
  geom_jitter(data = b, aes(x = 2, y = price), size = 2,height=0,width=0.1,colour='sienna1') +
  geom_violin(data = a, aes(x = 1, y = price),width = 0.8,fill = 'dodgerblue',alpha = 0.1) +
  geom_violin(data = b, aes(x = 2, y = price),width = 0.8,fill = 'sienna1',alpha = 0.1) +
  geom_boxplot(data = a, aes(x = 1, y = price),width = 0.4,fill = 'dodgerblue',alpha = 0.2) +
  geom_boxplot(data = b, aes(x = 2, y = price),width = 0.4,fill = 'sienna1',alpha = 0.2,outlier.size=0) +
  geom_text(data = slice(a, 1L),aes(x = 1,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(b, 1L),aes(x = 2,y = price),size=5,position = position_nudge(y = +0.15)) +
  scale_y_continuous(labels = comma,trans='log10', breaks = c(500,5000,50000,500000,5000000,50000000)) +
  scale_x_continuous(breaks=1:2, labels=(xtext)) +
  theme_classic() +
  ggtitle("Biggest GDPR Fines") +
  xlab("Type of Violation") +
  ylab("Fine (€)") +
  theme(plot.title = element_text(size = 40,hjust = 0.5),axis.text.x= element_text(face="bold",size=15,lineheight=.5),axis.text.y= element_text(face="bold",size=15),axis.title.x= element_text(face="bold",size=20),axis.title.y= element_text(face="bold",size=20)) +
  ggsave(filename="2.png",width = 176, height = 109,units = "mm")

# plot chart 3
# x axis
wrap20 <- wrap_format(20)
xtext <- wrap20(c(a[1,9], b[1,9], c[1,9]))
# plot
ggplot(data = table1, aes(y = price, label=controller)) +
  coord_cartesian(xlim = c(0.5, 6.5)) +
  geom_jitter(data = c, aes(x = 3, y = price), size = 2,height=0,width=0.1,colour='forestgreen') +
  geom_violin(data = a, aes(x = 1, y = price),width = 0.8,fill = 'dodgerblue',alpha = 0.1) +
  geom_violin(data = b, aes(x = 2, y = price),width = 0.8,fill = 'sienna1',alpha = 0.1) +
  geom_violin(data = c, aes(x = 3, y = price),width = 0.8,fill = 'forestgreen',alpha = 0.1) +
  geom_boxplot(data = a, aes(x = 1, y = price),width = 0.4,fill = 'dodgerblue',alpha = 0.2) +
  geom_boxplot(data = b, aes(x = 2, y = price),width = 0.4,fill = 'sienna1',alpha = 0.2) +
  geom_boxplot(data = c, aes(x = 3, y = price),width = 0.4,fill = 'forestgreen',alpha = 0.2,outlier.size=0) +
  geom_text(data = slice(a, 1L),aes(x = 1,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(b, 1L),aes(x = 2,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(c, 1L),aes(x = 3,y = price),size=5,position = position_nudge(y = +0.15)) +
  scale_y_continuous(labels = comma,trans='log10', breaks = c(500,5000,50000,500000,5000000,50000000)) +
  scale_x_continuous(breaks=1:3, labels=(xtext)) +
  theme_classic() +
  ggtitle("Biggest GDPR Fines") +
  xlab("Type of Violation") +
  ylab("Fine (€)") +
  theme(plot.title = element_text(size = 40,hjust = 0.5),axis.text.x= element_text(face="bold",size=15,lineheight=.5),axis.text.y= element_text(face="bold",size=15),axis.title.x= element_text(face="bold",size=20),axis.title.y= element_text(face="bold",size=20)) +
  ggsave(filename="3.png",width = 176, height = 109,units = "mm")

# plot chart 4
# x axis
wrap20 <- wrap_format(20)
xtext <- wrap20(c(a[1,9], b[1,9], c[1,9], d[1,9]))
# plot
ggplot(data = table1, aes(y = price, label=controller)) +
  coord_cartesian(xlim = c(0.5, 6.5)) +
  geom_jitter(data = d, aes(x = 4, y = price), size = 2,height=0,width=0.1,colour='goldenrod1') +
  geom_violin(data = a, aes(x = 1, y = price),width = 0.8,fill = 'dodgerblue',alpha = 0.1) +
  geom_violin(data = b, aes(x = 2, y = price),width = 0.8,fill = 'sienna1',alpha = 0.1) +
  geom_violin(data = c, aes(x = 3, y = price),width = 0.8,fill = 'forestgreen',alpha = 0.1) +
  geom_violin(data = d, aes(x = 4, y = price),width = 0.8,fill = 'goldenrod1',alpha = 0.1) +
  geom_boxplot(data = a, aes(x = 1, y = price),width = 0.4,fill = 'dodgerblue',alpha = 0.2) +
  geom_boxplot(data = b, aes(x = 2, y = price),width = 0.4,fill = 'sienna1',alpha = 0.2) +
  geom_boxplot(data = c, aes(x = 3, y = price),width = 0.4,fill = 'forestgreen',alpha = 0.2) +
  geom_boxplot(data = d, aes(x = 4, y = price),width = 0.4,fill = 'goldenrod1',alpha = 0.2,outlier.size=0) +
  geom_text(data = slice(a, 1L),aes(x = 1,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(b, 1L),aes(x = 2,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(c, 1L),aes(x = 3,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(d, 1L),aes(x = 4,y = price),size=5,position = position_nudge(y = +0.15)) +
  scale_y_continuous(labels = comma,trans='log10', breaks = c(500,5000,50000,500000,5000000,50000000)) +
  scale_x_continuous(breaks=1:4, labels=(xtext)) +
  theme_classic() +
  ggtitle("Biggest GDPR Fines") +
  xlab("Type of Violation") +
  ylab("Fine (€)") +
  theme(plot.title = element_text(size = 40,hjust = 0.5),axis.text.x= element_text(face="bold",size=15,lineheight=.5),axis.text.y= element_text(face="bold",size=15),axis.title.x= element_text(face="bold",size=20),axis.title.y= element_text(face="bold",size=20)) +
  ggsave(filename="4.png",width = 176, height = 109,units = "mm")

# plot chart 5
# x axis
wrap20 <- wrap_format(20)
xtext <- wrap20(c(a[1,9], b[1,9], c[1,9], d[1,9], e[1,9]))
# plot
ggplot(data = table1, aes(y = price, label=controller)) +
  coord_cartesian(xlim = c(0.5, 6.5)) +
  geom_jitter(data = e, aes(x = 5, y = price), size = 2,height=0,width=0.1,colour='darkorchid1') +
  geom_violin(data = a, aes(x = 1, y = price),width = 0.8,fill = 'dodgerblue',alpha = 0.1) +
  geom_violin(data = b, aes(x = 2, y = price),width = 0.8,fill = 'sienna1',alpha = 0.1) +
  geom_violin(data = c, aes(x = 3, y = price),width = 0.8,fill = 'forestgreen',alpha = 0.1) +
  geom_violin(data = d, aes(x = 4, y = price),width = 0.8,fill = 'goldenrod1',alpha = 0.1) +
  geom_violin(data = e, aes(x = 5, y = price),width = 0.8,fill = 'darkorchid1',alpha = 0.1) +
  geom_boxplot(data = a, aes(x = 1, y = price),width = 0.4,fill = 'dodgerblue',alpha = 0.2) +
  geom_boxplot(data = b, aes(x = 2, y = price),width = 0.4,fill = 'sienna1',alpha = 0.2) +
  geom_boxplot(data = c, aes(x = 3, y = price),width = 0.4,fill = 'forestgreen',alpha = 0.2) +
  geom_boxplot(data = d, aes(x = 4, y = price),width = 0.4,fill = 'goldenrod1',alpha = 0.2) +
  geom_boxplot(data = e, aes(x = 5, y = price),width = 0.4,fill = 'darkorchid1',alpha = 0.2,outlier.size=0) +
  geom_text(data = slice(a, 1L),aes(x = 1,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(b, 1L),aes(x = 2,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(c, 1L),aes(x = 3,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(d, 1L),aes(x = 4,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(e, 1L),aes(x = 5,y = price),size=5,position = position_nudge(y = +0.15)) +
  scale_y_continuous(labels = comma,trans='log10', breaks = c(500,5000,50000,500000,5000000,50000000)) +
  scale_x_continuous(breaks=1:5, labels=(xtext)) +
  theme_classic() +
  ggtitle("Biggest GDPR Fines") +
  xlab("Type of Violation") +
  ylab("Fine (€)") +
  theme(plot.title = element_text(size = 40,hjust = 0.5),axis.text.x= element_text(face="bold",size=15,lineheight=.5),axis.text.y= element_text(face="bold",size=15),axis.title.x= element_text(face="bold",size=20),axis.title.y= element_text(face="bold",size=20)) +
  ggsave(filename="5.png",width = 176, height = 109,units = "mm")

# plot chart 6
# x axis
wrap20 <- wrap_format(20)
xtext <- wrap20(c(a[1,9], b[1,9], c[1,9], d[1,9], e[1,9], "Other"))
# plot
ggplot(data = table1, aes(y = price, label=controller)) +
  coord_cartesian(xlim = c(0.5, 6.5)) +
  geom_jitter(data = other, aes(x = 6, y = price), size = 2,height=0,width=0.1,colour='peachpuff3') +
  geom_violin(data = a, aes(x = 1, y = price),width = 0.8,fill = 'dodgerblue',alpha = 0.1) +
  geom_violin(data = b, aes(x = 2, y = price),width = 0.8,fill = 'sienna1',alpha = 0.1) +
  geom_violin(data = c, aes(x = 3, y = price),width = 0.8,fill = 'forestgreen',alpha = 0.1) +
  geom_violin(data = d, aes(x = 4, y = price),width = 0.8,fill = 'goldenrod1',alpha = 0.1) +
  geom_violin(data = e, aes(x = 5, y = price),width = 0.8,fill = 'darkorchid1',alpha = 0.1) +
  geom_violin(data = other, aes(x = 6, y = price),width = 0.8,fill = 'peachpuff3',alpha = 0.1) +
  geom_boxplot(data = a, aes(x = 1, y = price),width = 0.4,fill = 'dodgerblue',alpha = 0.2) +
  geom_boxplot(data = b, aes(x = 2, y = price),width = 0.4,fill = 'sienna1',alpha = 0.2) +
  geom_boxplot(data = c, aes(x = 3, y = price),width = 0.4,fill = 'forestgreen',alpha = 0.2) +
  geom_boxplot(data = d, aes(x = 4, y = price),width = 0.4,fill = 'goldenrod1',alpha = 0.2) +
  geom_boxplot(data = e, aes(x = 5, y = price),width = 0.4,fill = 'darkorchid1',alpha = 0.2) +
  geom_boxplot(data = other, aes(x = 6, y = price),width = 0.4,fill = 'peachpuff3',alpha = 0.2,outlier.size=0) +
  geom_text(data = slice(a, 1L),aes(x = 1,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(b, 1L),aes(x = 2,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(c, 1L),aes(x = 3,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(d, 1L),aes(x = 4,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(e, 1L),aes(x = 5,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(other, 1L),aes(x = 6,y = price),size=5,position = position_nudge(y = +0.15)) +
  annotate("text", x = 5.7, y = other[1,4]-40000000, label = annot,hjust=1,size=4,lineheight = .5) +
  scale_y_continuous(labels = comma,trans='log10', breaks = c(500,5000,50000,500000,5000000,50000000)) +
  scale_x_continuous(breaks=1:6, labels=(xtext)) +
  theme_classic() +
  ggtitle("Biggest GDPR Fines") +
  xlab("Type of Violation") +
  ylab("Fine (€)") +
  theme(plot.title = element_text(size = 40,hjust = 0.5),axis.text.x= element_text(face="bold",size=15,lineheight=.5),axis.text.y= element_text(face="bold",size=15),axis.title.x= element_text(face="bold",size=20),axis.title.y= element_text(face="bold",size=20)) +
  ggsave(filename="6.png",width = 176, height = 109,units = "mm")

# One way ANOVA
table2 <- table1 %>% filter(substring(date,7,10)>2017) %>% filter(type == toString(data[1,1]) | type == toString(data[2,1]) | type == toString(data[3,1]) | type == toString(data[4,1]) | type == toString(data[5,1]))
res.aov <- aov(price ~ type, data = table2)

# plot chart 7
# x axis
wrap20 <- wrap_format(20)
xtext <- wrap20(c(a[1,9], b[1,9], c[1,9], d[1,9], e[1,9], "Other"))
# plot
ggplot(data = table1, aes(y = price, label=controller)) +
  coord_cartesian(xlim = c(0.5, 6.5)) +
  geom_violin(data = a, aes(x = 1, y = price),width = 0.8,fill = 'dodgerblue',alpha = 0.1) +
  geom_violin(data = b, aes(x = 2, y = price),width = 0.8,fill = 'sienna1',alpha = 0.1) +
  geom_violin(data = c, aes(x = 3, y = price),width = 0.8,fill = 'forestgreen',alpha = 0.1) +
  geom_violin(data = d, aes(x = 4, y = price),width = 0.8,fill = 'goldenrod1',alpha = 0.1) +
  geom_violin(data = e, aes(x = 5, y = price),width = 0.8,fill = 'darkorchid1',alpha = 0.1) +
  geom_violin(data = other, aes(x = 6, y = price),width = 0.8,fill = 'peachpuff3',alpha = 0.1) +
  geom_boxplot(data = a, aes(x = 1, y = price),width = 0.4,fill = 'dodgerblue',alpha = 0.2) +
  geom_boxplot(data = b, aes(x = 2, y = price),width = 0.4,fill = 'sienna1',alpha = 0.2) +
  geom_boxplot(data = c, aes(x = 3, y = price),width = 0.4,fill = 'forestgreen',alpha = 0.2) +
  geom_boxplot(data = d, aes(x = 4, y = price),width = 0.4,fill = 'goldenrod1',alpha = 0.2) +
  geom_boxplot(data = e, aes(x = 5, y = price),width = 0.4,fill = 'darkorchid1',alpha = 0.2) +
  geom_boxplot(data = other, aes(x = 6, y = price),width = 0.4,fill = 'peachpuff3',alpha = 0.2) +
  geom_text(data = slice(a, 1L),aes(x = 1,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(b, 1L),aes(x = 2,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(c, 1L),aes(x = 3,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(d, 1L),aes(x = 4,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(e, 1L),aes(x = 5,y = price),size=5,position = position_nudge(y = +0.15)) +
  geom_text(data = slice(other, 1L),aes(x = 6,y = price),size=5,position = position_nudge(y = +0.15)) +
  annotate("text", x = 5.7, y = other[1,4]-40000000, label = annot,hjust=1,size=4,lineheight = .5) +
  annotate("text", x = 3.5, y = other[1,4]+40000000, label = paste("Type of violation had no significant effect on fine (F[",summary(res.aov)[[1]][["Df"]][1],",",summary(res.aov)[[1]][["Df"]][2],"] = ",round(summary(res.aov)[[1]][["F value"]][1],digits=3),", p = ",round(summary(res.aov)[[1]][["Pr(>F)"]][1],digits=3),")",sep=""),,hjust=0.5,size=8,lineheight = .5) +
  annotate("segment", x = 5.72, xend = 5.95, y = other[1,4]-10000000, yend = other[1,4]-5000000, colour = "black", size=0.5,arrow=arrow()) +
  scale_y_continuous(labels = comma,trans='log10', breaks = c(500,5000,50000,500000,5000000,50000000)) +
  scale_x_continuous(breaks=1:6, labels=(xtext)) +
  theme_classic() +
  ggtitle("Biggest GDPR Fines") +
  xlab("Type of Violation") +
  ylab("Fine (€)") +
  theme(plot.title = element_text(size = 40,hjust = 0.5),axis.text.x= element_text(face="bold",size=15,lineheight=.5),axis.text.y= element_text(face="bold",size=15),axis.title.x= element_text(face="bold",size=20),axis.title.y= element_text(face="bold",size=20)) +
  ggsave(filename="7.png",width = 176, height = 109,units = "mm")

# create gif
list.files(pattern = "*.png") %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(delay=c(100,100,100,100,100,100,1000)) %>% # animates, can opt for number of loops
  image_write("GDPR.gif") # write to current dir