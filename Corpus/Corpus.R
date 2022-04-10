#Data import
rm(list=ls(all=TRUE))
COHA_case <- read.delim("/Users/brassjin/Desktop/Corpus DATA/COHA_case.txt",header=T)
COHA_comp <- read.delim("/Users/brassjin/Desktop/Corpus DATA/COHA_comp.txt",header=T)
COCA_case <- read.delim("/Users/brassjin/Desktop/Corpus DATA/COCA_case.txt",header=T)
COCA_comp <- read.delim("/Users/brassjin/Desktop/Corpus DATA/COCA_comp.txt",header=T)
COCA_per <- read.delim("/Users/brassjin/Desktop/Corpus DATA/COCA_per.txt",header=T)


# library import
library(ggplot2)
library(ggpubr)
postscript(family="Times")

# vector for year label
Year_lab <-c()

for (i in 1:20) {
  Year_lab <- append(Year_lab, 1810 + i*10)
}

# COHA ACC vs. NOM plot
COHA_AN <- ggplot(COHA_case, aes(x = Year, 
                            y = Freq, 
                            fill = Case)) +
  geom_bar(position = position_fill(reverse = TRUE), stat ="identity") +
  scale_x_continuous(breaks = Year_lab,
                     labels = factor(Year_lab)) +
  ggtitle("a. Relative Frequency of Case Usage") + 
  ylab("Rel_Freq") +
  theme_bw() + 
  labs(fill = NULL) + 
  theme(axis.text.x=element_text(size = 8, 
                                 colour = "black", 
                                 angle = 90, 
                                 hjust = 0, 
                                 vjust = 0.5), 
        text = element_text(family = "Times", size = 10)) + 
  scale_fill_grey(start=0.9, end=0.7) 

# COHA That vs. Who plot
COHA_TW <- ggplot(COHA_comp, aes(x = Year, 
                            y = Freq, 
                            fill = Comp)) +
  geom_bar(position = position_fill(reverse = TRUE), stat ="identity") +
  scale_x_continuous(breaks = Year_lab,
                     labels = factor(Year_lab)) +
  ggtitle("b. Relative Frequency of Complementizer Usage") + 
  ylab("Rel_Freq") +
  theme_bw() + 
  labs(fill = NULL) + 
  theme(axis.text.x=element_text(size = 8, 
                                 colour = "black", 
                                 angle = 90, 
                                 hjust = 0, 
                                 vjust = 0.5), 
        text = element_text(family = "Times", size = 10)) + 
  scale_fill_grey(start=0.9, end=0.7)

# Print two plots in one
setwd("/Users/brassjin/Desktop")
png(filename="COHA_plot.png",width=5680,height=4720,res=720, unit="px")
ggarrange(COHA_AN, COHA_TW, nrow = 2)
dev.off()


# COCA ACC vs. NOM plot
COCA_AN <- ggplot(COCA_case, aes(x = '', y = Freq, fill = Case)) + 
  geom_bar(stat ="identity") + 
  coord_polar("y", start = 0) + 
  ggtitle("a. Case Usage Ratio") + 
  geom_text(aes(label = Rel, family = "Times"), 
            position = position_stack(vjust=0.5), 
            size = 4) + 
  theme_void(base_family = "Times") +
  theme(legend.position = c(.9, .9), 
        text = element_text(size=10)) + 
  labs(fill = NULL) + 
  scale_fill_grey(start=0.9, end=0.7)

# COCA that vs. who plot
COCA_TW <- ggplot(COCA_comp, aes(x = '', y = Freq, fill = Comp)) + 
  geom_bar(stat ="identity") + 
  coord_polar("y", start = 0) +
  ggtitle("b. Complementizer Usage Ratio") + 
  geom_text(aes(label = Rel, family = "Times"), 
            position = position_stack(vjust=0.5), 
            size = 4) + 
  theme_void(base_family = "Times") +
  theme(legend.position = c(.9, .9), 
        text = element_text(size=10)) + 
  labs(fill = NULL)+ 
  scale_fill_grey(start=0.9, end=0.7)

# COCA person plot
COCA_P <- ggplot(COCA_per, aes(x = Ord, y = Freq)) + 
  geom_bar(stat = 'identity', fill = "grey") +
  ggtitle("c. Pronominal Usage Frequency") + 
  scale_x_discrete("Pronominal", 
                   labels = c("I","me","we","us","he","him","she","her","they","them")) + 
  theme_bw() +
  theme(text = element_text("Times", size = 10)) + 
  geom_text(aes(label = Freq, family = "Times"), 
            position = position_stack(vjust=1), 
            size = 4)


# Print three plots in one
setwd("/Users/brassjin/Desktop")
png(filename="COCA_plot.png",width=5680,height=4720,res=720, unit="px")
grid.arrange(COCA_P, 
             COCA_AN, COCA_TW,
             ncol = 2, nrow = 2, 
             layout_matrix = rbind(c(2,3), c(1,1)))
dev.off()




             