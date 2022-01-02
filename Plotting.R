wave1 <- read_csv("./data/wave1.weekly.csv")
wave2 <- read_csv("./data/wave2.weekly.csv")
wave3 <- read_csv("./data/wave3.weekly.csv")
wave4 <- read_csv("./data/wave4.weekly.csv")

wave <- rbind(wave1, wave2, wave3, wave4)

waveBundesland <- wave %>%
  group_by(Bundesland, Kalenderwoche) %>%
  summarise(Inzidenz = mean(Inzidenz), RWert7 = mean(RWert7, na.rm=TRUE)) %>%
  arrange(Bundesland, Kalenderwoche)


waveBundesland$Bundesland = as.factor(waveBundesland$Bundesland)

ggplot(data=waveBundesland, aes(Kalenderwoche, Bundesland)) +
  geom_tile(aes(fill=Inzidenz, colour="red"), colour="white") +
  scale_fill_gradientn(colours=c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

