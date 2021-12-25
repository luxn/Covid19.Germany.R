wave4 <- read_csv("./wave.weekly.csv")
wave4Bundesland <- wave4 %>% group_by(Bundesland, Kalenderwoche) %>% summarise(Inzidenz = mean(Inzidenz), RWert7 = mean(RWert7, na.rm=TRUE))

wave4Bundesland$Bundesland = as.factor(wave4Bundesland$Bundesland)

ggplot(data=wave4Bundesland, aes(Kalenderwoche, Bundesland)) +
  geom_tile(aes(fill=Inzidenz, colour="red"), colour="white") +
  scale_fill_gradientn(colours=c("#FFFFFF", "#FF0000"))
