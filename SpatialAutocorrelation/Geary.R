wave <- read_csv("./wave.weekly.csv") %>% filter(Kalenderwoche == 18)

wave <- wave %>% append_landkreis_geometry
nb <- poly2nb(wave, queen=T)
lw <- nb2listw(nb, style="W")

geary(wave$Inzidenz, lw, length(nb), length(nb)-1, Szero(lw))
