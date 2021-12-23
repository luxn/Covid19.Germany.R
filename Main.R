source("./Libraries.R")
source("./Preprocessing/AppendGeometry.R")
source("./HotSpot/GetisOrd.R")
source("./HotSpot/LISA.R")
source("./SpatialAutocorrelation/Moran.R")
#source("./Preprocessing/Wave.R")

wave1.complete <- read_csv("./Data/wave1.complete.csv")
wave2.complete <- read_csv("./Data/wave2.complete.csv")
wave3.complete <- read_csv("./Data/wave3.complete.csv")
wave4.complete <- read_csv("./Data/wave4.complete.csv")

hotspot.lisa(wave1.complete, "complete", "Wave1")
hotspot.lisa(wave2.complete, "complete", "Wave2")
hotspot.lisa(wave3.complete, "complete", "Wave3")
hotspot.lisa(wave4.complete, "complete", "Wave4")


wave4.weekly <- read_csv("./Data/wave4.weekly.csv")
hotspot.gstat(wave4.weekly, "weekly", "2021/45")
hotspot.gstat(wave4.weekly, "weekly", "2021/46")
hotspot.gstat(wave4.weekly, "weekly", "2021/47")

wave2.daily <- read_csv("./Data/wave2.daily.csv")
globalmoran.plot(wave2.daily, "daily", "2020-12-24")

wave3.weekly <- read_csv("./Data/wave3.weekly.csv")
wave3.bundesland <- wave3.weekly %>%
  group_by(Bundesland, Kalenderwoche) %>%
  summarize(
    Inzidenz = mean(Inzidenz),
    Fallzahl = sum(Fallzahl),
    RWert7 = mean(RWert7, na.rm = TRUE)) %>%
  arrange(Kalenderwoche, Bundesland)
globalmoran.plot(wave3.bundesland, "weekly", "2021/10", bundesland = T)

