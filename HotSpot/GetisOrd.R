wave <- read_csv("./wave.weekly.csv")
kw <- 16
wave <- wave %>% filter(Kalenderwoche == kw) %>% append_landkreis_geometry()

nb <- dnearneigh(st_centroid(wave), 0, 60000) # 60km

nb_lw <- nb2listw(nb, style = 'W')

local_g <- localG(wave$Inzidenz, nb_lw)
local_g <- cbind(wave, as.matrix(local_g))

names(local_g)[8] <- "gstat"

pal <- c("#0000FF",  "#6666FF", "#9999FF", "#FFFFFF", "#FF6666", "#FF3333", "#FF0000")

tm_shape(local_g) + tm_fill("gstat",
                            palette = pal,
                            style = "pretty") + tm_borders(alpha=.4)

