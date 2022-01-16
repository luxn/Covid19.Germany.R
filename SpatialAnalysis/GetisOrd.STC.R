df <- read_csv("./data/phase6.weekly.csv")

#cube.df <- df %>% group_by(Id, Name) %>% summarize(Id = first(Id), Name=first(Name)) %>% arrange(Id)

df <- append_landkreis_geometry(df)

weeks <- unique(date(df$Kalenderwoche))
for (week in weeks) {
  week <- as.Date(week, origin=origin)
  week.df <- filter(df, Kalenderwoche == week) %>% arrange(Id)
  nb <- dnearneigh(st_centroid(week.df), 0, 60000) # 60km
  nb_lw <- nb2listw(nb, style = 'W')
  local_g <- localG(week.df$Inzidenz, nb_lw)

  p <- ggplot(week.df) +
    geom_sf(aes(fill=cut(as.matrix(local_g), c(-Inf, -2.576, -1.960, -1.645, 1.645, 1.960, 2.576, +Inf),
                         labels=c(">99% ColdSpot", ">95% ColdSpot", ">90% ColdSpot", "Uncertain", ">90% HotSpot", ">95% HotSpot", ">99% HotSpot"))),
            lwd =0.25) +
    scale_fill_manual(values = rev(c('#b2182b','#ef8a62','#fddbc7','#f7f7f7','#d1e5f0','#67a9cf','#2166ac')), name= "Gi* values",) +
    ggtitle(sprintf("Gi* - Phase 6 - Week of %s", week))

  ggsave(sprintf("./Images/lisa.getisord.phase6.%s.png", week), p, dpi="retina", width=7.5, height=6)

  #cube.df <- cbind(cube.df, as.matrix(local_g))
  #names(cube.df)[length(names(cube.df))] <- as.character(week)
}
