df <- read_csv("./Data/phase.all.weekly.csv")

bundesland.df <- df %>%
  group_by(Bundesland, Kalenderwoche) %>%
  summarise(Inzidenz = mean(Inzidenz), Fallzahl = sum(Fallzahl)) %>%
  arrange(Bundesland, Kalenderwoche)

bundesland.df$Bundesland <- as.factor(bundesland.df$Bundesland)

p <- ggplot(data=bundesland.df, aes(Kalenderwoche, Bundesland)) +
  geom_tile(aes(fill=Inzidenz), colour="white") +
  scale_fill_gradientn(breaks=c(0, 10, 50, 100, 200, 500, 1000, as.integer(max(bundesland.df$Inzidenz))), trans="sqrt", colours=rev(c(
    '#730153', '#8c0165', '#8c0165','#ae017e','#ae017e',
    '#bd0026','#bd0026','#bd0026','#f03b20','#f03b20',
    '#f03b20','#fd8d3c','#feb24c',
    '#fed976','#ffffbf','#cccccc', '#cccccc'))) +
  ylab(NULL) + xlab(NULL) + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        legend.key.height = unit(1.1, "cm")) + ggtitle("Verlauf der 7-Tage Inzidenz pro 100.000 Einwohner nach Bundesland")


ggsave("./Images/inzidenzverlauf.bundesland.png", p, dpi="retina", width=9, height=3)
