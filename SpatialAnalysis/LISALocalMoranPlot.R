dfs <- list(
  read_csv("./data/phase0.complete.csv"),
  read_csv("./data/phase1.complete.csv"),
  read_csv("./data/phase2.complete.csv"),
  read_csv("./data/phase3.complete.csv"),
  read_csv("./data/phase4.complete.csv"),
  read_csv("./data/phase5.complete.csv"),
  read_csv("./data/phase6.complete.csv")
)
i <- 0
for (dframe in dfs) {
  df <- append_landkreis_geometry(dframe)

  W <- queen_weights(df)
  lisa <- local_moran(W, df['Inzidenz'])

  lisa_colors <- lisa_colors(lisa)
  lisa_labels <- lisa_labels(lisa)
  lisa_clusters <- lisa_clusters(lisa)

  p <- ggplot(df) +
    geom_sf(aes(fill=factor(lisa_clusters)), lwd =0.25) +
    scale_fill_manual(values = lisa_colors, name= "LISA Cluster", labels=lisa_labels) +
    ggtitle(sprintf("Local Moran LISA Cluster - Phase %s", i))

  ggsave(sprintf("./Images/lisa.localmoran.phase%s.png", i), p, dpi="retina", width=7.5, height=6)
  i <- i + 1
}
