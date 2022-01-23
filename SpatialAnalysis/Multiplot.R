plot1 <- plot.inz / plot.moran / plot.bundesland + plot_annotation(tag_levels = 'A') + theme(plot.tag = element_text(size = 16))
ggsave("./Images/plot1.pdf", plot1, dpi="retina", width=13, height=13)




