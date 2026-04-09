library(flume)
net = readRDS("ex1_network.rds")

rr = state(net, "resources")
vlcol = ifelse(rr < 0.7, "black", "white")
pdf(file = "figures/fig_ex1_start_r.pdf", width = 7.5, height = 6.5)
plot(net, attribute = "resource", legend_args = list(title = "[R]"), vertex.label.color = vlcol)
dev.off()