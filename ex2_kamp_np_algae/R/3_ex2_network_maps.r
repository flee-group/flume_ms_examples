library(data.table)
library(flume)
data(kamp)
kamp = kamp$network

sim_results = readRDS("results/simulation_summary.rds")


r_map_pol = sim_results[pollution_conc == 15 & pollution_reach == 34, .(reach, richness, NtoP = (NtoP_median))]
r_map_nopol = sim_results[pollution_conc == 0 & pollution_reach == 34, .(reach, richness, NtoP = (NtoP_median))]

# reorder the rows to match the order in the river network
r_map_pol = r_map_pol[match(attr(kamp, "names_sites"), r_map_pol$reach)]
r_map_nopol = r_map_nopol[match(attr(kamp, "names_sites"), r_map_nopol$reach)]

# print n:p ratio for the reaches beyond the maximum concentrations
i = which(r_map_pol$NtoP > 100)
for(ii in i)
    cat("Reach", r_map_pol$reach[ii], ":", "N:P = ", r_map_pol$NtoP[ii], "\n")

atval = -1
line = 1.2
zlim = c(0, 100)
pdf(width = 20, height = 20, file = "figures/fig_ex2_map.pdf", pointsize = 24)
par(mfrow = c(2, 2), mar = c(0, 1, 3, 0))
plot(kamp, attribute = r_map_pol$richness, legend_args = list(title = "ASV\nrichness"), v_palette = "Purples")
mtext("A", side = 3, at = atval, line = line)

text_col = rep("black", length(kamp))
text_col[c(10:12, 23, 29, 31:36, 42:43, 52)] = "white"
plot(kamp, attribute = r_map_pol$NtoP, legend_args = list(title = "N:P"), vertex.label.color = text_col,
    v_palette = "OrRd", zlim = zlim)
mtext("B", side = 3, at = atval, line = line)

plot(kamp, attribute = r_map_nopol$richness, legend_args = list(title = "ASV\nrichness"), v_palette = "Purples")
mtext("C", side = 3, at = atval, line = line)


text_col = rep("black", length(kamp))
text_col[c(10:12, 23, 29, 35:36, 42:43, 52)] = "white"
plot(kamp, attribute = r_map_nopol$NtoP, legend_args = list(title = "N:P"), vertex.label.color = text_col,
    v_palette = "OrRd", zlim = zlim)
mtext("D", side = 3, at = atval, line = line)
dev.off()
