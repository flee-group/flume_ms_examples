library(data.table)
library(ggplot2)
library(flume)

sim_results = readRDS("results/simulation_summary.rds")

lags = list(r41 = data.table(
    reach = as.character(
        c(1,  2,  3, 4, 25, 26, 27, 28, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 51, 52)),
    lag = 
        c(11, 10, 9, 8, 7,  5,  6,  4,  3,  2,  1,  0,  -2, -1, -1, -2, -3, -3, -2, -3),
        pollution_reach = 41
), r15 = data.table(
    reach = as.character(
        c(1,   2, 3, 4, 5, 6, 7, 8, 9, 13, 14, 15, 16, 17, 18, 19, 23)),
    lag =   
        c(11, 10, 9, 8, 7, 6, 5, 4, 3,  1,  2,  0, -1, -1, -2, -3, -3),
        pollution_reach = 15
), r34 = data.table(
    reach = as.character(
        c(1,  2,  3,  4, 25, 26, 27, 28, 29, 31, 32, 33, 34, 35, 36, 37)),
    lag =   
        c(12, 11, 10, 9, 8,  6,  7,  5,  4,  1,  2,  3,  0,  -2, -1, -1),
        pollution_reach = 34
))
lags = rbindlist(lags)
rich_plot = merge(sim_results, lags, by = c("reach", "pollution_reach"))
rich_plot$pollution_label = "control"
rich_plot$pollution_label[rich_plot$pollution_conc == 15] = "low pollution"
rich_plot$pollution_label[rich_plot$pollution_conc == 60] = "high pollution"

pdf(width=10, height = 20, file = "figures/fig_ex2_rich.pdf", pointsize=36)

ggplot(rich_plot, aes(x = lag, y = richness, colour = factor(pollution_conc))) + geom_line() + 
    geom_errorbar(aes(ymin = richness_lo, ymax = richness_hi), width = 0.2) + 
    facet_grid(rows = vars(pollution_reach)) + 
    # geom_label(aes(y = 40, label = label), colour = "black", size = 8, label.padding = unit(0.1, "lines")) + 
    geom_vline(xintercept = 0, lty=2, colour = "#777777") + 
    ylim(0, 45) + theme_light(base_size=24) + xlab("Lag (reaches from pollution source)") + 
    ylab("ASV Richness") +
    scale_colour_manual("[N] (mg/L)", values = c("#fc8d59", "#d7301f", "#7f0000"))
dev.off()
