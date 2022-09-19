
## plot
## x-axis: local diversity
## y-axis: ecosystem functioning
## dots: individual stream reaches
## colors: individual replicates
## panels: 2x2 grid, 1-direction shows dispersal (high vs low), one shows gamma diversity

library(data.table)
library(ggplot2)

results = readRDS("ex_kamp_dispersal_comp/res_processed/run_summary.rds")
results = results[alpha %in% c(0.4, 2) & gamma == 20 & comp_sc == 0.1]
results = results[, .(richness = mean(richness), ef.r1 = mean(ef.r1)), keyby = .(scenario, reach, alpha)]
results$sc_id = factor(0)
results[alpha == 0.4, sc_id := factor(as.integer(factor(scenario)))]
results[alpha == 2, sc_id := factor(as.integer(factor(scenario)))]
results[, alpha := factor(alpha, labels = c("Low dispersal", "High dispersal"))]

dir.create("ex_kamp_dispersal_comp/figures", showWarnings = FALSE)
pdf(file = "ex_kamp_dispersal_comp/figures/fig_reach_bef.pdf", width = 7.5, height = 4)
ggplot(results, aes(x = richness, y = ef.r1, colour = sc_id)) + geom_point(size = 0.7) + 
	geom_smooth(method = "lm", se = FALSE, lwd = 0.5) + 
	xlab("Local species richness") + ylab("Resource consumption") + 
	facet_grid(. ~ alpha) + theme_light() + scale_colour_discrete(guide = "none") +
	geom_smooth(data = results, aes(x = richness, y = ef.r1, colour = "1"), 
		colour = "black", method = "lm", se = FALSE, lwd = 1.5) + xlim(c(0, 13)) + ylim(c(0, 0.6))
dev.off()
