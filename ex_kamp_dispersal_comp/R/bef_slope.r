library(data.table)
library(ggplot2)

sc_results = readRDS("ex_kamp_dispersal_comp/res_processed/run_summary.rds")
plot_summary = sc_results[, .(bef_slope = coef(lm(ef.r1 ~ richness))[2], 
		# bef_sterr = summary(lm(ef.r1 ~ richness))$coefficients[2, 2],
		bef_lower = confint(lm(ef.r1 ~ richness), 2)[1], 
		bef_upper = confint(lm(ef.r1 ~ richness), 2)[2]), 
		keyby = .(family, gamma, alpha, breadth, sc_e, ru, comp_sc)]

plot_summary = plot_summary[gamma %in% c(20,40) & comp_sc %in% c(0.001, 0.1)]

comp_labs = c("weak", "strong")
names(comp_labs) = unique(plot_summary$comp_sc)

dir.create("ex_kamp_dispersal_comp/figures", showWarnings = FALSE)
pdf(file = "ex_kamp_dispersal_comp/figures/fig_bef_slope.pdf", width = 7.5, height = 6.5)
ggplot(plot_summary, aes(x=alpha, y = bef_slope)) + geom_point() + 
	geom_smooth() + 
	geom_errorbar(aes(x = alpha, ymin = bef_lower, ymax = bef_upper), width = 0) + 
	facet_grid(gamma ~ comp_sc, labeller = labeller(comp_sc = comp_labs)) + 
	xlab(expression(Active~dispersal~ability~(alpha))) + 
	ylab("Biodiversity-ecosystem functioning slope") + 
	scale_y_continuous(sec.axis = sec_axis(~ . , name = "Metacommunity gamma diversity", breaks = NULL, labels = NULL)) +
 	scale_x_continuous(sec.axis = sec_axis(~ . , name = "Competition strength", breaks = NULL, labels = NULL))
dev.off()

