library(flume)
library(ggplot2)

mc = readRDS("ex2_mc.rds")


# quick and dirty - find limits of the niches of species
res_seq = cbind(seq(0, 200, 0.01), 1)
theoretical_fn = f_niche(mc, res_seq, component = "lambda")

# x is the output of f_niche for a single species
# R is the resource conditions sent to f_niche
# we return R[,1] because we have fixed P = 1 just for the illustration (only the ratio matters)
get_lims = function(x, R) {
    i = min(which(x > 0))
    j = max(which(x > 0))
    c(R[i,1], R[j,1])
}
sp_lims = apply(theoretical_fn, 2, get_lims, R = res_seq)

# add a vertical line where no species can persis
pdf("figures/fig_ex2_metacom.pdf", width = 20, height = 12, pointsize = 12)
plot(mc, plot_comp = FALSE, sp_legend = FALSE) + 
    geom_vline(xintercept = max(sp_lims), linewidth = 2, linetype = "dashed") + 
    theme(text = element_text(size = 36), 
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 20))) + 
    xlab("N:P Ratio")
dev.off()
