library(flume)
library(ggplot2)
library(stringi)

# since species are random, we just generate a species list here using the same params to make an
# example figure

nsp = 20
dopts = list(alpha = 0.4, beta = 0.1)
nopts = list(
	location = sort(runif(nsp, 0.1, 0.9)), 
	breadth = 0.1, 
	scale_e = 1.25e-7, 
	scale_c = 6e-6, 
	r_use = 5e-4)

mcom = metacommunity(nsp = nsp, nr = 1, niches = niches_custom, niche_args = nopts, 
				dispersal = dispersal_custom, dispersal_args = dopts, comp_scale = 0.02)


## here we reproduce the code in plot.metacommunity, to allow a bit more customisation than is
## currently implemented in the package

xlim = c(0,1)
lwd = 0.7
R = matrix(seq(xlim[1], xlim[2], length.out = 500), ncol = 1)
niches = f_niche(mcom, N = R)
pldat = as.data.frame(niches)
pldat$r = R[,1]
pldat = reshape2::melt(pldat, id.vars = "r", variable.name = "species")
ylim = c(-0.1*max(pldat$value), max(pldat$value))

p1 = ggplot(pldat) + 
	geom_line(aes(x = r, y = value, group = species), colour = "#8b72c9", size = lwd) +
	ggplot2::theme_minimal() + ggplot2::xlab(attr(mcom, "niche_names")[1]) + 
	ggplot2::ylim(ylim[1], ylim[2]) +
	ggplot2::ylab("c(R) - m(R)") + xlab("Resource concentration (R)") + 
	ggplot2::geom_hline(ggplot2::aes(yintercept = 0), size = 1.2) + ggtitle("A")
# p1

comp = mcom$competition
comp[upper.tri(comp)] = NA
comp = reshape2::melt(comp)
comp = comp[complete.cases(comp), ]
colnames(comp) = c("sp1", "sp2", "Competition")
# comp$sp1 = factor(attr(mcom, "sp_names")[comp$sp1], levels = niche_par(mcom))
# comp$sp2 = factor(attr(mcom, "sp_names")[comp$sp2], levels = niche_par(mcom))

comp$sp1 = factor(stri_pad_right(as.character(round(niche_par(mcom)[comp$sp1], 2)), 4, 0))
comp$sp2 = factor(stri_pad_right(as.character(round(niche_par(mcom)[comp$sp2], 2)), 4, 0))


p2 = ggplot2::ggplot(comp) +
	ggplot2::geom_tile(ggplot2::aes(x = sp1, y = sp2, fill = Competition)) +
	ggplot2::scale_fill_viridis_c(option = "plasma") + ggplot2::theme_minimal() +
	ggplot2::xlab("Species' Niche Location") + ggplot2::ylab("") + ggtitle("B") + 
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = rel(0.8)))


dir.create("ex_kamp_dispersal_comp/figures", showWarnings = FALSE)
pdf(file = "ex_kamp_dispersal_comp/figures/fig_metacom.pdf", width = 7.5, height = 4)
gridExtra::grid.arrange(p1, p2, nrow = 1)
dev.off()

## a vertical one for the poster
# png(file = "ex_kamp_dispersal_comp/figures/fig_metacom_poster.png", height = 1200, width = 800)
pdf(file = ".ex_kamp_dispersal_comp/figures/fig_metacom_poster.pdf", height = 11, width = 7)
gridExtra::grid.arrange(p1, p2, ncol = 1)
dev.off()