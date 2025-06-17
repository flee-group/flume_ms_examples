library(data.table)
library(flume)
library(ggplot2)
res_files = list.files("ex2_kamp_np_algae/results")
data(kamp)

parse_fname = function(f) {
	regex = "poll_conc(\\d+)_Q(\\d+)_n(\\d+)\\.rds"
	if(f == "no_pollution.rds") {
		conc = 0
		Q = 0
		node = NA
	} else {
		conc = as.integer(sub(regex, "\\1", f))
		Q = as.integer(sub(regex, "\\2", f))
		node = as.integer(sub(regex, "\\3", f))
	}
	list(conc = conc, Q = Q, node = node)
}

process_results = function(f) {
	rr = readRDS(file.path("ex2_kamp_np_algae/results", f))
	params = parse_fname(f)

	# initial exploration showed this one was not needed
	# summ1 = summarise(rr, "reach", stat = "richness", t_steps = 131:161)
	# summ1$time_steps = "131:161"
	summ1 = summarise(rr, "reach", stat = "richness", t_steps = 202:231)

	summ2 = summarise(rr, c("reach", "resources"), stat = "concentration", t_steps = 202:231)
	summ2 = summ2[resources == "N"]
	summ2 = summ2[,resources := NULL]
	colnames(summ2) = c("reach", "N", "N_lo", "N_hi")
	
	res = merge(summ1, summ2, "reach")
	res$pollution_conc = params$conc
	res$pollution_Q = params$Q
	res$pollution_reach = params$node

	res
}

sim_results = rbindlist(parallel::mclapply(res_files, process_results, mc.cores = 4))

### Figure 1: richness map, using the highest concentration and a single pollution reach, days 202:231
### TODO: for sup mat, create maps of all scenarios with uncertainties
r_map_pol = sim_results[pollution_conc == 60 & pollution_Q == 200 & pollution_reach == 41, 
						.(reach, richness, N)]
r_map_npol = sim_results[pollution_conc == 0, .(reach, richness, N)]

# reorder the rows to match the order in the river network
r_map_pol = r_map_pol[match(attr(kamp, "names_sites"), r_map_pol$reach)]
r_map_npol = r_map_npol[match(attr(kamp, "names_sites"), r_map_npol$reach)]

## TODO: this should be replaced with plot.flume() methods now

atval = -1
line = 1.2
pdf(width=20, height = 20, file = "ex2_kamp_np_algae/figures/fig_ex2_map.pdf", pointsize=24)
par(mfrow=c(2,2), mar = c(0,1,3,0))
plot(kamp, attribute = r_map_pol$richness, legend_args = list(title = "Species\nrichness"), v_palette = "Purples")
mtext("A", side = 3, at = atval, line = line)
plot(kamp, attribute = r_map_pol$N, legend_args = list(title = "[N] (mg/L)"), v_palette = "OrRd")
mtext("B", side = 3, at = atval, line = line)
plot(kamp, attribute = r_map_npol$richness, legend_args = list(title = "Species\nrichness"), v_palette = "Purples")
mtext("C", side = 3, at = atval, line = line)
plot(kamp, attribute = r_map_npol$N, legend_args = list(title = "[N] (mg/L)"), v_palette = "OrRd")
mtext("D", side = 3, at = atval, line = line)
dev.off()




### Figure 2: compared across all treatments in polluted reaches

lags = list(r41 = data.table(
	reach = as.character(c(1,  2,  3, 4, 25, 26, 27, 28, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 51, 52)),
	lag =   c(11, 10, 9, 8, 7,  5,  6,  4,  3,  2,  1,  0,  -2, -1, -1, -2, -3, -3, -2, -2)
), r15 = data.table(
	reach = as.character(c(1,  2, 3, 5, 6, 7, 8, 9, 13, 14, 15, 16, 17)),
	lag =   c(10, 9, 8, 7, 6, 5, 4, 3, 1,  2,  0, -1, -1)
), r34 = data.table(
	reach = as.character(c(1,  2,  3,  4, 25, 26, 27, 28, 29, 31, 32, 33, 34, 35, 36, 37)),
	lag =   c(12, 11, 10, 9, 8,  6,  7,  5,  4,  1,  2,  3,  0,  -2, -1, -1)
))

# duplicate the control across each pollution reach
res_lplot = rbindlist(lapply(c(41, 15, 34), \(r) {
	res = sim_results[pollution_reach == r | is.na(pollution_reach)]
	res$pollution_reach = r
	merge(res, lags[[paste0('r', r)]], by = "reach", all.x = TRUE)
}))
res_lplot$label = NA
res_lplot$label[res_lplot$reach == "4"] = "4"


pdf(width=20, height = 20, file = "ex2_kamp_np_algae/figures/fig_ex2_rich_all.pdf")
ggplot(res_lplot, aes(x = lag, y = richness, colour = factor(pollution_conc))) + geom_line() + 
	geom_errorbar(aes(ymin = richness_lo, ymax = richness_hi), width = 0.5) + 
	facet_grid(rows = vars(pollution_reach), cols = vars(pollution_Q)) + 
	geom_label(aes(y = 40, label = label), colour = "black", size = 8, label.padding = unit(0.1, "lines")) + 
	geom_vline(xintercept = 0, lty=2, colour = "#777777") + 
	ylim(0, 45) + theme_light(base_size=24) + xlab("Lag (reaches from pollution source)") + ylab("Species Richness") +
	scale_colour_manual("[N] (mg/L)", values = c("#fdd49e", "#fc8d59", "#d7301f", "#7f0000"))
dev.off()

pdf(width=10, height = 20, file = "ex2_kamp_np_algae/figures/fig_ex2_rich_100.pdf", pointsize=36)
ggplot(res_lplot[pollution_Q == 100 | pollution_Q == 0], aes(x = lag, y = richness, colour = factor(pollution_conc))) + geom_line() + 
	geom_errorbar(aes(ymin = richness_lo, ymax = richness_hi), width = 0.5) + 
	facet_grid(rows = vars(pollution_reach)) + 
	geom_label(aes(y = 40, label = label), colour = "black", size = 8, label.padding = unit(0.1, "lines")) + 
	geom_vline(xintercept = 0, lty=2, colour = "#777777") + 
	ylim(0, 45) + theme_light(base_size=24) + xlab("Lag (reaches from pollution source)") + ylab("Species Richness") +
	scale_colour_manual("[N] (mg/L)", values = c("#a6bddb", "#fc8d59", "#d7301f", "#7f0000"))
dev.off()





### Figure 3: beta diversity
#' Compute jaccard index
jaccard = function(x, ...)
	UseMethod("jaccard", x)

#' Compute jaccard for a single experiment
#' @param x A flume model
#' @param output Whether to return a data.table (with quantiles) or a matrix (median only)
#' @param quantiles The quantiles to use if `output = 'data.table'`
#' @param tsteps The time steps to analyse
#' @param thresh The number of time steps a species must be present in order to be consdered part of the community
jaccard.flume = function(x, output = c("data.frame", "matrix"), quantiles = c(0.1, 0.9),
				   tsteps = 202:231, thresh = floor(0.25 * (max(tsteps) - min(tsteps)))) {
	output = match.arg(output)
	snames = attr(x$networks[[1]], "names_sites")
	species = lapply(x$networks, \(xx) state(xx, "species", history = TRUE)[tsteps])
	species = lapply(species, \(xx) Reduce(`+`, xx))
	species = lapply(species, \(xx) 1 * (xx >= thresh))
	J = lapply(species, jaccard)
	J = array(unlist(J), c(nrow(J[[1]]), ncol(J[[1]]), length(J)))
	Jmed = apply(J, c(1,2), median, na.rm = TRUE)
	rownames(Jmed) = colnames(Jmed) = snames

	if(output == "matrix")
		return(Jmed)
	
	Jlo = apply(J, c(1,2), quantile, quantiles[1], na.rm = TRUE)
	Jhi = apply(J, c(1,2), quantile, quantiles[2], na.rm = TRUE)
	rownames(Jlo) = colnames(Jlo) = rownames(Jhi) = colnames(Jhi) = snames
	Jmed = reshape2::melt(Jmed, varnames = c("reach1", "reach2"), value.name = "Jaccard")
	Jlo = reshape2::melt(Jlo, varnames = c("reach1", "reach2"), value.name = "Jaccard_lo")
	Jhi = reshape2::melt(Jhi, varnames = c("reach1", "reach2"), value.name = "Jaccard_hi")
	J_dt = merge(Jmed, Jlo, by = c("reach1", "reach2"))
	merge(J_dt, Jhi, by = c("reach1", "reach2"))
}

#' Compute the jaccard index from a site by species matrix
#' @param x A site-by-species matrix
jaccard.matrix = function(x) {
	site_sim = x %*% t(x)
	rich = rowSums(x)
	ra = matrix(rich, nrow=length(rich), ncol=length(rich))
	J = 1 - (site_sim / (ra + t(ra) - site_sim))
}


jaccard_pcoa = function(f) {
	rr = readRDS(file.path("ex2_kamp_np_algae/results", f))
	params = parse_fname(f)
	J = jaccard(rr, "matrix")
	res = data.table(ape::pcoa(J)$vectors[,1:3])
	res$pollution_conc = params$conc
	res$pollution_Q = params$Q
	res$pollution_reach = params$node
	res$reach = rownames(J)
	res
}


jacc_results = rbindlist(parallel::mclapply(res_files, jaccard_pcoa, mc.cores = 4))



# duplicate the control across each pollution reach
res_jacplot = rbindlist(lapply(c(41, 15, 34), \(r) {
	res = jacc_results[pollution_reach == r | is.na(pollution_reach)]
	res$network_status = ifelse(res$pollution_conc == 0, "control", "polluted")
	res$pollution_reach = r
	res = merge(res, lags[[paste0('r', r)]], by = "reach", all.x = TRUE)
	res$reach_status = ifelse(is.na(res$lag) | res$lag < 0, "not polluted", "polluted")
	res
}))

res_jacplot_subset = res_jacplot[(pollution_conc == 15 | pollution_conc == 0) & (pollution_Q == 50 | pollution_Q == 0)]
axgr = "#666666aa"
pdf(width=7, height = 10, file = "ex2_kamp_np_algae/figures/fig_ex2_beta.pdf")
ggplot(res_jacplot_subset, aes(x = Axis.1, y = Axis.2, colour = reach_status)) + 
	geom_vline(xintercept = 0, colour = axgr) + geom_hline(yintercept = 0, colour = axgr) + 
	geom_point() +
	facet_grid(pollution_reach ~ network_status) + 
	theme_minimal() + xlab("PCoA Axis 1") + ylab("PCoA Axis 2") + 
	scale_colour_manual("Reach Status", values = c("#8da0cb", "#fc8d62"))
dev.off()












### color nonsense, it is not working well for comparison

# convert PCoA axes to RGB colors
jaccard2rgb = function(ax1, ax2, ax3) {
	sc = function(ax) {
		ax = ax - min(ax)
		ax / max(ax)
	}
	ax1 = sc(ax1)
	ax2 = sc(ax2)
	ax3 = sc(ax3)
	rgb(ax1, ax2, ax3)
}


jmap_pol = res_jacplot[pollution_conc == 15 & pollution_Q == 50 & pollution_reach == 34]
jmap_pol$col = jaccard2rgb(jmap_pol$Axis.1, jmap_pol$Axis.2, jmap_pol$Axis.3)

jmap_npol = res_jacplot[pollution_conc == 0 & pollution_Q == 0 & pollution_reach == 34]
jmap_npol$col = jaccard2rgb(jmap_npol$Axis.1, jmap_npol$Axis.2, jmap_npol$Axis.3)

# reorder the rows to match the order in the river network
jmap_npol = jmap_npol[match(attr(kamp, "names_sites"), jmap_npol$reach)]
jmap_npol = jmap_npol[match(attr(kamp, "names_sites"), jmap_npol$reach)]

par(mfrow=c(1,2))
plot(kamp, vertex.color = jmap_pol$col)
plot(kamp, vertex.color = jmap_npol$col)

