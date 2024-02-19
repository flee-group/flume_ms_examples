library(data.table)
library(flume)
library(ggplot2)
res_files = list.files("results")

process_results = function(f) {
	rr = readRDS(file.path("results", f))
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

	summ1 = summarise(rr, "reach", stat = "richness", t_steps = 131:161)
	summ1$time_steps = "131:161"
	summ2 = summarise(rr, "reach", stat = "richness", t_steps = 201:231)
	summ2$time_steps = "201:231"

	res = rbind(summ1, summ2)
	res$concentration = conc
	res$Q = Q
	res$pollution_reach = node

	res
}

sim_results = rbindlist(parallell:mclapply(res_files, process_results, mc.cores = 8))

## first figure: compare mean species richness in polluted nodes at different pollution discharge and concentration
fig1_res = sim_results[(is.na(pollution_reach) & reach %in% unique(pollution_reach)) | pollution_reach == reach]
fig1_res$polluted = !is.na(fig1_res$pollution_reach)
ggplot(fig1_res, aes(x = concentration, y = richness, color = factor(polluted))) + 
	geom_point() + 
	geom_errorbar(aes(x = concentration, ymin = richness_lo, ymax = richness_hi)) +
	facet_grid(reach ~ time_steps)
# clear story here - no effect on species richness

## second figure: show change in richness compared with control at the same time period as a line graph moving downstream
		## does richness converge on control?


model_settings = readRDS("model_settings.rds")

# replicate the control across different "treatments" for comparison
# note - should prob re-doc control where we *actually* change discharge with no poll
fig2_ctl = rbind(sim_results[is.na(pollution_reach)], 
	sim_results[is.na(pollution_reach)], sim_results[is.na(pollution_reach)])
fig2_ctl$Q = rep(c(50, 100, 200), each = sum(is.na(sim_results$pollution_reach)))

xorder_15 = c(21, 20, 19, 18, 17, 15, 14, 13, 9, 8, 7, 6, 5, 4, 3, 2, 1)
fig2_res_15 = rbind(
	sim_results[reach %in% xorder_15 & pollution_reach == 15],
	fig2_ctl[reach %in% xorder_15])
fig2_res_15$pollution_reach = 15
fig2_res_15$xpos = match(fig2_res_15$reach, xorder_15)

xorder_34 = c(35, 36, 34, 31, 32, 33, 29, 28, 26, 27, 25, 4, 3, 2, 1)
fig2_res_34 = rbind(
	sim_results[reach %in% xorder_34 & pollution_reach == 34],
	fig2_ctl[reach %in% xorder_34])
fig2_res_34$pollution_reach = 34
fig2_res_34$xpos = match(fig2_res_34$reach, xorder_34)

xorder_41 = c(42, 43, 41, 40, 39, 38, 28, 26, 27, 25, 4, 3, 2, 1)
fig2_res_41 = rbind(
	sim_results[reach %in% xorder_41 & pollution_reach == 41],
	fig2_ctl[reach %in% xorder_41])
fig2_res_41$pollution_reach = 41
fig2_res_41$xpos = match(fig2_res_41$reach, xorder_41)

fig2_res = rbind(fig2_res_15, fig2_res_34, fig2_res_41)
fig2_res = fig2_res[time_steps == "131:161"]
fig2_res[, is_pnode := (reach == pollution_reach)]

ggplot(fig2_res, aes(x = xpos, y = richness, colour = as.factor(concentration), pch = is_pnode)) + 
	geom_point() + geom_line() + 
	facet_grid(pollution_reach ~ factor(Q))

# the control is identical to pollution treatments
# at this point, need to consider why this is
# two hypotheses - maybe dispersal from outside is driving this, or maybe niche-based dynamics too
#    weak. might need a U-shaped extinction-curve to balance this. Can also make col niche narrower



# check beta diversity for above stories
## look at EF and the BEF relationship of polluted and non-polluted reaches and their downstream catchments


gridExtra::grid.arrange(
	plot(res_pol),
	plot(res_pol, "ef"),
	plot(res_nopol),
	plot(res_nopol, "ef"),
	ncol = 2
)
