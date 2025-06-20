library(flume)
library(ggplot2)
library(data.table)

# data(algae) # not needed as of 0.6.0.9009
data(kamp)
data(kamp_raw) # for the raw niche params, better customisation than just using the mc in data(kamp)
options(mc.cores = 4)

dispersal_params = list(alpha = 0.5, beta = 0.05)

n_args = with(algae,
			  list(location = niche_params$location, breadth = 0.5*niche_params$breadth, 
			  	 scale = list(col = 10* 1e-5 * niche_params$scale, ext = 1e-7), efun = "inverse_gaussian",
			  	 r_use = c(1e-3, 5e-4), ratio = matrix(1:2, ncol=2)))

mc = metacommunity(nsp = nrow(algae$niche_params), nr = 2, niches = niches_custom, 
	dispersal = dispersal_custom, sp_names = rownames(algae$niche_params), r_names = c("N", "P"), 
	niche_args = n_args, dispersal_args = dispersal_params, comp_scale = 1e-4)


# Add boundary conditions to keep species from extinction. 
# It assumes that dispersal from outside the network
# is equal to 1/4 of a site, for each site, using both active and passive dispersal
qmat = matrix(boundary(kamp, "Q"),
	nrow = length(kamp),
	ncol = attr(mc, "n_species"))
bflux = sweep(qmat, 2, dispersal_params(mc)$beta, `*`)
bflux[bflux < 0] = 0
# bnd_sp = 0.25 * sweep(bflux, 2, dispersal_params(mc)$alpha, `+`)
## 19 march, try drastically reducing this
# bnd_sp = 0 * sweep(bflux, 2, dispersal_params(mc)$alpha, `+`)
## 30 April try bringing it back, but 1/8 of a site
bnd_sp = 0.125 * sweep(bflux, 2, dispersal_params(mc)$alpha, `+`)

# set up the model
mod = flume(mc, kamp, algae$sp0, algae$r0, spb = bnd_sp)


## simple experiment: try pollution next to control to see if there is any effect

# simulate a point source of pollution by adding a point source of nitrogen
# EU water quality standards limit 50 mg/L NO3 (which is 11.3 mg/L of N from NO3)
# limit of sewage discharge into freshwater is 30 mg/L of N
# see: 
# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Archive:Agri-environmental_indicator_-_nitrate_pollution_of_water&oldid=127849
# https://environment.ec.europa.eu/topics/water/nitrates_en
# this one has lots of nice info, with some limits. 30 mg (total N) is the limit for discharges
# https://www.sciencedirect.com/science/article/pii/S0960852408002915
# here we choose three concentrations, equal to 0.5x, 1x, and 2x the maximum
poll_concentrations = c(15, 30, 60)

# Choose three different nodes to try and the source of pollution
pnodes = c(15, 34, 41)

# Will need to ensure that the extra discharge propagates downstream of the given node
# These are the nodes that are downstream of our polluted nodes
stop("TODO: replace this with the new add_discharge() interface")
dsnodes = list("n15" = c(13, 14, 1:9), "n34" = c(31:33, 25:29, 1:4), "n41" = c(38:40, 25:28, 1:4))

# Choose three different discharges for pollution: 50, 100, and 200 L/sec
poll_discharges = c(0.05, 0.1, 0.2)


# Phase 1: pre-pollution warm-up
phase1_len = 100

# Phase 2: run simulation after pollution
phase2_len = 130

# Number of replicated networks within a given flume
reps = 20

outdir = "ex2_kamp_np_algae/results"

model_settings = list(
	poll_concentrations = poll_concentrations, pnodes = pnodes, dsnodes = dsnodes, 
	poll_discharges = poll_discharges, phase1_len = phase1_len, phase2_len = phase2_len, reps = reps
)

saveRDS(model_settings, "model_settings.rds")


j = 1
maxj = length(pnodes) * length(poll_discharges) * length(poll_concentrations)
for(p_node in pnodes) {
	pn_name = paste0("n", p_node)
	p_ds = dsnodes[[pn_name]]
	for(p_discharge in poll_discharges) {
		for(p_conc in poll_concentrations) {
			fname = paste0("poll_conc", p_conc, "_Q", p_discharge*1000, "_", 
				pn_name, ".rds")
			cat(j, "of", maxj, "-", fname, "\n")
			# run first phase with no pollution
			res_pol = run_simulation(mod, phase1_len, reps = reps)

			# add pollution
			for(i in 1:reps) {
				boundary(res_pol$networks[[i]], "resources")[p_node, 1] = p_conc

				# add discharge to the polluted and all downstream nodes
				stop("TODO: replace this with the new add_discharge() interface")
				discharge(res_pol$networks[[i]])[c(p_node, p_ds), 1] = 
					discharge(res_pol$networks[[i]])[c(p_node, p_ds), 1] + p_discharge
			}
			# run second phase with the pollution
			res_pol = run_simulation(res_pol, phase2_len)

			fpath = file.path(outdir, fname)
			saveRDS(res_pol, fpath)
			j = j+1
		}
	}
}



# comparison with no pollution
res_nopol = run_simulation(mod, phase1_len + phase2_len, reps = reps)
saveRDS(res_nopol, file.path(outdir, "no_pollution.rds"))


# river network maps
# TODO: add an option to reduce the base size of the edge width

pol_summary = summarise(res_pol, stat = "richness", quantile = 0.5, t_steps = 201:231)
pol_summary = pol_summary[, .(richness = mean(richness)), by = .(reach)]
npol_summary = summarise(res_nopol, stat = "richness", quantile = 0.5, t_steps = 201:231)
npol_summary = npol_summary[, .(richness = mean(richness)), by = .(reach)]

par(mfrow = c(2,2), mar = c(0,0,0,0), oma = c(0,4,0,0))
i = match(attr(res_pol$networks[[1]],"names_sites"), pol_summary$reach)
smax = max(c(pol_summary$richness, npol_summary$richness))
plot.river_network(res_pol$networks[[1]], attribute = pol_summary$richness[i],
					legend_args = list(title = "richness"), zlim = c(0, smax))
mtext("Polluted", side = 2)
zmax = max(state(res_pol$networks[[1]], "resources")[,'N'])
plot.river_network(res_pol$networks[[1]], attribute = "resource", v_palette = "OrRd", zlim = c(0,zmax), log = TRUE)

i = match(attr(res_nopol$networks[[1]],"names_sites"), npol_summary$reach)
plot.river_network(res_nopol$networks[[1]], attribute = npol_summary$richness[i],
					legend_args = list(title = "richness"), zlim = c(0, smax))
mtext("Not Polluted", side = 2)
plot.river_network(res_nopol$networks[[1]], attribute = "resource", v_palette = "OrRd", zlim = c(0,zmax), log = TRUE)




# 
# hist(as.vector(col_prob(mod$metacom, mod$networks[[1]], mod$dt)), main = "Col prob, default")
# hist(as.vector(ext_prob(mod$metacom, mod$networks[[1]], mod$dt)), main = "Ext prob, default")
# 
# hist(as.vector(col_prob(res_pol$metacom, res_pol$networks[[1]], res_pol$dt)), main = "Col prob, polluted")
# hist(as.vector(ext_prob(res_pol$metacom, res_pol$networks[[1]], res_pol$dt)), main = "Ext prob, polluted")
# 
