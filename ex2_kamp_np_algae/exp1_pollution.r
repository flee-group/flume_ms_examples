library(flume)
library(ggplot2)
library(data.table)

data(algae)
data(kamp)



dispersal_params = list(alpha = 0.05, beta = 0.1)

# set up metacommunity
n_args = with(algae,
	list(location = niche_params$location, breadth = niche_params$breadth, 
	scale_c = 9e-6 * niche_params$scale, scale_e = 1e-6 * niche_params$scale, 
	r_use = c(1e-3, 5e-4), ratio = matrix(1:2, ncol=2)))
mc = metacommunity(nsp = nrow(algae$niche_params), nr = 2, niches = niches_custom, 
	dispersal = dispersal_custom, sp_names = rownames(algae$niche_params), r_names = c("N", "P"), 
	niche_args = n_args, dispersal_args = dispersal_params, comp_scale = 1e-4)



# try adding boundary conditions to keep species from extinction. It assumes that dispersal from outside the network
# is equal to 1/4 of a site, for each site, using both active and passive dispersal
# this is useful, might make sense to put into package
qmat = matrix(boundary(kamp, "Q"), 
	nrow = length(kamp), 
	ncol = attr(mc, "n_species"))
bflux = sweep(qmat, 2, dispersal_params(mc)$beta, `*`)
bflux[bflux < 0] = 0
bnd_sp = 0.25 * sweep(bflux, 2, dispersal_params(mc)$alpha, `+`)



# set up the model
mod = flume(mc, kamp, algae$sp0, algae$r0, spb = bnd_sp)

# test
plot(res)
plot(res, "ef")


# simulate a point source of pollution
# add very high N to node 41
# EU water quality standards limit 50 mg/L NO3 (which is 11.3 mg/L of N from NO3)
# limit of sewage discharge into freshwater is 30 mg/L of N - we use this, assume
# 	a sewage plant of 100 L/s
# see: 
# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Archive:Agri-environmental_indicator_-_nitrate_pollution_of_water&oldid=127849
# https://environment.ec.europa.eu/topics/water/nitrates_en
# this one has lots of nice info, with some limits. 30 mg (total N) is the limit for discharges
# https://www.sciencedirect.com/science/article/pii/S0960852408002915

pnode = 41
p_ds = c(38:40, 25:28, 1:4)
p_discharge = 0.1

# run for 50 days no pollution
res_pol = run_simulation(mod, 50, reps = 1)


# NOTE for the package - this is ugly! A nice interface to update all of this would be great
# at the very least, having an option to propagate discharge changes downstream

# add pollution
boundary(res_pol$networks[[1]], "resources")[pnode, 1] = 30

# add discharge to the polluted and all downstream nodes
discharge(res_pol$networks[[1]])[c(pnode, p_ds), 1] = 
	discharge(res_pol$networks[[1]])[c(pnode, p_ds), 1] + p_discharge


# run for 200 more days with the pollution
res_pol = run_simulation(res_pol, 200, reps = 1)

# comparison with no pollution
res_nopol = run_simulation(mod, 250, reps = 1)

gridExtra::grid.arrange(
	plot(res_pol),
	plot(res_pol, "ef"),
	plot(res_nopol),
	plot(res_nopol, "ef"),
	ncol = 2
)


## idea: summarise species in affected nodes
## can compute beta diversity by node in polluted networks vs not-polluted networks
## abundance score is just the number of times present, replicated both across time and number of
## networks
## 4 groups: polluted network, affected nodes
## polluted, unafected
## both for nonpolluted
## can also plot beta div by distance from pollution for affected nodes in polluted and nonpolluted
summ = summarise(res_pol, by = c("species", "time", "reach"), stat = "occupancy")
# start from 50 time steps after disturbance
summ = summ[time >= 101]




# seems relatively ok with these parameters
# time to try next step - some kind of experiment

# res = run_simulation(res, 100, reps = 1)

# ## notes after an initial run
# # result - everything is a bit too cold. need to tweak to make it move faster.
# # occupancy gradually declines for all species, and they are in general way too stable
# ## Need to explore a bit (see what the e function, c function, and dispersal rates look like) to see what to tweak
# network = mod[["networks"]][[1]]
# comm = mod[["metacom"]]
# R = state(network, "resources")
# S = state(network, "species")
# col_prob(comm, network, dt = mod$dt)
# ext_prob(comm, network, dt = mod$dt)

# ## explore colonisation (by niche and dispersal portions)
# col_prob(comm, network, dt = mod$dt, components = TRUE)

# ## explore extinction
# ext_prob(comm, network, dt = mod$dt, components = TRUE)




# save everything