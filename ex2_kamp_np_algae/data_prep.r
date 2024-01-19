library(flume)
library(ggplot2)
library(data.table)

data(algae)
data(kamp)

dispersal_params = list(alpha = 0.05, beta = 0.1)

# set up metacommunity
n_args = with(algae,
	list(location = niche_params$location, breadth = niche_params$breadth, 
	scale_c = 6e-6 * niche_params$scale, scale_e = 1.25e-7 * niche_params$scale, 
	r_use = 5e-4, ratio = matrix(1:2, ncol=2)))
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
res = run_simulation(mod, 120, reps = 1)
plot(res)
plot(res, "ef")

## notes after an initial run
# result - everything is a bit too cold. need to tweak to make it move faster.
# N consumption pretty low relative to P, probably the opposite of what it should be
# N is also more or less perfectly stable
# occupancy gradually declines for all species, and they are in general way too stable
## Need to explore a bit (see what the e function, c function, and dispersal rates look like) to see what to tweak
network = mod[["networks"]][[1]]
comm = mod[["metacom"]]
R = state(network, "resources")
S = state(network, "species")
col_prob(comm, network, dt = mod$dt)
ext_prob(comm, network, dt = mod$dt)

## explore colonisation (by niche and dispersal portions)
col_prob(comm, network, dt = mod$dt, components = TRUE)

## explore extinction
ext_prob(comm, network, dt = mod$dt, components = TRUE)




# save everything