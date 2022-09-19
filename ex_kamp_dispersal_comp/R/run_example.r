library(flume)
library(Matrix)
library(units)
library(data.table)

data("flume_networks")
net = flume_networks$kamp

set.seed(1703)

options(mc.cores = 4)

# generate x species with same niche breadth, niche height but random location
nsp = 50

# default values for different niche options
location = sort(runif(nsp, 0.1, 0.9))
breadth = 0.1
scale_e = 1.25e-7
scale_c = 6e-6
r_use = 5e-4
comp_constant = 0.02

# number of different dispersal abilities
alpha = c(0.05 , 0.4, 0.8, 1.2, 1.6, 2.0, 2.4, 2.8)
n_disp = length(alpha)

# passive dispersal is kept constant at the moment
beta = 0.1

# starting prevalence for species in the network
prev = 0.2

# starting/boundary state of the network
start_r = matrix(0.5, nrow = length(net), ncol = 1)
## headwaters are most extreme
start_r[c(10:12, 16, 20:24), ] = 0.9
start_r[c(42:43, 46, 47:52), ] = 0.1

start_r[c(9, 13:15, 17:19), ] = 0.7
start_r[c(38:41, 44:45), ] = 0.3


## gamma diversity scenarios
## how many species are possible in each scenario
g_div = c(10, 20, 30, 40)
n_gamma = length(g_div)

## how many times to repeat each particular species richness (with new random species)
## and dispersal combo
n_reps = 10

## how many times to repeat each particular combination
## multithreaded, so up to mc.cores is free
n_iter = 2 * getOption("mc.cores")

nt = 120 # number of time steps of one simulation


# generate random species lists for each gamma
sps = lapply(g_div, \(gam) t(sapply(1:n_reps, \(n) sample(nsp, gam, replace = FALSE))))
names(sps) = paste("g", g_div, sep="")
saveRDS(sps, "ex_kamp_dispersal_comp/splists.rds")

###
# competition constant
###
comp = c(1/1000, 1/100, 1/10, 1)
scenarios = data.table(expand.grid(alpha = alpha, beta = beta, breadth = breadth, sc_e = scale_e, 
		sc_c = scale_c, ru = r_use, nsp = nsp, comp_sc = comp, prevalence = prev, nt = nt, 
		niter = n_iter, gamma = g_div, splist_number = 1:n_reps, family = "comp_sc"))
saveRDS(scenarios, "ex_kamp_dispersal_comp/scenarios.rds")




run_scenario = function(params, locs, network, spids, start_r) {
	with(params, {
		dopts = list(alpha = alpha, beta = beta)
		nopts = list(location = locs, breadth = breadth, scale_e = sc_e, scale_c = sc_c, r_use = ru)
		mcom = metacommunity(nsp = nsp, nr = 1, niches = niches_custom, niche_args = nopts, 
				dispersal = dispersal_custom, dispersal_args = dopts, comp_scale = comp_sc)
		start_sp = matrix(0, nrow = length(network), ncol = nsp)
		for(i in spids)
			start_sp[,i] = rbinom(length(network), 1, prevalence)
		mod = flume(mcom, network, start_sp, start_r)
		suppressMessages(run_simulation(mod, nt, reps = niter))
	})
}

# create results directory if needed
dir.create("ex_kamp_dispersal_comp/res", showWarnings = FALSE)

for(i in 1:nrow(scenarios)) {
	cat("scenario", i, "of", nrow(scenarios), "\r")
	scenario = scenarios[i]
	if(scenario$gamma != 40) next() # skip non-40 scenarios, there was an error in initial runs
	splist = sps[[paste0("g", scenario$gamma)]][scenario$splist_number, ]
	res = run_scenario(scenario, location, net, splist, start_r)
	saveRDS(res, paste0("ex_kamp_dispersal_comp/res/scenario_", i, ".rds"))
}

