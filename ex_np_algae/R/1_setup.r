library(flume)
data("algae")

set.seed(7777)

options(mc.cores = 4)


# note - modifications to the metacommunity to set niche options will probably be needed
# should probably experiment with this here, and then set it in the data script for the package
# at least for the defaults; will maybe want to tweak some aspects in different scenarios

# now: these are defaults for the package, and they are awful
# should also change these so they are working better in real life
# location, breadth vary by species
# scale_c: 0.5
# scale_e: 0.2
# r_use: 0.05
# comp_constant: 0.2 (1/nsp)

# dispersal
# alpha 0.05
# beta 0.5



#### values here from the first scenario, which seem reasonable to start
# location = sort(runif(nsp, 0.1, 0.9))
# breadth = 0.1
# scale_e = 1.25e-7
# scale_c = 6e-6
# r_use = 5e-4
# comp_constant = comp = c(1/1000, 1/100, 1/10, 1)
# alpha = c(0.05 , 0.4, 0.8, 1.2, 1.6, 2.0, 2.4, 2.8)
# beta = 0.1

mod = with(algae, flume(metacommunity, network, sp0, r0))

res = run_simulation(mod, 120, reps = 1)

## notes after an initial run
## not surprisingly, everything is too hot
## 3 species go immediately extinct and stay that way (no external colonisation)
## the remaining two occupy completely disjoint sites (comp too high?)
## every time step they switch (col prob and ext prob are both 1 for both species)





##### OLDY MODLY CODE BELOW


## how many times to repeat each particular scenario
## multithreaded, so up to mc.cores is free
n_iter = 2 * getOption("mc.cores")

nt = 120 # number of time steps of one simulation



###
# competition constant
###

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

