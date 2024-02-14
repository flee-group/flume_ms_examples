library(data.table)
library(flume)

res_files = list.files("results")

process_results = function(f) {
	rr = readRDS(file.path("results", f))
	regex = "poll_conc(\\d+)_Q(\\d+)_n(\\d+)_r(\\d)\\.rds"
	conc = as.integer(sub(regex, "\\1", f))
	Q = as.integer(sub(regex, "\\2", f))
	node = as.integer(sub(regex, "\\3", f))
	rep = sub(regex, "\\4", f)

}


## first figure: compare mean species richness in polluted nodes at different pollution discharge and concentration
## second figure: show change in richness compared with control at the same time period as a line graph moving downstream
		## does richness converge on control?
## look at EF and the BEF relationship of polluted and non-polluted reaches and their downstream catchments


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