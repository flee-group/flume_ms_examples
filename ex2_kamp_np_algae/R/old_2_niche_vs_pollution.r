library(flume)
library(ggplot2)

setwd("ex2_kamp_np_algae")
mc = readRDS("ex2_mc.rds")
res = readRDS("results/no_pollution_pnoden15.rds")

istate = lapply(state(res, "resources", history = TRUE), \(x) x[[1]])
endstate = state(res, "resources")

# get an average across all networks
endstate_mean = Reduce("+", endstate) / length(endstate)
istate_mean = Reduce("+", istate) / length(istate)

# print the range of initial and ending conditions
cat("Initial state range: ", range(istate_mean[, 1] / istate_mean[, 2]), "\n")
cat("Ending state range (no pollution): ", range(endstate_mean[, 1] / endstate_mean[, 2]), "\n")


# grab polluted scenarios
poll_files = outer(paste0("results/poll_conc", c(15, 60)), paste0("_pnoden", c(15, 34, 41), ".rds"), "paste0")
poll_res = lapply(poll_files, readRDS)

tlims = c(length(poll_res[[1]]) - 29, length(poll_res[[1]]) + 1)


# for a file, extract the last 30 time steps, then average them
avg_last30 = function(x) {
    reps = state(x, "resources", history = TRUE)
    reps_avg = lapply(reps, \(rn) Reduce("+", rn[lims[1]:lims[2]]) / (lims[2] - lims[1]))
    Reduce("+", reps_avg) / length(reps_avg)
}

# get the state by reach matrix for each scenario
state_by_reach = lapply(poll_res, avg_last30)

# output the extremes under different pollution scenarios
names(state_by_reach) = c("Low Poll, Reach 15", "Low Poll, Reach 34", "Low Poll, Reach 41",
    "High Poll, Reach 15", "High Poll, Reach 34", "High Poll, Reach 41")
cat("Polluted state ranges: \n")
for(i in seq_along(state_by_reach))
    cat("  ", names(state_by_reach)[i], " -- ", range(state_by_reach[[i]][, 1] / state_by_reach[[i]][, 2]), "\n")



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

# get some details about the niche locations and ending N:P ratios in an unpolluted network
cat("Range of niche locations: ", range(niche_par(mc)), "\n")
cat("Range of species extremes: ", range(sp_lims), "\n")



# make a figure showing the theoretical species richness under these conditions
res_seq = cbind(N = seq(15, 95, 0.1), P = 1)
pres = f_niche(mc, res_seq, component = "lambda") > 0
figdata = data.frame(NtoP = res_seq[, 1], richness = rowSums(pres), treat = 0)
figdata_state = data.frame(treat = 0, NtoP = endstate_mean[, 1] / endstate_mean[, 2])

ggplot(figdata) + geom_tile(aes(x = NtoP, y = treat, fill = richness)) +
    geom_point(data = figdata_state, aes(x = NtoP, y = treat), col = "maroon", size = 2) +
    scale_fill_viridis_c(limits = c(0, 45)) + xlab("N:P") + ylab("") + 
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())


range(istate[,1] / istate[,2])


