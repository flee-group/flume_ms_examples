library(flume)
library(data.table)

files = list.files("ex_kamp_dispersal_comp/res", full.names = TRUE, pattern = 'rds')
## for all metrics, average over the last 30 time steps
tsteps = 62:91

scenarios = readRDS("ex_kamp_dispersal_comp/scenarios.rds")
scenarios$id = 1:nrow(scenarios)

#' Get length of time a flume has been run
flume_t = function(x) {
	length(x$networks[[1]][[".species"]])
}

#' Get richness by reach from a fitted flume model
#' @param x A fitted flume model
#' @param t The time steps to summarize
#' @param agg The aggregation function across time, default is mean
reach_div = function(x, t = ceiling(flume_t(x)/2):flume_t(x), agg = mean) {
	div = lapply(x$networks, \(n) {
		sp_hist = state(n, "species", history = TRUE)[t]
		div = sapply(sp_hist, \(y) rowSums(y != 0))
		div = apply(div, 1, agg)
		data.table(reach = names(div), richness = div)
	})
	data.table::rbindlist(div, id = "replicate")
}

#' Get ecosystem function by reach from a fitted flume model
#' @param x A fitted flume model
#' @param t The time steps to summarize
#' @param agg The aggregation function across time, default is +
reach_ef = function(x, t = ceiling(flume_t(x)/2):flume_t(x), agg = `+`) {
	ef = lapply(x$networks, \(n) {
		ef_hist = state(n, "reaction", history = TRUE)[t]
		ef = Reduce(`+`, ef_hist)
		data.table(reach = rownames(ef), ef = -ef)
	})
	data.table::rbindlist(ef, id = "replicate")
}


sc_results = lapply(files, \(f) {
	fit = readRDS(f)
	num = as.integer(sub(".+scenario_(\\d+)\\.rds", "\\1", f))
	res = reach_div(fit, tsteps)
	res = merge(res, reach_ef(fit, tsteps), by = c("reach", "replicate"))
	res$scenario = num
	res
})
sc_results = data.table::rbindlist(sc_results)
sc_results = merge(sc_results, scenarios[, .(id, family, gamma, alpha, breadth, sc_e, ru, comp_sc)], 
	by.x = "scenario", by.y = "id")

dir.create("ex_kamp_dispersal_comp/res_processed", showWarnings = FALSE)
saveRDS(sc_results, "ex_kamp_dispersal_comp/res_processed/run_summary.rds")
