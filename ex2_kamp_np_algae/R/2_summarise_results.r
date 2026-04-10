library(data.table)
library(flume)

res_files = list.files("results")
res_files = res_files[which(res_files != "simulation_summary.rds")]
x = sub("no_pollution", "poll_conc0", res_files)
pat = "poll_conc(\\d+)_pnoden(\\d+)\\.rds"

results_files = data.table(file = res_files,
    conc = as.integer(sub(pat, "\\1", x)),
    node = as.integer(sub(pat, "\\2", x)))


process_results = function(f, conc, node) {
    rr = readRDS(file.path("results", f))

    # initial exploration showed this one was not needed
    # summ1 = summarise(rr, "reach", stat = "richness", t_steps = 131:161)
    # summ1$time_steps = "131:161"
    summ1 = summarise(rr, "reach", stat = "richness", t_steps = 201:231)

    summ2 = summarise(rr, c("reach", "resources"), stat = "concentration", t_steps = 201:231)

    s2_med = dcast(summ2, reach ~ resources, value.var = "concentration")
    colnames(s2_med) = c("reach", "N_median", "P_median")
    s2_med[, NtoP_median := .(N_median / P_median)]

    s2_lo = dcast(summ2, reach ~ resources, value.var = "concentration_lo")
    colnames(s2_lo) = c("reach", "N_lo", "P_lo")
    s2_lo[, NtoP_lo := .(N_lo / P_lo)]

    s2_hi = dcast(summ2, reach ~ resources, value.var = "concentration_hi")
    colnames(s2_hi) = c("reach", "N_hi", "P_hi")
    s2_hi[, NtoP_hi := .(N_hi / P_hi)]

    res = merge(s2_med, s2_lo, by = "reach")
    res = merge(res, s2_hi, by = "reach")
    res = merge(res, summ1, by = "reach")

    res$pollution_conc = conc
    res$pollution_reach = node

    res
}
# ggplot(sim_summary) + geom_point(aes(x = NtoP_median, y = richness, color = factor(pollution_conc)))

sim_summary = parallel::mcmapply(process_results,
    f = results_files$file, conc = results_files$conc, node = results_files$node, mc.cores = 4, SIMPLIFY = FALSE)
sim_summary = rbindlist(sim_summary)
saveRDS(sim_summary, "results/simulation_summary.rds")
