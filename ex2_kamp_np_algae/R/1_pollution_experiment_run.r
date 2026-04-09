library(flume)
library(ggplot2)
library(data.table)

data(kamp)
data(kamp_raw) # for the raw niche params, better customisation than just using the mc in data(kamp)
options(mc.cores = 4)

dispersal_params = list(alpha = 0.5, beta = 0.05)

n_args = list(location = kamp_raw$niches$location, breadth = 0.5 * kamp_raw$niches$breadth,
    scale = list(col = 1e-4 * kamp_raw$niches$scale, ext = 1e-7), efun = "inverse_gaussian",
    r_use = c(1e-3, 5e-4), ratio = matrix(1:2, ncol = 2))

mc = metacommunity(nsp = nrow(kamp_raw$niches), nr = 2, niches = niches_custom,
    dispersal = dispersal_custom, sp_names = rownames(kamp_raw$niches), r_names = c("N", "P"),
    niche_args = n_args, dispersal_args = dispersal_params, comp_scale = 1e-4)
saveRDS(mc, "ex2_mc.rds")


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
poll_concentrations = c(15, 60)

# Choose three different nodes to try and the source of pollution
pnodes = c(15, 34, 41)

# Discharge rate of polluted water
p_discharge = 0.1

# Phase 1: pre-pollution warm-up
phase1_len = 100

# Phase 2: run simulation after pollution
phase2_len = 130

# Number of replicated networks within a given flume
reps = 20

outdir = "results"
dir.create(outdir, showwarnings = FALSE)

model_settings = list(
    poll_concentrations = poll_concentrations, pnodes = pnodes, p_discharge = p_discharge,
    phase1_len = phase1_len, phase2_len = phase2_len, reps = reps
)

saveRDS(model_settings, "model_settings.rds")


j = 1
maxj = length(pnodes) * length(poll_concentrations)
for(p_node in pnodes) {
    pn_name = paste0("n", p_node)
    p_ds = dsnodes[[pn_name]]
    for(p_conc in poll_concentrations) {
        fname = paste0("poll_conc", p_conc, "_pnode", pn_name, ".rds")
        cat(j, "of", maxj, "-", fname, "\n")
        # run first phase with no pollution
        res_pol = run_simulation(mod, phase1_len, reps = reps)

        # add pollution; first we change the boundary concentration and then we propagate discharge
        # need to do it to each rep separately
        for(i in 1:reps) {
            boundary(res_pol$networks[[i]], "resources")[p_node, 1] = p_conc
            discharge(res_pol$networks[[i]]) = add_discharge(res_pol$networks[[i]], p_node, p_discharge)
        }

        # run second phase with the pollution
        res_pol = run_simulation(res_pol, phase2_len)

        fpath = file.path(outdir, fname)
        saveRDS(res_pol, fpath)
        j = j + 1
    }

    cat("Running control for node",  p_node, "\n")
    fname = paste0("no_pollution", "_pnode", pn_name, ".rds")
    # run first phase with no pollution
    res_nopol = run_simulation(mod, phase1_len, reps = reps)

    # add discharge; need to do it to each rep separately
    for(i in 1:reps) {
        discharge(res_nopol$networks[[i]]) = add_discharge(res_nopol$networks[[i]], p_node, p_discharge)
    }

    # run second phase with the added discharge
    res_nopol = run_simulation(res_nopol, phase2_len)
    saveRDS(res_nopol, file.path(outdir, fname))
}
