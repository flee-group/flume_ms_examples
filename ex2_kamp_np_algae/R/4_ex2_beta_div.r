library(flume)
library(data.table)
library(vegan)
library(ggplot2)

### Figure 3: beta diversity
#' Compute jaccard index
jaccard = function(x, ...)
    UseMethod("jaccard", x)

#' Extract an occupancy matrix from a flume model
#' pull out the site by species matrix for applicable time steps
#' sum them, then determine occupancy based on exceeding the threshold
#' 
#' @param x A flume model
#' @param tsteps The time steps to analyse
#' @param thresh The number of time steps a species must be present in order to be consdered part of the community
site_by_species_occupancy.flume = function(x, tsteps, thresh) {
    species = lapply(x$networks, \(xx) state(xx, "species", history = TRUE)[tsteps])
    species = lapply(species, \(xx) Reduce(`+`, xx))
    species = lapply(species, \(xx) 1 * (xx >= thresh))
    species
}

#' Compute jaccard for a single experiment
#' @param x A flume model
#' @param output Whether to return a data.table (with quantiles) or a matrix (median only) or a list (all results)
#' @param quantiles The quantiles to use if `output = 'data.table'`
#' @param tsteps The time steps to analyse
#' @param thresh The number of time steps a species must be present in order to be consdered part of the community
jaccard.flume = function(x, output = c("data.frame", "matrix", "list"), quantiles = c(0.1, 0.9),
                   tsteps = 202:231, thresh = floor(0.25 * (max(tsteps) - min(tsteps)))) {
    output = match.arg(output)
    snames = attr(x$networks[[1]], "names_sites")
    species = site_by_species_occupancy.flume(x, tsteps, thresh)
    J = lapply(species, jaccard.matrix)

    if(output == "list")
        return(J)

    J = array(unlist(J), c(nrow(J[[1]]), ncol(J[[1]]), length(J)))

    Jmed = apply(J, c(1,2), median, na.rm = TRUE)
    rownames(Jmed) = colnames(Jmed) = snames

    if(output == "matrix")
        return(Jmed)
    
    Jlo = apply(J, c(1,2), quantile, quantiles[1], na.rm = TRUE)
    Jhi = apply(J, c(1,2), quantile, quantiles[2], na.rm = TRUE)
    rownames(Jlo) = colnames(Jlo) = rownames(Jhi) = colnames(Jhi) = snames
    Jmed = reshape2::melt(Jmed, varnames = c("reach1", "reach2"), value.name = "Jaccard")
    Jlo = reshape2::melt(Jlo, varnames = c("reach1", "reach2"), value.name = "Jaccard_lo")
    Jhi = reshape2::melt(Jhi, varnames = c("reach1", "reach2"), value.name = "Jaccard_hi")
    J_dt = merge(Jmed, Jlo, by = c("reach1", "reach2"))
    merge(J_dt, Jhi, by = c("reach1", "reach2"))
}



#' Compute a single jaccard dissimilarity matrix for multiple flume experiments
#' @param x A list of flume models
#' @param tsteps The time steps to analyse
#' @param thresh The number of time steps a species must be present in order to be consdered part of the community
jaccard_multi.flume = function(x, tsteps = 202:231, 
    thresh = floor(0.25 * (max(tsteps) - min(tsteps)))) {
    # we assume all networks have the same site names
    snames = attr(x[[1]]$networks[[1]], "names_sites")

    # parse out the model names
    poll = sub("poll_conc(\\d+)_.+", "\\1", names(x))
    poll[grepl("no_pollution", poll)] = "0"
    poll = as.integer(poll)
    node = as.integer(sub(".+pnoden(\\d+).+", "\\1", names(x)))

    si_by_sp = lapply(x, site_by_species_occupancy.flume, tsteps, thresh)

    # apply row names that reflect experimental parameters, collapse into
    # a single mega matrix
    si_by_sp_all = list()
    for(i in seq_along(si_by_sp)) {
        mod_name = paste0("p", poll[i], "_n", node[i], "_")
        for(j in seq_along(si_by_sp[[i]])) {
            si_names = paste0(mod_name, "r", j, "_s", snames)
            rownames(si_by_sp[[i]][[j]]) = si_names
        }
        si_by_sp_all[[i]] = do.call(rbind, si_by_sp[[i]])
    }
    si_by_sp_all = do.call(rbind, si_by_sp_all)

    J = jaccard.matrix(si_by_sp_all)
}

#' Compute the jaccard index from a site by species matrix
#' @param x A site-by-species matrix
jaccard.matrix = function(x) {
    site_sim = x %*% t(x)
    rich = rowSums(x)
    ra = matrix(rich, nrow=length(rich), ncol=length(rich))
    J = 1 - (site_sim / (ra + t(ra) - site_sim))
    J[which(is.nan(J))] = 1 # dissimilarity is maximum if a focal site has no species
    J
}

res_path = file.path("results")
res_files = list.files(res_path, pattern = ".rds")
res_files = res_files[-which(res_files == "simulation_summary.rds")]
poll_conc = sub("no_pollution.+\\.rds", "0", res_files)
poll_conc = as.integer(sub("poll_conc(\\d+).+\\.rds", "\\1", poll_conc))
poll_node = as.integer(sub(".+pnoden(\\d+)\\.rds", "\\1", res_files))
res = lapply(file.path(res_path, res_files), readRDS)
names(res) = res_files

# lump ALL sites, all replicates into one mega matrix and compute NMDS on this
# very slow, so we will save this and cache it for re-running later
jacc_all = jaccard_multi.flume(res)
# nmds_all = metaMDS(jacc_all, distance = "jaccard", k = 2)

######## START HERE

saveRDS(nmds_all, "ex2_nmds.rds")
nmds_all = readRDS("ex2_nmds.rds")

# now re-separate the different experiments
# compute the average and quantiles NMDS coords by site and treatment combo


####### OLD STUFF BELOW

jaccard_list = lapply(res, jaccard.flume, "matrix")
jacc_nmds = lapply(jaccard_list, metaMDS, distance = "jaccard", k = 2)


col_npol = "#0066cc"
col_pol = "#cc0099"
col_pol2 = "#9900cc"
col_pol3 = "#3300cc"
col15 = col34 = col41 = rep(col_npol, 52)
col15[13:15] = col_pol
col15[5:9] = col_pol2
col15[1:4] = col34[1:4] = col41[1:4] = col_pol3
col34[31:34] = col_pol
col34[25:29] = col41[25:29] = col_pol2
col41[38:41] = col_pol


pdf(width=15, height = 20, file = "figures/fig_ex2_beta.pdf")

par(mfrow = c(3, 2), bty = 'l', cex = 1.5, mar = c(4,4,0,0), oma = c(1, 1, 1, 1))
    plot(jacc_nmds[[1]]$points[,1], jacc_nmds[[1]]$points[,2], xlab = "", ylab = "NMDS 2", 
        pch = 16, col = col15)
    legend("topleft", legend = "Reach 15\nControl", pch = 16, col = "#ffffff", bty='n')
    plot(jacc_nmds[[4]]$points[,1], jacc_nmds[[4]]$points[,2], xlab = "", ylab = "", 
        pch = 16, col = col15)
    legend("topleft", legend = "Reach 15\nPolluted", pch = 16, col = "#ffffff", bty='n')

    plot(jacc_nmds[[2]]$points[,1], jacc_nmds[[2]]$points[,2], xlab = "", ylab = "NMDS 2", 
        pch = 16, col = col34)
    legend("topleft", legend = "Reach 34\nControl", pch = 16, col = "#ffffff", bty='n')
    plot(jacc_nmds[[5]]$points[,1], jacc_nmds[[5]]$points[,2], xlab = "", ylab = "", 
        pch = 16, col = col34, xlim = c(-0.6, 0.5))
    legend("topleft", legend = "Reach 34\nPolluted", pch = 16, col = "#ffffff", bty='n')

    plot(jacc_nmds[[3]]$points[,1], jacc_nmds[[3]]$points[,2], xlab = "NMDS 1", ylab = "NMDS 2", 
        pch = 16, col = col41)
    legend("topleft", legend = "Reach 41\nControl", pch = 16, col = "#ffffff", bty='n')
    legend("topright", legend = c("Unpolluted", "Polluted", "Diluted 1", "Diluted 2"), pch = 16, 
        col = c(col_npol, col_pol, col_pol2, col_pol3))
    plot(jacc_nmds[[6]]$points[,1], jacc_nmds[[6]]$points[,2], xlab = "NMDS 1", ylab = "", 
        pch = 16, col = col41)
    legend("topleft", legend = "Reach 41\nPolluted", pch = 16, col = "#ffffff", bty='n')

dev.off()





