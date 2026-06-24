library(flume)
library(data.table)
library(vegan)
library(plotrix)

set.seed(17294)
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
#' @param mean Compute mean jaccard (averaging across replicates)?
#' @param mean.thresh Number of replicates the sp must be present in to be present if averaging
jaccard_multi.flume = function(x, tsteps = 202:231,
    thresh = floor(0.25 * (max(tsteps) - min(tsteps))),
    mean = FALSE, mean.thresh = 0.25) {
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
        if(mean) {
            si_by_sp_res = Reduce(`+`, si_by_sp[[i]])
            si_by_sp_res = si_by_sp_res / length(si_by_sp[[i]])
            si_by_sp_res = 1.0 * (si_by_sp_res > mean.thresh)
            rownames(si_by_sp_res) = paste0(mod_name, "s", snames)
        } else {
            for(j in seq_along(si_by_sp[[i]])) {
                si_names = paste0(mod_name, "r", j, "_s", snames)
                rownames(si_by_sp[[i]][[j]]) = si_names
            }
            si_by_sp_res = do.call(rbind, si_by_sp[[i]])
        }
        si_by_sp_all[[i]] = si_by_sp_res
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
# drop the high pollution treatment - many sites have zero richness and this messes
# with the nmds
res = res[!grepl("conc60", names(res))]
jacc_all = jaccard_multi.flume(res, mean = TRUE)
nmds_all = metaMDS(jacc_all, distance = "jaccard", k = 2)

# only for running the huge matrix with all replicates
# saveRDS(nmds_all, "ex2_nmds.rds")
# nmds_all = readRDS("ex2_nmds.rds")

# now re-separate the different experiments
# compute the average and quantiles NMDS coords by site and treatment combo
pts = nmds_all$points
nmds_dt = data.table(nmds1 = pts[,1], nmds2 = pts[,2], label = rownames(jacc_all))
# pat = "p(\\d+)_n(\\d+)_r(\\d+)_s(\\d+)"
pat = "p(\\d+)_n(\\d+)_s(\\d+)"
nmds_dt[, pollution := as.integer(sub(pat, "\\1", label))]
nmds_dt[, node := as.integer(sub(pat, "\\2", label))]
nmds_dt[, reach := as.integer(sub(pat, "\\3", label))]

# nmds_dt[, replicate := as.integer(sub(pat, "\\3", label))]
# nmds_dt[, reach := as.integer(sub(pat, "\\4", label))]

nmds_dt[, pollution_status := "Unpolluted"]
nmds_dt[reach %in% 13:15 & node == 15, pollution_status := "Polluted"]
nmds_dt[reach %in% 31:34 & node == 34, pollution_status := "Polluted"]
nmds_dt[reach %in% 38:41 & node == 41, pollution_status := "Polluted"]
nmds_dt[reach %in% 5:9 & node == 15, pollution_status := "Diluted1"]
nmds_dt[reach %in% 25:29 & node == 34, pollution_status := "Diluted1"]
nmds_dt[reach %in% 25:28 & node == 41, pollution_status := "Diluted1"]
nmds_dt[reach %in% 1:4, pollution_status := "Diluted2"]

nmds_agg = nmds_dt[, .(nmds1 = mean(nmds1), nmds2 = mean(nmds2),
        nmds1_lo = quantile(nmds1, 0.1), nmds1_hi = quantile(nmds1, 0.9),
        nmds2_lo = quantile(nmds2, 0.1), nmds2_hi = quantile(nmds2, 0.9)),
        .(pollution, node, reach, pollution_status)]

col_npol = "#000044"
col_pol = "#ff6666"
col_d1 = "#aa33cc"
col_d2 = "#5522cc"
nmds_agg$color = col_npol
nmds_agg[pollution_status == "Polluted"]$color = col_pol
nmds_agg[pollution_status == "Diluted1"]$color = col_d1
nmds_agg[pollution_status == "Diluted2"]$color = col_d2


ell_data = nmds_agg[, .(nmds1 = mean(nmds1), nmds2 = mean(nmds2), 
    a = diff(range(nmds1)), b = diff(range(nmds2)), angle = 0),
    .(pollution_status, pollution, node, color)]

# some manual tweaking of the ellipses

# reach 15, control, diluted1
i = 2
ell_data[i]$a = ell_data[i]$a * 0.7
ell_data[i]$b = ell_data[i]$b * 0.7

# reach 15, pollution treatment, diluted 1
i = 14
ell_data[i]$a = ell_data[i]$a * 0.5

# reach 15, pollution treatment, polluted reaches
i = 16
ell_data[i]$b = ell_data[i]$b * 0.57
ell_data[i]$nmds1 = ell_data[i]$nmds1 - 0.0045 * ell_data[i]$nmds1
ell_data[i]$nmds2 = ell_data[i]$nmds2 + 0.003 * ell_data[i]$nmds2


# reach 34, control, polluted reaches
i = 8
ell_data[i]$a = ell_data[i]$a * 0.3
ell_data[i]$angle = pi/4

# reach 41, control, diluted 1
i = 23
ell_data[i]$a = ell_data[i]$a * 0.8


# reach 41, pollution treatment, diluted 2
i = 21
ell_data[i]$a = ell_data[i]$a * 0.6
ell_data[i]$angle = -pi/8





pdf(width=10, height = 20, file = "figures/fig_ex2_beta.pdf", pointsize = 8)

ellipse.lwd = 4
labsize = 2
lab.x = -0.000249
lab.y = -0.001475
oma = c(1, 2, 2, 1)
par(mfrow = c(2, 1), bty = 'l', cex = 1.5, cex.axis = 1.5,
    mar = c(4,4,0,0), oma = oma, cex.lab = 2)
xl = range(nmds_agg[node == 15]$nmds1)
yl = range(nmds_agg[node == 15]$nmds2)
with(nmds_agg[node == 15 & pollution == 0],
    plot(nmds1, nmds2, col = color, pch = 16, cex = 2, xlim = xl, ylim = yl, xlab = "", ylab = "")
)
with(ell_data[node == 15 & pollution == 0 & pollution_status != "Unpolluted"],
    draw.ellipse(x = nmds1, y = nmds2, a = a, b = b, angle = angle, border = color, lwd = ellipse.lwd, deg = FALSE)
)
text(lab.x, lab.y, "Control", cex = labsize)

with(nmds_agg[node == 15 & pollution == 15],
    plot(nmds1, nmds2, col = color, pch = 16, cex = 2, xlim = xl, ylim = yl, xlab = "", ylab = "")
)
with(ell_data[node == 15 & pollution == 15 & pollution_status != "Unpolluted"],
    draw.ellipse(x = nmds1, y = nmds2, a = a, b = b, angle = angle, border = color, lwd = ellipse.lwd, deg = FALSE)
)
text(lab.x, lab.y, "Polluted", cex = labsize)
legend("bottomleft", pch = 16, col = c(col_npol, col_pol, col_d1, col_d2),
    legend = c("Unpolluted", "Pollluted", "Diluted-1", "Diluted-2"), cex = 2, bty = 'n')

mtext("NMDS 1", side = 1, outer = TRUE, cex = 3)
mtext("NMDS 2", side = 2, outer = TRUE, cex = 3)

dev.off()


# make additional sup mat figures for the other two sites
pdf(width=10, height = 20, file = "figures/fig_ex2_beta_supp34.pdf", pointsize = 8)

# lab.x = -0.000245
# lab.y = -0.00147
par(mfrow = c(2, 1), bty = 'l', cex = 1.5, cex.axis = 1.5,
    mar = c(4,4,0,0), oma = oma, cex.lab = 2)
xl = range(nmds_agg[node == 34 & pollution_status != "Polluted"]$nmds1)
yl = range(nmds_agg[node == 34 & pollution_status != "Polluted"]$nmds2)
with(nmds_agg[node == 34 & pollution == 0],
    plot(nmds1, nmds2, col = color, pch = 16, cex = 2, xlim = xl, ylim = yl, xlab = "", ylab = "")
)
with(ell_data[node == 34 & pollution == 0 & pollution_status != "Unpolluted"],
    draw.ellipse(x = nmds1, y = nmds2, a = a, b = b, angle = angle, border = color, lwd = ellipse.lwd, deg = FALSE)
)
text(lab.x, lab.y, "Control", cex = labsize)

with(nmds_agg[node == 34 & pollution == 15],
    plot(nmds1, nmds2, col = color, pch = 16, cex = 2, xlim = xl, ylim = yl, xlab = "", ylab = "")
)
with(ell_data[node == 34 & pollution == 15 & pollution_status != "Unpolluted"],
    draw.ellipse(x = nmds1, y = nmds2, a = a, b = b, angle = angle, border = color, lwd = ellipse.lwd, deg = FALSE)
)
text(lab.x, lab.y, "Polluted", cex = labsize)
legend("bottomleft", pch = 16, col = c(col_npol, col_pol, col_d1, col_d2),
    legend = c("Unpolluted", "Pollluted", "Diluted-1", "Diluted-2"), cex = 2, bty = 'n')

mtext("NMDS 1", side = 1, outer = TRUE, cex = 3)
mtext("NMDS 2", side = 2, outer = TRUE, cex = 3)

dev.off()



pdf(width=10, height = 20, file = "figures/fig_ex2_beta_supp41.pdf", pointsize = 8)

lab.x = -0.000255
lab.y = -0.00147
par(mfrow = c(2, 1), bty = 'l', cex = 1.5, cex.axis = 1.5,
    mar = c(4,4,0,0), oma = oma, cex.lab = 2)
xl = range(nmds_agg[node == 41 & pollution_status != "Polluted"]$nmds1)
yl = range(nmds_agg[node == 41 & pollution_status != "Polluted"]$nmds2)
with(nmds_agg[node == 41 & pollution == 0],
    plot(nmds1, nmds2, col = color, pch = 16, cex = 2, xlim = xl, ylim = yl, xlab = "", ylab = "")
)
with(ell_data[node == 41 & pollution == 0 & pollution_status != "Unpolluted"],
    draw.ellipse(x = nmds1, y = nmds2, a = a, b = b, angle = angle, border = color, lwd = ellipse.lwd, deg = FALSE)
)
text(lab.x, lab.y, "Control", cex = labsize)

with(nmds_agg[node == 41 & pollution == 15],
    plot(nmds1, nmds2, col = color, pch = 16, cex = 2, xlim = xl, ylim = yl, xlab = "", ylab = "")
)
with(ell_data[node == 41 & pollution == 15 & pollution_status != "Unpolluted"],
    draw.ellipse(x = nmds1, y = nmds2, a = a, b = b, angle = angle, border = color, lwd = ellipse.lwd, deg = FALSE)
)
text(lab.x, lab.y, "Polluted", cex = labsize)
legend("bottomleft", pch = 16, col = c(col_npol, col_pol, col_d1, col_d2),
    legend = c("Unpolluted", "Pollluted", "Diluted-1", "Diluted-2"), cex = 2, bty = 'n')

mtext("NMDS 1", side = 1, outer = TRUE, cex = 3)
mtext("NMDS 2", side = 2, outer = TRUE, cex = 3)

dev.off()
