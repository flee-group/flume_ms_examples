library(flume)

# header
header = "\\begin{tabular}{ll|ccc|ccc} \n \\toprule Polluted Reach & Reach Number\\textsuperscript{*} & \\multicolumn{3}{c|}{N:P} & \\multicolumn{3}{c}{Species Richness} \\\\ \n & & Control & Low Pollution & High Pollution & Control & Low Pollution & High Pollution \\\\ \n \\midrule \n"
footer = "\\multicolumn{8}{l}{\\footnotesize \\textsuperscript{*}Reaches are ordered from upstream to down, starting with the polluted reach} \\\\ \n \\bottomrule \\end{tabular} \n"


# the data matrix part of this table
# 3 tables, one per bloc
tab15 <- matrix("", nrow = 4, ncol = 8)
tab15[1,1] = "\\multirow{4}{*}{15}"
tab15[2:3, 1] = ""
tab15[,2] = c(15, 13, 14, 9)

tab34 <- matrix("", nrow = 4, ncol = 8)
tab34[1,1] = "\\multirow{4}{*}{34}"
tab34[2:3, 1] = ""
tab34[,2] = c(34,31,32,33)

tab41 <- matrix("", nrow = 4, ncol = 8)
tab41[1,1] = "\\multirow{4}{*}{41}"
tab41[2:3, 1] = ""
tab41[,2] = c(41,40,39,38)


### REACH 15 scenarios
# get N:P ratios for control treatment for interesting reaches
res = readRDS("results/no_pollution_pnoden15.rds")
i = c(15, 13, 14, 9)
endstate = state(res, "resources")
endstate_mean = Reduce("+", endstate) / length(endstate)
tab15[1:4, 3] <- round(endstate_mean[i, 1] / endstate_mean[i, 2], 1)
endstate = state(res, "species")
enstate_rich = lapply(endstate, rowSums)
enstate_rich = Reduce("+", enstate_rich) / length(enstate_rich)
tab15[1:4, 6] <- round(enstate_rich[i], 1)

res = readRDS("results/poll_conc15_pnoden15.rds")
endstate = state(res, "resources")
endstate_mean = Reduce("+", endstate) / length(endstate)
tab15[1:4, 4] <- round(endstate_mean[i, 1] / endstate_mean[i, 2], 1)
endstate = state(res, "species")
enstate_rich = lapply(endstate, rowSums)
enstate_rich = Reduce("+", enstate_rich) / length(enstate_rich)
tab15[1:4, 7] <- round(enstate_rich[i], 1)

res = readRDS("results/poll_conc60_pnoden15.rds")
endstate = state(res, "resources")
endstate_mean = Reduce("+", endstate) / length(endstate)
tab15[1:4, 5] <- round(endstate_mean[i, 1] / endstate_mean[i, 2], 1)
endstate = state(res, "species")
enstate_rich = lapply(endstate, rowSums)
enstate_rich = Reduce("+", enstate_rich) / length(enstate_rich)
tab15[1:4, 8] <- round(enstate_rich[i], 1)


tab15 = paste0(apply(tab15, 1, paste0, collapse = " & "), collapse = " \\\\ \n ")
tab15 = paste0(tab15, " \\\\ \n \\midrule \n")


### REACH 34 scenarios
# get N:P ratios for control treatment for interesting reaches
res = readRDS("results/no_pollution_pnoden34.rds")
endstate = state(res, "resources")
endstate_mean = Reduce("+", endstate) / length(endstate)
i = c(34, 31, 32, 33)
tab34[1:4, 3] <- round(endstate_mean[i, 1] / endstate_mean[i, 2], 1)
endstate = state(res, "species")
enstate_rich = lapply(endstate, rowSums)
enstate_rich = Reduce("+", enstate_rich) / length(enstate_rich)
tab34[1:4, 6] <- round(enstate_rich[i], 1)

res = readRDS("results/poll_conc15_pnoden34.rds")
endstate = state(res, "resources")
endstate_mean = Reduce("+", endstate) / length(endstate)
tab34[1:4, 4] <- round(endstate_mean[i, 1] / endstate_mean[i, 2], 1)
endstate = state(res, "species")
enstate_rich = lapply(endstate, rowSums)
enstate_rich = Reduce("+", enstate_rich) / length(enstate_rich)
tab34[1:4, 7] <- round(enstate_rich[i], 1)

res = readRDS("results/poll_conc60_pnoden34.rds")
endstate = state(res, "resources")
endstate_mean = Reduce("+", endstate) / length(endstate)
tab34[1:4, 5] <- round(endstate_mean[i, 1] / endstate_mean[i, 2], 1)
endstate = state(res, "species")
enstate_rich = lapply(endstate, rowSums)
enstate_rich = Reduce("+", enstate_rich) / length(enstate_rich)
tab34[1:4, 8] <- round(enstate_rich[i], 1)

tab34 = paste0(apply(tab34, 1, paste0, collapse = " & "), collapse = " \\\\ \n ")
tab34 = paste0(tab34, " \\\\ \n \\midrule \n")

tab = paste0(header, tab15, tab34, footer)
cat(tab, file = "figures/table_ex2.tex")


### REACH 41 scenarios
# get N:P ratios for control treatment for interesting reaches
res = readRDS("results/no_pollution_pnoden41.rds")
endstate = state(res, "resources")
endstate_mean = Reduce("+", endstate) / length(endstate)
i = c(41,40,39,38)
tab41[1:4, 3] <- round(endstate_mean[i, 1] / endstate_mean[i, 2], 1)
endstate = state(res, "species")
enstate_rich = lapply(endstate, rowSums)
enstate_rich = Reduce("+", enstate_rich) / length(enstate_rich)
tab41[1:4, 6] <- round(enstate_rich[i], 1)

res = readRDS("results/poll_conc15_pnoden41.rds")
endstate = state(res, "resources")
endstate_mean = Reduce("+", endstate) / length(endstate)
tab41[1:4, 4] <- round(endstate_mean[i, 1] / endstate_mean[i, 2], 1)
endstate = state(res, "species")
enstate_rich = lapply(endstate, rowSums)
enstate_rich = Reduce("+", enstate_rich) / length(enstate_rich)
tab41[1:4, 7] <- round(enstate_rich[i], 1)

res = readRDS("results/poll_conc60_pnoden41.rds")
endstate = state(res, "resources")
endstate_mean = Reduce("+", endstate) / length(endstate)
tab41[1:4, 5] <- round(endstate_mean[i, 1] / endstate_mean[i, 2], 1)
endstate = state(res, "species")
enstate_rich = lapply(endstate, rowSums)
enstate_rich = Reduce("+", enstate_rich) / length(enstate_rich)
tab41[1:4, 8] <- round(enstate_rich[i], 1)

tab41 = paste0(apply(tab41, 1, paste0, collapse = " & "), collapse = " \\\\ \n ")
tab41 = paste0(tab41, " \\\\ \n \\midrule \n")

tab = paste0(header, tab15, tab34, tab41, footer)
cat(tab, file = "figures/table_ex2.tex")
