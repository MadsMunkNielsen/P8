library(xpose)
library(xpose.nlmixr2)
library(ggplot2)

SaveXpose <- function(filename, plotname, folder = "Data/XposePlots/"){
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

included_models <- c()
for (names in names(results_))

xpose_data <- list()

for (i in Amount_Of_Subjects) {
  for (j in 1:length(initials_list)) {
    # Create key for storing results_BW_Eta_All
    key_cp <- paste0("BW_ETA_ALL_CP_", i, "_Init_", j)
    key_dv <- paste0("BW_ETA_ALL_DV_", i, "_Init_", j)

    # Fit for CP
    results_BW_Eta_All[[key_cp]] <- nlmixr(
      do.call(two.cmt.depot.All.BW, initials_list[[j]]),
      subsetTrue[subsetTrue$ID >= 1 & subsetTrue$ID <= i, ],
      est = estMethod
    )

    # Fit for DV
    results_BW_Eta_All[[key_dv]] <- nlmixr(
      do.call(two.cmt.depot.All.BW, initials_list[[j]]),
      subsetNoise[subsetNoise$ID >= 1 & subsetNoise$ID <= i, ],
      est = estMethod
    )
  }
}


myfit <- results_None$NoBSV_DV_100_Init_1

xpdb <- xpose_data_nlmixr(myfit)