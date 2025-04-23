source("Fits/Models/Models.R")


# Amount of subjects to include in the model
Amount_Of_Subjects <- c(100)

# Specify the estimation method 
estMethod <- "focei"

# The initial values needed in the model
initials_list <- list(
  list(tka_init = 0.0025, tcl_init = 0.034, tvc_init = 3.59, tf_init = c(0, 0.847, 1), eta.q_init = 0.2, eta.vp_init = 0.2, prop.sd_init = 0.2, cov = 0.00172)
)

results1Comp <- list()

for (i in Amount_Of_Subjects) {
  for (j in 1:length(initials_list)) {
    # Create key for storing results1Comp
    key_cp <- paste0("1Comp_CP_", i, "_Init_", j)
    key_dv <- paste0("1Comp_DV_", i, "_Init_", j)

    # Fit for CP
    results1Comp[[key_cp]] <- nlmixr(
      do.call(one.cmt.depot.All.BW, initials_list[[j]]),
      subsetTrue[subsetTrue$ID >= 1 & subsetTrue$ID <= i, ],
      est = estMethod
    )

    # Fit for DV
    results1Comp[[key_dv]] <- nlmixr(
      do.call(one.cmt.depot.All.BW, initials_list[[j]]),
      subsetNoise[subsetNoise$ID >= 1 & subsetNoise$ID <= i, ],
      est = estMethod
    )
  }
}



# Initialize an empty data frame for summarizing results1Comp
statSummary <- data.frame()

# Extract objDf for each model and concatenate results1Comp into statSummary
for (result_name in names(results1Comp)) {
  if (!is.null(results1Comp[[result_name]]$objDf)) {
    statSummary <- bind_rows(statSummary, results1Comp[[result_name]]$objDf)
  }
}

# Assign new row names based on the results1Comp
rownames(statSummary) <- names(results1Comp)

# Create data frames for each group based on a pattern
statSummary_1Comp_CP <- statSummary[grep("1Comp_CP_", rownames(statSummary)), ]
statSummary_1Comp_DV <- statSummary[grep("1Comp_DV_", rownames(statSummary)), ]



# MSE_All df
MSE1COMP <- numeric(length = length(results1Comp))

for (i in seq_along(results1Comp)) {
  MSE1COMP[i] <- (1 / length(results1Comp[[i]]$IRES)) * sum((results1Comp[[i]]$IRES)^2)
}

MSE_1comp <- data.frame(MSE_1comp = MSE1COMP)
rownames(MSE_1comp) <- names(results1Comp)



#Coefficient summary
coefSummary <- data.frame()

for (result_name in names(results1Comp)) {
  if (!is.null(coef(results1Comp[[result_name]])[[1]])) {
    coefSummary <- bind_rows(coefSummary, coef(results1Comp[[result_name]])[[1]])
  }
}

rownames(coefSummary) <- names(results1Comp)

coefSummary_1Comp_CP <- coefSummary[grep("1Comp_CP_", rownames(coefSummary)), ]
coefSummary_1Comp_DV <- coefSummary[grep("1Comp_DV_", rownames(coefSummary)), ]