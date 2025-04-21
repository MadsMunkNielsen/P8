source("Data/Models/Models.R")


# Amount of subjects to include in the model
Amount_Of_Subjects <- c(10, 25, 50, 100)

# Specify the estimation method 
estMethod <- "focei"

# The initial values needed in the model
initials_list <- list(
  list(tka_init = 0.0025, tq_init = 0.304, tcl_init = 0.034, tvc_init = 3.59, tvp_init = 4.1, tf_init = 0.847, eta.q_init = 0.2, eta.vp_init = 0.2, prop.sd_init = 0.2)
  #list(tka_init = 1.1, tq_init = 3.0, tcl_init = 2.5, tvc_init = 25, tvp_init = 55, tf_init = 1.5, eta.q_init = 17, eta.vp_init = 17, prop.sd_init = 0.4),
  #list(tka_init = 1.5, tq_init = 2.5, tcl_init = 1.9, tvc_init = 35, tvp_init = 65, tf_init = 2.0, eta.q_init = 12, eta.vp_init = 12, prop.sd_init = 0.4)
)

results <- list()

for (i in Amount_Of_Subjects) {
  for (j in 1:length(initials_list)) {
    # Create key for storing results
    key_cp <- paste0("ETA_ALL_CP_", i, "_Init_", j)
    key_dv <- paste0("ETA_ALL_DV_", i, "_Init_", j)

    # Fit for CP
    results[[key_cp]] <- nlmixr(
      do.call(two.cmt.depot.All, initials_list[[j]]),
      subsetTrue[subsetTrue$ID >= 1 & subsetTrue$ID <= i, ],
      est = estMethod
    )

    # Fit for DV
    results[[key_dv]] <- nlmixr(
      do.call(two.cmt.depot.All, initials_list[[j]]),
      subsetNoise[subsetNoise$ID >= 1 & subsetNoise$ID <= i, ],
      est = estMethod
    )
  }
}



# Initialize an empty data frame for summarizing results
statSummary <- data.frame()

# Extract objDf for each model and concatenate results into statSummary
for (result_name in names(results)) {
  if (!is.null(results[[result_name]]$objDf)) {
    statSummary <- bind_rows(statSummary, results[[result_name]]$objDf)
  }
}

# Assign new row names based on the results
rownames(statSummary) <- names(results)

# Create data frames for each group based on a pattern
statSummary_ETA_ALL_CP <- statSummary[grep("ETA_ALL_CP_", rownames(statSummary)), ]
statSummary_ETA_ALL_DV <- statSummary[grep("ETA_ALL_DV_", rownames(statSummary)), ]



# MSE_All df
MSE_All_values <- numeric(length = length(results))

for (i in seq_along(results)) {
  MSE_All_values[i] <- 1 / length(results[[i]]$IRES) * sum((results[[i]]$IRES)^2)
}

MSE_All <- data.frame(MSE_All = MSE_All_values)
rownames(MSE_All) <- names(results)



#Coefficient summary
coefSummary <- data.frame()

for (result_name in names(results)) {
  if (!is.null(coef(results[[result_name]])[[1]])) {
    coefSummary <- bind_rows(coefSummary, coef(results[[result_name]])[[1]])
  }
}

rownames(coefSummary) <- names(results)

coefSummary_ETA_ALL_CP <- coefSummary[grep("ETA_ALL_CP_", rownames(coefSummary)), ]
coefSummary_ETA_ALL_DV <- coefSummary[grep("ETA_ALL_DV_", rownames(coefSummary)), ]