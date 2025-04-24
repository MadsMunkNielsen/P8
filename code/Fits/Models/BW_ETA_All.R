source("Fits/Models/Models.R")


# Amount of subjects to include in the model
Amount_Of_Subjects <- c(10, 25, 50, 100)

# Specify the estimation method 
estMethod <- "focei"

# The initial values needed in the model
initials_list <- list(
  list(tka_init = 0.0025, tq_init = 0.304, tcl_init = 0.034, tvc_init = 3.59, tvp_init = 4.1, tf_init = c(0, 0.847, 1), eta.q_init = 0.2, eta.vp_init = 0.2, prop.sd_init = 0.2, cov = 0),
  list(tka_init = 0.0025, tq_init = 0.304, tcl_init = 0.034, tvc_init = 3.59, tvp_init = 4.1, tf_init = c(0, 0.847, 1), eta.q_init = 0.3, eta.vp_init = 0.2, prop.sd_init = 0.2, cov = 0.00172)
)


resultsBW <- list()

for (i in Amount_Of_Subjects) {
  for (j in 1:length(initials_list)) {
    # Create key for storing resultsBW
    key_cp <- paste0("BW_ETA_ALL_CP_", i, "_Init_", j)
    key_dv <- paste0("BW_ETA_ALL_DV_", i, "_Init_", j)

    # Fit for CP
    resultsBW[[key_cp]] <- nlmixr(
      do.call(two.cmt.depot.All.BW, initials_list[[j]]),
      subsetTrue[subsetTrue$ID >= 1 & subsetTrue$ID <= i, ],
      est = estMethod
    )

    # Fit for DV
    resultsBW[[key_dv]] <- nlmixr(
      do.call(two.cmt.depot.All.BW, initials_list[[j]]),
      subsetNoise[subsetNoise$ID >= 1 & subsetNoise$ID <= i, ],
      est = estMethod
    )
  }
}



# Initialize an empty data frame for summarizing resultsBW
statSummary <- data.frame()

# Extract objDf for each model and concatenate resultsBW into statSummary
for (result_name in names(resultsBW)) {
  if (!is.null(resultsBW[[result_name]]$objDf)) {
    statSummary <- bind_rows(statSummary, resultsBW[[result_name]]$objDf)
  }
}

# Assign new row names based on the resultsBW
rownames(statSummary) <- names(resultsBW)

# Create data frames for each group based on a pattern
statSummary_BW_ETA_ALL_CP <- statSummary[grep("BW_ETA_ALL_CP_", rownames(statSummary)), ]
statSummary_BW_ETA_ALL_DV <- statSummary[grep("BW_ETA_ALL_DV_", rownames(statSummary)), ]



# MSE_All_BW df
MSE_All_BW_values <- numeric(length = length(resultsBW))

for (i in seq_along(resultsBW)) {
  MSE_All_BW_values[i] <- (1 / length(resultsBW[[i]]$IRES)) * sum((resultsBW[[i]]$IRES)^2)
}

MSE_All_BW <- data.frame(MSE_All_BW = MSE_All_BW_values)
rownames(MSE_All_BW) <- names(resultsBW)



#Coefficient summary
coefSummary <- data.frame()

for (result_name in names(resultsBW)) {
  if (!is.null(coef(resultsBW[[result_name]])[[1]])) {
    coefSummary <- bind_rows(coefSummary, coef(resultsBW[[result_name]])[[1]])
  }
}

rownames(coefSummary) <- names(resultsBW)

coefSummary_BW_ETA_ALL_CP <- coefSummary[grep("BW_ETA_ALL_CP_", rownames(coefSummary)), ]
coefSummary_BW_ETA_ALL_DV <- coefSummary[grep("BW_ETA_ALL_DV_", rownames(coefSummary)), ]


for (result_name in names(resultsBW)) {
  if (!is.null(resultsBW[[result_name]]$parFixed)) {
    
    # Create a new data frame for the current result_name
    current_data_frame <- resultsBW[[result_name]]$parFixed

    # Create a separate variable for each result_name
    assign(paste0("two.cmt.BW.parFixed_", result_name), current_data_frame)
  }
}
