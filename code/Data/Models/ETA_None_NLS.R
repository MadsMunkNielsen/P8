source("Data/Models/Models.R")


# Amount of subjects to include in the model
Amount_Of_Subjects <- c(10, 25, 50, 100)

# Specify the estimation method 
estMethod <- "nls"

# The initial values needed in the model
initials_list <- list(
  list(tka_init = 0.0253, tq_init = 0.304, tcl_init = 0.0348, tvc_init = 3.59, tvp_init = 4.1, prop.sd_init = 0.2)
)


results_None_NLS <- list()

for (i in Amount_Of_Subjects) {
  for (j in 1:length(initials_list)) {
    # Create key for storing results_None_NLS
    key_cp <- paste0("NoBSV_CP_", i, "_Init_", j)
    key_dv <- paste0("NoBSV_DV_", i, "_Init_", j)

    # Fit for CP
    results_None_NLS[[key_cp]] <- nlmixr(
      do.call(two.cmt.depot.no.BSV, initials_list[[j]]),
      subsetTrue[subsetTrue$ID >= 1 & subsetTrue$ID <= i, ],
      est = estMethod
    )

    # Fit for DV
    results_None_NLS[[key_dv]] <- nlmixr(
      do.call(two.cmt.depot.no.BSV, initials_list[[j]]),
      subsetNoise[subsetNoise$ID >= 1 & subsetNoise$ID <= i, ],
      est = estMethod
    )
  }
}



# Initialize an empty data frame for summarizing results_None_NLS
statSummary <- data.frame()

# Extract objDf for each model and concatenate results_None_NLS into statSummary
for (result_name in names(results_None_NLS)) {
  if (!is.null(results_None_NLS[[result_name]]$objDf)) {
    statSummary <- bind_rows(statSummary, results_None_NLS[[result_name]]$objDf)
  }
}

# Assign new row names based on the results_None_NLS
rownames(statSummary) <- names(results_None_NLS)

# Create data frames for each group based on a pattern
statSummary_NoBSV_CP_NLS <- statSummary[grep("NoBSV_CP_", rownames(statSummary)), ]
statSummary_NoBSV_DV_NLS <- statSummary[grep("NoBSV_DV_", rownames(statSummary)), ]



# MSE_None df
MSE_None_NLS_values <- numeric(length = length(results_None_NLS))

for (i in seq_along(results_None_NLS)) {
  MSE_None_NLS_values[i] <- 1 / length(results_None_NLS[[i]]$IRES) * sum((results_None_NLS[[i]]$IRES)^2)
}

MSE_None_NLS <- data.frame(MSE_None = MSE_None_NLS_values)
rownames(MSE_None_NLS) <- names(results_None_NLS)



#Coefficient summary
coefSummary <- data.frame()

for (result_name in names(results_None_NLS)) {
  if (!is.null(coef(results_None_NLS[[result_name]])[[1]])) {
    coefSummary <- bind_rows(coefSummary, coef(results_None_NLS[[result_name]])[[1]])
  }
}

rownames(coefSummary) <- names(results_None_NLS)

coefSummary_NoBSV_CP_NLS <- coefSummary[grep("NoBSV_CP_", rownames(coefSummary)), ]
coefSummary_NoBSV_DV_NLS <- coefSummary[grep("NoBSV_DV_", rownames(coefSummary)), ]


for (result_name in names(results_None_NLS)) {
  if (!is.null(results_None_NLS[[result_name]]$parFixed)) {
    
    # Create a new data frame for the current result_name
    current_data_frame <- results_None_NLS[[result_name]]$parFixed

    # Create a separate variable for each result_name
    assign(paste0("parFixed_NLS_", result_name), current_data_frame)
  }
}