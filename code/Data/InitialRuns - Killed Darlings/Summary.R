source("Data/InitialRuns/NoBSV.R")
source("Data/InitialRuns/WithETA_CL_Q.R")
source("Data/InitialRuns/WithETA_V2_V3.R")
source("Data/InitialRuns/WithETA.R")


# Combine the data frames including the new variables
statSummary <- bind_rows(
    NoBSV_CP_10$objDf,
    NoBSV_CP_25$objDf,
    NoBSV_CP_50$objDf,
    NoBSV_CP_100$objDf,
    NoBSV_DV_10$objDf,
    NoBSV_DV_25$objDf,
    NoBSV_DV_50$objDf,
    NoBSV_DV_100$objDf,
    wCLQ_CP_10$objDf,
    wCLQ_CP_25$objDf,
    wCLQ_CP_50$objDf,
    wCLQ_CP_100$objDf,
    wCLQ_DV_10$objDf,
    wCLQ_DV_25$objDf,
    wCLQ_DV_50$objDf,
    wCLQ_DV_100$objDf,
    V2V3_CP_10$objDf,
    V2V3_CP_25$objDf,
    V2V3_CP_50$objDf,
    V2V3_CP_100$objDf,
    V2V3_DV_10$objDf,
    V2V3_DV_25$objDf,
    V2V3_DV_50$objDf,
    V2V3_DV_100$objDf,
    AllETA_CP_10$objDf,
    AllETA_CP_25$objDf,
    AllETA_CP_50$objDf,
    AllETA_CP_100$objDf,
    AllETA_DV_10$objDf,
    AllETA_DV_25$objDf,
    AllETA_DV_50$objDf,
    AllETA_DV_100$objDf
)

# Convert to data frame (optional, since bind_rows() returns a data frame)
statSummary <- as.data.frame(statSummary)

# Assign new row namest
row_names <- c(
    "NoBSV_CP_10",
    "NoBSV_CP_25",
    "NoBSV_CP_50",
    "NoBSV_CP_100",
    "NoBSV_DV_10",
    "NoBSV_DV_25",
    "NoBSV_DV_50",
    "NoBSV_DV_100",
    "wCLQ_CP_10",
    "wCLQ_CP_25",
    "wCLQ_CP_50",
    "wCLQ_CP_100",
    "wCLQ_DV_10",
    "wCLQ_DV_25",
    "wCLQ_DV_50",
    "wCLQ_DV_100",
    "V2V3_CP_10",
    "V2V3_CP_25",
    "V2V3_CP_50",
    "V2V3_CP_100",
    "V2V3_DV_10",
    "V2V3_DV_25",
    "V2V3_DV_50",
    "V2V3_DV_100",
    "AllETA_CP_10",
    "AllETA_CP_25",
    "AllETA_CP_50",
    "AllETA_CP_100",
    "AllETA_DV_10",
    "AllETA_DV_25",
    "AllETA_DV_50",
    "AllETA_DV_100"
)

rownames(statSummary) <- row_names
rm(row_names)

# Create data frames for each group based 
statSummary_NoBSV_CP <- statSummary[grep("NoBSV_CP_", rownames(statSummary)), ]
statSummary_NoBSV_DV <- statSummary[grep("NoBSV_DV_", rownames(statSummary)), ]

statSummary_wCLQ_CP <- statSummary[grep("wCLQ_CP_", rownames(statSummary)), ]
statSummary_wCLQ_DV <- statSummary[grep("wCLQ_DV_", rownames(statSummary)), ]

statSummary_V2V3_CP <- statSummary[grep("V2V3_CP_", rownames(statSummary)), ]
statSummary_V2V3_DV <- statSummary[grep("V2V3_DV_", rownames(statSummary)), ]

statSummary_AllETA_CP <- statSummary[grep("AllETA_CP_", rownames(statSummary)), ]
statSummary_AllETA_DV <- statSummary[grep("AllETA_DV_", rownames(statSummary)), ]