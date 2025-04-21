library(kableExtra)


DFSAVEmse <- function(Df, Dfname, folder = "Data/Dataframes/") {
    # Ensure the directory exists
    if (!dir.exists(folder)) {
        dir.create(folder, recursive = TRUE)
    }
    
full_path <- paste0(folder, Dfname, ".tex")
kable_output <- kbl(Df, 
                    booktabs = TRUE, 
                    col.names = c("Model", "MSE"), 
                    linesep = "",
                    digits=2,  
                    "latex") %>%
                    kable_classic(full_width = TRUE) %>%
                    kable_styling(font_size = 8) %>%
                    row_spec(0, bold = TRUE) %>%
                    row_spec(2:nrow(Df)-1, hline_after = TRUE) 

cat(kable_output, file = full_path)
}

DFSAVEcoef <- function(Df, Dfname, folder = "Data/Dataframes/") {
    # Ensure the directory exists
    if (!dir.exists(folder)) {
        dir.create(folder, recursive = TRUE)
    }
    
full_path <- paste0(folder, Dfname, ".tex")
kable_output <- kbl(Df, 
                    booktabs = TRUE, 
                    col.names = c("Model", "tka", "tq", "tcl", "tvc", "tvp", "tf", "prop.sd"), 
                    linesep = "",
                    digits=2,  
                    "latex") %>%
                    kable_classic(full_width = TRUE) %>%
                    row_spec(0, bold = TRUE) %>%
                    row_spec(2:nrow(Df)-1, hline_after = TRUE) 

cat(kable_output, file = full_path)
}

DFSAVEcoefBW <- function(Df, Dfname, folder = "Data/Dataframes/") {
    # Ensure the directory exists
    if (!dir.exists(folder)) {
        dir.create(folder, recursive = TRUE)
    }
    
full_path <- paste0(folder, Dfname, ".tex")
kable_output <- kbl(Df, 
                    booktabs = TRUE, 
                    col.names = c("Model", "tka", "tq", "tcl", "tvc", "tvp", "tf", "pow", "prop.sd"), 
                    linesep = "",
                    digits=2,  
                    "latex") %>%
                    kable_classic(full_width = TRUE) %>%
                    row_spec(0, bold = TRUE) %>%
                    row_spec(2:nrow(Df)-1, hline_after = TRUE) 

cat(kable_output, file = full_path)
}

DFSAVEcoefBWAll <- function(Df, Dfname, folder = "Data/Dataframes/") {
    # Ensure the directory exists
    if (!dir.exists(folder)) {
        dir.create(folder, recursive = TRUE)
    }
    
full_path <- paste0(folder, Dfname, ".tex")
kable_output <- kbl(Df, 
                    booktabs = TRUE, 
                    col.names = c("Model", "tka", "tq", "tcl", "tvc", "tvp", "tf", "pow1", "pow2", "prop.sd"), 
                    linesep = "",
                    digits=2,  
                    "latex") %>%
                    kable_classic(full_width = TRUE) %>%
                    row_spec(0, bold = TRUE) %>%
                    row_spec(2:nrow(Df)-1, hline_after = TRUE) 

cat(kable_output, file = full_path)
}

DFSAVEstat <- function(Df, Dfname, folder = "Data/Dataframes/") {
    # Ensure the directory exists
    if (!dir.exists(folder)) {
        dir.create(folder, recursive = TRUE)
    }
    
full_path <- paste0(folder, Dfname, ".tex")
kable_output <- kbl(Df, 
                    booktabs = TRUE, 
                    col.names = c("Model", "OBJF", "AIC", "BIC", "Log-Lik", "Cov", "Cor"), 
                    linesep = "",
                    digits=1, 
                    "latex") %>%
                    kable_classic(full_width = TRUE) %>%
                    kable_styling(font_size = 8) %>%
                    row_spec(0, bold = TRUE) %>%
                    row_spec(2:nrow(Df)-1, hline_after = TRUE) 

cat(kable_output, file = full_path)
}


dataframesMSE <- list(
    list(df = MSE_All_BW, name = "MSE_All_BW"),
    list(df = MSE_CL_BW, name = "MSE_CL_BW"),
    list(df = MSE_V_BW, name = "MSE_V_BW"),
    list(df = MSE_All, name = "MSE_All"),
    list(df = MSE_None, name = "MSE_None"),
    list(df = MSE_CL, name = "MSE_CL"),
    list(df = MSE_V, name = "MSE_V")
)

dataframesCOEF <- list(
    list(df = coefSummary_ETA_CLQ_CP, name = "coefSummary_ETA_CLQ_CP"),
    list(df = coefSummary_ETA_CLQ_DV, name = "coefSummary_ETA_CLQ_DV"),
    list(df = coefSummary_ETA_V_CP, name = "coefSummary_ETA_V_CP"),
    list(df = coefSummary_ETA_V_DV, name = "coefSummary_ETA_V_DV"),
    list(df = coefSummary_NoBSV_CP, name = "coefSummary_NoBSV_CP"),
    list(df = coefSummary_NoBSV_DV, name = "coefSummary_NoBSV_DV"),
    list(df = coefSummary_ETA_ALL_CP, name = "coefSummary_ETA_ALL_CP"),
    list(df = coefSummary_ETA_ALL_DV, name = "coefSummary_ETA_ALL_DV")
)

dataframesCOEFBW <- list(
    list(df = coefSummary_BW_ETA_CLQ_CP, name = "coefSummary_BW_ETA_CLQ_CP"),
    list(df = coefSummary_BW_ETA_CLQ_DV, name = "coefSummary_BW_ETA_CLQ_DV"),
    list(df = coefSummary_BW_ETA_V_CP, name = "coefSummary_BW_ETA_V_CP"),
    list(df = coefSummary_BW_ETA_V_DV, name = "coefSummary_BW_ETA_V_DV")
)

dataframesCOEFBWAll <- list(
    list(df = coefSummary_BW_ETA_ALL_CP, name = "coefSummary_BW_ETA_ALL_CP"),
    list(df = coefSummary_BW_ETA_ALL_DV, name = "coefSummary_BW_ETA_ALL_DV")
)

dataframesStat <- list(
    list(df = statSummary_BW_ETA_ALL_CP, name = "statSummary_BW_ETA_ALL_CP"),
    list(df = statSummary_BW_ETA_ALL_DV, name = "statSummary_BW_ETA_ALL_DV"),
    list(df = statSummary_BW_ETA_CLQ_CP, name = "statSummary_BW_ETA_CLQ_CP"),
    list(df = statSummary_BW_ETA_CLQ_DV, name = "statSummary_BW_ETA_CLQ_DV"),
    list(df = statSummary_BW_ETA_VCP, name = "statSummary_BW_ETA_V_CP"),
    list(df = statSummary_BW_ETA_VDV, name = "statSummary_BW_ETA_V_DV"),
    list(df = statSummary_ETA_ALL_CP, name = "statSummary_ETA_ALL_CP"),
    list(df = statSummary_ETA_ALL_DV, name = "statSummary_ETA_ALL_DV"),
    list(df = statSummary_NoBSV_CP, name = "statSummary_NoBSV_CP"),
    list(df = statSummary_NoBSV_DV, name = "statSummary_NoBSV_DV"),
    list(df = statSummary_ETA_CLQ_CP, name = "statSummary_ETA_CLQ_CP"),
    list(df = statSummary_ETA_CLQ_DV, name = "statSummary_ETA_CLQ_DV"),
    list(df = statSummary_ETA_V_CP, name = "statSummary_ETA_V_CP"),
    list(df = statSummary_ETA_V_DV, name = "statSummary_ETA_V_DV")
)


for (item in dataframesMSE) {
    DFSAVEmse(item$df, item$name)
}

for (item in dataframesCOEF) {
    DFSAVEcoef(item$df, item$name)
}

for (item in dataframesCOEFBW) {
    DFSAVEcoefBW(item$df, item$name)
}

for (item in dataframesCOEFBWAll) {
    DFSAVEcoefBWAll(item$df, item$name)
}

for (item in dataframesStat) {
    DFSAVEstat(item$df, item$name)
}


# Overvej at lave de her dataframes
test <- results$ETA_ALL_DV_100_Init_1$parFixedDf
omega <- results$ETA_ALL_DV_100_Init_1$omega

