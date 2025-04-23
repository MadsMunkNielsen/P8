library(kableExtra)


DFSAVEmse <- function(Df, Dfname, folder = "Fits/Dataframes/") {
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

DFSAVEcoef <- function(Df, Dfname, folder = "Fits/Dataframes/") {
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

DFSAVEcoefBWAll <- function(Df, Dfname, folder = "Fits/Dataframes/") {
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

DFSAVEstat <- function(Df, Dfname, folder = "Fits/Dataframes/") {
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

DFSAVEtableOne <- function(Df, Dfname, folder = "Fits/Dataframes/") {
    # Ensure the directory exists
    if (!dir.exists(folder)) {
        dir.create(folder, recursive = TRUE)
    }
    
full_path <- paste0(folder, Dfname, ".tex")
kable_output <- kbl(Df, 
                    booktabs = TRUE, 
                    col.names = c("Parameter", "Est", "SE", "RSE", "Back-transformed", "BSV", "Shrinkage"), 
                    linesep = "",
                    digits=1, 
                    "latex") %>%
                    kable_classic(full_width = TRUE) %>%
                    kable_styling(font_size = 8) %>%
                    row_spec(0, bold = TRUE) %>%
                    row_spec(2:nrow(Df)-1, hline_after = TRUE) 

cat(kable_output, file = full_path)
}

DFSAVEtableFive <- function(Df, Dfname, folder = "Fits/Dataframes/") {
    # Ensure the directory exists
    if (!dir.exists(folder)) {
        dir.create(folder, recursive = TRUE)
    }
    
full_path <- paste0(folder, Dfname, ".tex")
kable_output <- kbl(Df, 
                    booktabs = TRUE, 
                    col.names = c("Parameter", "Est", "Back-transformed", "BSV", "Shrinkage"), 
                    linesep = "",
                    digits=1, 
                    "latex") %>%
                    kable_classic(full_width = TRUE) %>%
                    kable_styling(font_size = 8) %>%
                    row_spec(0, bold = TRUE) %>%
                    row_spec(2:nrow(Df)-1, hline_after = TRUE) 

cat(kable_output, file = full_path)
}

DFSAVEtableSeven <- function(Df, Dfname, folder = "Fits/Dataframes/") {
    # Ensure the directory exists
    if (!dir.exists(folder)) {
        dir.create(folder, recursive = TRUE)
    }
    
full_path <- paste0(folder, Dfname, ".tex")
kable_output <- kbl(Df, 
                    booktabs = TRUE, 
                    col.names = c("Parameter", "Est", "SE", "RSE", "Back-transformed", "BSV", "Shrinkage"), 
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
    list(df = MSE_All, name = "MSE_All"),
    list(df = MSE_1comp, name = "MSE_1comp"),

)

dataframesCOEF <- list(
    list(df = coefSummary_ETA_ALL_CP, name = "coefSummary_ETA_ALL_CP"),
    list(df = coefSummary_ETA_ALL_DV, name = "coefSummary_ETA_ALL_DV"),
    list(df = coefSummary_1Comp_CP, name = "coefSummary_1Comp_CP"),
    list(df = coefSummary_1Comp_DV, name = "coefSummary_1Comp_DV")
)

dataframesCOEFBWAll <- list(
    list(df = coefSummary_BW_ETA_ALL_CP, name = "coefSummary_BW_ETA_ALL_CP"),
    list(df = coefSummary_BW_ETA_ALL_DV, name = "coefSummary_BW_ETA_ALL_DV")
)

dataframesStat <- list(
    list(df = statSummary_BW_ETA_ALL_CP, name = "statSummary_BW_ETA_ALL_CP"),
    list(df = statSummary_BW_ETA_ALL_DV, name = "statSummary_BW_ETA_ALL_DV"),
    list(df = statSummary_ETA_ALL_CP, name = "statSummary_ETA_ALL_CP"),
    list(df = statSummary_ETA_ALL_DV, name = "statSummary_ETA_ALL_DV"),
    list(df = statSummary_1Comp_CP, name = "statSummary_1Comp_CP"),
    list(df = statSummary_1Comp_DV, name = "statSummary_1Comp_DV")
)

dataframestableOne <- list(
    list(df = one.cmt.parFixed_Comp_CP_10_Init_1, name = "one.cmt.parFixed_Comp_CP_10_Init_1"),
    list(df = one.cmt.parFixed_Comp_CP_100_Init_1, name = "one.cmt.parFixed_Comp_CP_100_Init_1"),
    list(df = one.cmt.parFixed_Comp_CP_25_Init_1, name = "one.cmt.parFixed_Comp_CP_25_Init_1"),
    list(df = one.cmt.parFixed_Comp_CP_50_Init_1, name = "one.cmt.parFixed_Comp_CP_50_Init_1"),
    list(df = one.cmt.parFixed_Comp_DV_10_Init_1, name = "one.cmt.parFixed_Comp_DV_10_Init_1"),
    list(df = one.cmt.parFixed_Comp_DV_100_Init_1, name = "one.cmt.parFixed_Comp_DV_100_Init_1"),
    list(df = one.cmt.parFixed_Comp_DV_25_Init_1, name = "one.cmt.parFixed_Comp_DV_25_Init_1"),
    list(df = one.cmt.parFixed_Comp_DV_50_Init_1, name = "one.cmt.parFixed_Comp_DV_50_Init_1")
)

dataframestableFive <- list(
    list(df = two.cmt.BW.parFixed_BW_ETA_ALL_CP_10_Init_1, name = "two.cmt.BW.parFixed_BW_ETA_ALL_CP_10_Init_1"),
    list(df = two.cmt.BW.parFixed_BW_ETA_ALL_CP_10_Init_2, name = "two.cmt.BW.parFixed_BW_ETA_ALL_CP_10_Init_2"),
    list(df = two.cmt.BW.parFixed_BW_ETA_ALL_CP_25_Init_2, name = "two.cmt.BW.parFixed_BW_ETA_ALL_CP_25_Init_2"),
    list(df = two.cmt.BW.parFixed_BW_ETA_ALL_CP_50_Init_2, name = "two.cmt.BW.parFixed_BW_ETA_ALL_CP_50_Init_2"),
    list(df = two.cmt.BW.parFixed_BW_ETA_ALL_DV_10_Init_2, name = "two.cmt.BW.parFixed_BW_ETA_ALL_DV_10_Init_2"),
    list(df = two.cmt.BW.parFixed_BW_ETA_ALL_DV_50_Init_1, name = "two.cmt.BW.parFixed_BW_ETA_ALL_DV_50_Init_1"),
    list(df = two.cmt.BW.parFixed_BW_ETA_ALL_DV_25_Init_1, name = "two.cmt.BW.parFixed_BW_ETA_ALL_DV_25_Init_1"),
    list(df = two.cmt.BW.parFixed_BW_ETA_ALL_DV_25_Init_2, name = "two.cmt.BW.parFixed_BW_ETA_ALL_DV_25_Init_2"),
    list(df = two.cmt.ETA.parFixed_ETA_ALL_CP_10_Init_1, name = "two.cmt.ETA.parFixed_ETA_ALL_CP_10_Init_1"),
    list(df = two.cmt.ETA.parFixed_ETA_ALL_CP_25_Init_1, name = "two.cmt.ETA.parFixed_ETA_ALL_CP_25_Init_1"),
    list(df = two.cmt.ETA.parFixed_ETA_ALL_DV_25_Init_2, name = "two.cmt.ETA.parFixed_ETA_ALL_DV_25_Init_2"),
    list(df = two.cmt.ETA.parFixed_ETA_ALL_DV_10_Init_1, name = "two.cmt.ETA.parFixed_ETA_ALL_DV_10_Init_1"),
    list(df = two.cmt.ETA.parFixed_ETA_ALL_DV_10_Init_2, name = "two.cmt.ETA.parFixed_ETA_ALL_DV_10_Init_2"),
    list(df = two.cmt.ETA.parFixed_ETA_ALL_DV_100_Init_1, name = "two.cmt.ETA.parFixed_ETA_ALL_DV_100_Init_1"),
    list(df = two.cmt.ETA.parFixed_ETA_ALL_DV_25_Init_1, name = "two.cmt.ETA.parFixed_ETA_ALL_DV_25_Init_1"),
    list(df = two.cmt.ETA.parFixed_ETA_ALL_DV_50_Init_1, name = "two.cmt.ETA.parFixed_ETA_ALL_DV_50_Init_1"),
    list(df = two.cmt.ETA.parFixed_ETA_ALL_DV_50_Init_2, name = "two.cmt.ETA.parFixed_ETA_ALL_DV_50_Init_2")
)

dataframestableSeven <- list(
    list(df = two.cmt.BW.parFixed_BW_ETA_ALL_CP_100_Init_1, name = "two.cmt.BW.parFixed_BW_ETA_ALL_CP_100_Init_1"),
    list(df = two.cmt.BW.parFixed_BW_ETA_ALL_CP_100_Init_2, name = "two.cmt.BW.parFixed_BW_ETA_ALL_CP_100_Init_2"),
    list(df = two.cmt.BW.parFixed_BW_ETA_ALL_CP_25_Init_1, name = "two.cmt.BW.parFixed_BW_ETA_ALL_CP_25_Init_1"),
    list(df = two.cmt.BW.parFixed_BW_ETA_ALL_CP_50_Init_1, name = "two.cmt.BW.parFixed_BW_ETA_ALL_CP_50_Init_1"),
    list(df = two.cmt.BW.parFixed_BW_ETA_ALL_DV_10_Init_1, name = "two.cmt.BW.parFixed_BW_ETA_ALL_DV_10_Init_1"),
    list(df = two.cmt.BW.parFixed_BW_ETA_ALL_DV_100_Init_1, name = "two.cmt.BW.parFixed_BW_ETA_ALL_DV_100_Init_1"),
    list(df = two.cmt.BW.parFixed_BW_ETA_ALL_DV_100_Init_2, name = "two.cmt.BW.parFixed_BW_ETA_ALL_DV_100_Init_2"),
    list(df = two.cmt.BW.parFixed_BW_ETA_ALL_DV_50_Init_2, name = "two.cmt.BW.parFixed_BW_ETA_ALL_DV_50_Init_2"),
    list(df = two.cmt.ETA.parFixed_ETA_ALL_CP_10_Init_2, name = "two.cmt.ETA.parFixed_ETA_ALL_CP_10_Init_2"),
    list(df = two.cmt.ETA.parFixed_ETA_ALL_CP_100_Init_1, name = "two.cmt.ETA.parFixed_ETA_ALL_CP_100_Init_1"),
    list(df = two.cmt.ETA.parFixed_ETA_ALL_CP_100_Init_2, name = "two.cmt.ETA.parFixed_ETA_ALL_CP_100_Init_2"),
    list(df = two.cmt.ETA.parFixed_ETA_ALL_CP_50_Init_1, name = "two.cmt.ETA.parFixed_ETA_ALL_CP_50_Init_1"),
    list(df = two.cmt.ETA.parFixed_ETA_ALL_CP_50_Init_2, name = "two.cmt.ETA.parFixed_ETA_ALL_CP_50_Init_2"),
    list(df = two.cmt.ETA.parFixed_ETA_ALL_DV_100_Init_2, name = "two.cmt.ETA.parFixed_ETA_ALL_DV_100_Init_2"),
    list(df = two.cmt.ETA.parFixed_ETA_ALL_DV_25_Init_2, name = "two.cmt.ETA.parFixed_ETA_ALL_DV_25_Init_2")
)



for (item in dataframesMSE) {
    DFSAVEmse(item$df, item$name)
}

for (item in dataframesCOEF) {
    DFSAVEcoef(item$df, item$name)
}

for (item in dataframesCOEFBWAll) {
    DFSAVEcoefBWAll(item$df, item$name)
}

for (item in dataframesStat) {
    DFSAVEstat(item$df, item$name)
}

for (item in dataframestableOne) {
    DFSAVEtableOne(item$df, item$name)
}

for (item in dataframestableFive) {
    DFSAVEtableFive(item$df, item$name)
}

for (item in dataframestableSeven) {
    DFSAVEtableSeven(item$df, item$name)
}