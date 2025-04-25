source("Data/Models/BW_ETA_All.R")

#install_github('UUPharmacometrics/xpose')
library(xpose)
library(xpose.nlmixr2)
library(ggplot2)

SaveXpose <- function(filename, plotname, folder = "Data/XposePlots/"){
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}


myfit <- results_Eta_V$ETA_V_DV_100_Init_1

xpdb <- xpose_data_nlmixr(myfit)

# Residuals and Predictions
dv_vs_pred(xpdb)                   # Dependent variable vs model prediction
dv_vs_ipred(xpdb)                  # Dependent variable vs individual predictions
res_vs_idv(xpdb, res="CWRES")      # Residuals vs dependent variable
res_vs_pred(xpdb, res="CWRES")     # Residuals vs model prediction
absval_res_vs_idv(xpdb, res="CWRES") # Absolute residuals vs dependent variable
absval_res_vs_pred(xpdb, res="CWRES") # Absolute residuals vs model prediction

# Grouped Plots
dv_vs_idv(xpdb, group="ID")        # Dependent variable vs Independent variable
ipred_vs_idv(xpdb, group="ID")     # Individual predictions vs Independent variable
pred_vs_idv(xpdb, group="ID")      # Model predictions vs Independent variable
dv_preds_vs_idv(xpdb)              # Dependent variable vs predictions

# Distribution and QQ Plots
prm_distrib(xpdb)                  # Parameter distribution
eta_distrib(xpdb)                  # Random effects distribution
cov_distrib(xpdb)                  # Covariance distribution
res_distrib(xpdb, res="CWRES")     # Residuals distribution
prm_qq(xpdb)                       # QQ plot for parameters
eta_qq(xpdb)                       # QQ plot for random effects
cov_qq(xpdb)                       # QQ plot for covariance
res_qq(xpdb, res="CWRES")          # QQ plot for residuals


vpcPlot <- vpcPlot(xpdb, n=1000) +
            theme_minimal()
vpcPlot
eta_distrib(xpdb) 


res_qq_plot <- res_qq(xpdb, res="CWRES") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
res_qq_plot

SaveXpose_RES_QQ(paste0(names(results_vector[i]), ".pdf"), res_qq_plot)
