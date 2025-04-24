source("Data/Models/BW_ETA_All.R")

#install_github('UUPharmacometrics/xpose')
library(xpose)
library(xpose.nlmixr2)
library(ggplot2)

SaveXpose <- function(filename, plotname, folder = "Data/XposePlots/"){
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}


plot(results_None$NoBSV_CP_10_Init_1)
myfit <- results_BW_Eta_All$BW_ETA_ALL_DV_100_Init_1

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






# Regular Plots
dv_vs_pred_plot <- dv_vs_pred(xpdb) +
  labs(title = "Dependent Variable vs Model Prediction", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("dv_vs_pred.pdf", dv_vs_pred_plot)

dv_vs_ipred_plot <- dv_vs_ipred(xpdb) +
  labs(title = "Dependent Variable vs Individual Predictions", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("dv_vs_ipred.pdf", dv_vs_ipred_plot)

res_vs_idv_plot <- res_vs_idv(xpdb, res="CWRES") +
  labs(title = "Residuals vs Dependent Variable", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("res_vs_idv.pdf", res_vs_idv_plot)

res_vs_pred_plot <- res_vs_pred(xpdb, res="CWRES") +
  labs(title = "Residuals vs Model Prediction", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("res_vs_pred.pdf", res_vs_pred_plot)

absval_res_vs_idv_plot <- absval_res_vs_idv(xpdb, res="CWRES") +
  labs(title = "Absolute Residuals vs Dependent Variable", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("absval_res_vs_idv.pdf", absval_res_vs_idv_plot)

absval_res_vs_pred_plot <- absval_res_vs_pred(xpdb, res="CWRES") +
  labs(title = "Absolute Residuals vs Model Prediction", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("absval_res_vs_pred.pdf", absval_res_vs_pred_plot)

# Grouped Plots
dv_vs_idv_plot <- dv_vs_idv(xpdb, group="ID") +
  labs(title = "Dependent Variable vs ID", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("dv_vs_idv.pdf", dv_vs_idv_plot)

ipred_vs_idv_plot <- ipred_vs_idv(xpdb, group="ID") +
  labs(title = "Individual Predictions vs ID", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("ipred_vs_idv.pdf", ipred_vs_idv_plot)

pred_vs_idv_plot <- pred_vs_idv(xpdb, group="ID") +
  labs(title = "Model Predictions vs ID", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("pred_vs_idv.pdf", pred_vs_idv_plot)

dv_preds_vs_idv_plot <- dv_preds_vs_idv(xpdb) +
  labs(title = "Dependent Variable vs Predictions", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("dv_preds_vs_idv.pdf", dv_preds_vs_idv_plot)

# Distribution and QQ Plots
prm_distrib_plot <- prm_distrib(xpdb) +
  labs(title = "Parameter Distribution", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("prm_distrib.pdf", prm_distrib_plot)

eta_distrib_plot <- eta_distrib(xpdb) +
  labs(title = "Random Effects Distribution", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("eta_distrib.pdf", eta_distrib_plot)

cov_distrib_plot <- cov_distrib(xpdb) +
  labs(title = "Covariance Distribution", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("cov_distrib.pdf", cov_distrib_plot)

res_distrib_plot <- res_distrib(xpdb, res="CWRES") +
  labs(title = "Residuals Distribution", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("res_distrib.pdf", res_distrib_plot)

prm_qq_plot <- prm_qq(xpdb) +
  labs(title = "QQ Plot for Parameters", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("prm_qq.pdf", prm_qq_plot)

eta_qq_plot <- eta_qq(xpdb) +
  labs(title = "QQ Plot for Random Effects", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("eta_qq.pdf", eta_qq_plot)

cov_qq_plot <- cov_qq(xpdb) +
  labs(title = "QQ Plot for Covariance", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("cov_qq.pdf", cov_qq_plot)

res_qq_plot <- res_qq(xpdb, res="CWRES") +
  labs(title = "QQ Plot for Residuals", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose("res_qq.pdf", res_qq_plot)

vpcPlot <- vpcPlot(myfit, n=1000) +
            theme_minimal()
vpcPlot


SaveXpose("vpcPlot.pdf", vpcPlot)
