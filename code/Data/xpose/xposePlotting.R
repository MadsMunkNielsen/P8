library(nlmixr2)
library(xpose)
library(xpose.nlmixr2)
library(ggplot2)

source("Data/xpose/SaveXpose.R")


results_vector <- c(results_Eta_V, results_Eta_CL_Q, results_Eta_All, results_BW_Eta_V, results_BW_Eta_CL_Q, results_BW_Eta_All)


for (i in seq_along(results_vector)) {

    xpdb <- xpose_data_nlmixr(results_vector[[i]])

dv_vs_pred_plot <- dv_vs_pred(xpdb) +
    labs(title = "", 
        subtitle = "", 
        caption = "") +
    theme_minimal()
SaveXpose_DV_PRED(paste0(names(results_vector[i]), ".pdf"), dv_vs_pred_plot)


dv_vs_ipred_plot <- dv_vs_ipred(xpdb) +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose_DV_VS_IPRED(paste0(names(results_vector[i]), ".pdf"), dv_vs_ipred_plot)


res_vs_idv_plot <- res_vs_idv(xpdb, res="CWRES") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose_DV_VS_IDV(paste0(names(results_vector[i]), ".pdf"), res_vs_idv_plot)


res_vs_pred_plot <- res_vs_pred(xpdb, res="CWRES") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose_RES_VS_PRED(paste0(names(results_vector[i]), ".pdf"), res_vs_pred_plot)


absval_res_vs_idv_plot <- absval_res_vs_idv(xpdb, res="CWRES") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose_ABSVAL_RES_VS_IDV(paste0(names(results_vector[i]), ".pdf"), absval_res_vs_idv_plot)


absval_res_vs_pred_plot <- absval_res_vs_pred(xpdb, res="CWRES") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose_ABSVAL_RES_VS_PRED(paste0(names(results_vector[i]), ".pdf"), absval_res_vs_pred_plot)


dv_vs_idv_plot <- dv_vs_idv(xpdb, group="ID") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose_DV_VS_IDV(paste0(names(results_vector[i]), ".pdf"), dv_vs_idv_plot)


ipred_vs_idv_plot <- ipred_vs_idv(xpdb, group="ID") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose_IPRED_VS_IDV(paste0(names(results_vector[i]), ".pdf"), ipred_vs_idv_plot)


pred_vs_idv_plot <- pred_vs_idv(xpdb, group="ID") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose_PRED_VS_IDV(paste0(names(results_vector[i]), ".pdf"), pred_vs_idv_plot)


dv_preds_vs_idv_plot <- dv_preds_vs_idv(xpdb) +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose_DV_PRED_VS_IDV(paste0(names(results_vector[i]), ".pdf"), dv_preds_vs_idv_plot)


# prm_distrib_plot <- prm_distrib(xpdb) +
#   labs(title = "", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose_PRM_DIST(paste0(names(results_vector[i]), ".pdf"), prm_distrib_plot)


eta_distrib_plot <- eta_distrib(xpdb) +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose_ETA_DIST(paste0(names(results_vector[i]), ".pdf"), eta_distrib_plot)


# cov_distrib_plot <- cov_distrib(xpdb) +
#   labs(title = "", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose_COV_DIST(paste0(names(results_vector[i]), ".pdf"), cov_distrib_plot)


res_distrib_plot <- res_distrib(xpdb, res="CWRES") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose_RES_DIST(paste0(names(results_vector[i]), ".pdf"), res_distrib_plot)


# prm_qq_plot <- prm_qq(xpdb) +
#   labs(title = "", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose_PRM_QQ(paste0(names(results_vector[i]), ".pdf"), prm_qq_plot)

eta_qq_plot <- eta_qq(xpdb) +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose_ETA_QQ(paste0(names(results_vector[i]), ".pdf"), eta_qq_plot)


# cov_qq_plot <- cov_qq(xpdb) +
#   labs(title = "", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose_COV_QQ(paste0(names(results_vector[i]), ".pdf"), cov_qq_plot)


res_qq_plot <- res_qq(xpdb, res="CWRES") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose_RES_QQ(paste0(names(results_vector[i]), ".pdf"), res_qq_plot)


vpcPlot <- vpcPlot(results_vector[[i]], n=1000) +
            theme_minimal()
SaveXpose_VPC(paste0(names(results_vector[i]), ".pdf"), vpcPlot)

    xpdb <- c()
}




xpdb <- xpose_data_nlmixr(results_vector[[1]])

vpcPlot <- vpcPlot(results_vector[[i]], n=1000) +
            theme_minimal()
SaveXpose_VPC(paste0(names(results_vector[i]), ".pdf"), vpcPlot)
vpcPlot

prm_qq_plot <- prm_qq(xpdb) +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose_PRM_QQ(paste0(names(results_vector[i]), ".pdf"), prm_qq_plot)


cov_distrib_plot <- cov_distrib(xpdb) +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal()
SaveXpose_COV_DIST(paste0(names(results_vector[i]), ".pdf"), cov_distrib_plot)
