library(xpose)
library(xpose.nlmixr2)
library(ggplot2)

SaveXpose <- function(filename, plotname, folder = "Data/XposePlots/"){
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

included_models <- c()
for (names in names(results_))

xpose_data <- list()

for (i in Amount_Of_Subjects) {
  for (j in 1:length(initials_list)) {
    # Create key for storing results_BW_Eta_All
    key_cp <- paste0("BW_ETA_ALL_CP_", i, "_Init_", j)
    key_dv <- paste0("BW_ETA_ALL_DV_", i, "_Init_", j)

    # Fit for CP
    results_BW_Eta_All[[key_cp]] <- nlmixr(
      do.call(two.cmt.depot.All.BW, initials_list[[j]]),
      subsetTrue[subsetTrue$ID >= 1 & subsetTrue$ID <= i, ],
      est = estMethod
    )

<<<<<<< HEAD
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
=======
    # Fit for DV
    results_BW_Eta_All[[key_dv]] <- nlmixr(
      do.call(two.cmt.depot.All.BW, initials_list[[j]]),
      subsetNoise[subsetNoise$ID >= 1 & subsetNoise$ID <= i, ],
      est = estMethod
    )
  }
}


myfit <- results_None$NoBSV_DV_100_Init_1

xpdb <- xpose_data_nlmixr(myfit)
>>>>>>> 916c6571d3a727ce86bb57a4f3c9caba38ccbdb6
