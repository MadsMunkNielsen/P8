library(xpose)
library(xpose.nlmixr2)
library(ggplot2)
library(nlmixr2)
library(vpc)
source("Data/xpose/SaveXpose.R")


results_vector <- c(results_Eta_V, results_Eta_CL_Q, results_Eta_All, results_BW_Eta_V, results_BW_Eta_CL_Q, results_BW_Eta_All)


vpc_theme <- new_vpc_theme(update = list(
  obs_color = "black",
  obs_ci_color = "black",
  obs_alpha = .6,
  sim_pi_fill = "#2A918B",
  sim_median_fill = "#005AD2",
  sim_pi_size = 4
))


for (i in names(results_vector)) {

    xpdb <- xpose_data_nlmixr(results_vector[[i]])

dv_vs_pred_plot <- dv_vs_pred(xpdb, line_alpha = 0, point_color = "#001965", smooth_color = "#3B97DE") +
    labs(title = "", 
        subtitle = "", 
        caption = "") +
    theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.position = "none"
  )
SaveXpose_DV_PRED(paste0(names(results_vector[i]), ".pdf"), dv_vs_pred_plot)


dv_vs_ipred_plot <- dv_vs_ipred(xpdb, line_alpha = 0, point_color = "#001965", smooth_color = "#3B97DE") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.position = "none"
  )
SaveXpose_DV_VS_IPRED(paste0(names(results_vector[i]), ".pdf"), dv_vs_ipred_plot)


res_vs_idv_plot <- res_vs_idv(xpdb, res="CWRES", line_alpha = 0, point_color = "#001965", smooth_color = "#3B97DE") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.position = "none"
  )
SaveXpose_RES_VS_IDV(paste0(names(results_vector[i]), ".pdf"), res_vs_idv_plot)


res_vs_pred_plot <- res_vs_pred(xpdb, res="CWRES", line_alpha = 0, point_color = "#001965", smooth_color = "#3B97DE") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.position = "none"
  )
SaveXpose_RES_VS_PRED(paste0(names(results_vector[i]), ".pdf"), res_vs_pred_plot)


absval_res_vs_idv_plot <- absval_res_vs_idv(xpdb, res="CWRES", line_alpha = 0, point_color = "#001965", smooth_color = "#3B97DE") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.position = "none"
  )
SaveXpose_ABSVAL_RES_VS_IDV(paste0(names(results_vector[i]), ".pdf"), absval_res_vs_idv_plot)


absval_res_vs_pred_plot <- absval_res_vs_pred(xpdb, res="CWRES", line_alpha = 0, point_color = "#001965", smooth_color = "#3B97DE") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.position = "none"
  )
SaveXpose_ABSVAL_RES_VS_PRED(paste0(names(results_vector[i]), ".pdf"), absval_res_vs_pred_plot)


dv_vs_idv_plot <- dv_vs_idv(xpdb, group="ID", line_alpha = 0, point_color = "#001965", smooth_color = "#3B97DE") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.position = "none"
  )
SaveXpose_DV_VS_IDV(paste0(names(results_vector[i]), ".pdf"), dv_vs_idv_plot)


ipred_vs_idv_plot <- ipred_vs_idv(xpdb, group="ID", line_alpha = 0.5, line_color = "#001965", point_color = "#001965", smooth_color = "#3B97DE") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.position = "none"
  )
SaveXpose_IPRED_VS_IDV(paste0(names(results_vector[i]), ".pdf"), ipred_vs_idv_plot)


pred_vs_idv_plot <- pred_vs_idv(xpdb, group="ID", line_alpha = 0.5, line_color = "#001965", point_color = "#001965", smooth_color = "#3B97DE") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.position = "none"
  )
SaveXpose_PRED_VS_IDV(paste0(names(results_vector[i]), ".pdf"), pred_vs_idv_plot)


dv_preds_vs_idv_plot <- dv_preds_vs_idv(xpdb, line_alpha = 0.5, line_color = "#001965", point_color = "#001965", smooth_color = "#3B97DE") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.position = "none"
  )
SaveXpose_DV_PRED_VS_IDV(paste0(names(results_vector[i]), ".pdf"), dv_preds_vs_idv_plot)


# prm_distrib_plot <- prm_distrib(xpdb) +
#   labs(title = "", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose_PRM_DIST(paste0(names(results_vector[i]), ".pdf"), prm_distrib_plot)


eta_distrib_plot <- eta_distrib(xpdb, histogram_fill = "#001965", histogram_color = "#939AA7", rug_color = "#939AA7") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.position = "none",
    strip.text = element_text(size = 25))
SaveXpose_ETA_DIST(paste0(names(results_vector[i]), ".pdf"), eta_distrib_plot)


# cov_distrib_plot <- cov_distrib(xpdb) +
#   labs(title = "", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose_COV_DIST(paste0(names(results_vector[i]), ".pdf"), cov_distrib_plot)


res_distrib_plot <- res_distrib(xpdb, res="CWRES", histogram_fill = "#001965", histogram_color = "#939AA7", rug_color = "#939AA7") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.position = "none",
    strip.text = element_text(size = 25))
SaveXpose_RES_DIST(paste0(names(results_vector[i]), ".pdf"), res_distrib_plot)


# prm_qq_plot <- prm_qq(xpdb) +
#   labs(title = "", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose_PRM_QQ(paste0(names(results_vector[i]), ".pdf"), prm_qq_plot)

eta_qq_plot <- eta_qq(xpdb, line_alpha = 0, point_color = "#001965", guide_color = "#3B97DE") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.position = "none",
    strip.text = element_text(size = 25))
SaveXpose_ETA_QQ(paste0(names(results_vector[i]), ".pdf"), eta_qq_plot)


# cov_qq_plot <- cov_qq(xpdb) +
#   labs(title = "", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose_COV_QQ(paste0(names(results_vector[i]), ".pdf"), cov_qq_plot)


res_qq_plot <- res_qq(xpdb, res="CWRES", line_alpha = 0, point_color = "#001965", guide_color = "#3B97DE") +
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.position = "none",
    strip.text = element_text(size = 25))
SaveXpose_RES_QQ(paste0(names(results_vector[i]), ".pdf"), res_qq_plot)


vpcPlot <- vpcPlot(results_vector[[i]], n=1000, vpc_theme = vpc_theme) +
            theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.position = "none")
SaveXpose_VPC(paste0(names(results_vector[i]), ".pdf"), vpcPlot)

    xpdb <- c()
}


# Creating an additional VPC plot
vpcPlot <- vpcPlot(results_None_NLS$NoBSV_DV_100_Init_1, n=1000, vpc_theme = vpc_theme) +
            theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.position = "none")
SaveXpose_VPC("NoBSV_DV_100_Init_1.pdf", vpcPlot)
