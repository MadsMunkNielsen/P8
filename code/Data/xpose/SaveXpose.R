SaveXpose_DV_VS_IPRED <- function(filename, plotname, folder = "Data/XposePlots/Dependent_Variable_vs_Individual_Predictions/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_RES_VS_IDV <- function(filename, plotname, folder = "Data/XposePlots/Residuals_vs_Dependent_Variable/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_RES_VS_PRED <- function(filename, plotname, folder = "Data/XposePlots/Residuals_vs_Model_Prediction/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_ABSVAL_RES_VS_IDV <- function(filename, plotname, folder = "Data/XposePlots/Absolute_Residuals_vs_Dependent_Variable/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_ABSVAL_RES_VS_PRED <- function(filename, plotname, folder = "Data/XposePlots/Absolute_Residuals_vs_Model_Prediction/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_DV_VS_IDV <- function(filename, plotname, folder = "Data/XposePlots/Dependent_Variable_vs_ID/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_IPRED_VS_IDV <- function(filename, plotname, folder = "Data/XposePlots/Individual_Predictions_vs_ID/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_PRED_VS_IDV <- function(filename, plotname, folder = "Data/XposePlots/Model_Predictions_vs_ID/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_DV_PRED_VS_IDV <- function(filename, plotname, folder = "Data/XposePlots/Dependent_Variable_vs_Predictions/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_PRM_DISTRIB <- function(filename, plotname, folder = "Data/XposePlots/Parameter_Distribution/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_ETA_DISTRIB <- function(filename, plotname, folder = "Data/XposePlots/Random_Effects_Distribution/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_COV_DISTRIB <- function(filename, plotname, folder = "Data/XposePlots/Covariance_Distribution/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_RES_DISTRIB <- function(filename, plotname, folder = "Data/XposePlots/Residuals_Distribution/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_PRM_QQ <- function(filename, plotname, folder = "Data/XposePlots/QQ_Plot_for_Parameters/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_ETA_QQ <- function(filename, plotname, folder = "Data/XposePlots/QQ_Plot_for_Random_Effects/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_COV_QQ <- function(filename, plotname, folder = "Data/XposePlots/QQ_Plot_for_Covariance/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_RES_QQ <- function(filename, plotname, folder = "Data/XposePlots/Res_QQ/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_VPC <- function(filename, plotname, folder = "Data/XposePlots/VPC/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

