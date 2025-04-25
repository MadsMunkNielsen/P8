SaveXpose_DV_VS_IPRED <- function(filename, plotname, folder = "Data/XposePlots/DV_Ipred/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_RES_VS_IDV <- function(filename, plotname, folder = "Data/XposePlots/Res_Idv/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_RES_VS_PRED <- function(filename, plotname, folder = "Data/XposePlots/Res_Pred/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_ABSVAL_RES_VS_IDV <- function(filename, plotname, folder = "Data/XposePlots/AbsRes_Idv/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_ABSVAL_RES_VS_PRED <- function(filename, plotname, folder = "Data/XposePlots/AbsRes_Pred/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_DV_VS_IDV <- function(filename, plotname, folder = "Data/XposePlots/DV_Idv/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_IPRED_VS_IDV <- function(filename, plotname, folder = "Data/XposePlots/Ipred_Idv/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_PRED_VS_IDV <- function(filename, plotname, folder = "Data/XposePlots/Pred_Idv/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_DV_PRED_VS_IDV <- function(filename, plotname, folder = "Data/XposePlots/DV_Pred_Idv/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_PRM_DIST <- function(filename, plotname, folder = "Data/XposePlots/Prm_Dist/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_ETA_DIST <- function(filename, plotname, folder = "Data/XposePlots/Eta_Dist/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_COV_DIST <- function(filename, plotname, folder = "Data/XposePlots/Cov_Dist/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_RES_DIST <- function(filename, plotname, folder = "Data/XposePlots/Res_Dist/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_PRM_QQ <- function(filename, plotname, folder = "Data/XposePlots/PRM_QQ/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_ETA_QQ <- function(filename, plotname, folder = "Data/XposePlots/Eta_QQ/") {
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SaveXpose_COV_QQ <- function(filename, plotname, folder = "Data/XposePlots/Cov_QQ/") {
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