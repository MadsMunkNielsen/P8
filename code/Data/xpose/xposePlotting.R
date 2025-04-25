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
        SaveXpose(paste0(names(results_vector[i]), ".pdf"), dv_vs_pred_plot)

    xpdb <- c()
}


        # dv_vs_pred_plot <- dv_vs_pred(xpdb) +
        #     labs(title = "", 
        #     subtitle = "", 
        #     caption = "") +
        #     theme_minimal()
        # SaveXpose(gsub("_Init_1", "",gsub("results_", "", paste0(model, names, ".pdf"))), dv_vs_pred_plot)


# dv_vs_ipred_plot <- dv_vs_ipred(xpdb) +
#   labs(title = "Dependent Variable vs Individual Predictions", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("dv_vs_ipred.pdf", dv_vs_ipred_plot)

# res_vs_idv_plot <- res_vs_idv(xpdb, res="CWRES") +
#   labs(title = "Residuals vs Dependent Variable", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("res_vs_idv.pdf", res_vs_idv_plot)

# res_vs_pred_plot <- res_vs_pred(xpdb, res="CWRES") +
#   labs(title = "Residuals vs Model Prediction", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("res_vs_pred.pdf", res_vs_pred_plot)

# absval_res_vs_idv_plot <- absval_res_vs_idv(xpdb, res="CWRES") +
#   labs(title = "Absolute Residuals vs Dependent Variable", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("absval_res_vs_idv.pdf", absval_res_vs_idv_plot)

# absval_res_vs_pred_plot <- absval_res_vs_pred(xpdb, res="CWRES") +
#   labs(title = "Absolute Residuals vs Model Prediction", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("absval_res_vs_pred.pdf", absval_res_vs_pred_plot)

# # Grouped Plots
# dv_vs_idv_plot <- dv_vs_idv(xpdb, group="ID") +
#   labs(title = "Dependent Variable vs ID", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("dv_vs_idv.pdf", dv_vs_idv_plot)

# ipred_vs_idv_plot <- ipred_vs_idv(xpdb, group="ID") +
#   labs(title = "Individual Predictions vs ID", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("ipred_vs_idv.pdf", ipred_vs_idv_plot)

# pred_vs_idv_plot <- pred_vs_idv(xpdb, group="ID") +
#   labs(title = "Model Predictions vs ID", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("pred_vs_idv.pdf", pred_vs_idv_plot)

# dv_preds_vs_idv_plot <- dv_preds_vs_idv(xpdb) +
#   labs(title = "Dependent Variable vs Predictions", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("dv_preds_vs_idv.pdf", dv_preds_vs_idv_plot)

# # Distribution and QQ Plots
# prm_distrib_plot <- prm_distrib(xpdb) +
#   labs(title = "Parameter Distribution", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("prm_distrib.pdf", prm_distrib_plot)

# eta_distrib_plot <- eta_distrib(xpdb) +
#   labs(title = "Random Effects Distribution", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("eta_distrib.pdf", eta_distrib_plot)

# cov_distrib_plot <- cov_distrib(xpdb) +
#   labs(title = "Covariance Distribution", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("cov_distrib.pdf", cov_distrib_plot)

# res_distrib_plot <- res_distrib(xpdb, res="CWRES") +
#   labs(title = "Residuals Distribution", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("res_distrib.pdf", res_distrib_plot)

# prm_qq_plot <- prm_qq(xpdb) +
#   labs(title = "QQ Plot for Parameters", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("prm_qq.pdf", prm_qq_plot)

# eta_qq_plot <- eta_qq(xpdb) +
#   labs(title = "QQ Plot for Random Effects", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("eta_qq.pdf", eta_qq_plot)

# cov_qq_plot <- cov_qq(xpdb) +
#   labs(title = "QQ Plot for Covariance", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("cov_qq.pdf", cov_qq_plot)

# res_qq_plot <- res_qq(xpdb, res="CWRES") +
#   labs(title = "QQ Plot for Residuals", 
#        subtitle = "", 
#        caption = "") +
#   theme_minimal()
# SaveXpose("res_qq.pdf", res_qq_plot)

# vpcPlot <- vpcPlot(myfit, n=1000) +
#             theme_minimal()
# SaveXpose("vpcPlot.pdf", vpcPlot)
#     }
# }





# included_models <- c()


# xpose_data <- list()
# names(results_None)



# myfit <- results_None$NoBSV_DV_100_Init_1

# xpdb <- xpose_data_nlmixr(myfit)