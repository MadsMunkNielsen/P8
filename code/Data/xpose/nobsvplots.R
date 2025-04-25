
Savegg <- function(filename, plotname, folder = "Data/XposePlots/nobsv/"){
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 11, height = 6, dpi = "retina")
}

# DV vs IPRED for 100 subjects med foci med y= x linje
dv_vs_ipred_100_focei <- ggplot(results_None$NoBSV_DV_100_Init_1) +
  # Plot IPRED variable with lines and points
  geom_point(aes(x = IPRED, y = DV, group = ID, color = as.factor(ID))) + 
  # Plot DV variable with a different aesthetic (different line type or color)
  geom_line(aes(x = IPRED, y = IPRED, group = ID, linetype = "DV"), size = 0.9) + # y = x line
  #geom_point(aes(x = IPRED, y = IPRED, group = ID), shape = 1) + # Use a different shape for DV points
  # Add labels, title, and customize the theme
  labs(title = "DV vs IPRED - No BSV, DV, 100, focei",
       x = "IPRED",
       y = "DV") +
  theme(legend.position = "none") +
  scale_linetype_manual(values = c("DV" = "solid"))



# DV vs IPRED for 100 subjects med nls med y= x linje
dv_vs_ipred_100_nls <- ggplot(results_None_NLS$NoBSV_DV_100_Init_1) +
  # Plot IPRED variable with lines and points
  geom_point(aes(x = IPRED, y = DV, group = ID, color = as.factor(ID))) + 
  # Plot DV variable with a different aesthetic (different line type or color)
  geom_line(aes(x = IPRED, y = IPRED, group = ID, linetype = "DV"), size = 0.9) + # y = x line
  #geom_point(aes(x = IPRED, y = IPRED, group = ID), shape = 1) + # Use a different shape for DV points
  # Add labels, title, and customize the theme
  labs(title = "DV vs IPRED - No BSV, DV, 100, nls",
       x = "IPRED",
       y = "DV") +
  theme(legend.position = "none") +
  scale_linetype_manual(values = c("DV" = "solid"))



# DV and IPRED vs Time for 100 subjects med foci med y= x linje
dv_and_ipred_vs_time_100_nls <- ggplot(results_None_NLS$NoBSV_DV_100_Init_1) +
  # Plot DV variable with a different aesthetic (different line type or color)
  geom_line(aes(x = TIME, y = DV, group = ID, linetype = "DV"), size = 0.9) + # y = x line
  geom_point(aes(x = TIME, y = DV, group = ID), shape = 1) + # Use a different shape for DV points
  # Plot IPRED variable with lines and points
  geom_line(aes(x = TIME, y = IPRED, group = ID, color = as.factor(ID)), size = 1) + 
  geom_point(aes(x = TIME, y = IPRED, group = ID, color = as.factor(ID))) + 
  # Add labels, title, and customize the theme
  labs(title = "DV and IPRED vs time- No BSV, DV, 100, nls",
       x = "Time",
       y = "DV",
       color = "ID",
       linetype = "Variables") +
  theme(legend.position = "none") +
  scale_linetype_manual(values = c("IPRED" = "solid", "DV" = "dashed"))



# DV and IPRED vs Time for 10 subjects med foci med y= x linje
dv_and_ipred_vs_time_10_nls <- ggplot(results_None_NLS$NoBSV_DV_10_Init_1) +
  # Plot DV variable with a different aesthetic (different line type or color)
  geom_line(aes(x = TIME, y = DV, group = ID, linetype = "DV"), size = 0.9) + # y = x line
  geom_point(aes(x = TIME, y = DV, group = ID), shape = 1) + # Use a different shape for DV points
  # Plot IPRED variable with lines and points
  geom_line(aes(x = TIME, y = IPRED, group = ID, color = as.factor(ID)), size = 1) + 
  geom_point(aes(x = TIME, y = IPRED, group = ID, color = as.factor(ID))) + 
  # Add labels, title, and customize the theme
  labs(title = "DV and IPRED vs time - No BSV, DV, 10, nls",
       x = "Time",
       y = "DV",
       color = "ID",
       linetype = "Variables") +
  theme(legend.position = "none") +
  scale_linetype_manual(values = c("IPRED" = "solid", "DV" = "dashed"))



# Saving the plots as pdf
Savegg("dv_vs_ipred_100_focei_plot.pdf", dv_vs_ipred_100_focei)
Savegg("dv_vs_ipred_100_nls_plot.pdf", dv_vs_ipred_100_nls)
Savegg("dv_and_ipred_vs_time_100_nls_plot.pdf", dv_and_ipred_vs_time_100_nls)
Savegg("dv_and_ipred_vs_time_10_nls_plot.pdf", dv_and_ipred_vs_time_10_nls)