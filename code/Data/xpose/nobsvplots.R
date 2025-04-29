Savegg <- function(filename, plotname, folder = "Data/XposePlots/nobsv/"){
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 11, height = 6, dpi = "retina")
}


# Joining the IPRED_NLS and IPRED_FOCEI such that the lines can be plotted in the same graph
data_NLS <- results_None_NLS$NoBSV_DV_100_Init_1 %>%
                  mutate(IPRED_NLS = IPRED)
data_FOCEI <- results_None$NoBSV_DV_100_Init_1 %>%
                  mutate(IPRED_FOCEI = IPRED)

combined_data <- right_join(data_NLS, data_FOCEI %>% select(ID, TIME, IPRED_FOCEI), by = c("ID", "TIME"))



combined_IPRED <- ggplot(combined_data, aes(x = IPRED)) + 
  geom_point(aes(y = DV, group = ID, color = as.factor(ID)), show.legend = FALSE) + 
  geom_line(aes(y = IPRED_NLS), size = 0.9, color = "#2A918B") +
  geom_line(aes(y = IPRED_FOCEI), size = 0.9, color = "#EEA7BF") +
  labs(title = "",
       x = "IPRED",
       y = "DV") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 15), 
    legend.position = "bottom") 


# Generate the plot
combined_IPRED.V2 <- ggplot(combined_data, aes(x = IPRED)) +
  geom_point(aes(y = DV, group = ID), color = "#001965") +  
  geom_line(aes(y = IPRED_NLS, color = "NLS"), size = 0.9) + 
  geom_line(aes(y = IPRED_FOCEI, color = "FOCEI"), size = 0.9) +
  labs(title = "",
       x = "IPRED",
       y = "DV") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.position = "bottom") +
  scale_color_manual(values = c(colors, "FOCEI" = "#3B97DE", "NLS" = "#005AD2"))  # Combine color scales




# DV and IPRED vs Time for 100 subjects est = NLS
dv_and_ipred_vs_time_100_nls <- ggplot(results_None_NLS$NoBSV_DV_100_Init_1, aes(x = TIME)) +
  geom_line(aes(y = DV, group = ID, linetype = "Solid"), size = 0.7, alpha= 0.5, color = "#001965") +
  geom_point(aes(y = DV, group = ID), shape = 16, size = 3, alpha= 0.7, color = "#001965") +
  geom_line(aes(y = IPRED, group = ID), size = 1, color = "#3B97DE") + e
  labs(title = "",
       x = "Time",
       y = "DV",
       linetype = "Variables") +
  theme_minimal() +
  theme(legend.position = "none") 


# DV and IPRED vs Time for 10 subject est = NLS
dv_and_ipred_vs_time_10_nls <- ggplot(results_None_NLS$NoBSV_DV_10_Init_1, aes(x = TIME)) +
  geom_line(aes(y = DV, group = ID, linetype = "Solid"), size = 0.7, alpha= 0.5, color = "#001965") +
  geom_point(aes(y = DV, group = ID), shape = 16, size = 3, alpha= 0.7, color = "#001965") +
  geom_line(aes(y = IPRED, group = ID), size = 1, color = "#3B97DE") + 
  labs(title = "",
       x = "Time",
       y = "DV",
       linetype = "Variables") +
  theme_minimal() +
  theme(legend.position = "none") 




# Saving the plots as pdf
Savegg("dv_vs_ipred_100_focei_plot.pdf", dv_vs_ipred_100_focei)
Savegg("dv_vs_ipred_100_nls_plot.pdf", dv_vs_ipred_100_nls)
Savegg("dv_and_ipred_vs_time_100_nls_plot.pdf", dv_and_ipred_vs_time_100_nls)
Savegg("dv_and_ipred_vs_time_10_nls_plot.pdf", dv_and_ipred_vs_time_10_nls)
Savegg("NLS_FOCEI_IPREDS.pdf", combined_IPRED)
Savegg("NLS_FOCEI_IPREDS2.pdf", combined_IPRED.V2)


### EXTRA ###

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
print(dv_vs_ipred_100_focei)


# DV vs IPRED for 100 subjects med nls med y= x linje
dv_vs_ipred_100_nls <- ggplot(results_None_NLS$NoBSV_DV_100_Init_1) +
  geom_point(aes(x = IPRED, y = DV, group = ID, color = as.factor(ID))) +
  geom_line(aes(x = IPRED, y = IPRED, group = ID, linetype = "DV"), size = 0.9) +
  labs(title = "DV vs IPRED - No BSV, DV, 100, nls",
       x = "IPRED",
       y = "DV") +
  theme_minimal(legend.position = "none") +
  scale_linetype_manual(values = c("DV" = "solid"))