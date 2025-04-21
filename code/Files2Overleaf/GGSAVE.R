library(ggplot2)

SavePlot <- function(filename, plotname, folder = "Plots/Preliminary plots/"){
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}
SavePlotRes <- function(filename, plotname, folder = "Plots/ResidualVariancePlots/"){
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SavePlotPD <- function(filename, plotname, folder = "Plots/PD Model Plots/"){
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}

SavePlotPKPD <- function(filename, plotname, folder = "Plots/PK PD Model Plots/"){
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 13, height = 6, dpi = "retina")
}



# Example usage

#source("ExposureModels.R")

#Sinds <- ggplot(data = my.df, aes(x = t, y = C_ORAL_One)) + 
#  geom_line(color = "#001965", size = 2) + 
#  ylab("Concentration (mg / L)") + 
#  xlab("Time (h)") +
#  theme_minimal() +
#  theme(
#    axis.title = element_text(size = 25),
#    axis.text = element_blank()) +
#  geom_segment(aes(x = 0, xend = max(t), y = 0, yend = 0), color = "black", size = 1) +
#  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(my.df$C_ORAL_One)), color = "black", size = 1) +
#  coord_cartesian(xlim = c(0.4, max(my.df$t) - 1))

#SavePlot("TestPlot.pdf", Sinds)