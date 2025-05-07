library(tidyverse)
library(stringr)
library(cowplot)

Savegg <- function(filename, plotname, folder = "Data/XposePlots/nobsv/"){
    full_path <- paste0(folder, filename)
    ggsave(full_path, plot = plotname, width = 11, height = 8, dpi = "retina")
}

# comparing parameters with the data generating parameters
true_parameters <- data.frame(
  parameter = c("tka", "tq", "tcl", "tvc", "tvp"),
  True_Value = c(0.0253, 0.304, 0.0348, 3.59, 4.1))

# Set colors
colors <- c("#005AD2", "#2A918B", "#EEA7BF", "#001965")



parameter_plot <- function(list_of_parFixed_tables, name_of_model) {
  # Making useful dataset for plotting
  parameters_for_plots <- data.frame()
  
  # Iterate through each model's DataFrame in the list
  for (i in seq_along(list_of_parFixed_tables)) {
    
    # Get the current DataFrame
    current_df <- list_of_parFixed_tables[[i]]
    
    # Extract information and create a long-format DataFrame
    temp_data <- current_df %>%
      mutate(
        estimate = as.numeric(str_extract(`Back-transformed(95%CI)`, "^[0-9.]+")),  # Extract the estimate
        lower_ci = as.numeric(str_extract(`Back-transformed(95%CI)`, "(?<=\\().+?(?=,)")),  # Extract lower CI
        upper_ci = as.numeric(str_extract(`Back-transformed(95%CI)`, "(?<=, ).+?(?=\\))")), # Extract upper CI
        parameter = rownames(current_df),
        Model = factor(c('10', '25', '50', '100')[i], levels = c("100","50","25","10"))
      ) %>%  
      select(parameter, Model, estimate, lower_ci, upper_ci) %>%
      filter(row.names(current_df) != "prop.sd")  # Filter out sd row
    
    # Filter based on the condition for upper_ci
    temp_data <- temp_data %>%
      filter(upper_ci <= 25 * estimate)  # Filter to include only valid CI ranges
    
    # Combine the results into the main DataFrame
    parameters_for_plots <- bind_rows(parameters_for_plots, temp_data)
  }

  # Preparing for plotting
  # Create a named vector for colors & prettier names for x axis title
  color_mapping <- setNames(colors, c("10","25","50","100"))

# Create a named vector for renaming with expressions for subscript

parameter_names <- c(
                     "tka" = expression(k[a]), 
                     "tcl" = "CL", 
                     "tq" = "Q", 
                     "tvc" = expression(V[c]), 
                     "tvp" = expression(V[p]))
  
  for (current_parameter in unique(parameters_for_plots$parameter)) {
    # Filter data for the current parameter
    filtered_data <- parameters_for_plots %>% filter(parameter == current_parameter)
    filtered_true_parameters <- true_parameters %>% filter(parameter == current_parameter)
    
    # Ensure that filtered_data is not empty
    if (nrow(filtered_data) > 0) {

      # Make a facet plot
      p <- ggplot(filtered_data, aes(x = estimate , y = Model, color = Model, shape = Model)) +
        geom_point(size = 4) +  # Points for estimates
        geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2, linewidth = 0.8) +  # Vertical error bars
        # Use true_parameters for geom_hline
        geom_vline(data = filtered_true_parameters, aes(xintercept = True_Value), 
                   linetype = "dashed", color = "darkred", size = 1) +  # Horizontal lines for true values
        scale_color_manual(values = color_mapping) +  # Custom colors
        scale_shape_manual(values = c(16, 17, 18, 15)) +  # Custom shapes for points
        labs(y = NULL,
             x = parameter_names[current_parameter],
             color = "Number of subjects",
             shape = "Number of subjects") +  # Axis labels
        theme_minimal(base_size = 20) +  # Minimal theme with larger base font size
        theme(
          axis.title.y = element_blank(),  # Further ensure the y-axis title is blank
          axis.ticks.y = element_blank(),   # Remove y-axis ticks
          axis.text.y = element_blank(),     # Remove y-axis text
          axis.title.x = element_text(size = 40, margin = margin(0.6, 0, 0, 0, "cm"), family = "serif"),  # X-axis title size
          axis.text.x = element_text(size = 30, hjust = 0.455, family = "serif"),    # X-axis tick labels size
          legend.position = "none",
          #legend.key.size = unit(2.8, "cm"),
          #legend.text = element_text(size = 38, family = "serif",),  # Increase legend text size
          #legend.title = element_text(size = 45, family = "serif",),  # Increase legend title size
          #legend.background = element_rect(fill = "#fdfdfd", linetype = "solid"),  # Background for the legend
          #legend.background = element_rect(fill = "lightgray", alpha = 0.5),
          panel.grid.major = element_line(color = "lightgray"),
          panel.grid.minor = element_blank())
          
          #aspect.ratio = 1/1.3  # Adjusted aspect ratio suitable for vertical plots
        
         # save plot as pdf
         Savegg(paste(name_of_model, current_parameter, "errorbar.pdf", sep = "_"), p, folder = paste("Data/parameterplots/", name_of_model, "/", sep = ""))
    }
  }
}

hardcode_parameter_plot <- function(list_of_parFixed_tables, name_of_model, picked_parameter, lower = NULL, upper = NULL) {
  # Making useful dataset for plotting
  parameters_for_plots <- data.frame()
  
  # Iterate through each model's DataFrame in the list
  for (i in seq_along(list_of_parFixed_tables)) {
    
    # Get the current DataFrame
    current_df <- list_of_parFixed_tables[[i]]
    
    # Extract information and create a long-format DataFrame
    temp_data <- current_df %>%
      mutate(
        estimate = as.numeric(str_extract(`Back-transformed(95%CI)`, "^[0-9.]+")),  # Extract the estimate
        lower_ci = as.numeric(str_extract(`Back-transformed(95%CI)`, "(?<=\\().+?(?=,)")),  # Extract lower CI
        upper_ci = as.numeric(str_extract(`Back-transformed(95%CI)`, "(?<=, ).+?(?=\\))")), # Extract upper CI
        parameter = rownames(current_df),
        Model = factor(c('10', '25', '50', '100')[i], levels = c("100","50","25","10"))
      ) %>%  
      select(parameter, Model, estimate, lower_ci, upper_ci) %>%
      filter(row.names(current_df) != "prop.sd")  # Filter out sd row
    
    # Combine the results into the main DataFrame
    parameters_for_plots <- bind_rows(parameters_for_plots, temp_data)
  }

  # Preparing for plotting
  # Create a named vector for colors & prettier names for x axis title
  color_mapping <- setNames(colors, c("10","25","50","100"))

# Create a named vector for renaming with expressions for subscript

parameter_names <- c(
                     "tka" = expression(k[a]), 
                     "tcl" = "CL", 
                     "tq" = "Q", 
                     "tvc" = expression(V[c]), 
                     "tvp" = expression(V[p]))
  
  current_parameter <- picked_parameter
    # Filter data for the current parameter
    filtered_data <- parameters_for_plots %>% filter(parameter == current_parameter)
    filtered_true_parameters <- true_parameters %>% filter(parameter == current_parameter)
    
    # Ensure that filtered_data is not empty
    if (nrow(filtered_data) > 0) {

      # Make a facet plot
      p <- ggplot(filtered_data, aes(x = estimate , y = Model, color = Model, shape = Model)) +
        geom_point(size = 4) +  # Points for estimates
        geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2, linewidth = 0.8) +  # Vertical error bars
        # Use true_parameters for geom_hline
        geom_vline(data = filtered_true_parameters, aes(xintercept = True_Value), 
                   linetype = "dashed", color = "darkred", size = 1) +  # Horizontal lines for true values
        scale_color_manual(values = color_mapping) +  # Custom colors
        scale_shape_manual(values = c(16, 17, 18, 15)) +  # Custom shapes for points
        labs(y = NULL,
             x = parameter_names[current_parameter],
             color = "Number of subjects",
             shape = "Number of subjects") +  # Axis labels
        theme_minimal(base_size = 20) +  # Minimal theme with larger base font size
        theme(
          axis.title.y = element_blank(),  # Further ensure the y-axis title is blank
          axis.ticks.y = element_blank(),   # Remove y-axis ticks
          axis.text.y = element_blank(),     # Remove y-axis text
          axis.title.x = element_text(size = 40, margin = margin(0.6, 0, 0, 0, "cm"), family = "serif"),  # X-axis title size
          axis.text.x = element_text(size = 30, hjust = 0.455, family = "serif"),    # X-axis tick labels size
          legend.position = "none",
          #legend.key.size = unit(2.8, "cm"),
          #legend.text = element_text(size = 38, family = "serif",),  # Increase legend text size
          #legend.title = element_text(size = 45, family = "serif",),  # Increase legend title size
          #legend.background = element_rect(fill = "#fdfdfd", linetype = "solid"),  # Background for the legend
          #legend.background = element_rect(fill = "lightgray", alpha = 0.5),
          panel.grid.major = element_line(color = "lightgray"),
          panel.grid.minor = element_blank()) +
          coord_cartesian(xlim = c(lower, upper), clip = "off")
          #aspect.ratio = 1/1.3  # Adjusted aspect ratio suitable for vertical plots
        
         # save plot as pdf
         Savegg(paste(name_of_model, current_parameter, "errorbar.pdf", sep = "_"), p, folder = paste("Data/parameterplots/", name_of_model, "/", sep = ""))
  
  }
}

parameter_plot_legend <- function(list_of_parFixed_tables, name_of_model) {
  # Making useful dataset for plotting
  parameters_for_plots <- data.frame()
  
  # Iterate through each model's DataFrame in the list
  for (i in seq_along(list_of_parFixed_tables)) {
    
    # Get the current DataFrame
    current_df <- list_of_parFixed_tables[[i]]
    
    # Extract information and create a long-format DataFrame
    temp_data <- current_df %>%
      mutate(
        estimate = as.numeric(str_extract(`Back-transformed(95%CI)`, "^[0-9.]+")),  # Extract the estimate
        lower_ci = as.numeric(str_extract(`Back-transformed(95%CI)`, "(?<=\\().+?(?=,)")),  # Extract lower CI
        upper_ci = as.numeric(str_extract(`Back-transformed(95%CI)`, "(?<=, ).+?(?=\\))")), # Extract upper CI
        parameter = rownames(current_df),
        Model = factor(c('10', '25', '50', '100')[i], levels = c("10","25","50","100"))
      ) %>%  
      select(parameter, Model, estimate, lower_ci, upper_ci) %>%
      filter(row.names(current_df) != "prop.sd")  # Filter out sd row
    
    # Filter based on the condition for upper_ci
    temp_data <- temp_data %>%
      filter(upper_ci <= 25 * estimate)  # Filter to include only valid CI ranges
    
    # Combine the results into the main DataFrame
    parameters_for_plots <- bind_rows(parameters_for_plots, temp_data)
  }

  # Preparing for plotting
  # Create a named vector for colors
  color_mapping <- setNames(colors, c("10","25","50","100"))
  
  for (current_parameter in unique(parameters_for_plots$parameter)) {
    # Filter data for the current parameter
    filtered_data <- parameters_for_plots %>% filter(parameter == current_parameter)
    filtered_true_parameters <- true_parameters %>% filter(parameter == current_parameter)
    
    # Ensure that filtered_data is not empty
    if (nrow(filtered_data) > 0) {

      # Make a facet plot
      p <- ggplot(filtered_data, aes(x = estimate , y = Model, color = Model, shape = Model)) +
        geom_point(size = 4) +  # Points for estimates
        geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2, linewidth = 0.8) +  # Vertical error bars
        # Use true_parameters for geom_hline
        geom_vline(data = filtered_true_parameters, aes(xintercept = True_Value), 
                   linetype = "dashed", color = "darkred", size = 1) +  # Horizontal lines for true value
        scale_color_manual(values = color_mapping) +  # colors
        scale_shape_manual(values = c(16, 17, 18, 15)) +  # shapes for points
        labs(y = NULL,
             x = paste("Estimate of", current_parameter),) + 
        theme_minimal(base_size = 20) +  # Minimal theme with larger base font size
        theme(
          axis.title.y = element_blank(),  # Further ensure the y-axis title is blank
          axis.ticks.y = element_blank(),   # Remove y-axis ticks
          axis.text.y = element_blank(),     # Remove y-axis text
          axis.title.x = element_text(size = 40, margin = margin(0.6, 0, 0, 0, "cm"), family = "serif"),  # X-axis title size
          axis.text.x = element_text(size = 30, family = "serif"),    # X-axis tick labels size
          legend.position = "right",
          legend.key.size = unit(3.5, "cm"),
          legend.text = element_text(size = 40, family = "serif"),  # Increase legend text size
          legend.title = element_blank(),  # Increase legend title size
          #legend.background = element_rect(fill = "#fdfdfd", linetype = "solid"),  # Background for the legend
          #legend.background = element_rect(fill = "lightgray", alpha = 0.5),
          panel.grid.major = element_line(color = "lightgray"),
          panel.grid.minor = element_blank())
          #aspect.ratio = 1/1.3  # Adjusted aspect ratio suitable for vertical plots
        
      
      # Extract the legend
      if (current_parameter == "tka") {
      legend <- get_legend(p + theme(legend.position = "right",
                               legend.box.margin = margin(0, 0, 2, 0, "cm") ))  # You could also specify sizes or other theme elements
      ggsave("Data/parameterplots/legend.pdf", plot = legend, width = 9, height = 6, dpi = "retina", units = "in", limitsize = FALSE)
      }
    }
  }
}


# Defining list with the different kinds of models
# No BSV - NLS
noBSV_NLS_list_parFixed_tables <- list(parFixed_NLS_NoBSV_DV_10_Init_1,
                                        parFixed_NLS_NoBSV_DV_25_Init_1,
                                        parFixed_NLS_NoBSV_DV_50_Init_1,
                                        parFixed_NLS_NoBSV_DV_100_Init_1)

noBSV_list_parFixed_tables <- list(parFixed_NoBSV_DV_10_Init_1,
                                        parFixed_NoBSV_DV_25_Init_1,
                                        parFixed_NoBSV_DV_50_Init_1,
                                        parFixed_NoBSV_DV_100_Init_1)

# ETA on CL & Q
ETA_CL_Q_list_parFixed_tables <- list(parFixed_ETA_CLQ_DV_10_Init_1,
                                        parFixed_ETA_CLQ_DV_25_Init_1,
                                        parFixed_ETA_CLQ_DV_50_Init_1,
                                        parFixed_ETA_CLQ_DV_100_Init_1)

# ETA on V's
ETA_V_list_parFixed_tables <- list(parFixed_ETA_V_DV_10_Init_1,
                                        parFixed_ETA_V_DV_25_Init_1,
                                        parFixed_ETA_V_DV_50_Init_1,
                                        parFixed_ETA_V_DV_100_Init_1)

# ETA on all
ETA_all_list_parFixed_tables <- list(parFixed_ETA_ALL_DV_10_Init_1,
                                        parFixed_ETA_ALL_DV_25_Init_1,
                                        parFixed_ETA_ALL_DV_50_Init_1,
                                        parFixed_ETA_ALL_DV_100_Init_1)

# BW + ETA on CL & Q
BW_ETA_CL_Q_list_parFixed_tables <- list(BW.parFixed_BW_ETA_CLQ_DV_10_Init_1,
                                        BW.parFixed_BW_ETA_CLQ_DV_25_Init_1,
                                        BW.parFixed_BW_ETA_CLQ_DV_50_Init_1,
                                        BW.parFixed_BW_ETA_CLQ_DV_100_Init_1)

# BW + ETA on V's
BW_ETA_V_list_parFixed_tables <- list(BW.parFixed_BW_ETA_V_DV_10_Init_1,
                                        BW.parFixed_BW_ETA_V_DV_25_Init_1,
                                        BW.parFixed_BW_ETA_V_DV_50_Init_1,
                                        BW.parFixed_BW_ETA_V_DV_100_Init_1)

# BW + ETA on all
BW_ETA_all_list_parFixed_tables <- list(BW.parFixed_BW_ETA_ALL_DV_10_Init_1,
                                        BW.parFixed_BW_ETA_ALL_DV_25_Init_1,
                                        BW.parFixed_BW_ETA_ALL_DV_50_Init_1,
                                        BW.parFixed_BW_ETA_ALL_DV_100_Init_1)

# BW + ETA on all
BW_ETA_all_Cor_list_parFixed_tables <- list(BW.parFixed_BW_ETA_ALL_Cor_DV_10_Init_1,
                                        BW.parFixed_BW_ETA_ALL_Cor_DV_25_Init_1,
                                        BW.parFixed_BW_ETA_ALL_Cor_DV_50_Init_1,
                                        BW.parFixed_BW_ETA_ALL_Cor_DV_100_Init_1)

ETA_all_Cor_list_parFixed_tables <- list(parFixed_ETA_ALL_Cor_DV_10_Init_1,
                                        parFixed_ETA_ALL_Cor_DV_25_Init_1,
                                        parFixed_ETA_ALL_Cor_DV_50_Init_1,
                                        parFixed_ETA_ALL_Cor_DV_100_Init_1)

# Calling the parameter_plot() for each model 
parameter_plot(noBSV_NLS_list_parFixed_tables, "noBSV")
parameter_plot(noBSV_list_parFixed_tables, "noBSV_focei")
parameter_plot(ETA_CL_Q_list_parFixed_tables, "CLQ")
parameter_plot(ETA_V_list_parFixed_tables, "V")
parameter_plot(ETA_all_list_parFixed_tables, "all")
parameter_plot(BW_ETA_CL_Q_list_parFixed_tables, "BW_CLQ")
parameter_plot(BW_ETA_V_list_parFixed_tables, "BW_V")
parameter_plot(BW_ETA_all_list_parFixed_tables, "BW_all")
parameter_plot(BW_ETA_all_Cor_list_parFixed_tables, "BW_all_cor")
parameter_plot(ETA_all_Cor_list_parFixed_tables, "all_cor")


# Call funktion to make legend
parameter_plot_legend(noBSV_NLS_list_parFixed_tables, "noBSV")



# Hardcoded plots
  # noBSV
  hardcode_parameter_plot(noBSV_NLS_list_parFixed_tables, "noBSV", "tka", 0, 0.23)
  hardcode_parameter_plot(noBSV_NLS_list_parFixed_tables, "noBSV", "tq", 0, 10)
  hardcode_parameter_plot(noBSV_NLS_list_parFixed_tables, "noBSV", "tvc", 0, 65)
  hardcode_parameter_plot(noBSV_NLS_list_parFixed_tables, "noBSV", "tvp", 0, 50)

  # CLQ
  hardcode_parameter_plot(ETA_CL_Q_list_parFixed_tables, "CLQ", "tka", 0.001, 0.04)
  hardcode_parameter_plot(ETA_CL_Q_list_parFixed_tables, "CLQ", "tq", 0, 1)
  # V
  hardcode_parameter_plot(ETA_V_list_parFixed_tables, "V", "tka", 0, 0.05)
  hardcode_parameter_plot(ETA_V_list_parFixed_tables, "V", "tq", 0, 1.25)
  hardcode_parameter_plot(ETA_V_list_parFixed_tables, "V", "tvc", 0, 5)
  hardcode_parameter_plot(ETA_V_list_parFixed_tables, "V", "tvp", 0, 6.5)

  # All
  #hardcode_parameter_plot(ETA_all_list_parFixed_tables, "all", "tka", 0, 0.03)
  hardcode_parameter_plot(ETA_all_list_parFixed_tables, "all", "tq", 0, 0.65)
  hardcode_parameter_plot(ETA_all_list_parFixed_tables, "all", "tvc", 0.3, 5)
  hardcode_parameter_plot(ETA_all_list_parFixed_tables, "all", "tvp", 1, 6)

  # BW_CLQ
  hardcode_parameter_plot(BW_ETA_CL_Q_list_parFixed_tables, "BW_CLQ", "tka", 0, 0.05)
  hardcode_parameter_plot(BW_ETA_CL_Q_list_parFixed_tables, "BW_CLQ", "tq", 0, 2)
  hardcode_parameter_plot(BW_ETA_CL_Q_list_parFixed_tables, "BW_CLQ", "tvc", 0, 5.1)
  hardcode_parameter_plot(BW_ETA_CL_Q_list_parFixed_tables, "BW_CLQ", "tvp", 1, 6.5)

  # BW_V
  hardcode_parameter_plot(BW_ETA_V_list_parFixed_tables, "BW_V", "tka", 0, 0.05)
  hardcode_parameter_plot(BW_ETA_V_list_parFixed_tables, "BW_V", "tq", 0, 0.62)
  hardcode_parameter_plot(BW_ETA_V_list_parFixed_tables, "BW_V", "tvc", 0, 6)
  hardcode_parameter_plot(BW_ETA_V_list_parFixed_tables, "BW_V", "tvp", 0.2, 6)