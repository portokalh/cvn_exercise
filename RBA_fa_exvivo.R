# Load required libraries
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(readxl)
library(openxlsx)
library(lme4)
library(lmerTest)
library(emmeans)

# Get today's date in YYYYMMDD format
today_date <- format(Sys.Date(), "%Y%m%d")

# Load the data from Excel file
#df_stats <- read_delim("../RBA_FILES/input_data/studywide_stats_for_volume.txt", delim = "\t", col_types = cols())
setwd("/Users/alex/AlexBadea_MyPapers/CVN_Exercise/cvn030425/")
#df_stats <- read_delim("RBA_FILES/input_data/studywide_stats_for_volume.txt", delim = "\t", col_types = cols())
# Load the data from Excel file
#df_stats <- read_delim("RBA_FILES/input_data/studywide_stats_for_volume.txt", delim = "	", col_types = cols()) %>% filter(!is.na(`structure`))

# Load the data from Excel file
df_stats <- read_excel("RBA_FILES/input_data/fa.xlsx", sheet = "transposed")
df_stats <- df_stats[, 1:163]

# Define output file
output_xlsx <- paste0("fa_exvivo_stats_posthoc_FDR_", today_date, ".xlsx")

# Load master sheet and append treatment column based on "SAMBA Brunno"
df_master <- read_excel("master_sheet_cvn_FINAL.xlsx", sheet = "Sheet1")


# Remove duplicate DWI entries from df_stats, keeping only the first occurrence
df_stats <- df_stats %>% distinct(DWI, .keep_all = TRUE)

# Merge the dataframes based on the "DWI" column, keeping only matching records
df_merged <- df_stats %>%
  inner_join(df_master %>% select(DWI, Treatment), by = "DWI")

# Select only numeric columns along with DWI and Treatment
numeric_columns <- df_merged %>% select(where(is.numeric)) %>% colnames()
df_merged <- df_merged %>% select(all_of(c("DWI", "Treatment", numeric_columns)))

# Check the number of unique DWI values after merging
unique_dwi_count <- n_distinct(df_merged$DWI)
df_stats<-df_merged
# Print the count of unique DWI values
print(unique_dwi_count)

# Save the merged dataset if needed
write.xlsx(df_merged, file = "fa_exvivo_Regional_Stats_FDR.xlsx")

# Get today's date in YYYYMMDD format
today_date <- format(Sys.Date(), "%Y%m%d")


# Initialize results storage
anova_results <- data.frame()
posthoc_results <- list()

# Perform linear model and ANOVA for each region
for (region in colnames(df_stats)[3:ncol(df_stats)]) {
  df_region <- df_stats %>% select(all_of(region), Treatment) %>% drop_na()
  
  # Compute means and SDs per treatment group
  summary_stats <- df_region %>% group_by(Treatment) %>%
    summarise(
      Mean = ifelse(n() > 0, mean(.data[[region]], na.rm = TRUE), NA),
      SD = ifelse(n() > 0, sd(.data[[region]], na.rm = TRUE), NA),
      .groups = 'drop'
    )
  
  # Ensure all treatment groups are present
  summary_stats <- summary_stats %>% complete(Treatment, fill = list(Mean = NA, SD = NA))
  
  # Fit linear model only if at least two groups have data
  if (n_distinct(df_region$Treatment) > 1) {
    #formula <- as.formula(paste(region, "~ Treatment"))
    formula <- as.formula(paste0("`", region, "` ~ Treatment"))
    
    model <- lm(formula, data = df_region)
    anova_result <- anova(model)
    
    # Extract ANOVA values
    f_value <- anova_result$`F value`[1]
    p_value <- anova_result$`Pr(>F)`[1]
    
    # Compute effect sizes
    eta_squared <- f_value / (f_value + nrow(df_region) - 1)
    cohen_f <- sqrt(eta_squared / (1 - eta_squared))
    
    emmeans_result <- emmeans(model, pairwise ~ Treatment, adjust = "tukey")
    
    # Convert to a dataframe
    posthoc_df <- as.data.frame(emmeans_result$contrasts)
    
    # Add region name to the dataframe
    posthoc_df$Region <- region
    
    # Store results in list
    posthoc_results[[region]] <- posthoc_df
    
    
  } else {
    f_value <- NA
    p_value <- NA
    eta_squared <- NA
    cohen_f <- NA
  }
  
  # Store results
  anova_results <- rbind(anova_results, data.frame(
    Region = region,
    Mean_Sedentary = summary_stats$Mean[summary_stats$Treatment == "SEDENTARY"],
    Mean_Treadmill = summary_stats$Mean[summary_stats$Treatment == "VOLUNTARY+ENFORCED"],
    Mean_Wheel = summary_stats$Mean[summary_stats$Treatment == "VOLUNTARY"],
    SD_Sedentary = summary_stats$SD[summary_stats$Treatment == "SEDENTARY"],
    SD_Treadmill = summary_stats$SD[summary_stats$Treatment == "VOLUNTARY+ENFORCED"],
    SD_Wheel = summary_stats$SD[summary_stats$Treatment == "VOLUNTARY"],
    F_value = f_value,
    P_value = p_value,
    Eta_squared = eta_squared,
    Cohen_F = cohen_f
  ))
}

# Apply FDR correction
anova_results$FDR_corrected_P <- p.adjust(anova_results$P_value, method = "fdr")

# Save results to an Excel file
wb <- createWorkbook()
addWorksheet(wb, "ANOVA Results")
writeData(wb, "ANOVA Results", anova_results)
saveWorkbook(wb, output_xlsx, overwrite = TRUE)

posthoc_results_df <- bind_rows(posthoc_results)
wb <- loadWorkbook(output_xlsx)
addWorksheet(wb, "Post Hoc Comparisons")
writeData(wb, "Post Hoc Comparisons", posthoc_results_df)
saveWorkbook(wb, output_xlsx, overwrite = TRUE)


print(paste("Analysis complete. Results saved to", output_xlsx))

# Generate violin plots for the top 9 most significant results where exercised mice have larger volumes than sedentary
top_regions <- anova_results %>%
  filter(FDR_corrected_P < 0.05 & Mean_Wheel > Mean_Sedentary) %>%
  arrange(FDR_corrected_P) %>%
  head(9)

# Define important regions for exercise and reward
selected_regions <- anova_results %>%
  filter(FDR_corrected_P < 0.05 & Mean_Wheel > Mean_Sedentary) %>%
  arrange(FDR_corrected_P) %>%
  head(9) %>%
  pull(Region)

selected_largest_regions <- anova_results %>%
  filter(FDR_corrected_P < 0.05 & Mean_Wheel > Mean_Sedentary) %>%
  arrange(desc(Mean_Sedentary + Mean_Wheel + Mean_Treadmill)) %>%
  head(9) %>%
  pull(Region)


# Generate violin plots for the selected regions
plots <- list()
for (region in selected_regions) {
  plot_data <- df_stats %>% filter(!is.na(.data[[region]]) & !is.na(Treatment))
  
  plot <- ggplot(plot_data, aes(x = Treatment, y = .data[[region]], color = Treatment)) +
    geom_violin() + geom_boxplot(width = 0.1, outlier.shape = NA) + geom_point(position=position_jitter(width=0.2), alpha=0.6) +
    stat_compare_means(method = "t.test", comparisons = list(c("SEDENTARY", "VOLUNTARY"), c("SEDENTARY", "VOLUNTARY+ENFORCED"), c("VOLUNTARY+ENFORCED", "VOLUNTARY")), label="p.signif") +
    theme_bw() + theme(legend.position = "none", axis.text = element_text(size = 10), axis.title = element_text(size = 12), plot.margin = margin(10, 20, 10, 10)) +
    ggtitle(paste( region)) +
    xlab("Treatment") +
    ylab("FA(AU)")
  
 
  
  plots[[region]] <- plot
}

# Save all plots in a single panel
panel_plot <- ggarrange(plotlist = plots, ncol = 3, nrow = 3)
output_plot <- paste0("fa_exvivo_pos_Violin_Panel_", today_date, ".png")
ggsave(output_plot, plot = panel_plot, width = 16, height = 16, dpi = 300)


# Generate and save a separate panel for the largest 9 regions
plots_largest <- list()
for (region in selected_largest_regions) {
  plot_data <- df_stats %>% filter(!is.na(.data[[region]]) & !is.na(Treatment))
  
  plot <- ggplot(plot_data, aes(x = Treatment, y = .data[[region]], color = Treatment)) +
    geom_violin() + geom_boxplot(width = 0.1, outlier.shape = NA) + geom_point(position=position_jitter(width=0.2), alpha=0.6) +
    stat_compare_means(method = "t.test", comparisons = list(c("SEDENTARY", "VOLUNTARY"), c("SEDENTARY", "VOLUNTARY+ENFORCED"), c("VOLUNTARY+ENFORCED", "VOLUNTARY")), label="p.signif") +
    theme_bw() + theme(legend.position = "none", axis.text = element_text(size = 10), axis.title = element_text(size = 12), plot.margin = margin(10, 20, 10, 10)) +
    ggtitle(paste( region)) +
    xlab("Treatment") +
    ylab("FA (AU)")
  
  
  plots_largest[[region]] <- plot
}

panel_plot_largest <- ggarrange(plotlist = plots_largest, ncol = 3, nrow = 3)
output_plot_largest <- paste0("FA_exvivo_Violin_Panel_Largest_", today_date, ".png")
ggsave(output_plot_largest, plot = panel_plot_largest, width = 16, height = 16, dpi = 300)




# Define important regions for exercise and reward
selected_regions <- anova_results %>%
  filter(FDR_corrected_P < 0.05 & Mean_Wheel < Mean_Sedentary) %>%
  arrange(FDR_corrected_P) %>%
  head(9) %>%
  pull(Region)

# Generate violin plots for the selected regions
plots <- list()
for (region in selected_regions) {
  plot_data <- df_stats %>% filter(!is.na(.data[[region]]) & !is.na(Treatment))
  
  plot <- ggplot(plot_data, aes(x = Treatment, y = .data[[region]], color = Treatment)) +
    geom_violin() + geom_boxplot(width = 0.1, outlier.shape = NA) + geom_point(position=position_jitter(width=0.2), alpha=0.6) +
    stat_compare_means(method = "t.test", comparisons = list(c("SEDENTARY", "VOLUNTARY"), c("SEDENTARY", "VOLUNTARY+ENFORCED"), c("VOLUNTARY+ENFORCED", "VOLUNTARY")), label="p.signif") +
    theme_bw() + theme(legend.position = "none", axis.text = element_text(size = 10), axis.title = element_text(size = 12), plot.margin = margin(10, 20, 10, 10)) +
    ggtitle(paste( region)) +
    xlab("Treatment") +
    ylab("FA(AU")
  
  
  
  plots[[region]] <- plot
}

# Save all plots in a single panel
panel_plot <- ggarrange(plotlist = plots, ncol = 3, nrow = 3)
output_plot <- paste0("FA_exvivo_neg_Violin_Panel_", today_date, ".png")
ggsave(output_plot, plot = panel_plot, width = 16, height = 16, dpi = 300)


print(paste("Analysis complete. Results saved to", output_xlsx))


selected_significant_regions <- anova_results %>%
  filter(FDR_corrected_P < 0.05 & Mean_Wheel > Mean_Sedentary) %>%
  arrange(FDR_corrected_P) %>%
  pull(Region)


if (!dir.exists("loose")) {
  dir.create("loose")
}

for (region in selected_significant_regions) {
  plot_data <- df_stats %>% filter(!is.na(.data[[region]]) & !is.na(Treatment))
  
  plot <- ggplot(plot_data, aes(x = Treatment, y = .data[[region]], color = Treatment)) +
    geom_violin() + geom_boxplot(width = 0.1, outlier.shape = NA) + geom_point(position=position_jitter(width=0.2), alpha=0.6) +
    stat_compare_means(method = "t.test", comparisons = list(c("SEDENTARY", "VOLUNTARY"), c("SEDENTARY", "VOLUNTARY+ENFORCED"), c("VOLUNTARY+ENFORCED", "VOLUNTARY")), label="p.signif") +
    theme_bw() + theme(legend.position = "none", axis.text = element_text(size = 10), axis.title = element_text(size = 12), plot.margin = margin(10, 20, 10, 10)) +
    ggtitle(paste(region)) +
    xlab("Treatment") +
    ylab("FA(AU)")
  
  output_plot_region <- paste0("loose/FA_exvivo_Violin_", gsub(" ", "", region), "", today_date, ".png")
  ggsave(output_plot_region, plot = plot, width = 5, height = 5, dpi = 300)
}

print(paste("Analysis complete. Results saved to", output_xlsx))


