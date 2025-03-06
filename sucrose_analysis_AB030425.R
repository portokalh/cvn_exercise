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
df <- read_excel("Sucrose_dayAB.xlsx")

# Reshape data to long format
df_long <- df %>%
  pivot_longer(cols = starts_with("S"), names_to = "Timepoint", values_to = "Sucrose_Preference") %>%
  mutate(Week = as.numeric(str_extract(Timepoint, "\\d{2}$")),
         Day = as.numeric(str_extract(Timepoint, "\\d+")),
         Treatment = as.factor(Treatment),
         Animal = as.factor(Animal))

# Define output file for saving results with date
output_xlsx <- paste0("Sucrose_Analysis_Results_", today_date, ".xlsx")

# Compute summary statistics (mean, median, SD)
summary_stats <- df_long %>%
  group_by(Treatment, Week) %>%
  summarise(
    Mean = mean(Sucrose_Preference, na.rm = TRUE),
    Median = median(Sucrose_Preference, na.rm = TRUE),
    SD = sd(Sucrose_Preference, na.rm = TRUE),
    N = n(),
    .groups = 'drop'
  )

# Fit linear mixed-effects model
model <- lmer(Sucrose_Preference ~ Treatment * Week + (1 | Animal), data = df_long)

# ANOVA results
anova_results <- as.data.frame(anova(model))

# Post-hoc pairwise comparisons with FDR correction
posthoc <- emmeans(model, pairwise ~ Treatment * Week, adjust = "fdr")
posthoc_results <- as.data.frame(posthoc$contrasts)

# Compute effect sizes (Cohen's d)
compute_cohen_d <- function(emmeans_obj) {
  comparisons <- as.data.frame(emmeans_obj$contrasts)
  comparisons$Cohen_d <- comparisons$estimate / sqrt(comparisons$SE^2)
  return(comparisons)
}

posthoc_results <- compute_cohen_d(posthoc)

# Save results to an Excel file
wb <- createWorkbook()
addWorksheet(wb, "Long Data")
addWorksheet(wb, "ANOVA Results")
addWorksheet(wb, "Posthoc Results")
addWorksheet(wb, "Summary Stats")

writeData(wb, "Long Data", df_long)
writeData(wb, "ANOVA Results", anova_results, rowNames = TRUE)
writeData(wb, "Posthoc Results", posthoc_results)
writeData(wb, "Summary Stats", summary_stats)

saveWorkbook(wb, output_xlsx, overwrite = TRUE)

# Print confirmation message
print(paste("Results saved to", output_xlsx))

# Define output file names for plots with date
output_plot_36 <- paste0("Sucrose_Preference_36W_", today_date, ".png")
output_plot_52 <- paste0("Sucrose_Preference_52W_", today_date, ".png")

# Define statistical comparisons (for t-tests with FDR correction)
my_comparisons <- list(c("SEDENTARY", "VOLUNTARY"), 
                       c("SEDENTARY", "VOLUNTARY + ENFORCED"),
                       c("VOLUNTARY", "VOLUNTARY + ENFORCED"))

# Generate Violin Plot for 36 Weeks
plot_36W <- ggplot(df_long %>% filter(Week == 36), aes(x=Treatment, y=Sucrose_Preference, color=Treatment)) +
  geom_violin() + geom_boxplot(width=0.1, outlier.shape = NA) +
  geom_point(position=position_jitter(width=0.2), alpha=0.6) +
  theme_bw() +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, p.adjust.method = "fdr", size = 5, label="p.signif") +
  theme(text=element_text(size=20), legend.position="none") +
  scale_y_continuous("Average Sucrose Preference 36W") +
  ggtitle("Sucrose Preference at 36 Weeks")

# Save the plot for 36 Weeks
ggsave(output_plot_36, plot = plot_36W, width = 8, height = 6, dpi = 300)

# Generate Violin Plot for 52 Weeks
plot_52W <- ggplot(df_long %>% filter(Week == 52), aes(x=Treatment, y=Sucrose_Preference, color=Treatment)) +
  geom_violin() + geom_boxplot(width=0.1, outlier.shape = NA) +
  geom_point(position=position_jitter(width=0.2), alpha=0.6) +
  theme_bw() +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, p.adjust.method = "fdr", size = 5, label="p.signif") +
  theme(text=element_text(size=20), legend.position="none") +
  scale_y_continuous("Average Sucrose Preference 52W") +
  ggtitle("Sucrose Preference at 52 Weeks")

# Save the plot for 52 Weeks
ggsave(output_plot_52, plot = plot_52W, width = 8, height = 6, dpi = 300)

# Print confirmation message
print(paste("Plots saved as", output_plot_36, "and", output_plot_52))
