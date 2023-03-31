# Statistical analysis

print("Essa análise requer os seguintes pacotes instalados no R:
	readxl, writexl, tidyverse, broom, openxlsx")

# Load required packages
library(tidyverse)
library(rio)
library(broom)
library(stats)

if (!dir.exists("output")) {
    dir.create("output")
}

# Read input data
data <- import("input/data.xlsx", readxl = FALSE, na.strings = "NA")

target_vars_stats <- names(read_csv("input/target-variables-stats.csv"))
target_vars_freq <- names(read_csv("input/target-variables-freq.csv"))
target_vars_group_diff <- names(read_csv("input/target-variables-group-diff.csv"))
target_vars_cor <- names(read_csv("input/target-variables-cor.csv"))

# Descriptive statistics
calc_descriptive_stats <- function(data, target_vars_stats) {
    data %>%
        select(all_of(target_vars_stats)) %>%
        gather(variable, value) %>%
        group_by(variable) %>%
        summarise(
            mean = mean(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            iqr = IQR(value, na.rm = TRUE)
        )
}

descriptive_stats <- calc_descriptive_stats(data, target_vars_stats)
write.csv(descriptive_stats, "output/descriptive-stats.csv")

# Frequency calculations
calc_frequencies <- function(data, vars) {
    data %>%
        select(Grupo, any_of(vars)) %>%
        gather(variable, value, -Grupo) %>%
        group_by(Grupo, variable, value) %>%
        tally() %>%
        mutate(freq_rel = n / sum(n)) %>%
        ungroup() %>%
        select(variable, value, Grupo, n, freq_rel)
}

frequencies <- calc_frequencies(data, target_vars_freq$variable)
write.csv(frequencies, "output/var-freq.csv")

# Group differences
calc_group_diff <- function(data, vars) {
    # Implement the appropriate significance tests and confidence intervals
}

group_diff <- calc_group_diff(data, target_vars_group_diff$variable)
write.csv(group_diff, "output/group-diff.csv")

# Correlation matrix
calc_correlations <- function(data, vars) {
    data %>%
        select(any_of(vars)) %>%
        cor(use = "pairwise.complete.obs") %>%
        as.data.frame() %>%
        rownames_to_column(var = "variable1") %>%
        gather(variable2, correlation, -variable1)
}

correlations <- calc_correlations(data, target_vars_cor$variable)
write.csv(correlations, "output/descriptive-cor.csv")

# Categorical age variable
data_with_age_cat <- data %>%
    mutate(age_cat = ifelse(Idade <= 20, "20_or_below", "above_20"))

write.csv(data_with_age_cat, "input/data-with-age-cat.csv")

# Age group differences
calc_age_group_diff <- function(data, vars) {
    # Implement the appropriate significance tests and confidence intervals
}

age_group_diff <- calc_age_group_diff(data_with_age_cat, target_vars_age_diff$variable)
write.csv(age_group_diff, "output/group-diff-age-cat.csv")
