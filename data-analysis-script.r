# Statistical analysis

print("Essa análise requer os seguintes pacotes instalados: rio, tidyverse, broom, stats")

## Load required packages
library(tidyverse)
library(rio)
library(broom)
library(stats)

if (!dir.exists("output")) {
    dir.create("output")
}

## Read input data
data <- import("input/data.xlsx", setclass = "tibble", na = "NA")

target_vars_stats <- names(import(
    "input/target-variables-descriptive-stats.csv",
    setclass = "tibble"
))
target_vars_frequencies <- names(import(
    "input/target-variables-frequecies.csv",
    setclass = "tibble"
))
target_vars_group_differences <- names(import(
    "input/target-variables-group-differences.csv",
    setclass = "tibble"
))
target_vars_correlations <- names(import(
    "input/target-variables-corrolations.csv",
    setclass = "tibble"
))

colnames(data) <- iconv(colnames(data),
    from = "UTF-8", to = "ASCII//TRANSLIT"
)

target_vars_stats <- iconv(target_vars_stats,
    from = "UTF-8", to = "ASCII//TRANSLIT"
)
target_vars_frequencies <- iconv(target_vars_frequencies,
    from = "UTF-8", to = "ASCII//TRANSLIT"
)
target_vars_group_differences <- iconv(target_vars_group_differences,
    from = "UTF-8", to = "ASCII//TRANSLIT"
)
target_vars_correlations <- iconv(target_vars_correlations,
    from = "UTF-8", to = "ASCII//TRANSLIT"
)

data_with_age_category <- data %>%
    mutate(age_category = ifelse(Idade <= 20, "20_or_below", "above_20"))

## Descriptive statistics
calculate_descriptive_stats <- function(data, target_vars_stats) {
    data %>%
        select(all_of(target_vars_stats)) %>%
        gather(Variable, Value) %>%
        group_by(Variable) %>%
        summarise(
            mean = mean(Value, na.rm = TRUE),
            median = median(Value, na.rm = TRUE),
            sd = sd(Value, na.rm = TRUE),
            iqr = IQR(Value, na.rm = TRUE)
        )
}

## Frequency calculations
calc_frequencies <- function(data, target_vars_frequencies) {
    data %>%
        select(Grupo, all_of(target_vars_frequencies)) %>%
        gather(Variable, Value, -Grupo) %>%
        group_by(Grupo, Variable, Value) %>%
        tally() %>%
        mutate(Frequencia_relativa = n / sum(n)) %>%
        ungroup() %>%
        select(Variable, Value, Grupo, n, Frequencia_relativa)
}

## Group differences
calc_group_differences <- function(data, vars) {
    # Implement the appropriate significance tests and confidence intervals
}


## Correlation matrix
calc_correlations <- function(data, target_vars_correlations) {
    data %>%
        select(all_of(target_vars_correlations)) %>%
        cor(use = "pairwise.complete.obs") %>%
        as.data.frame() %>%
        rownames_to_column(var = "Variable1") %>%
        gather(Variable2, correlation, -Variable1)
}


## Age group differences
calc_age_group_diff <- function(data, vars) {
    # Implement the appropriate significance tests and confidence intervals
}

# Run analysis
age_group_diff <- calc_age_group_diff(data_with_age_category, target_vars_age_diff)
descriptive_stats <- calculate_descriptive_stats(data, target_vars_stats)
frequencies <- calc_frequencies(data, target_vars_frequencies)
group_diff <- calc_group_differences(data, target_vars_group_differences)
correlations <- calc_correlations(data, target_vars_correlations)
DNA_corrigido15x_correlations <- correlations %>%
    filter(Variable1 == "DNA_corrigido15x")

# Export results
write_csv(age_group_diff, "output/sub20-vs-above20-differences.csv")
write_csv(data_with_age_category, "input/data-with-age-category.csv")
write_csv(descriptive_stats, "output/descriptive-stats.csv")
write_csv(frequencies, "output/variables-frequency.csv")
write_csv(group_diff, "output/group-differences.csv")
write_csv(DNA_corrigido15x_correlations, "output/DNA_corrigido15x-correlations.csv")
write_csv(correlations, "output/variables-correlations.csv")
