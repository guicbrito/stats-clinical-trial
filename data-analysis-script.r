# Statistical analysis
print("Essa análise requer os seguintes pacotes instalados: rio, tidyverse, broom, stats")

## Load required packages
library(tidyverse)
library(rio)
library(broom)
library(stats)

# Create output directory if it doesn't exist
if (!dir.exists("output")) {
    dir.create("output")
}

# Function to read input files and return their column names
read_input <- function(file_path) {
    names(import(file_path, setclass = "tibble"))
}

## Read input data and target variable files
data <- import("input/data.xlsx", setclass = "tibble", na = "NA")
target_vars_stats <- read_input("input/target-variables-descriptive-stats.csv")
target_vars_frequencies <- read_input("input/target-variables-frequecies.csv")
target_vars_group_differences <- read_input("input/target-variables-group-differences.csv")
target_vars_correlations <- read_input("input/target-variables-corrolations.csv")
target_vars_age_difference <- read_input("input/target-variables-age-differences.csv")

# Convert column names to ASCII
iconv_columns <- function(names_vector) {
    iconv(names_vector, from = "UTF-8", to = "ASCII//TRANSLIT")
}

colnames(data) <- iconv_columns(colnames(data))
target_vars_stats <- iconv_columns(target_vars_stats)
target_vars_frequencies <- iconv_columns(target_vars_frequencies)
target_vars_group_differences <- iconv_columns(target_vars_group_differences)
target_vars_correlations <- iconv_columns(target_vars_correlations)
target_vars_age_difference <- iconv_columns(target_vars_age_difference)

# Add Categoria_Idade to the data
data_with_age_category <- data %>%
    mutate(age_category_new = case_when(
        Idade <= 20 ~ "20_ou_menos",
        TRUE ~ "acima_de_20"
    )) %>%
    add_column(Categoria_Idade = .$age_category_new, .after = "Idade") %>%
    select(-age_category_new)

## Descriptive statistics
shapiro_test <- function(data, target_vars_stats, p.value) {
    data %>%
        select(all_of(target_vars_stats)) %>%
        gather(Variable, Value) %>%
        group_by(Variable) %>%
        summarise(
            shapiro_w = shapiro.test(Value)$statistic,
            shapiro_p = shapiro.test(Value)$p.value
        )
}

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
        mutate(
            Frequencia_relativa = n / sum(n),
            Frequencia_absoluta = n
        ) %>%
        ungroup() %>%
        select(Variable, Value, Grupo, Frequencia_absoluta, Frequencia_relativa)
}


## Group differences
calc_group_differences <- function(data, vars, normality_tests, p.value) {
    group_diff_results <- lapply(vars, function(var) {
        normality_test <- filter(normality_tests, Variable == var)
        is_normal <- normality_test$shapiro_p > p.value

        if (is_normal) {
            # Perform t-test
            test_result <- t.test(data[[var]] ~ data$Grupo)
        } else {
            # Perform Wilcoxon rank-sum test
            test_result <- wilcox.test(data[[var]] ~ data$Grupo)
        }

        return(list(
            Variable = var,
            Test = ifelse(is_normal, parametric_diff_test, non_parametric_diff_test),
            Statistic = test_result$statistic,
            P.Value = test_result$p.value
        ))
    })

    return(do.call(rbind.data.frame, group_diff_results))
}



## Correlation matrix
calc_correlations <- function(data, target_vars_correlations, normality_tests, p.value) {
    correlations_results <- lapply(target_vars_correlations, function(var1) {
        lapply(target_vars_correlations, function(var2) {
            if (var1 == var2) {
                return(NULL)
            }

            normality_test_var1 <- filter(normality_tests, Variable == var1)
            normality_test_var2 <- filter(normality_tests, Variable == var2)
            is_normal_var1 <- normality_test_var1$shapiro_p > p.value
            is_normal_var2 <- normality_test_var2$shapiro_p > p.value

            if (is_normal_var1 && is_normal_var2) {
                # Perform Pearson correlation
                corr_result <- cor.test(data[[var1]], data[[var2]], method = parametric_corr_test)
            } else {
                # Perform Spearman correlation
                corr_result <- cor.test(data[[var1]], data[[var2]], method = non_parametric_corr_test)
            }

            return(list(
                Variable1 = var1,
                Variable2 = var2,
                Test = ifelse(is_normal_var1 && is_normal_var2, parametric_corr_test, non_parametric_corr_test),
                Correlation = corr_result$estimate,
                P.Value = corr_result$p.value
            ))
        })
    })

    return(do.call(rbind.data.frame, unlist(correlations_results, recursive = FALSE)))
}


## Age group differences
calc_age_group_diff <- function(data_with_age_category, vars, normality_tests, p.value) {
    age_group_diff_results <- lapply(vars, function(var) {
        normality_test <- filter(normality_tests, Variable == var)
        is_normal <- normality_test$shapiro_p > p.value

        if (is_normal) {
            # Perform t-test
            test_result <- t.test(data_with_age_category[[var]] ~ data_with_age_category$Categoria_Idade)
        } else {
            # Perform Wilcoxon rank-sum test
            test_result <- wilcox.test(data_with_age_category[[var]] ~ data_with_age_category$Categoria_Idade)
        }

        return(list(
            Variable = var,
            Test = ifelse(is_normal, parametric_diff_test, non_parametric_diff_test),
            Statistic = test_result$statistic,
            P.Value = test_result$p.value
        ))
    })

    return(do.call(rbind.data.frame, age_group_diff_results))
}


# Export results to CSV files
export_results <- function(data_list, file_names) {
    for (i in seq_along(data_list)) {
        write_csv(data_list[[i]], paste0("output/", file_names[[i]]))
    }
}

# Define desired analysis
p.value <- 0.05
parametric_corr_test <- "pearson"
non_parametric_corr_test <- "spearman"
parametric_diff_test <- "t-test"
non_parametric_diff_test <- "wilcox"

# Run analysis
normality_tests <- shapiro_test(data, target_vars_stats, p.value)
age_group_diff <- suppressWarnings(calc_age_group_diff(data_with_age_category, target_vars_age_difference, normality_tests, p.value))
descriptive_stats <- calculate_descriptive_stats(data, target_vars_stats)
frequencies <- calc_frequencies(data, target_vars_frequencies)
group_diff <- suppressWarnings(calc_group_differences(data, target_vars_group_differences, normality_tests, p.value))
correlations <- suppressWarnings(calc_correlations(data, target_vars_correlations, normality_tests, p.value))
DNA_corrigido15x_correlations <- correlations %>%
    filter(Variable1 == "DNA_corrigido15x")
# Export results
data_list <- list(
    normality_tests,
    age_group_diff,
    data_with_age_category,
    descriptive_stats,
    frequencies,
    group_diff,
    DNA_corrigido15x_correlations,
    correlations
)

file_names <- c(
    "normality-tests.csv",
    "sub20-vs-above20-differences.csv",
    "data-with-age-category.csv",
    "descriptive-stats.csv",
    "variables-frequency.csv",
    "group-differences.csv",
    "DNA_corrigido15x-correlations.csv",
    "variables-correlations.csv"
)

export_results(data_list, file_names)
