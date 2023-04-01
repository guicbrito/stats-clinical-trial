library(tidyverse)
library(rio)
library(broom)

dir.create("output", showWarnings = FALSE)

read_input <- function(file_path) {
    names(import(file_path, setclass = "tibble"))
}

convert_encoding <- function(data) {
    lapply(data, function(x) {
        iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    })
}

input_files <- c(
    "input/target-variables-descriptive-stats.csv", "input/target-variables-frequencies.csv",
    "input/target-variables-group-differences.csv", "input/target-variables-correlations.csv",
    "input/target-variables-age-differences.csv"
)

input_data <- lapply(input_files, read_input)
input_data <- lapply(input_data, convert_encoding)
names(input_data) <- c("stats", "frequencies", "group_diff", "correlations", "age_diff")

data <- import("input/data.xlsx", setclass = "tibble", na = "NA") %>%
    setNames(iconv(colnames(.), from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
    mutate(Categoria_Idade = if_else(Idade <= 20, "20_ou_menos", "acima_de_20"))


shapiro_test <- function(data, vars) {
    data %>%
        select(all_of(vars)) %>%
        gather(Variable, Value) %>%
        group_by(Variable) %>%
        summarise(
            shapiro_w = shapiro.test(Value)$statistic,
            shapiro_p = shapiro.test(Value)$p.value
        )
}

summarise_stats <- function(data, vars) {
    data %>%
        select(all_of(vars)) %>%
        gather(Variable, Value) %>%
        group_by(Variable) %>%
        summarise(
            mean = mean(Value, na.rm = TRUE),
            median = median(Value, na.rm = TRUE),
            sd = sd(Value, na.rm = TRUE),
            iqr = IQR(Value, na.rm = TRUE)
        )
}

calc_frequencies <- function(data, vars) {
    data %>%
        select(Grupo, all_of(vars)) %>%
        gather(Variable, Value, -Grupo) %>%
        count(Grupo, Variable, Value) %>%
        mutate(
            Frequencia_relativa = n / sum(n),
            Frequencia_absoluta = n
        ) %>%
        select(Variable, Value, Grupo, Frequencia_absoluta, Frequencia_relativa)
}

calc_group_diff <- function(data, vars, normality_tests, p.value, group_var) {
    map_df(vars, function(var) {
        normality_test <- filter(normality_tests, Variable == var)
        is_normal <- normality_test$shapiro_p > p.value
        test_result <- if (is_normal) t.test(data[[var]] ~ data[[group_var]]) else wilcox.test(data[[var]] ~ data[[group_var]], conf.int = TRUE)
        tibble(
            Variable = var,
            Test = ifelse(is_normal, "t-test", "wilcox"),
            Statistic = test_result$statistic,
            p.value = test_result$p.value,
            Significance = ifelse(test_result$p.value < p.value, "*", "-"),
            CI_min = test_result$conf.int[1],
            CI_max = test_result$conf.int[2]
        )
    })
}


calc_correlations <- function(data, target_vars_correlations, normality_tests, p.value, parametric_corr_test, non_parametric_corr_test) {
    combn(target_vars_correlations, 2, simplify = FALSE) %>%
        map_df(function(pair) {
            normality_test1 <- filter(normality_tests, Variable == pair[1])
            normality_test2 <- filter(normality_tests, Variable == pair[2])
            is_normal <- normality_test1$shapiro_p > p.value & normality_test2$shapiro_p > p.value
            corr_result <- if (is_normal) cor.test(data[[pair[1]]], data[[pair[2]]], method = parametric_corr_test) else cor.test(data[[pair[1]]], data[[pair[2]]], method = non_parametric_corr_test)
            tibble(
                Variable1 = pair[1],
                Variable2 = pair[2],
                Test = ifelse(is_normal, parametric_corr_test, non_parametric_corr_test),
                Correlation = corr_result$estimate,
                P.Value = corr_result$p.value,
                Significance = ifelse(corr_result$p.value < p.value, "*", "-")
            )
        })
}

p.value <- 0.05
parametric_corr_test <- "pearson"
non_parametric_corr_test <- "spearman"
parametric_diff_test <- "t-test"
non_parametric_diff_test <- "wilcox"

normality_tests <- shapiro_test(data, unlist(input_data$stats))
descriptive_stats <- summarise_stats(data, unlist(input_data$stats))
frequencies <- calc_frequencies(data, unlist(input_data$frequencies))
group_diff <- calc_group_diff(data, unlist(input_data$group_diff), normality_tests, p.value, "Grupo")
age_group_diff <- calc_group_diff(data, unlist(input_data$age_diff), normality_tests, p.value, "Categoria_Idade")
correlations <- calc_correlations(data, unlist(input_data$correlations), normality_tests, p.value, parametric_corr_test, non_parametric_corr_test)

data_list <- list(
    normality_tests,
    age_group_diff,
    descriptive_stats,
    frequencies,
    group_diff,
    filter(correlations, Variable1 == "DNA_corrigido15x"),
    correlations
)

file_names <- c(
    "normality-tests.csv",
    "sub20-vs-above20-differences.csv",
    "descriptive-stats.csv",
    "variables-frequency.csv",
    "group-differences.csv",
    "DNA_corrigido15x-correlations.csv",
    "variables-correlations.csv"
)

for (i in seq_along(data_list)) {
    write_csv(data_list[[i]], paste0("output/", file_names[[i]]))
}
