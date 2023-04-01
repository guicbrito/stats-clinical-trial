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


# Plots
choose_plot_type <- function(variable, normality_tests, p.value) {
    normality_test <- filter(normality_tests, Variable == variable)
    is_normal <- normality_test$shapiro_p > p.value
    if (is_normal) {
        return("histogram")
    } else {
        return("boxplot")
    }
}

normality_plot <- ggplot(normality_tests, aes(x = reorder(Variable, shapiro_p), y = shapiro_p)) +
    geom_point(size = 3) +
    geom_hline(yintercept = p.value, linetype = "dashed", color = "red") +
    labs(title = "Shapiro-Wilk Normality Test", x = "Variable", y = "p-value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

descriptive_stats_long <- descriptive_stats %>%
    gather(key = "Statistic", value = "Value", -Variable) %>%
    left_join(descriptive_stats %>% select(Variable, sd), by = "Variable")

descriptive_stats_plot <- descriptive_stats_long %>%
    ggplot(aes(x = Variable, y = Value, fill = Statistic)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes(ymin = Value - sd, ymax = Value + sd), width = 0.2, position = position_dodge(0.9)) +
    labs(title = "Descriptive Statistics", x = "Variable", y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), axis.text.y = element_text(size = 12), legend.text = element_text(size = 12)) +
    scale_fill_brewer(palette = "Set1")


frequencies_plot <- ggplot(frequencies, aes(x = Value, y = Frequencia_relativa, fill = Grupo)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Variable, scales = "free_x") +
    labs(title = "Frequencies by Group", x = "Value", y = "Relative Frequency") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")

group_diff_plot <- ggplot(group_diff, aes(x = Variable, y = p.value, fill = Test)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = p.value, linetype = "dashed", color = "red") +
    labs(title = "Group Differences", x = "Variable", y = "p-value") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")

age_group_diff_plot <- ggplot(age_group_diff, aes(x = Variable, y = p.value, fill = Test)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = p.value, linetype = "dashed", color = "red") +
    labs(title = "Age Group Differences", x = "Variable", y = "p-value") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")

correlations_plot <- ggplot(correlations, aes(x = Variable1, y = Variable2, fill = Correlation, label = round(Correlation, 2))) +
    geom_tile() +
    geom_text(size = 4, color = "black") +
    labs(title = "Correlations", x = "Variable 1", y = "Variable 2") +
    theme_minimal() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), name = "Correlation")


# Save data
data_list <- list(
    normality_tests,
    age_group_diff,
    descriptive_stats,
    frequencies,
    group_diff,
    filter(correlations, Variable1 == "DNA_corrigido15x"),
    correlations
)

data_file_names <- c(
    "normality-tests.csv",
    "sub20-vs-above20-differences.csv",
    "descriptive-stats.csv",
    "variables-frequency.csv",
    "group-differences.csv",
    "DNA_corrigido15x-correlations.csv",
    "variables-correlations.csv"
)

for (i in seq_along(data_list)) {
    write_csv(data_list[[i]], paste0("output/", data_file_names[[i]]))
}

# Save plots
save_plot <- function(plot, file_name, width = 10, height = 5, dpi = 300) {
    ggsave(file_name, plot, width = width, height = height, dpi = dpi, bg = "white")
}

plot_list <- list(
    normality_plot,
    descriptive_stats_plot,
    frequencies_plot,
    group_diff_plot,
    age_group_diff_plot,
    correlations_plot
)

plot_file_names <- c(
    "normality_plot.png",
    "descriptive_stats_plot.png",
    "frequencies-plot.png",
    "group-differences-plot.png",
    "age-group-differences-plot.png",
    "correlations-plot.png"
)

print(plot_list[1])
print(plot_file_names)

mapply(save_plot, plot_list, plot_file_names)
