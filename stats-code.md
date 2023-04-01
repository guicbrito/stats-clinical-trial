
We will write an R script that does the following:
- Reads data from a file named "data.xlsx" in the "input" folder of the current working directory, which is an excel table with a clinical trial data with each subject's data on a line and variables as columns.
- The data will be in tidy format, which has each variable as a column and each observation as a row, with duplications as necessary. From now on all data should be in the tidy format, unless otherwise specified.
- Calculates the mean, median, standard deviation and interquartile range for each of a certain list of variables that the user can input in a file named "target-variables-stats.csv". These statistic should be calculated for the entire sample and for each group individually. The output should be in table format with the statistics for each variable, one variable per line and the statistics as columns, saved in a file named "descriptive-stats.csv" on a folder named "output" in the current working directory.
- Calculates the absolute and relative frequency for each of a certain list of variables that the user can input in a file named "target-variables-freq.csv". These statistic should be calculated for the entire sample and for each group individually. The output should be in table format with the statistics for each variable, one variable per line and the statistics as columns, saved in a file named "var-freq.csv" on a folder named "output" in the current working directory.
- Calculates the difference between groups for each of a certain list of variables that the user can input in a file named "target-variables-group-diff.csv". These differences should include significance tests and confidence intervals. The output should be in table format with the statistics for each variable, one variable per line and the statistics as columns, saved in a file named "group-diff.csv" on a folder named "output" in the current working directory. The output should inform the statistical test and significance level used, as well as any other important information for the calculations.
- Calculates the correlation between each pair of variables in a certain list of variables that the user can input in a file named "target-variables-cor.csv". The output should be a correlation matrix and inform the correlation coefficient and significance level for each correlation. The output should be saved in a file named "descriptive-cor.csv" on a folder named "output" in the current working directory.
- Based on the variable "Idade" (the age of subjects) create a cathegorical variable for those under or equalto 20 years of age and those over 20 years of age. Save the data including this new variable in a file named "data-with-age-cat.csv" on the "input" folder. Use this data to calculate the difference between subjects under 20 years of age and those above 20 years of age for the entire sample for each of a certain list of variables that the user can input in a file named "target-variables-age-diff.csv". These differences should include significance tests and confidence intervals. The output should be in table format with the statistics for each variable, one variable per line and the statistics as columns, saved in a file named "group-diff-age-cat.csv" on a folder named "output" in the current working directory. The output should inform the statistical test and significance level used, as well as any other important information for the calculations.
We won't work on charts or data visualization for this project.
The data contain the following variables, most of them with numeric values, one with date and NA representing missing values, in the specific order below. These are the first three line of the dataset:
ID,Grupo,Idade,Sexo,Mutacao,Pseudomonas_cronico,Dias_ATB,Internacao,Dias_internacao,Altura_cm,Peso_kg,IMC_absoluto,IMC_escorez,DNA_1,DNA_2,DNA_corrigido15x,DNAcat243,VEF1absoluto,VEF1percprevist,VEF1_menor80,VEF1_menor60,CVFabsoluto,CVFpercprevist,VEF1CVFabsoluto,FEF2575absoluto,FEF2575percprevist,Data_TECP,FCinicial,FCLimiarAnaeróbico,FCfinal,FCmudanca,FCmaxatingida,FCmax,ReservaCardíaca,PASistInicial,PADiastInicial,PASistFinal,PADiastFinal,SpO2inicial,SpO2LimiarAnaeróbico,SpO2final,SPO2cat,SpO2mudanca,SpO2dessaturou,VO2maxpicoouplato,VO2Lminabsolutoinicial,VO2Lminabsolutofinal,VO2mLkgmininicial,VO2mLkgminfinal,VO2categ,Veabsolutoinicial,VEabsolutofinal,VEVO2repouso,VEVO2final,VEVCO2repouso,VEVCO2final,VVM,ReservaVentilatória,PulsodeO2,RQrepouso,RQfinal,VO2LALmin,VO2LAmLKgMin,VOLA,VELminLA,VEVO2LA,VEVCO2LA
1,1,11.41,1,1,2,42,2,0,154.0,65.5,27.6,2.84,4.79,7.42,91.6,1,2.36,91.5,2,2,2.78,91.5,0.85,2.50,86.5,15-Aug-2018,93.0,171.0,200.0,107.0,200.0,100.0,0.01,110,60,140,60,98,98,98,2,0,2,1,0.63,2.28,9.80,35.50,2,13.20,64.80,21.00,28.40,24.40,25.60,87.8,26.2,11.4,0.86,1.11,1.63,25.3,71.3,36.4,22.3,22.3
2,1,9.67,1,3,2,56,1,14,129.0,28.3,17.0,0.39,5.51,NA,82.7,1,0.58,34.7,1,1,1.02,53.1,0.57,0.28,13.9,10/11/2017,139.0,161.0,182.0,43.0,182.0,90.2,9.81,100,60,120,70,98,94,97,2,-1,2,1,0.27,0.84,10.50,32.60,2,8.10,28.50,30.00,33.90,38.60,31.30,31.1,8.4,8.5,0.78,1.08,0.66,25.6,78.5,20.3,30.8,29.4

For now, explain each step this script should have, considering good pratices and explain how you should write this script. Don't output the script yet.










The calc_group_differences funcitons is not working as intended, fix it.Make it more similar to calc_age_group_diff, as it has similar objectives, similar inputs and outputs and is working fine.
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
            test_result <- t.test(Value ~ Grupo, data = filter(data, Variable == var))
        } else {
            # Perform Wilcoxon rank-sum test
            test_result <- wilcox.test(Value ~ Grupo, data = filter(data, Variable == var))
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
calc_age_group_diff <- function(data_with_age_category, target_vars_age_difference, normality_tests, p.value) {
    age_group_diff_results <- lapply(target_vars_age_difference, function(var_age_diff) {
        normality_test <- filter(normality_tests, Variable == var_age_diff)
        is_normal <- normality_test$shapiro_p > p.value

        if (is_normal) {
            # Perform t-test
            test_result <- t.test(data_with_age_category[[var_age_diff]] ~ data_with_age_category$Categoria_Idade)
        } else {
            # Perform Wilcoxon rank-sum test
            test_result <- wilcox.test(data_with_age_category[[var_age_diff]] ~ data_with_age_category$Categoria_Idade)
        }

        return(list(
            Variable = var_age_diff,
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
age_group_diff <- calc_age_group_diff(data_with_age_category, target_vars_age_difference, normality_tests, p.value)
descriptive_stats <- calculate_descriptive_stats(data, target_vars_stats)
frequencies <- calc_frequencies(data, target_vars_frequencies)
group_diff <- calc_group_differences(data, target_vars_group_differences, normality_tests, p.value)
correlations <- calc_correlations(data, target_vars_correlations, normality_tests, p.value)
DNA_corrigido15x_correlations <- correlations %>%
    filter(Variable1 == "DNA_corrigido15x")

# Export results
data_list <- list(
    normality_tests,
    # age_group_diff,
    data_with_age_category,
    descriptive_stats,
    frequencies,
    # group_diff,
    DNA_corrigido15x_correlations,
    correlations
)

file_names <- c(
    "normality-tests.csv",
    # "sub20-vs-above20-differences.csv",
    "data-with-age-category.csv",
    "descriptive-stats.csv",
    "variables-frequency.csv",
    # "group-differences.csv",
    "DNA_corrigido15x-correlations.csv",
    "variables-correlations.csv"
)

export_results(data_list, file_names)
