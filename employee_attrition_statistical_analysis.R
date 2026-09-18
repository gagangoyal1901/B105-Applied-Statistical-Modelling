install.packages(c("tidyverse", "car", "rcompanion", "effectsize"))


## Load Libraries

library(tidyverse)
library(car)
library(rcompanion)
library(effectsize)


## Load Dataset

data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")

head(data)
dim(data)
names(data)
str(data)
summary(data)


## Data Quality Checks

colSums(is.na(data))
sum(duplicated(data))


## Check Constant Variables

table(data$EmployeeCount)
table(data$Over18)
table(data$StandardHours)


## Remove Unnecessary Variables

data <- data %>%
  select(-EmployeeNumber, -EmployeeCount, -Over18, -StandardHours)

dim(data)


## Convert Categorical Variables to Factors

data$Attrition <- factor(data$Attrition)
data$BusinessTravel <- factor(data$BusinessTravel)
data$Department <- factor(data$Department)
data$EducationField <- factor(data$EducationField)
data$Gender <- factor(data$Gender)
data$JobRole <- factor(data$JobRole)
data$MaritalStatus <- factor(data$MaritalStatus)
data$OverTime <- factor(data$OverTime)


## Convert Ordinal Variables

data$Education <- factor(
  data$Education,
  levels = c(1, 2, 3, 4, 5),
  labels = c("Below College", "College", "Bachelor", "Master", "Doctor"),
  ordered = TRUE
)

data$EnvironmentSatisfaction <- factor(
  data$EnvironmentSatisfaction,
  levels = c(1, 2, 3, 4),
  labels = c("Low", "Medium", "High", "Very High"),
  ordered = TRUE
)

data$JobInvolvement <- factor(
  data$JobInvolvement,
  levels = c(1, 2, 3, 4),
  labels = c("Low", "Medium", "High", "Very High"),
  ordered = TRUE
)

data$JobSatisfaction <- factor(
  data$JobSatisfaction,
  levels = c(1, 2, 3, 4),
  labels = c("Low", "Medium", "High", "Very High"),
  ordered = TRUE
)

data$RelationshipSatisfaction <- factor(
  data$RelationshipSatisfaction,
  levels = c(1, 2, 3, 4),
  labels = c("Low", "Medium", "High", "Very High"),
  ordered = TRUE
)

data$WorkLifeBalance <- factor(
  data$WorkLifeBalance,
  levels = c(1, 2, 3, 4),
  labels = c("Bad", "Good", "Better", "Best"),
  ordered = TRUE
)


## Final Data Structure

str(data)
summary(data)


## Sampling Decision

nrow(data)


## Descriptive Statistics

data %>%
  summarise(
    Employees = n(),
    Mean_Age = mean(Age),
    Median_Age = median(Age),
    SD_Age = sd(Age),
    Mean_Income = mean(MonthlyIncome),
    Median_Income = median(MonthlyIncome),
    SD_Income = sd(MonthlyIncome),
    Mean_Distance = mean(DistanceFromHome),
    Mean_Working_Years = mean(TotalWorkingYears),
    Mean_Years_Company = mean(YearsAtCompany)
  )


## Attrition Frequency

table(data$Attrition)
prop.table(table(data$Attrition)) * 100


## Overtime Frequency

table(data$OverTime)
prop.table(table(data$OverTime)) * 100


## Department Frequency

table(data$Department)
prop.table(table(data$Department)) * 100


## Job Role Frequency

table(data$JobRole)
prop.table(table(data$JobRole)) * 100


## Job Satisfaction Frequency

table(data$JobSatisfaction)
prop.table(table(data$JobSatisfaction)) * 100


## Monthly Income by Attrition

data %>%
  group_by(Attrition) %>%
  summarise(
    Employees = n(),
    Mean = mean(MonthlyIncome),
    Median = median(MonthlyIncome),
    SD = sd(MonthlyIncome),
    Minimum = min(MonthlyIncome),
    Maximum = max(MonthlyIncome)
  )


## Monthly Income by Job Role

data %>%
  group_by(JobRole) %>%
  summarise(
    Employees = n(),
    Mean_Income = mean(MonthlyIncome),
    Median_Income = median(MonthlyIncome),
    SD_Income = sd(MonthlyIncome)
  ) %>%
  arrange(desc(Mean_Income))


## Figure 1: Employee Attrition

ggplot(data, aes(x = Attrition)) +
  geom_bar() +
  labs(
    title = "Employee Attrition Distribution",
    x = "Attrition",
    y = "Number of Employees"
  ) +
  theme_minimal()


## Figure 2: Attrition by Overtime

ggplot(data, aes(x = OverTime, fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(
    title = "Employee Attrition by Overtime Status",
    x = "Overtime",
    y = "Proportion",
    fill = "Attrition"
  ) +
  theme_minimal()


## Figure 3: Monthly Income Distribution

ggplot(data, aes(x = MonthlyIncome)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Monthly Income",
    x = "Monthly Income",
    y = "Frequency"
  ) +
  theme_minimal()


## Figure 4: Monthly Income by Attrition

ggplot(data, aes(x = Attrition, y = MonthlyIncome)) +
  geom_boxplot() +
  labs(
    title = "Monthly Income by Employee Attrition",
    x = "Attrition",
    y = "Monthly Income"
  ) +
  theme_minimal()


## Figure 5: Monthly Income by Job Role

ggplot(data, aes(x = JobRole, y = MonthlyIncome)) +
  geom_boxplot() +
  labs(
    title = "Monthly Income by Job Role",
    x = "Job Role",
    y = "Monthly Income"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Figure 6: Job Satisfaction and Attrition

ggplot(data, aes(x = JobSatisfaction, fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(
    title = "Employee Attrition by Job Satisfaction",
    x = "Job Satisfaction",
    y = "Proportion",
    fill = "Attrition"
  ) +
  theme_minimal()


## Figure 7: Attrition by Department

ggplot(data, aes(x = Department, fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(
    title = "Employee Attrition by Department",
    x = "Department",
    y = "Proportion",
    fill = "Attrition"
  ) +
  theme_minimal()


## Hypothesis 1: Overtime and Employee Attrition

overtime_table <- table(data$OverTime, data$Attrition)

overtime_table


## Chi-Square Test

chi_result <- chisq.test(overtime_table)

chi_result


## Expected Frequencies

chi_result$expected


## Cramer's V Effect Size

cramerV(overtime_table)


## Attrition Percentage by Overtime

data %>%
  group_by(OverTime) %>%
  summarise(
    Employees = n(),
    Attrition_Count = sum(Attrition == "Yes"),
    Attrition_Percent = mean(Attrition == "Yes") * 100
  )


## Hypothesis 2: Monthly Income and Employee Attrition

data %>%
  group_by(Attrition) %>%
  summarise(
    N = n(),
    Mean = mean(MonthlyIncome),
    Median = median(MonthlyIncome),
    SD = sd(MonthlyIncome)
  )


## Normality Check

ggplot(data, aes(sample = MonthlyIncome)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Attrition) +
  labs(
    title = "Q-Q Plot of Monthly Income by Attrition"
  ) +
  theme_minimal()


## Shapiro-Wilk Test

shapiro.test(data$MonthlyIncome[data$Attrition == "Yes"])
shapiro.test(data$MonthlyIncome[data$Attrition == "No"])


## Equal Variance Test

leveneTest(MonthlyIncome ~ Attrition, data = data)


## Welch Independent Samples T-Test

income_ttest <- t.test(
  MonthlyIncome ~ Attrition,
  data = data,
  var.equal = FALSE,
  conf.level = 0.95
)

income_ttest


## Cohen's d Effect Size

cohens_d(
  MonthlyIncome ~ Attrition,
  data = data
)


## Wilcoxon Robustness Test

wilcox.test(
  MonthlyIncome ~ Attrition,
  data = data,
  exact = FALSE,
  conf.int = TRUE
)


## Hypothesis 3: Monthly Income Across Job Roles

data %>%
  group_by(JobRole) %>%
  summarise(
    N = n(),
    Mean = mean(MonthlyIncome),
    Median = median(MonthlyIncome),
    SD = sd(MonthlyIncome)
  ) %>%
  arrange(desc(Mean))


## Homogeneity of Variance

leveneTest(
  MonthlyIncome ~ JobRole,
  data = data
)


## One-Way ANOVA

anova_model <- aov(
  MonthlyIncome ~ JobRole,
  data = data
)

summary(anova_model)


## ANOVA Residual Normality

qqnorm(residuals(anova_model))
qqline(residuals(anova_model))

shapiro.test(residuals(anova_model))


## Welch ANOVA

oneway.test(
  MonthlyIncome ~ JobRole,
  data = data,
  var.equal = FALSE
)


## Effect Size

eta_squared(anova_model)


## Tukey Post-Hoc Test

TukeyHSD(anova_model)


## Additional Analysis: Working Experience and Monthly Income

ggplot(data, aes(x = TotalWorkingYears, y = MonthlyIncome)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Total Working Years and Monthly Income",
    x = "Total Working Years",
    y = "Monthly Income"
  ) +
  theme_minimal()


## Spearman Correlation

cor.test(
  data$TotalWorkingYears,
  data$MonthlyIncome,
  method = "spearman",
  exact = FALSE
)


## Additional Attrition Analysis

data %>%
  group_by(Department) %>%
  summarise(
    Employees = n(),
    Attrition_Count = sum(Attrition == "Yes"),
    Attrition_Percent = mean(Attrition == "Yes") * 100
  )


data %>%
  group_by(JobRole) %>%
  summarise(
    Employees = n(),
    Attrition_Count = sum(Attrition == "Yes"),
    Attrition_Percent = mean(Attrition == "Yes") * 100
  ) %>%
  arrange(desc(Attrition_Percent))


## Final Summary Statistics

summary(data$Age)
summary(data$MonthlyIncome)
summary(data$DistanceFromHome)
summary(data$TotalWorkingYears)
summary(data$YearsAtCompany)

table(data$Attrition)
table(data$OverTime, data$Attrition)
table(data$Department, data$Attrition)
table(data$JobSatisfaction, data$Attrition)