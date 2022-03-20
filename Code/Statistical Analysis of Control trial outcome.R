## Loading Packages
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("psych")
library(psych)

## Loading Dataset
data_fsl <- read.csv("Food_Security_Database.csv")
view(data_fsl)
head(data_fsl)
str(data_fsl)

##Describe the n, min, max, mean, and standard deviation BY GROUP of the following ##indicators: baseline ##income, number of bags in storage in 2012, amount of money in savings in 2012

data_fsl %>%
  group_by(Group) %>%
  summarize(n = n(),
            min = min(`Income_in_2012`),
            max = max(`Income_in_2012`),
            mean = mean(`Income_in_2012`),
            standard_deviation = sd(`Income_in_2012`,na.rm = FALSE))

data_fsl %>%
  group_by(Group) %>%
  summarize(n = n(),
            min = min(`Number_of_Bags_in_Storage_2012`),
            max = max(`Number_of_Bags_in_Storage_2012`),
            mean = mean(`Number_of_Bags_in_Storage_2012`),
            standard_deviation = sd(`Number_of_Bags_in_Storage_2012`,na.rm = FALSE))

data_fsl %>%
  group_by(Group) %>%
  summarize(n = n(),
            min = min(`Amount_of_Money_in_Savings_2012`),
            max = max(`Amount_of_Money_in_Savings_2012`),
            mean = mean(`Amount_of_Money_in_Savings_2012`),
            standard_deviation = sd(`Amount_of_Money_in_Savings_2012`,na.rm = FALSE))

##Produce histograms for the following two indicators by Group: Baseline Savings, Maize Acres

ggplot(data_fsl, aes(x = `Amount_of_Money_in_Savings_2012`)) + 
  geom_histogram(aes(color = Group, fill = Group),
                 position = "identity", bins = 30, alpha = 0.4) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(title= "Baseline savings Vs Number of observation By Group", y = "Number of observation")

ggplot(data_fsl, aes(x = `Maize_acres`)) +
  geom_histogram(aes(color = Group, fill = Group),
                 position = "identity", bins = 30, alpha = 0.4) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(title= "Maize Acres Vs Number of observation By Group", y = "Number of observation")

##Selecting numerical indicators
cor_data <- data_fsl %>%
  select("Group", "Own_at_least_1_cow",  
         "Income_in_2012", "Maize_acres", 
         "Own_a_Solar_Light", 
         "Number_of_Bags_in_Storage_2012", 
         "Amount_of_Money_in_Savings_2012", 
         "Number_of_Bags_in_Storage_2013", 
         "Amount_of_Money_in_Savings_2013")

## Recoding True as 1 and False as 0 in column: Own atleast one cow
cor_data$`Own_at_least_1_cow`[cor_data$`Own_at_least_1_cow` == "TRUE"] <- 1
cor_data$`Own_at_least_1_cow`[cor_data$`Own_at_least_1_cow` == "FALSE"] <- 0
cor_data$`Own_at_least_1_cow` <- as.numeric(cor_data$`Own_at_least_1_cow`)

## Recoding OAF as 1 and Control as 0 in column: Group
cor_data$`Group`[cor_data$`Group` == "OAF"] <- 1
cor_data$`Group`[cor_data$`Group` == "Control"] <- 0
cor_data$`Group` <- as.numeric(cor_data$`Group`)

## Describing the correlation Matrix
corr.test(cor_data,method = "pearson")
corrmatrix <- corr.test(cor_data,method = "pearson")
print(corrmatrix, short = FALSE)
write.csv(corrmatrix$r,"Correlation.csv")
write.csv(corrmatrix$p,"pvalue.csv")
write.csv(corrmatrix$ci,"confidlevel.csv")

## Plotting scatterplot for highly correlated variables

install.packages("GGally")
library(GGally)

cor_gmdata <- data_fsl %>%
  select("Group",  "Income_in_2012", "Maize_acres", "Number_of_Bags_in_Storage_2012", "Amount_of_Money_in_Savings_2012", "Number_of_Bags_in_Storage_2013", "Amount_of_Money_in_Savings_2013")
ggpairs(data = cor_gmdata,
        columns = 2:7,
        mapping = ggplot2::aes(color = Group),
        upper = list(continuous = "cor"),
        lower = list(continuous = "points"),
        diag = list(continuous ="densityDiag"),
        title = "Matrix Scatterplots of indicator variables" )

##Statistical Significance of Variables

## T-Test to Determine the Statistical Significance of the Baseline Food Security BY GROUP

install.packages("lessR")
library(lessR)

## T-test for baseline food security: Number of Bags in Storage 2012
ttest(Number_of_Bags_in_Storage_2012 ~ Group, data = data_fsl, paired = FALSE)

## T-test for baseline food security: Amount of money in savingd 2012
ttest(Amount_of_Money_in_Savings_2012 ~ Group, data = data_fsl, paired = FALSE)

## T-Test to Determine the Statistical Significance of the Post Intervention Food Security BY GROUP
## T-test for Post Intervention food security: Number of Bags in Storage 2013
ttest(Number_of_Bags_in_Storage_2013 ~ Group, data = data_fsl, paired = FALSE)

## T-test for Post Intervention food security: Amount of Money in Savings 2013
ttest(Amount_of_Money_in_Savings_2013 ~ Group, data = data_fsl, paired = FALSE)

## T-Test to Determine the Statistical Significance of the Difference of Differences of Food Security BY GROUP
## T-Test for Difference of Differences of Food Security: Number of Bags in Storage 2013 - 2012

data_fsl_1 <- 
  data_fsl %>%
  mutate(diff_Number_of_bags_in_storage = (Number_of_Bags_in_Storage_2013 - Number_of_Bags_in_Storage_2012), diff_Amount_of_Money_in_savings = (Amount_of_Money_in_Savings_2013 - Amount_of_Money_in_Savings_2012))
ttest(diff_Number_of_bags_in_storage ~ Group, data = data_fsl_1, paired = FALSE)

## T-Test for Difference of Differences of Food Security: Amount of Money in Savings 2013 - 2012
ttest(diff_Amount_of_Money_in_savings ~ Group, data = data_fsl_1, paired = FALSE)

## Regression Analysis of Dependent Variable - Food Security Indicator
## Describe what you choose to include in the regression and why
## Because the major focus of the statistically analysis is to predict the impact of the OAF ## program on farmer. This assumption make Number_of_Bags_in_Storage_2013 column my dependents variable and Maize_acres and Group as my independent variables or predictors. The reason why i choose those indicators were; Variables are already proven in the literature to be related to the outcome (Food security), Variables are considered the cause of the outcome (casuality relationship), High variability of the data and absence of missing values, and also that the sample is randomly selected and free of collinearity, and dependent values was choosen because of its statistically significance .
## Describe the regression formula.
## multivariate multiple Regression;
## Y1 + Y2 = Bo + B1X1 + B2X2 + B3X3
## B0 = Intercept
## X1 = continuous variable:  maize acres
## X2 = continuous variable: Income in 2012
## X3 = categorical Variable: Group
## Y1 = dependent variable: Number of bags in Storage 2013
## Y2 = dependent variable: Amount of savings 2013

data_fsl$Group <- as.factor(data_fsl$Group)

mr <-  lm(cbind(Number_of_Bags_in_Storage_2013, Amount_of_Money_in_Savings_2013)~Maize_acres + Income_in_2012 + Group,
          data = data_fsl)
summary(mr)

anova(mr)
