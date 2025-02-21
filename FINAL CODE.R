#packages and libraries -----------------------------------------------------------------------------------------------------------
install.packages("dplyr")
install.packages("reshape2")
install.packages("tidyverse")
install.packages("zoo")
install.packages("ggplot2")
install.packages("readr")
install.packages("plm")
install.packages("car")
install.packages("wooldridge")
install.packages("xfun")
install.packages("dynlm")
install.packages("Hmisc")
install.packages("Rcpp")
install.packages("lmtest")
install.packages("stargazer")
install.packages("sandwich")
install.packages("fpp2")
install.packages("readxl")
install.packages("forecast")
install.packages("lme4")
install.packages("plm")
install.packages("lmtest")
install.packages("nortest")
install.packages("tseries")
install.packages("AER")
install.packages("ivreg")

library(dplyr)
library(reshape2)
library(tidyverse)
library(zoo)
library(ggplot2)
library(readr)
library(plm)
library(car)
library(wooldridge)
library(xfun)
library(dynlm)
library(Hmisc)
library(Rcpp)
library(lmtest)
library(stargazer)
library(sandwich)
library(fpp2)
library(readxl)
library(forecast)
library(lme4)
library(plm)
library(lmtest)
library(tseries)
library(AER)
library(ivreg)

#Import IHME data -----------------------------------------------------------------------------------------------------------
IHME1 <- read.csv("IHME1.csv", header=TRUE)
IHME2 <- read.csv("IHME2.csv", header=TRUE)
IHME3 <- read.csv("IHME3.csv", header=TRUE)
IHME4 <- read.csv("IHME4.csv", header=TRUE)
IHME5 <- read.csv("IHME5.csv", header=TRUE)
IHME6 <- read.csv("IHME6.csv", header=TRUE)
IHME7 <- read.csv("IHME7.csv", header=TRUE)
IHME9 <- read.csv("IHME9.csv", header=TRUE)

names(IHME9)[names(IHME9) == 'cause'] <- "rei"

df_full <- rbind(IHME1, IHME2)
df_full <- rbind(df_full, IHME3)
df_full <- rbind(df_full, IHME4)
df_full <- rbind(df_full, IHME5)

#remove pollution (polution rei)
df_full <- subset(df_full, !rei %in% c("Ambient particulate matter pollution", "Household air pollution from solid fuels", "Household air pollution from solid fuels", "Ambient ozone pollution"))

unique(IHME7$rei)

df_full <- rbind(df_full, IHME6)
df_full <- rbind(df_full, IHME7)
df_full <- rbind(df_full, IHME9)

#average income data ------------------------------------------------------------------------------------------------
avg_income <- read.csv("avg_income.csv", header=TRUE)

#now need to re-format - years are the individual observations (use notes for this)
avg_income <- melt(avg_income, id.vars = "country", variable.name = "year", value.name = "income")
avg_income$year <- as.character(avg_income$year)
year_vector <- avg_income$year
year_vector <- substr(year_vector, 2, nchar(year_vector))
avg_income$year <- year_vector

avg_income <- filter(avg_income, year > 1989)
avg_income <- filter(avg_income, year < 2020)

#average years of schooling data -------------------------------------------------------------------------------------
avg_years_school <- read.csv("mean_years_schooling_male.csv", header=TRUE)
avg_years_school$NATMON_IND <- NULL
avg_years_school$Flag.Codes <- NULL
avg_years_school$Flags <- NULL
avg_years_school$Time <-  NULL
avg_years_school$LOCATION <- NULL
names(avg_years_school)[names(avg_years_school) == 'TIME'] <- "Year"
names(avg_years_school)[names(avg_years_school) == 'Value'] <- "Mean Years of Schooling"

#delete rows we do not need
condition_1 <- avg_years_school$Indicator == "Mean years of schooling (ISCED 1 or higher), population 25+ years, both sexes"
rows_to_delete_1 <- which(condition_1)
avg_years_school <- avg_years_school[-rows_to_delete_1, ]

condition_2 <- avg_years_school$Indicator == "Mean years of schooling (ISCED 1 or higher), population 25+ years, adjusted gender parity index (GPIA)"
rows_to_delete_2 <- which(condition_2)
avg_years_school <- avg_years_school[-rows_to_delete_2, ]

#adding gender varibale
avg_years_school$gender <- 'male'
avg_years_school$gender[avg_years_school$Indicator == "Mean years of schooling (ISCED 1 or higher), population 25+ years, female"] <- 'female'

#remove indicator variable
avg_years_school$Indicator <- NULL

avg_years_school <- filter(avg_years_school, Year > 1989)
avg_years_school <- filter(avg_years_school, Year < 2020)

avg_years_school <- dcast(avg_years_school, Country + Year ~ gender, value.var = "Mean Years of Schooling")

countries <- unique(avg_years_school$Country)
df <- data.frame(Country = countries)

years <- 1990:2019

# Create a data frame with all combinations of countries and years
country_years <- expand.grid(Country = countries, Year = years)

avg_years_school <- merge(country_years, avg_years_school, by = c("Country", "Year"), all = TRUE)


avg_years_school <- avg_years_school %>%
  group_by(Country) %>%
  arrange(Year) %>%  # Ensure data is sorted by year within each country
  mutate(Moving_Avg_Female = rollapply(female, width = 5, FUN = function(x) mean(x, na.rm = TRUE), 
                                       fill = NA, align = "right", partial = TRUE)) %>%
  ungroup()  # Ungroup to return to a regular data frame

avg_years_school <- avg_years_school %>%
  group_by(Country) %>%
  arrange(Year) %>%  # Ensure data is sorted by year within each country
  mutate(Moving_Avg_Male = rollapply(male, width = 5, FUN = function(x) mean(x, na.rm = TRUE), 
                                     fill = NA, align = "right", partial = TRUE)) %>%
  ungroup()  # Ungroup to return to a regular data frame

avg_years_school$female <- NULL
avg_years_school$male <- NULL

names(avg_years_school)[names(avg_years_school) == 'Moving_Avg_Female'] <- "female"
names(avg_years_school)[names(avg_years_school) == 'Moving_Avg_Male'] <- "male"

#avg_years_school <- melt(avg_years_school, id = c("Country"), variable.name = "Year")

avg_years_school <- avg_years_school %>%
  pivot_longer(cols = c(male, female),
               names_to = "gender",
               values_to = "Mean Years of Schooling")

#education gov expenditure per capita -------------------------------------------------------------------------------
gov_edu_expenditure <- read.csv("gov_exp_edu.csv", header=TRUE)
#in real US dollars btw, and in millions

total_expenditure <- aggregate(Value ~ Country + Time, data = gov_edu_expenditure, FUN = sum)
names(total_expenditure)[names(total_expenditure) == 'Value'] <- "Edu_Expenditure"
names(total_expenditure)[names(total_expenditure) == 'Time'] <- "Year"


total_expenditure$Edu_Expenditure <- total_expenditure$Edu_Expenditure*1000000
#now it is the real values (not in millions anymore)

#need to import population database
pop <- read.csv("pop_size.csv", header=TRUE)

#now need to re-format - years are the individual observations (use notes for this)
pop <- melt(pop, id.vars = "country", variable.name = "Year", value.name = "Population Size")
names(pop)[names(pop) == 'country'] <- "Country"


pop$Year <- as.character(pop$Year)
year_vector_pop <- pop$Year
year_vector_pop <- substr(year_vector_pop, 2, nchar(year_vector_pop))
pop$Year <- year_vector_pop

pop <- filter(pop, Year < 2020)
pop <- filter(pop, Year > 1989)

#create function to convert to numerical values
convert_population <- function(value) {
  if (grepl("k", value, fixed = TRUE)) {
    return(as.numeric(sub("k", "", value)) * 1000)
  } else if (grepl("M", value, fixed = TRUE)) {
    return(as.numeric(sub("M", "", value)) * 1000000)
  } else if (grepl("B", value, fixed = TRUE)) {
    return(as.numeric(sub("B", "", value)) * 1000000000)
  } else {
    return(as.numeric(value))
  }
}

# Apply the function to the population column
pop$population_size <- sapply(pop$`Population Size`, convert_population)
#get rid of previous non-numerical value column 
pop$`Population Size` <- NULL


#now need to merge both based on year and country
edu_capita_exp = merge(pop, total_expenditure, by.x=c("Country", "Year"), by.y=c("Country", "Year"))
#now add a column that calculates per capita expenditure
edu_capita_exp$Edu_Expenditure <- edu_capita_exp$Edu_Expenditure / edu_capita_exp$population_size
edu_capita_exp$population_size <- NULL

#format world bank data again --------------------------------------------------------------------------------------
worldbank_data <- read.csv("worldbank_all.csv", header=TRUE)
worldbank_data$Country.Code <- NULL

names(worldbank_data)[names(worldbank_data) == 'Country.Name'] <- "Country"

#change the series name attribute
worldbank_data$Series.Name <- ifelse(worldbank_data$Series.Name == "Access to clean fuels and technologies for cooking (% of population)", "Access_Clean_Fuels", worldbank_data$Series.Name)
worldbank_data$Series.Name <- ifelse(worldbank_data$Series.Name == "Access to electricity (% of population)", "Access_Electricity", worldbank_data$Series.Name)
worldbank_data$Series.Name <- ifelse(worldbank_data$Series.Name == "Investment in energy with private participation (current US$)", "Energy_Invst_Private_Participation", worldbank_data$Series.Name)
worldbank_data$Series.Name <- ifelse(worldbank_data$Series.Name == "Public private partnerships investment in energy (current US$)", "Partner_Energy_Invst", worldbank_data$Series.Name)
worldbank_data <- worldbank_data[worldbank_data$Series.Name != "", ]

unique(worldbank_data$Series.Name)

#change series name - name
names(worldbank_data)[names(worldbank_data) == 'Series.Name'] <- "Measurement"
worldbank_data$Series.Code <- NULL

worldbank_data$Measurement <- as.factor(worldbank_data$Measurement)
summary(worldbank_data)

# Extract the year from column names and rename them
for (col_name in names(worldbank_data)) {
  if (grepl("YR[0-9]{4}", col_name)) { # Check if the column name contains the pattern YR followed by 4 digits
    year <- sub(".*YR([0-9]{4}).*", "\\1", col_name) # Extract the year
    names(worldbank_data)[names(worldbank_data) == col_name] <- year # Rename the column
  }
}

worldbank_data <- melt(worldbank_data, id = c("Country", "Measurement"), variable.name = "Year")

worldbank_data <- dcast(worldbank_data, Country + Year ~ Measurement, value.var = "value")

#format IHME data ------------------------------------------------------------------------------------------------
unique(df_full$rei)
unique(IHME1$rei)

#only including SEV in this table
df_full$metric <- NULL
df_full$measure <- NULL

names(df_full)[names(df_full) == 'location'] <- "Country"

#remove upper and lower but mention it in the limitations
df_full$upper <- NULL
df_full$lower <- NULL

df_full$rei <- as.factor(df_full$rei)

df_full <- dcast(df_full, Country + year + sex + age ~ rei, value.var = "val")

#consistent column names --------------------------------------------------------------------------------------------
names(df_full)[names(df_full) == 'sex'] <- "Sex"
names(df_full)[names(df_full) == 'year'] <- "Year"
names(df_full)[names(df_full) == 'age'] <- "Age"

names(avg_income)[names(avg_income) == 'country'] <- "Country"
names(avg_income)[names(avg_income) == 'year'] <- "Year"
names(avg_income)[names(avg_income) == 'income'] <- "Income"

names(avg_years_school)[names(avg_years_school) == 'gender'] <- "Sex"

avg_years_school$Sex <- sapply(avg_years_school$Sex, function(x) {
  paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "")
})

#full final df ----------------------------------------------------------------------------------------------------
merged_df <- merge(worldbank_data, edu_capita_exp, by = c("Year", "Country"))
merged_df <- merge(merged_df, avg_income, by = c("Year", "Country"))



bp_data <- merge(df_full, merged_df, by = c("Year", "Country"), all = TRUE)
bp_data <- merge(bp_data, avg_years_school, by = c("Year", "Country", "Sex"), all = TRUE)

#data cleaning ----------------------------------------------------------------------------------------------------
#first for some of the world bank data - there are some variables that have .. instead of NA - need to deal with this
bp_data$Access_Clean_Fuels <- NULL
bp_data$Access_Electricity <- NULL
bp_data$Energy_Invst_Private_Participation <- NULL
bp_data$Partner_Energy_Invst <- NULL


# Specify the columns to check for NA values
columns_to_check <- c("Edu_Expenditure", "Income", "Mean Years of Schooling")

# Check for NA values in the specified columns
na_mask <- is.na(bp_data[columns_to_check])

# Count the number of rows where any of the specified columns have NA values
num_observations_with_na <- sum(apply(na_mask, 1, all))

#remove all of these NAs

bp_data <- bp_data[!apply(na_mask, 1, any), ]

#remove all underage observations as they tend to have more than others
bp_data <- subset(bp_data, !Age %in% c("<5 years", "5-9 years", "5-14 years", "10-14 years", "15-19 years", "20-24 years", "All ages"))

missing_values_df <- bp_data[!complete.cases(bp_data), ]

#Change the column names to be more concise
names(bp_data)[names(bp_data) == 'Ambient particulate matter pollution'] <- "APM"
names(bp_data)[names(bp_data) == 'Particulate matter pollution'] <- "PM"
names(bp_data)[names(bp_data) == 'Cardiovascular diseases'] <- "CVD"
names(bp_data)[names(bp_data) == 'Alcohol use'] <- "ALC"
names(bp_data)[names(bp_data) == 'Dietary risks'] <- "DR"
names(bp_data)[names(bp_data) == 'Drug use'] <- "DU"
names(bp_data)[names(bp_data) == 'High body-mass index'] <- "BMI"
names(bp_data)[names(bp_data) == 'High LDL cholesterol'] <- "CHOL"
names(bp_data)[names(bp_data) == 'High systolic blood pressure'] <- "BP"
names(bp_data)[names(bp_data) == 'Household air pollution from solid fuels'] <- "HAP"
names(bp_data)[names(bp_data) == 'Lead exposure'] <- "LE"
names(bp_data)[names(bp_data) == 'Residential radon'] <- "RR"
names(bp_data)[names(bp_data) == 'Edu_Expenditure'] <- "EDU"
names(bp_data)[names(bp_data) == 'Mean Years of Schooling'] <- "SCH"
names(bp_data)[names(bp_data) == 'Tobacco'] <- "TBC"
names(bp_data)[names(bp_data) == 'Income'] <- "INC"
names(bp_data)[names(bp_data) == 'Occupational particulate matter, gases, and fumes'] <- "OPM"


#exporting or inserting the complete dataset ----------------------------------------------------------------------------------------------------
write.csv(bp_data, "bp_data.csv", row.names = FALSE)
bp_data <- read.csv("bp_data.csv", header = TRUE)

getwd()












#prepare the data for the panel regression ----------------------------------------------------------------------------------------------------panel_data <- bp_data
panel_data <- bp_data

panel_data <- ts(panel_data)

#panel_data$AgeSex <- paste(panel_data$Age, panel_data$Sex, sep = "_")
#panel_data$Age <- NULL
#panel_data$Sex <- NULL

# Convert your data frame to a pdata.frame with the new composite index
panel_data <- pdata.frame(panel_data, index = c("Country", "Year", "Age"))
panel_data$Country <- NULL
panel_data$Year <- NULL
panel_data$Age <- NULL


#panel regression code ----------------------------------------------------------------------------------------------------
#DV - CVD
#IVs - HAP
#Moderating - EDU, INC, SCH
#Control - APM, PM, ALC, DR, DU, BMI, CHOL, BP, LE, RR, TBC, OPM


#Check there are time fixed effects ----------------------------------------------------------------------------------
fe_model_individual <- plm(CVD ~ factor(Sex) + HAP * EDU + HAP * INC + HAP * SCH + 
                             APM + PM + ALC + DR + DU + BMI + CHOL + BP + LE + RR + TBC + OPM, 
                           data = panel_data, 
                           model = "within", effect = "individual")

summary(fe_model_individual)

fe_model_twoways <- plm(CVD ~ factor(Sex) + HAP * EDU + HAP * INC + HAP * SCH + 
                  APM + PM + ALC + DR + DU + BMI + CHOL + BP + LE + RR + TBC + OPM, 
                data = panel_data, 
                model = "within", effect = "twoways")


summary(fe_model_twoways)

pFtest(fe_model_twoways, fe_model_individual)
#we reject the NULL hypothesis and recognize that there are time fixed effects within the model that we should include

#Check for random or fixed effects within the model --------------------------------------------------------
re_model_twoways <- plm(CVD ~ factor(Sex) + HAP * EDU + HAP * INC + HAP * SCH + 
                          APM + PM + ALC + DR + DU + BMI + CHOL + BP + LE + RR + TBC + OPM, 
                        data = panel_data, 
                        model = "random", effect = "individual")



# Compute correlation matrix for all variables in your dataset
cor_matrix <- cor(panel_data)

# Generate heatmap visualization
heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100), # Define color palette
        symm = TRUE, # Ensure symmetry in the heatmap
        margins = c(5, 10), # Set margins for the heatmap
        main = "Correlation Heatmap" # Add a main title
)



phtest(fe_model_twoways, re_model_twoways)

#check entity and time effects ------------------------------------------------------------------------------------------------------------------
# Create a vector of unique countries and assign them to four groups
avg_hap_data <- bp_data %>%
  dplyr::group_by(Country, Year) %>%
  dplyr::summarize(Average_HAP = mean(HAP, na.rm = TRUE))

unique_countries <- unique(avg_hap_data$Country)
n <- length(unique_countries)
group_labels <- rep(1:4, length.out = n)

# Create a data frame to map countries to groups
country_groups <- data.frame(Country = unique_countries, Group = paste("Group", group_labels))

# Merge this mapping back to the original data
avg_hap_data <- merge(avg_hap_data, country_groups, by = "Country")

# Create the ggplot
ggplot(data = avg_hap_data, aes(x = Year, y = Average_HAP, colour = as.factor(Country), group = Country)) + 
  geom_line() +  # Adjust line size for better visibility
  labs(x = "Year", y = "Average_HAP", colour = "Country") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove the legend to focus on the plots
    axis.text = element_text(size = 10),  # Adjust axis text size to a readable value
    axis.title = element_text(size = 12)  # Adjust axis title text size to a readable value
  ) +
  facet_wrap(~ Group)  # Create separate plots for each group

#based on the plot indeed we should use fixed effects

#check entity effects
HAPplot_years <- ggplot(data = panel_data, aes(x = as.factor(Year), y = HAP)) + geom_line() + 
  labs(x = "Years", y = "HAP", colour = "Country")

HAPplot_years


ggplot(data = panel_data, aes(x = as.factor(Year), y = HAP)) + 
  geom_point() + 
  geom_line(aes(x = as.factor(Year), y = HAP, group = 1), col = "red") +
  labs(x = "Country", y = "HAP") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1),  # Adjust x-axis text size and angle
        axis.text.y = element_text(size = 10),  # Adjust y-axis text size
        axis.title = element_text(size = 12),  # Adjust axis title text size
        plot.title = element_text(size = 14, face = "bold")) 

# Plot for HAP for every country in our data
plot2 <- ggplot() +
  # Plot individual black data points for each country
  geom_point(data = panel_data, aes(x = Country, y = HAP), color = "black") +
  # Plot annual means and connect them with red line
  geom_line(data = avg_hap_data, aes(x = Country, y = HAP, group = 1), color = "red", size = 1) +
  labs(title = "Health-Adjusted Life Expectancy (HAP) by Country",
       x = "Country",
       y = "HAP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

print(plot2)

#check for multicolinearity ----------------------------------------------------------------------------------------------------------
#Fit a pooled OLS model
pooled_model <- plm(CVD ~ HAP * EDU + HAP * INC + HAP * SCH + 
                      APM + PM + ALC + DR + DU + BMI + CHOL + BP + LE + RR + TBC + OPM + factor(Sex) + factor(Age), 
                    data = bp_data, 
                    model = "pooling")
vif_values <- vif(pooled_model)
print(vif_values)

pooled_model <- plm(CVD ~ HAP * EDU + HAP * INC + HAP * SCH + 
                      APM + ALC + DR + DU + BMI + CHOL + BP + LE + RR + TBC + OPM + factor(Sex), 
                    data = bp_data, 
                    model = "pooling")
vif_values <- vif(pooled_model)
print(vif_values)


#We conclude we can take out PM of our model and include Age in the Index

#new model
fe_model <- plm(CVD ~ factor(Sex) + HAP * EDU + HAP * INC + HAP * SCH + 
                          APM + ALC + DR + DU + BMI + CHOL + BP + LE + RR + TBC + OPM, 
                        data = panel_data, 
                        model = "within", effect = "twoways")


summary(fe_model)


#check for homoskedasticity and autocorrelation issues ----------------------------------------------------------------------------------------------------------
bptest(fe_model)
# p-value < 2.2e-16 < 0.05: the null hypothesis that the variance of the error
# term is constant is rejected. There is heteroskedasticity

bg_test <- pbgtest(fe_model_twoways, order = 1)  # Test for first-order autocorrelation
print(bg_test)

residuals_fe <- residuals(fe_model)
fitted_values <- fitted(fe_model)

# Correcting for heteroskedasticity with robust standard errors
fe_robust <- coeftest(fe_model, vcov = vcovHC(fe_model, type = "HAC"))
print(fe_robust)

#Both issues have now been dealt with 

#this is just to visualize
stargazer(fe_model, fe_robust,
          type = "text",
          column.labels = c("Standard Errors", "Robust Standard Errors"),
          covariate.labels = c("HAP", "EDU", "INC", "SCH", "APM", "ALC", "DR", "DU", "BMI", "CHOL", "BP", "LE", "RR", "TBC", "OPM", "HAP:EDU", "HAP:INC", "HAP:SCH"),
          keep = c("HAP", "EDU", "INC", "SCH", "APM", "ALC", "DR", "DU", "BMI", "CHOL", "BP", "LE", "RR", "TBC", "OPM", "HAP:EDU", "HAP:INC", "HAP:SCH"),
          omit.stat = c("rsq", "f"),
          title = "Comparison of Standard Errors")



#check for the normality assumption ----------------------------------------------------------------------------------------------------------
#Extract residuals
residuals_fe <- residuals(fe_model)

#Plot Q-Q plot
qqnorm(residuals_fe)
qqline(residuals_fe, col = "red")

#Histogram of residuals
hist(residuals_fe, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

#Jarque bera test for normality - models with a lot of residuals
jarque.bera.test(residuals_fe)


#check for the exogeneity assumption ----------------------------------------------------------------------------------------------------------
# Instrumental variable analysis
# Estimate instrumental variable regression model
iv_model <- ivreg(CVD ~ HAP * EDU + HAP * INC + HAP * SCH + 
                    APM + ALC + DR + DU + BMI + CHOL + BP + LE + RR + TBC + OPM + factor(Sex), data = panel_data)

summary(iv_model)

jtest(fe_robust)

checkresiduals(fe_model)


iv_model <- ivreg(y ~ x1 + x2 | z1 + z2, data = data)
summary(iv_model)

# Test for instrument relevance
weakIVtest(iv_model)

# Test for instrument validity
estfun(iv_model)

#random plots
ggplot(data = bp_data, aes(x = BMI, y = CVD)) +
  geom_point() +  # Add points for individual data points
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear trend line
  labs(x = "Body Mass Index (BMI)", y = "Cardiovascular Disease (CVD)") +  # Add axis labels
  theme_minimal()  # Apply a minimal theme for better readability





