setwd("~/Desktop/IBA 3/BP/Data Analysis ")
install.packages("dplyr")
library(dplyr)
install.packages("reshape2")
library(reshape2)
install.packages("tidyverse")
library(tidyverse)

# Combined list of countries from all income groups
all_countries <- unique(c(
  "Afghanistan", "Benin", "Burkina Faso", "Burundi", "Central African Republic",
  "Chad", "Comoros", "Democratic Republic of the Congo", "Eritrea", "Ethiopia",
  "Gambia, The", "Guinea", "Guinea-Bissau", "Haiti", "Democratic People's Republic of Korea",
  "Liberia", "Madagascar", "Malawi", "Mali", "Mozambique",
  "Nepal", "Niger", "Rwanda", "São Tomé and Príncipe", "Senegal",
  "Sierra Leone", "Solomon Islands", "Somalia", "South Sudan", "Sudan",
  "Syrian Arab Republic", "Tajikistan", "Tanzania", "Togo", "Uganda",
  "Yemen, Republic of", "Zambia", "Zimbabwe",
  "Albania", "Algeria", "American Samoa", "Angola", "Argentina",
  "Armenia", "Azerbaijan", "Bangladesh", "Belarus", "Belize",
  "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana",
  "Brazil", "Bulgaria", "Cambodia", "Cameroon", "Cape Verde",
  "China", "Colombia", "Comoros", "Costa Rica", "Côte d'Ivoire",
  "Cuba", "Djibouti", "Dominica", "Dominican Republic", "Ecuador",
  "Egypt, Arab Rep.", "El Salvador", "Equatorial Guinea", "Eswatini",
  "Fiji", "Gabon", "Georgia", "Ghana", "Grenada",
  "Guatemala", "Guyana", "Honduras", "India", "Indonesia",
  "Iran, Islamic Rep.", "Iraq", "Jamaica", "Jordan", "Kazakhstan",
  "Kenya", "Kiribati", "Kosovo", "Kyrgyz Republic", "Lao PDR",
  "Lebanon", "Lesotho", "Libya", "North Macedonia", "Malaysia",
  "Maldives", "Marshall Islands", "Mauritania", "Mauritius", "Mexico",
  "Micronesia, Fed. Sts.", "Moldova", "Mongolia", "Montenegro", "Morocco",
  "Myanmar", "Namibia", "Nauru", "Nicaragua", "Nigeria",
  "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay",
  "Peru", "Philippines", "Romania", "Russian Federation", "Samoa",
  "Serbia", "Seychelles", "South Africa", "Sri Lanka", "St. Kitts and Nevis",
  "St. Lucia", "St. Vincent and the Grenadines", "Suriname", "Thailand",
  "Timor-Leste", "Tonga", "Tunisia", "Turkey", "Turkmenistan",
  "Tuvalu", "Ukraine", "Uruguay", "Uzbekistan", "Vanuatu",
  "Venezuela, RB", "Vietnam", "West Bank and Gaza"
))

#IHME data -----------------------------------------------------------------------------------------------------------
IHME1 <- read.csv("IHME1.csv", header=TRUE)
IHME2 <- read.csv("IHME2.csv", header=TRUE)
IHME3 <- read.csv("IHME3.csv", header=TRUE)
IHME4 <- read.csv("IHME4.csv", header=TRUE)
IHME5 <- read.csv("IHME5.csv", header=TRUE)

df_full <- rbind(IHME1, IHME2)
df_full <- rbind(df_full, IHME3)
df_full <- rbind(df_full, IHME4)
df_full <- rbind(df_full, IHME5)

#filter for developing countries
df_full <- df_full[df_full$location %in% all_countries, ]

#average income data ------------------------------------------------------------------------------------------------
avg_income_raw <- read.csv("avg_income.csv", header=TRUE)
#filter for developing countries
avg_income <- avg_income_raw[avg_income_raw$country %in% all_countries, ]
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

#filter for developing countries
#filter for developing countries
avg_years_school <- avg_years_school[avg_years_school$Country %in% all_countries, ]

#education gov expenditure per capita -------------------------------------------------------------------------------
gov_edu_expenditure <- read.csv("gov_exp_edu.csv", header=TRUE)
#in real US dollars btw, and in millions
 
total_expenditure <- aggregate(Value ~ Country + Time, data = gov_edu_expenditure, FUN = sum)
names(total_expenditure)[names(total_expenditure) == 'Value'] <- "Edu_Expenditure"
names(total_expenditure)[names(total_expenditure) == 'Time'] <- "Year"


total_expenditure$Edu_Expenditure <- total_expenditure$Edu_Expenditure*1000000
#now it is the real values (not in millions anymore)

#also need to filter for only developing countries
total_expenditure <- total_expenditure[total_expenditure$Country %in% all_countries, ]

#need to import population database
pop <- read.csv("pop_size.csv", header=TRUE)

#filter for developing countries
pop <- pop[pop$country %in% all_countries, ]
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
worldbank_data <- read.csv("worldbank_data.csv", header=TRUE)
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

#remove non-LMIC countries
worldbank_data <- worldbank_data[worldbank_data$Country %in% all_countries, ]

#format IHME data ------------------------------------------------------------------------------------------------
unique(df_full$rei)

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

merged_df$Country %in% all_countries



df_final <- merge(df_full, merged_df, by = c("Year", "Country"), all = TRUE)
df_final <- merge(df_final, avg_years_school, by = c("Year", "Country", "Sex"), all = TRUE)

library(readr)
bachelorproject_data <- read_csv("bachelorproject_data.csv")
View(bachelorproject_data)

na.indices <- sapply(bachelorproject_data[,5], is.na)
bachelorproject_data[na.indices,c("Alcohol use")] <- 0

na.indices2 <- sapply(bachelorproject_data[,9], is.na)
bachelorproject_data[na.indices2,c("Drug use")] <- 0


