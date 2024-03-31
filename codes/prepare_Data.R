###Preparing Data:
# Load_Packages -----------------------------------------------------------
library("pacman")
library("dplyr")
library("ggplot2")
library("readr")
library("tidyr")
library("stringr")
library("Hmisc")
library("expss")
library("plm")
library("modelsummary")
library("sandwich")
library("eurostat")
library(corrplot)
library("fixest")
library("modelsummary")
library("rnaturalearth")
library("sf")

#co2_emission ---------------------------------------------------------------------

#Load_Co2
co2_emission <- read_csv("Co2.csv", na="..",n_max = 217)

# reshape wide to long
co2_emission<-co2_emission %>% 
  pivot_longer(cols=3:65,names_to = "Years", values_to = "co2_emission") %>% 
  mutate(Years=str_sub(Years, 1,4)) %>%
  apply_labels(co2_emission="co2_emission (metric tons per capita)") %>% 
  rename(country_name="Country Name",
         country_code="Country Code")

# GDP ---------------------------------------------------------------
#load _GDP
gdp <- read_csv("GDP_percapita.csv", na="..",n_max = 217)

#reshape_wide_to_long
gdp<-gdp %>% 
  pivot_longer(cols=3:65,names_to = "Years", values_to = "gdp_percapita") %>% 
  mutate(Years=str_sub(Years, 1,4)) %>%
  apply_labels(gdp_percapita="GDP per capita (current US$)")%>% 
  rename(country_name="Country Name",
         country_code="Country Code")

# Life_Expectancy ---------------------------------------------------------

#load Life_expectancy

life <- read_csv("life_expectancy.csv", na="..",n_max = 217)

#remove 2 columns of Serious Name & Serious Code
life <- life[, -c(1,2)]

#reshape wide to long
life<-life %>% 
  pivot_longer(cols=3:65,names_to = "Years", values_to = "life") %>% 
  mutate(Years=str_sub(Years, 1,4)) %>%
  apply_labels(life="life_expectancy")%>% 
  rename(country_name="Country Name",
         country_code="Country Code")



# GDP_expenditure (% of GDP) -----------------------------------------

#load life_expenditure_GDP (% of GDP)
GDP_expenditure <- read_csv("Current_health_expenditure_(% of GDP).csv", na="..", n_max = 217)

#reshape wide to long
GDP_expenditure <- GDP_expenditure %>% 
  pivot_longer(cols = 3:65, names_to = "Years", values_to = "expenditure_GDP") %>% 
  mutate(Years = str_sub(Years, 1,4)) %>% 
  rename(country_name = "Country Name",
         country_code = "Country Code")



# Mortality ---------------------------------------------------------------
#load Mortality
mortality <- read_csv("Total_mortality.csv", na="..", n_max = 217)
#remove two columns of Serious Name & Code
mortality<- mortality[ , -c(1,2)]

#reshape wide to long
mortality <- mortality %>% 
  pivot_longer(cols = 3:65, names_to = "Years", values_to = "mortality") %>% 
  mutate(Years = str_sub(Years, 1,4)) %>% 
  rename(country_name = "Country Name",
         country_code = "Country Code")


# Population ---------------------------------------------------------------


#load population_total
population <- read_csv("population.csv", na="..", n_max = 217)

#remove two columns of Serious Name & Code
population<- population[ , -c(1,2)]

#reshape wide to long
population <- population %>% 
  pivot_longer(cols = 3:65, names_to = "Years", values_to = "population") %>% 
  mutate(Years = str_sub(Years, 1,4)) %>% 
  rename(country_name = "Country Name",
         country_code = "Country Code")


#electricity ---------------------------------------------------------------------
electricity<-read_csv("electricity.csv", na="..", n_max= 217)
#remove two columns of Serious Name & Code
electricity<- electricity[ , -c(1,2)]

#reshape wide to long
electricity <- electricity %>%
  pivot_longer(cols = 3:65, names_to = "Years", values_to = "electricity") %>%
  mutate(Years = str_sub(Years, 1,4)) %>%
  rename(country_name = "Country Name",
         country_code = "Country Code")


# merge_data _sets ----------------------------------------------------------

# merge all data set

full_data<- co2_emission %>% 
  left_join(gdp,by=c("country_code", "Years")) %>% 
  left_join(life, by=c("country_code", "Years")) %>%
  left_join(GDP_expenditure, by=c("country_code", "Years"))%>%
  left_join(mortality, by=c("country_code", "Years"))%>%
  left_join(electricity, by=c("country_code", "Years"))%>%
  left_join(population, by=c("country_code", "Years"))

# Clustering_countries ----------------------------------------------------

income <- readxl::read_xlsx(path = "CLASS.xlsx")
names(income) <- str_replace_all(names(income), " ", "_")

income <- income %>% 
  mutate(Income_group = case_when(Code == "VEN" ~ "Upper middle income",
                                  TRUE ~ Income_group))
#checking datatype
sapply(income, class)
income_group <- as.character(full_data$country_code)

# Adding_Income_Group_column -----------------------------------------------
full_data <- left_join(full_data, income[, c("Code", "Income_group")], 
                       by = c("country_code" = "Code"))
write.csv(full_data, file = "full_data.csv", row.names = FALSE)
df <- full_data