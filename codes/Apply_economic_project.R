
library("tidyverse")
library("fixest")
library("modelsummary")
df <- read.csv("full_data.csv")

df <- na.omit(df[, c("country_name", "country_code", "Years",
                                   "co2_emission", "gdp_percapita", "life","expenditure_GDP",
                                   "electricity","population","mortality","Income_group"
)])

correlation_matrix <- cor(df[, c("co2_emission", "gdp_percapita", "electricity", "population", "mortality", "expenditure_GDP")])
# # Plotting Correlation Heat map
heatmap(correlation_matrix)
# graphs etc --------------------------------------------------------------
#summary(df)

nas <- sapply(df, function(s) {
  sum(is.na(s))
})

test <- df %>% 
  group_by(country_name) %>% 
  summarise(
    avg_life = mean(life, na.rm = TRUE),
    sd_life = sd(life, na.rm = TRUE),
    avg_mort = mean(life, na.rm = TRUE), 
    sd_mort = sd(life, na.rm = TRUE)     
  )

#graph plot of life and CO2_relation
ggplot()+
  geom_point(data = df , 
             aes(color = "country_code", y = (life)
                            , x = co2_emission))
# showing specific countries in the world map
# world <- rnaturalearth::ne_countries() |>
#   st_as_sf()
# ggplot()+
#   geom_sf(data = world, fill = "green", color = "darkgreen")
#   geom_sf(data = world |>
#             filter(adm0_a3 %in% c("DEU", "IRN", "ISL", "IND", "AUS", "AUT")),
#           fill = "purple", color = "violet")
# call package of world ---------------------------------------------------
world <- rnaturalearth::ne_countries()
ggplot()+
  geom_sf(data = world, fill = NA)+
  geom_sf(data = world %>% 
            dplyr::filter(adm0_a3 %in% (df %>% dplyr::filter(co2_emission >= 11.7) %>% dplyr::pull(country_code))), 
          fill = "yellow", color = "orange")

ggplot()+
  geom_sf(data = world, fill = NA)+
  geom_sf(data = world %>% 
            dplyr::filter(adm0_a3 %in% (df %>% dplyr::filter(co2_emission >= 10.21) %>% dplyr::pull(country_code))), 
          fill = "pink", color = "magenta")+
  labs(title = "World Map with Highlighted Countries has CO2_Emission more than vertex. (9.01)")+
  theme_minimal()


# Equations ---------------------------------------------------------------
# equations <- list()
# equations[[1]] <- life ~ co2_emission  | country_code + as.factor(Years)
# equations[[2]] <- life ~ I(co2_emission^2) + log(gdp_percapita) | country_code + as.factor(Years)
# equations[[3]] <- life ~ I(co2_emission^2) + log(gdp_percapita) + (electricity) | country_code + as.factor(Years)
# equations[[4]] <- life ~ I(co2_emission^2) + log(gdp_percapita) + (electricity) + log(population)  | country_code + as.factor(Years)
# equations[[5]] <- life ~ I(co2_emission^2)+ log(gdp_percapita) + (electricity) + log(population) + expenditure_GDP  | country_code + as.factor(Years)
# equations[[6]] <- life ~ I(co2_emission^2)+ log(gdp_percapita) + (electricity) + log(population) + expenditure_GDP+ mortality| country_code + as.factor(Years)
# equations[[7]] <- life ~ co2_emission + log(gdp_percapita) + electricity + log(population) + expenditure_GDP + mortality + as.factor(Income_group) + I(co2_emission^2) | as.factor(country_code) + as.factor(Years)
# 
# res <- lapply(equations, function(e) fixest::feols(fml = as.formula(e), data = df, panel.id = ~country_code + Years + Income_group))
# modelsummary(res, stars = TRUE)
# 
# # Quadratic_relation_co2&Life in different Income group ---------------------------------------------
# equations[[8]] <- life ~ co2_emission * I(Income_group == "High income") +
#   I(co2_emission^2) * I(Income_group == "High income") + log(gdp_percapita) + electricity +
#   log(population) + expenditure_GDP + mortality + factor(country_code) + factor(Years)
# 
# # Upper middle income group interaction
# equations[[9]] <- life ~ co2_emission * I(Income_group == "Upper middle income") +
#   I(co2_emission^2) * I(Income_group == "Upper middle income") + log(gdp_percapita) + electricity +
#   log(population) + expenditure_GDP + mortality + i(country_code, Years)
# 
# print(equations[9])
# 
# # Fit and summarize the new set of models
# results <- lapply(equations[9], function(e) fixest::feols(fml = as.formula(e), data = df, panel.id = ~country_code + Years + Income_group))
# 
# modelsummary(results, stars = TRUE)

equations1 <- list()
equations1[[1]] <- life ~ co2_emission  | country_code + as.factor(Years)
equations1[[2]] <- life ~ co2_emission + log(gdp_percapita) | country_code + as.factor(Years)
equations1[[3]] <- life ~ co2_emission + log(gdp_percapita) + (electricity) | country_code + as.factor(Years)
equations1[[4]] <- life ~ co2_emission + log(gdp_percapita) + (electricity) + log(population)  | country_code + as.factor(Years)
equations1[[5]] <- life ~ co2_emission + log(gdp_percapita) + (electricity) + log(population) + expenditure_GDP  | country_code + as.factor(Years)
equations1[[6]] <- life ~ co2_emission + log(gdp_percapita) + (electricity) + log(population) + expenditure_GDP+ mortality| country_code + as.factor(Years)

# Quadratic_relation_co2&Life ---------------------------------------------
#equations <- list(
df <- df %>% 
  dplyr::mutate(
    d_highincome = Income_group == "High income",
    d_uppermiddleincome = Income_group == "Upper middle income"
  )
equations2 <- list()
equations2[[1]] <- equations1[[6]]
equations2[[2]] <- life ~ co2_emission+
  I(co2_emission^2) +  
  log(gdp_percapita) + electricity + 
  log(population) + expenditure_GDP + mortality  | as.factor(country_code) + as.factor(Years)
equations2[[3]] <- life ~ co2_emission:d_highincome + co2_emission+
    I(co2_emission^2) + I(co2_emission^2):d_highincome + 
    log(gdp_percapita) + electricity + 
    log(population) + expenditure_GDP + mortality  | as.factor(country_code) + as.factor(Years)
# Quadratic of Upper Middle Income group
equations2[[4]] <- life ~ co2_emission:I(Income_group == "Upper middle income") + co2_emission +
  I(co2_emission^2) + I(co2_emission^2):I(Income_group == "Upper middle income") + 
  log(gdp_percapita) + electricity + 
  log(population) + expenditure_GDP + mortality  | as.factor(country_code) + as.factor(Years)

res1 <- lapply(equations1, function(e) fixest::feols(fml = as.formula(e), data = df, panel.id = ~country_code + Years + Income_group))
res2 <- lapply(equations2, function(e) fixest::feols(fml = as.formula(e), data = df, panel.id = ~country_code + Years + Income_group))

modelsummary(res1, stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1))
modelsummary(res2, stars = TRUE)

#Uniq income groups:
income_names <- unique(df$Income_group)
print(income_names)
# compare independent Variables among quadratic or linear relation ----------------

# Fit linear model
lm_model_linear <- lm( life ~ co2_emission + log(gdp_percapita) + (electricity) + log(population) + expenditure_GDP+ mortality, data = df)

# Fit quadratic model
lm_model_quadratic <- lm(life ~ poly(co2_emission, 2) + poly(log(gdp_percapita), 2) + poly(electricity, 2) +
                           poly(log(population), 2) + poly(expenditure_GDP, 2) + poly(mortality, 2), data = df)


#compare linear and quadratic
anova(lm_model_linear, lm_model_quadratic)

#result of quadratic model
summary(lm_model_quadratic)

#vertex of quadratic model:
numinator <- 0.789-0.607 
print (numinator)

a <- 0.029 + 0.039 
print(a)

denominator <- a * 2 
print(denominator)

vertex = numinator / denominator 
print(vertex)
