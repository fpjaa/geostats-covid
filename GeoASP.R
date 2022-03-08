###----------------------------------###
### Thesis: ASP data + Geostatistics ###
###----------------------------------###

## Load spatial packages

#library(lattice)      ## Data management
#library(geoR)         ## Geostatistics

#### Setup ####

## Clear the workspace
rm(list=ls())

## Functions for graphics 
#v.f <- function(x, ...){100-cov.spatial(x, ...)}
#v.f.est<-function(x,C0, ...){C0-cov.spatial(x, ...)}

#### Load and see data ####

setwd('/home/fpjaa/Documents/GitHub/geostats-covid/eurostat_datasets/')
library(readr)

#import TSV file into data frame

air <- read_tsv('air_passengers.tsv')
air <- air[,c(1,ncol(air))]
colnames(air)[ncol(air)] <- 'air'

avail <- read_tsv('available_hospital_beds_nuts2.tsv')
avail <- avail[,c(1,ncol(avail))]
colnames(avail)[ncol(avail)] <- 'avail'

causes <- read_tsv('causes_of_death_crude_death_rate_3year_average_by_nuts2.tsv')
causes <- causes[,c(1,ncol(causes))]
colnames(causes)[ncol(causes)] <- 'causes'

compens <- read_tsv('compensation_of_employees_by_nuts2.tsv')
compens <- compens[,c(1,ncol(compens))]
colnames(compens)[ncol(compens)] <- 'compens'

death <- read_tsv('deaths.tsv')
death <- death[,c(1,ncol(death))]
colnames(death)[ncol(death)] <- 'death'

early <- read_tsv('early_leavers_from_education_and_training_by_sex_percentage_nuts2.tsv')
early <- early[,c(1,ncol(early))]
colnames(early)[ncol(early)] <- 'early'

employ <- read_tsv('employment_thousand_hours_worked_nuts2.tsv')
employ <- employ[,c(1,ncol(employ))]
colnames(employ)[ncol(employ)] <- 'employ'

farm <- read_tsv('farm_labour_force.tsv')
farm <- farm[,c(1,ncol(farm))]
colnames(farm)[ncol(farm)] <- 'farm'

health <- read_tsv('health_personnel_by_nuts2.tsv')
health <- health[,c(1,ncol(health))]
colnames(health)[ncol(health)] <- 'health'

hosp <- read_tsv('hospital_discharges_resp_diseases_j00_to_j99_nuts2.tsv')
hosp <- hosp[,c(1,ncol(hosp))]
colnames(hosp)[ncol(hosp)] <- 'hosp'

life <- read_tsv('life_expectancy.tsv')
life <- life[,c(1,ncol(life))]
colnames(life)[ncol(life)] <- 'life'

longterm <- read_tsv('longterm_care_beds_per_hundred_thousand_nuts2.tsv')
longterm <- longterm[,c(1,ncol(longterm))]
colnames(longterm)[ncol(longterm)] <- 'longterm'

nama <- read_tsv('nama_10r_2gdp.tsv')
nama <- nama[,c(1,ncol(nama))]
colnames(nama)[ncol(nama)] <- 'nama'

partic <- read_tsv('participation_in_education_and_training.tsv')
partic <- partic[,c(1,ncol(partic))]
colnames(partic)[ncol(partic)] <- 'partic'

popd <- read_tsv('pop_density.tsv')
popd <- popd[,c(1,ncol(popd))]
colnames(popd)[ncol(popd)] <- 'popd'

popul <- read_tsv('population_nuts2.tsv')
popul <- popul[,c(1,ncol(popul))]
colnames(popul)[ncol(popul)] <- 'popul'

pupils <- read_tsv('pupils_and_students_enrolled_by_sex_age_and_nuts2.tsv')
pupils <- pupils[,c(1,ncol(pupils))]
colnames(pupils)[ncol(pupils)] <- 'pupils'

regGVA <- read_tsv('real_growth_rate_of_regional_gross_value_added_GVA_at_basic_prices_by_nuts2.tsv')
regGVA <- regGVA[,c(1,ncol(regGVA))]
colnames(regGVA)[ncol(regGVA)] <- 'regGVA'

stock <- read_tsv('stock_of_vehicles_by_category_and_nuts2.tsv')
stock <- stock[,c(1,ncol(stock))]
colnames(stock)[ncol(stock)] <- 'stock'

students <- read_tsv('students_enrolled_in_tertiary_education_by_education_level_programme_orientation_sex_and_nuts2.tsv')
students <- students[,c(1,ncol(students))]
colnames(students)[ncol(students)] <- 'students'

unempl <- read_tsv('unemployment_rate_nuts2.tsv')
unempl <- unempl[,c(1,ncol(unempl))]
colnames(unempl)[ncol(unempl)] <- 'unempl'

utilized <- read_tsv('utilised_agricultural_area.tsv')
utilized <- utilized[,c(1,ncol(utilized))]
colnames(utilized)[ncol(utilized)] <- 'utilized'

young <- read_tsv('young_people_neither_in_employment_nor_in_education_and_training_by_sex_NEET_RATE_nuts2.tsv')
young <- young[,c(1,ncol(young))]
colnames(young)[ncol(young)] <- 'young'

df_list <- list(air, avail, causes, compens, death, early, employ, farm, health,
                hosp, life, longterm, nama, partic, popd, popul, pupils, regGVA,
                stock, students, unempl, utilized, young)      
#merge all data frames together
means_by_country <- Reduce(function(x, y) merge(x, y, all=TRUE, by='geo'), df_list)

means_by_country$country = substr(means_by_country$geo,1,2)

library(dplyr)
means_by_country <- means_by_country[, -1] %>%
  group_by(country) %>%
  summarise_all("mean", na.rm = TRUE)

setwd('/home/fpjaa/Documents/GitHub/geostats-covid/')

data <- read.csv("ASPdataset.csv", header=TRUE, stringsAsFactors=FALSE)
data <- data[, -c(25, 26, 27, 28)]
data <- data[!is.na(data$cases_density_first_wave),]

#### Fill NAs ----

data$country = substr(data$NUTS,1,2)

missing <- is.na(means_by_country)
missing <- rowSums(missing)
missing <- data.frame(cbind(means_by_country$country, missing))
colnames(missing) <- c('country', 'nans')
missing$nans <- as.numeric(missing$nans)

par(mfrow=c(1,1))
barplot(missing$nans,
        main = "Missing features by country",
        xlab = "Country",
        ylab = "NAs",
        names.arg = missing$country,
        col = "darkred",
        horiz = FALSE)

data1 <- merge(data[,c(1,27)], means_by_country, by = 'country')

for (i in 2:24){
  data[,i] <- ifelse(is.na(data[,i]), data1[,i+1], data[,i])
}

rm(data1, df_list,
   air, avail, causes, compens, death, early, employ, farm, health,
   hosp, life, longterm, nama, partic, popd, popul, pupils, regGVA,
   stock, students, unempl, utilized, young)

means_by_country <- data[, -c(1, 25, 26)] %>%
  group_by(country) %>%
  summarise_all("mean", na.rm = TRUE)

missing <- is.na(means_by_country)
missing <- rowSums(missing)
missing <- data.frame(cbind(means_by_country$country, missing))
colnames(missing) <- c('country', 'nans')
missing$nans <- as.numeric(missing$nans)

par(mfrow=c(1,1))
barplot(missing$nans,
        main = "Missing features by country",
        xlab = "Country",
        ylab = "NAs",
        names.arg = missing$country,
        col = "darkred",
        horiz = FALSE)

#data1 <- merge(data[,c(1,27)], means_by_country, by = 'country')

#for (i in 2:24){ data[,i] <- ifelse(is.na(data[,i]), data1[,i+1], data[,i]) }

#means_by_country <- data[, -c(1, 25, 26)] %>% group_by(country) %>% summarise_all("mean", na.rm = TRUE)

#missing <- is.na(means_by_country)
#missing <- rowSums(missing)
#missing <- data.frame(cbind(means_by_country$country, missing))
#colnames(missing) <- c('country', 'nans')
#missing$nans <- as.numeric(missing$nans)

#par(mfrow=c(1,1))
#barplot(missing$nans, main = "Missing features by country", xlab = "Country", ylab = "NAs", names.arg = missing$country, col = "darkred", horiz = FALSE)

#### Summary ----

#summar <- list(
#  mean = ~mean(.x, na.rm = TRUE),
#  sd = ~sd(.x, na.rm = TRUE),
#  min = ~min(.x, na.rm = TRUE),
#  q25 = ~quantile(.x, .25, na.rm = TRUE),
#  median = ~median(.x, na.rm = TRUE),
#  q75 = ~quantile(.x, .75, na.rm = TRUE),
#  max = ~max(.x, na.rm = TRUE)
#)

#resumen <- data.frame(t(data[,-c(1,25, 26, 27)])) %>% rowwise() %>% 
#  summarise(mean = rowMeans(), sd = sd, min = min, q25 = quantile(., .25), median = median,
#            q75 = quantile(., .75), max = max)

#resumen <- summary(data[,-c(1,25, 26, 27)])

#install.packages('psych')
library(psych) 
#create summary table
resumen <- describe(data[,-c(1,25, 26, 27)])
resumen <- data.frame(t(resumen))
resumen <- cbind(row.names(resumen), resumen)
write_csv(resumen, 'summary.csv')

rm(resumen)

#### Add locations ####

#install.packages('geojsonR')
library(geojsonR)
spdf <- FROM_GeoJson("NUTS_LB_2021_4326.geojson")

locations <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(locations) <- c('latitude', 'longitude', 'NUTS')
for (i in 1:2010){
     lat <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][1]
     lon <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][2]
     nuts <- spdf[["features"]][[i]][["properties"]][["NUTS_ID"]]
     new <- c(lat, lon, nuts)                       # Create new row
     locations[nrow(locations) + 1, ] <- new
}

dataset <- merge(data, locations, by = 'NUTS')

lost <- anti_join(data, locations, by="NUTS")
# FI13 not found -> Candidates FI1C3 FI1D3
## Found in 2006
# FI18 -> FI1D8
## Found in 2006
# FI1A not found
## Found in 2006
# FR23 not found -> Candidates FRC23 FRE23 FRF23 FRI23 FRK23
## Found in 2013
# FR24 not found -> Candidates FRF24 FRI24 FRK24
## Found in 2013
# FR26 not found -> Candidates FRI26 FRK26
## Found in 2013
# FR30 -> FRY30
## Found in 2013
# FR41 not found
## Found in 2013
# FR51 not found
## Found in 2013
# FR52 not found
## Found in 2013
# FR61 not found
## Found in 2013
# FR62 not found
## Found in 2013
# FR71 not found
## Found in 2013
# FR82 not found
## Found in 2013
# FRM0 not found
# Found in 2021 and 2016
# FRO not found -> Candidates FRB0 FRG0 FRH0 FRL0 FRM0
## Found in 2013
# FRY 1-5 not found
## Found in 2021 and 2016
# GRL not found -> Has NaN in case density
# HR04 not found
## Found in 2016 and 2013

spdf <- FROM_GeoJson("NUTS_LB_2013_4326.geojson")

locations <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(locations) <- c('latitude', 'longitude', 'NUTS')
for (i in 1:2010){
  lat <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][1]
  lon <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][2]
  nuts <- spdf[["features"]][[i]][["properties"]][["NUTS_ID"]]
  new <- c(lat, lon, nuts)                       # Create new row
  locations[nrow(locations) + 1, ] <- new
}

fill <- merge(lost, locations, by = 'NUTS')
dataset <- rbind(dataset, fill)

lost <- anti_join(lost, fill, by="NUTS")

spdf <- FROM_GeoJson("NUTS_LB_2006_4326.geojson")

locations <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(locations) <- c('latitude', 'longitude', 'NUTS')
for (i in 1:2010){
  lat <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][1]
  lon <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][2]
  nuts <- spdf[["features"]][[i]][["properties"]][["NUTS_ID"]]
  new <- c(lat, lon, nuts)                       # Create new row
  locations[nrow(locations) + 1, ] <- new
}

fill <- merge(lost, locations, by = 'NUTS')

dataset <- rbind(dataset, fill)

rm(fill, locations, lost, spdf, data, i, lat, lon, new, nuts)

dataset$latitude <- as.numeric(dataset$latitude)
dataset$longitude <- as.numeric(dataset$longitude)


write_csv(dataset, 'dataset.csv')

#### Map plots ----

library(plotly)
library(ggplot2)

g <- list(
  scope = 'europe',
  resolution = 50,
  showland = TRUE,
  landcolor = toRGB("gray85"),
  showframe = TRUE,
  showcountries = T,
  countrycolor = toRGB("gray50")
)

#fig <- plot_ly(type = 'scattergeo', mode = 'markers')
#fig <- fig %>% layout(geo = g)
#fig

fig <- plot_geo(dataset, sizes = c(1, 126))
fig <- fig %>% add_markers(
  x = ~latitude, y = ~longitude, size=~population_nuts2, color = ~cases_density_first_wave, hoverinfo = "text",
  text = ~paste(dataset$NUTS)
)
fig <- fig %>% layout(title = 'Europe regions',
                      geo = g) %>% colorbar(title = "Cases density<br />First wave")
fig

fig <- plot_geo(dataset, sizes = c(1, 126))
fig <- fig %>% add_markers(
  x = ~latitude, y = ~longitude, size=~population_nuts2, color = ~cases_density_second_wave, hoverinfo = "text",
  text = ~paste(dataset$NUTS)
)
fig <- fig %>% layout(title = 'Europe regions',
                      geo = g) %>% colorbar(title = "Cases density<br />Second wave")
fig

#### DHARMa test -----

dataset <- merge(data, locations, by = 'NUTS')

nonsp_mod <- lm(dataset$cases_density_first_wave ~ ., dataset[,2:25], na.action=na.exclude)

#dataset$resid <- resid(nonsp_mod)
#dataset <- dataset %>% drop_na('resid')
#coordinates(dataset) <- c('latitude','longitude')
#bubble(dataset,'resid',do.log=TRUE, key.space='bottom')
## Same down below

#install.packages('DHARMa')
library(DHARMa)
nonsp_mod <- lm(dataset$cases_density_first_wave ~ ., dataset[,2:25])
sims <- simulateResiduals(nonsp_mod)

nonsp_mod <- lm(dataset$cases_density_first_wave ~ ., dataset[,2:25], na.action=na.exclude)
dataset$resid <- resid(nonsp_mod)
dataset <- dataset[!is.na(dataset$resid),]
testSpatialAutocorrelation(sims, x = dataset$latitude,
                           y = dataset$longitude, plot = TRUE)

# Logarithmic scale

dataset <- merge(data, locations, by = 'NUTS')

nonsp_mod_log <- lm(log(dataset$cases_density_first_wave) ~ ., dataset[,2:25], na.action=na.exclude)

nonsp_mod_log <- lm(log(dataset$cases_density_first_wave) ~ ., dataset[,2:25])
sims_log <- simulateResiduals(nonsp_mod_log)

nonsp_mod_log <- lm(log(dataset$cases_density_first_wave) ~ ., dataset[,2:25], na.action=na.exclude)
dataset$resid_log <- resid(nonsp_mod_log)
dataset <- dataset[!is.na(dataset$resid_log),]
testSpatialAutocorrelation(sims_log, x = dataset$latitude,
                           y = dataset$longitude, plot = TRUE)

# Second wave

dataset <- merge(data, locations, by = 'NUTS')

nonsp_mod2 <- lm(dataset$cases_density_second_wave ~ ., dataset[,2:25])
sims2 <- simulateResiduals(nonsp_mod2)

nonsp_mod2 <- lm(dataset$cases_density_second_wave ~ ., dataset[,2:25], na.action=na.exclude)
dataset$resid2 <- resid(nonsp_mod2)
dataset <- dataset[!is.na(dataset$resid2),]
testSpatialAutocorrelation(sims2, x = dataset$latitude,
                           y = dataset$longitude, plot = TRUE)

#### spmMM model -------

dataset <- merge(data, locations, by = 'NUTS')
dataset$latitude <- as.numeric(dataset$latitude)
dataset$longitude <- as.numeric(dataset$longitude)

#install.packages('spaMM')
library(spaMM)
#install.packages('Rcpp')
library(Rcpp)
# fit the model
m_spamm <- fitme(log(cases_density_first_wave) ~ air_passengers +
                         available_hospital_beds_nuts2 +
                         causes_of_death_crude_death_rate_3year_average_by_nuts2 +
                         compensation_of_employees_by_nuts2 +
                         deaths + early_leavers_from_education_and_training_by_sex_percentage_nuts2 +
                         employment_thousand_hours_worked_nuts2 + farm_labour_force +
                         health_personnel_by_nuts2 +
                         hospital_discharges_resp_diseases_j00_to_j99_nuts2 +
                         life_expectancy +
                         longterm_care_beds_per_hundred_thousand_nuts2 + nama_10r_2gdp +
                         participation_in_education_and_training + pop_density +
                         population_nuts2 +
                         pupils_and_students_enrolled_by_sex_age_and_nuts2 +
                         real_growth_rate_of_regional_gross_value_added_GVA_at_basic_prices_by_nuts2 +
                         stock_of_vehicles_by_category_and_nuts2 +
                         students_enrolled_in_tertiary_education_by_education_level_programme_orientation_sex_and_nuts2 +
                         unemployment_rate_nuts2 + utilised_agricultural_area +
                         young_people_neither_in_employment_nor_in_education_and_training_by_sex_NEET_RATE_nuts2 +
                         Matern(1 | latitude + longitude), data = dataset, family = "gaussian")
# this takes a bit of time
# model summary
summary(m_spamm)

dd <- dist(dataset[,c("latitude", "longitude")])
# Get nu and rho from output of model summary
mm <- MaternCorr(dd, nu = 0.0522041050, rho = 0.0000787316)
plot(as.numeric(dd), as.numeric(mm), xlab = "Distance between pairs of location [in m]", ylab = "Estimated correlation")

library(DHARMa)
sims <- simulateResiduals(m_spamm)
plot(sims)

# Second wave

m_spamm <- fitme(cases_density_second_wave ~ air_passengers +
                         available_hospital_beds_nuts2 +
                         causes_of_death_crude_death_rate_3year_average_by_nuts2 +
                         compensation_of_employees_by_nuts2 +
                         deaths + early_leavers_from_education_and_training_by_sex_percentage_nuts2 +
                         employment_thousand_hours_worked_nuts2 + farm_labour_force +
                         health_personnel_by_nuts2 +
                         hospital_discharges_resp_diseases_j00_to_j99_nuts2 +
                         life_expectancy +
                         longterm_care_beds_per_hundred_thousand_nuts2 + nama_10r_2gdp +
                         participation_in_education_and_training + pop_density +
                         population_nuts2 +
                         pupils_and_students_enrolled_by_sex_age_and_nuts2 +
                         real_growth_rate_of_regional_gross_value_added_GVA_at_basic_prices_by_nuts2 +
                         stock_of_vehicles_by_category_and_nuts2 +
                         students_enrolled_in_tertiary_education_by_education_level_programme_orientation_sex_and_nuts2 +
                         unemployment_rate_nuts2 + utilised_agricultural_area +
                         young_people_neither_in_employment_nor_in_education_and_training_by_sex_NEET_RATE_nuts2 +
                         Matern(1 | latitude + longitude), data = dataset, family = "gaussian")
# this takes a bit of time
# model summary
summary(m_spamm)

dd <- dist(dataset[,c("latitude", "longitude")])
# Get nu and rho from output of model summary
mm <- MaternCorr(dd, nu = 2.679804e-02, rho = 7.873381e-05)
plot(as.numeric(dd), as.numeric(mm), xlab = "Distance between pairs of location [in m]", ylab = "Estimated correlation")

library(DHARMa)
sims <- simulateResiduals(m_spamm)
plot(sims)

#### Other tests -----

#install.packages("glmmTMB")
library(glmmTMB)

dataset <- merge(data, locations, by = 'NUTS')
dataset$latitude <- as.numeric(dataset$latitude)
dataset$longitude <- as.numeric(dataset$longitude)

# first we need to create a numeric factor recording the coordinates of the sampled locations
dataset$pos <- numFactor(scale(dataset$latitude), scale(dataset$longitude))
# then create a dummy group factor to be used as a random term
dataset$ID <- factor(rep(1, nrow(dataset)))

# fit the model
m_tmb <- glmmTMB(log(cases_density_first_wave) ~ air_passengers +
                         available_hospital_beds_nuts2 +
                         causes_of_death_crude_death_rate_3year_average_by_nuts2 +
                         compensation_of_employees_by_nuts2 +
                         deaths + early_leavers_from_education_and_training_by_sex_percentage_nuts2 +
                         employment_thousand_hours_worked_nuts2 + farm_labour_force +
                         health_personnel_by_nuts2 +
                         hospital_discharges_resp_diseases_j00_to_j99_nuts2 +
                         life_expectancy +
                         longterm_care_beds_per_hundred_thousand_nuts2 + nama_10r_2gdp +
                         participation_in_education_and_training + pop_density +
                         population_nuts2 +
                         pupils_and_students_enrolled_by_sex_age_and_nuts2 +
                         real_growth_rate_of_regional_gross_value_added_GVA_at_basic_prices_by_nuts2 +
                         stock_of_vehicles_by_category_and_nuts2 +
                         students_enrolled_in_tertiary_education_by_education_level_programme_orientation_sex_and_nuts2 +
                         unemployment_rate_nuts2 + utilised_agricultural_area +
                         young_people_neither_in_employment_nor_in_education_and_training_by_sex_NEET_RATE_nuts2 +
                         mat(pos + 0 | ID), dataset) # take some time to fit
# model summary of fixed effects
summary(m_tmb)

library(DHARMa)
sims <- simulateResiduals(m_tmb)
plot(sims)

# Second wave

m_tmb <- glmmTMB(log(cases_density_second_wave) ~ air_passengers +
                         available_hospital_beds_nuts2 +
                         causes_of_death_crude_death_rate_3year_average_by_nuts2 +
                         compensation_of_employees_by_nuts2 +
                         deaths + early_leavers_from_education_and_training_by_sex_percentage_nuts2 +
                         employment_thousand_hours_worked_nuts2 + farm_labour_force +
                         health_personnel_by_nuts2 +
                         hospital_discharges_resp_diseases_j00_to_j99_nuts2 +
                         life_expectancy +
                         longterm_care_beds_per_hundred_thousand_nuts2 + nama_10r_2gdp +
                         participation_in_education_and_training + pop_density +
                         population_nuts2 +
                         pupils_and_students_enrolled_by_sex_age_and_nuts2 +
                         real_growth_rate_of_regional_gross_value_added_GVA_at_basic_prices_by_nuts2 +
                         stock_of_vehicles_by_category_and_nuts2 +
                         students_enrolled_in_tertiary_education_by_education_level_programme_orientation_sex_and_nuts2 +
                         unemployment_rate_nuts2 + utilised_agricultural_area +
                         young_people_neither_in_employment_nor_in_education_and_training_by_sex_NEET_RATE_nuts2 +
                         mat(pos + 0 | ID), dataset) # take some time to fit
# model summary of fixed effects
summary(m_tmb)

library(DHARMa)
sims <- simulateResiduals(m_tmb)
plot(sims)

#### Transformation of response ----

# Box-Cox requires x>0, we need a transform for [0,1[
# Ln, Sqrt, ()^-1 are not viable either
# arcsine transformation arcsin(srqt(y)) has output still in 0,1
# Hellinger standardization sqrt(x / sum) -> In our case it'd lower the values

# ln transform adding 1 -> monotonic >0
# power transform x^(1/lambda) or 0 -> >=0 (note lambda >1 is root function, prefer lower values)
# logit transform log(p/(1-p))
## qlogis(x) -> stats package
## logit(pp) -> rgr package
## logit(p, percents=T, adjust) -> car package, p=0 or 1, logit is undefined, can remap
## Works with 0 and won't crash with 1
# Z transform (x-mean)/std -> output N(0,1)

par(mfrow=c(2,3))
# histogram
hist(jointdataset$cases_density_first_wave, breaks=16, col="grey", main='Histogram of wave 1 cases', prob = TRUE, xlab = 'Cases density')
# highly skewed, transform to the log
hist(log10(jointdataset$cases_density_first_wave+1), breaks=16, col="grey", main='Histogram of log(W1_cases+1)', prob = TRUE, xlab = 'log(W1_cases)')
# power transform
hist(sqrt(jointdataset$cases_density_first_wave), breaks=16, col="grey", main='Histogram of sqrt(W1_cases)', prob = TRUE, xlab = 'sqrt(W1_cases)')
# logit transform
hist(qlogis(jointdataset$cases_density_first_wave), breaks=16, col="grey", main='Histogram of logit(W1_cases)', prob = TRUE, xlab = 'logit(W1_cases)')
# Z transform
w1.z <- (jointdataset$cases_density_first_wave - mean(jointdataset$cases_density_first_wave))/sd(jointdataset$cases_density_first_wave)
hist(w1.z, breaks=16, col="grey", main='Histogram of Z(W1_cases)', prob = TRUE, xlab = 'Z(W1_cases)')
# power transform
hist((jointdataset$cases_density_first_wave)^(1/3), breaks=16, col="grey", main='Histogram of (W1_cases)^1/3', prob = TRUE, xlab = '(W1_cases)^1/3')

par(mfrow=c(2,3))
# histogram
hist(jointdataset$cases_density_second_wave, breaks=16, col="grey", main='Histogram of wave 2 cases', prob = TRUE, xlab = 'Cases density')
# highly skewed, transform to the log
hist(log10(jointdataset$cases_density_second_wave+1), breaks=16, col="grey", main='Histogram of log(W2_cases+1)', prob = TRUE, xlab = 'log(W2_cases)')
# power transform
hist(sqrt(jointdataset$cases_density_second_wave), breaks=16, col="grey", main='Histogram of sqrt(W2_cases)', prob = TRUE, xlab = 'sqrt(W2_cases)')
# logit transform
hist(qlogis(jointdataset$cases_density_second_wave), breaks=16, col="grey", main='Histogram of logit(W2_cases)', prob = TRUE, xlab = 'logit(W2_cases)')
# Z transform
w1.z <- (jointdataset$cases_density_second_wave - mean(jointdataset$cases_density_second_wave))/sd(jointdataset$cases_density_second_wave)
hist(w1.z, breaks=16, col="grey", main='Histogram of Z(W2_cases)', prob = TRUE, xlab = 'Z(W2_cases)')
# power transform
hist((jointdataset$cases_density_second_wave)^(1/3), breaks=16, col="grey", main='Histogram of (W2_cases)^1/3', prob = TRUE, xlab = '(W2_cases)^1/3')


# https://chrischizinski.github.io/SNR_R_Group/2016-08-10-Data-Transformations


# Instead of transforming, could do logistic regression (with LASSO)
## diagnostics for logistic regression are different from OLS regression
## we'll work with the residuals so it can be problematic

#### LASSO ----

#install.packages('glmnet')
library(glmnet)

coeff2dt <- function(fitobject, s) {
  coeffs <- coef(fitobject, s) 
  coeffs.dt <- data.frame(name = coeffs@Dimnames[[1]][coeffs@i + 1], coefficient = coeffs@x) 
  
  # reorder the variables in term of coefficients
  return(coeffs.dt[order(coeffs.dt$coefficient, decreasing = T),])
}

#### Wave 1 ----

dataset$response1 <- qlogis(dataset$cases_density_first_wave)
#qlogis(dataset$cases_density_first_wave)
## intercept is largest, 9 features
#sqrt(dataset$cases_density_first_wave)
##worse than logit
#log10(dataset$cases_density_first_wave+1)
##all betas 0
#(dataset$cases_density_first_wave - mean(dataset$cases_density_first_wave))/sd(dataset$cases_density_first_wave)
##best so far but intercept is largest
#(dataset$cases_density_first_wave)^(1/3)
##same as sqrt

# Build the matrix of predictors
options(na.action='na.omit')
#options(na.action='na.pass')
x <- model.matrix(response1 ~ air_passengers +
                    available_hospital_beds_nuts2 +
                    causes_of_death_crude_death_rate_3year_average_by_nuts2 +
                    compensation_of_employees_by_nuts2 +
                    deaths + early_leavers_from_education_and_training_by_sex_percentage_nuts2 +
                    employment_thousand_hours_worked_nuts2 + farm_labour_force +
                    health_personnel_by_nuts2 +
                    hospital_discharges_resp_diseases_j00_to_j99_nuts2 +
                    life_expectancy +
                    longterm_care_beds_per_hundred_thousand_nuts2 + nama_10r_2gdp +
                    participation_in_education_and_training +
                    pop_density +
                    population_nuts2 +
                    pupils_and_students_enrolled_by_sex_age_and_nuts2 +
                    real_growth_rate_of_regional_gross_value_added_GVA_at_basic_prices_by_nuts2 +
                    stock_of_vehicles_by_category_and_nuts2 +
                    students_enrolled_in_tertiary_education_by_education_level_programme_orientation_sex_and_nuts2 +
                    unemployment_rate_nuts2 + utilised_agricultural_area +
                    young_people_neither_in_employment_nor_in_education_and_training_by_sex_NEET_RATE_nuts2, data=dataset)[,-1]
# Build the vector of response
y <- dataset[row.names(x),]$response1

boxplot(y)

# Let's set a grid of candidate lambda's for the estimate
lambda.grid <- 10^seq(5,-3,length=100)
fit.lasso <- glmnet(x,y, lambda = lambda.grid) # default: alpha=1 -> lasso

par(mfrow=c(1,1))
plot(fit.lasso, xvar='lambda', label=TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=0.5)

# Let's set lambda via cross validation
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) # default: 10-fold CV

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

plot(cv.lasso)

coeffs.table <- coeff2dt(fitobject = cv.lasso, s = 'lambda.min')
barplot(coeffs.table$coefficient, col = rainbow(dim(x)[2]))
legend('bottomleft', coeffs.table$name, col =  rainbow(dim(x)[2]), lty=1, cex=0.6)

rm(x, y, fit.lasso, cv.lasso, bestlam.lasso, lambda.grid)

##### Wave 1 residuals -----------------

#library(MASS)
#library(car)

# Assumption: Eps ~ N(0, sigma^2)

# Logit transform
fm1 <- lm(response1 ~ air_passengers +
                 available_hospital_beds_nuts2 +
                 causes_of_death_crude_death_rate_3year_average_by_nuts2 +
                 early_leavers_from_education_and_training_by_sex_percentage_nuts2 +
                 farm_labour_force +
                 hospital_discharges_resp_diseases_j00_to_j99_nuts2 +
                 life_expectancy +
                 longterm_care_beds_per_hundred_thousand_nuts2 +
                 pop_density +
                 pupils_and_students_enrolled_by_sex_age_and_nuts2 +
                 real_growth_rate_of_regional_gross_value_added_GVA_at_basic_prices_by_nuts2 +
                 stock_of_vehicles_by_category_and_nuts2 +
                 unemployment_rate_nuts2 +
              young_people_neither_in_employment_nor_in_education_and_training_by_sex_NEET_RATE_nuts2, data=dataset)

# Z transform
'''
fm1 <- lm(response1 ~ available_hospital_beds_nuts2 +
            causes_of_death_crude_death_rate_3year_average_by_nuts2 +
            compensation_of_employees_by_nuts2 +
            early_leavers_from_education_and_training_by_sex_percentage_nuts2 +
            farm_labour_force +
            hospital_discharges_resp_diseases_j00_to_j99_nuts2 +
            life_expectancy +
            longterm_care_beds_per_hundred_thousand_nuts2 +
            pupils_and_students_enrolled_by_sex_age_and_nuts2 +
            real_growth_rate_of_regional_gross_value_added_GVA_at_basic_prices_by_nuts2 +
            stock_of_vehicles_by_category_and_nuts2 +
            unemployment_rate_nuts2 + 
            utilised_agricultural_area +
            young_people_neither_in_employment_nor_in_education_and_training_by_sex_NEET_RATE_nuts2, data=dataset)
'''

par(mfrow=c(2,2))
plot(fm1)

shapiro.test(residuals(fm1))

library(gstat)  ## Geostatistics
library(sp)  ##Data management
resid_w1 <- data.frame(residuals(fm1))
resid_w1 <- merge(dataset[row.names(resid_w1), c('latitude', 'longitude')], resid_w1, by=0)
resid_w1$latitude <- as.numeric(resid_w1$latitude)
resid_w1$longitude <- as.numeric(resid_w1$longitude)
resid_w1$residuals.fm1. <- as.numeric(resid_w1$residuals.fm1.)
resid_w1$Row.names <- as.numeric(resid_w1$Row.names)
coordinates(resid_w1) <- c('latitude','longitude')

# sample variogram (binned estimator)
samp_vgm <- variogram(residuals.fm1. ~ 1, data=resid_w1)
plot(samp_vgm, main = 'Sample Variogram', pch=19)
# "~ 1" stands for a single constant predictor (hp: spatially constant mean)
# automatically decides to ignore direction: point pairs are merged on the
# basis of distance to compute the empirical variogram

# residual variogram w.r.t. a linear trend:
res_vgm <- variogram(residuals.fm1.~latitude+longitude, data=resid_w1)
plot(res_vgm, main = 'Residual Variogram', pch=19)
# automatically decides to ignore direction: point pairs are merged on the
# basis of distance to compute the empirical variogram

# default decisions: direction dependence, cutoff, lag width

plot(variogram(residuals.fm1. ~ 1, data=resid_w1,
               alpha = c(0, 45, 90, 135)), pch=19, main = 'Sample Variogram')
# point pairs whose separation vector has a given direction are used in each
# panel (not too many directions otherwise noise will increase)
# Note: zonal anisotropy

plot(variogram(residuals.fm1. ~ latitude+longitude, data=resid_w1,
               alpha = c(0, 45, 90, 135)), pch=19, main = 'Residual Variogram')
# point pairs whose separation vector has a given direction are used in each
# panel (not too many directions otherwise noise will increase)
# Note: zonal anisotropy

# cutoff distance: maximum distance up to which point pairs are considered
#                  (default = bbox diagonal / 3)
# lag width: width of distance intervals over which point pairs are averaged
#            in bins (default = cutoff distance / 15)

coff <- 20

plot(variogram(residuals.fm1. ~ 1, data=resid_w1,
               cutoff = coff, width = coff/15), pch=19, main = 'Sample Variogram (cutoff=20)')

plot(variogram(residuals.fm1. ~ latitude+longitude, data=resid_w1,
               cutoff = coff, width = coff/15), pch=19, main = 'Residual Variogram (cutoff=20)')

rm(fm1, coeffs.table, samp_vgm, res_vgm)

#### Variogram modeling ----

v <- variogram(residuals.fm1. ~ 1, data=resid_w1,
               cutoff = coff, width = coff/15)

## weighted least squares fitting a variogram model to the sample variogram
## STEPS:
## 1) choose a suitable model
plot(v,pch=19)
vgm()
## 2) choose suitable initial values for partial sill, range & nugget
v.fit1 <- fit.variogram(v, vgm(1, "Exp", 5, 5))
v.fit2 <- fit.variogram(v, vgm(1, "Exc", 5, 5))
v.fit3 <- fit.variogram(v, vgm(1, "Bes", 5, 5))

## 3) fit the model using one of the possible fitting criteria

plot(v, v.fit1, pch = 19, main="Exponential model")
plot(v, v.fit2, pch = 19, main="Exclass model")
plot(v, v.fit3, pch = 19, main="Bessel model")

rm(resid_w1, v, v.fit1, v.fit2, v.fit3, coff)

#### Wave 2 ----

dataset$response2 <- (dataset$cases_density_second_wave - mean(dataset$cases_density_second_wave))/sd(dataset$cases_density_second_wave)
#qlogis(dataset$cases_density_second_wave)
## nice
#sqrt(dataset$cases_density_second_wave)
## still ok
#log10(dataset$cases_density_second_wave+1)
## worse
#(dataset$cases_density_second_wave - mean(dataset$cases_density_second_wave))/sd(dataset$cases_density_second_wave)
## nice
#(dataset$cases_density_second_wave)^(1/3)
## ok

# Build the matrix of predictors
options(na.action='na.omit')
#options(na.action='na.pass')
x <- model.matrix(response2 ~ air_passengers +
                    available_hospital_beds_nuts2 +
                    causes_of_death_crude_death_rate_3year_average_by_nuts2 +
                    compensation_of_employees_by_nuts2 +
                    deaths + early_leavers_from_education_and_training_by_sex_percentage_nuts2 +
                    employment_thousand_hours_worked_nuts2 + farm_labour_force +
                    health_personnel_by_nuts2 +
                    hospital_discharges_resp_diseases_j00_to_j99_nuts2 +
                    life_expectancy +
                    longterm_care_beds_per_hundred_thousand_nuts2 + nama_10r_2gdp +
                    participation_in_education_and_training + pop_density +
                    population_nuts2 +
                    pupils_and_students_enrolled_by_sex_age_and_nuts2 +
                    real_growth_rate_of_regional_gross_value_added_GVA_at_basic_prices_by_nuts2 +
                    stock_of_vehicles_by_category_and_nuts2 +
                    students_enrolled_in_tertiary_education_by_education_level_programme_orientation_sex_and_nuts2 +
                    unemployment_rate_nuts2 + utilised_agricultural_area +
                    young_people_neither_in_employment_nor_in_education_and_training_by_sex_NEET_RATE_nuts2, data=dataset)[,-1]
# Build the vector of response
y <- dataset[row.names(x),]$response2

par(mfrow = c(1,1))
boxplot(y)

# Let's set a grid of candidate lambda's for the estimate
lambda.grid <- 10^seq(5,-3,length=100)
fit.lasso <- glmnet(x,y, lambda = lambda.grid) # default: alpha=1 -> lasso

par(mfrow=c(1,1))
plot(fit.lasso, xvar='lambda', label=TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=0.5)

# Let's set lambda via cross validation
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) # default: 10-fold CV

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

plot(cv.lasso)

coeffs.table <- coeff2dt(fitobject = cv.lasso, s = 'lambda.min')
barplot(coeffs.table$coefficient, col = rainbow(dim(x)[2]))
legend('bottomleft', coeffs.table$name, col =  rainbow(dim(x)[2]), lty=1, cex=0.6)

rm(x, y, fit.lasso, cv.lasso, bestlam.lasso, lambda.grid)

##### Wave 2 residuals -----------------

#library(MASS)
#library(car)

# Assumption: Eps ~ N(0, sigma^2)

# Logit transform
'''
fm2 <- lm(response2 ~ air_passengers +
            available_hospital_beds_nuts2 +
            causes_of_death_crude_death_rate_3year_average_by_nuts2 +
            compensation_of_employees_by_nuts2 +
            early_leavers_from_education_and_training_by_sex_percentage_nuts2 +
            health_personnel_by_nuts2 +
            life_expectancy +
            longterm_care_beds_per_hundred_thousand_nuts2 +
            participation_in_education_and_training +
            pop_density +
            pupils_and_students_enrolled_by_sex_age_and_nuts2 +
            real_growth_rate_of_regional_gross_value_added_GVA_at_basic_prices_by_nuts2 +
            unemployment_rate_nuts2 +
            utilised_agricultural_area +
            young_people_neither_in_employment_nor_in_education_and_training_by_sex_NEET_RATE_nuts2, data=dataset)
'''

# Z transform
fm2 <- lm(response2 ~ available_hospital_beds_nuts2 +
            causes_of_death_crude_death_rate_3year_average_by_nuts2 +
            compensation_of_employees_by_nuts2 +
            early_leavers_from_education_and_training_by_sex_percentage_nuts2 +
            farm_labour_force +
            health_personnel_by_nuts2 +
            life_expectancy +
            longterm_care_beds_per_hundred_thousand_nuts2 +
            participation_in_education_and_training +
            pop_density +
            pupils_and_students_enrolled_by_sex_age_and_nuts2 +
            real_growth_rate_of_regional_gross_value_added_GVA_at_basic_prices_by_nuts2 +
            students_enrolled_in_tertiary_education_by_education_level_programme_orientation_sex_and_nuts2 +
            unemployment_rate_nuts2 + 
            young_people_neither_in_employment_nor_in_education_and_training_by_sex_NEET_RATE_nuts2, data=dataset)

par(mfrow=c(2,2))
plot(fm2)

shapiro.test(residuals(fm2))

library(gstat)  ## Geostatistics
library(sp)  ##Data management
resid_w2 <- data.frame(residuals(fm2))
resid_w2 <- merge(dataset[row.names(resid_w2), c('latitude', 'longitude')], resid_w2, by=0)
resid_w2$latitude <- as.numeric(resid_w2$latitude)
resid_w2$longitude <- as.numeric(resid_w2$longitude)
resid_w2$residuals.fm2. <- as.numeric(resid_w2$residuals.fm2.)
resid_w2$Row.names <- as.numeric(resid_w2$Row.names)
coordinates(resid_w2) <- c('latitude','longitude')

# sample variogram (binned estimator)
samp_vgm <- variogram(residuals.fm2. ~ 1, data=resid_w2)
plot(samp_vgm, main = 'Sample Variogram', pch=19)
# "~ 1" stands for a single constant predictor (hp: spatially constant mean)
# automatically decides to ignore direction: point pairs are merged on the
# basis of distance to compute the empirical variogram

# residual variogram w.r.t. a linear trend:
res_vgm <- variogram(residuals.fm2.~latitude+longitude, data=resid_w2)
plot(res_vgm, main = 'Residual Variogram', pch=19)
# automatically decides to ignore direction: point pairs are merged on the
# basis of distance to compute the empirical variogram

# default decisions: direction dependence, cutoff, lag width

plot(variogram(residuals.fm2. ~ 1, data=resid_w2,
               alpha = c(0, 45, 90, 135)), pch=19, main = 'Sample Variogram')
# point pairs whose separation vector has a given direction are used in each
# panel (not too many directions otherwise noise will increase)
# Note: zonal anisotropy

plot(variogram(residuals.fm2. ~ latitude+longitude, data=resid_w2,
               alpha = c(0, 45, 90, 135)), pch=19, main = 'Residual Variogram')
# point pairs whose separation vector has a given direction are used in each
# panel (not too many directions otherwise noise will increase)
# Note: zonal anisotropy

# cutoff distance: maximum distance up to which point pairs are considered
#                  (default = bbox diagonal / 3)
# lag width: width of distance intervals over which point pairs are averaged
#            in bins (default = cutoff distance / 15)

coff <- 20

plot(variogram(residuals.fm2. ~ 1, data=resid_w2,
               cutoff = coff, width = coff/15), pch=19, main = 'Sample Variogram (cutoff=20)')

plot(variogram(residuals.fm2. ~ latitude+longitude, data=resid_w2,
               cutoff = coff, width = coff/15), pch=19, main = 'Residual Variogram (cutoff=20)')

rm(fm2, coeffs.table, samp_vgm, res_vgm)

#### Variogram modeling ----

v <- variogram(residuals.fm2. ~ 1, data=resid_w2,
               cutoff = coff, width = coff/15)

## weighted least squares fitting a variogram model to the sample variogram
## STEPS:
## 1) choose a suitable model
plot(v,pch=19)
#vgm()
## 2) choose suitable initial values for partial sill, range & nugget
v.fit1 <- fit.variogram(v, vgm(1, "Exp", 5, 5))
v.fit2 <- fit.variogram(v, vgm(1, "Pen", 5, 5))
v.fit3 <- fit.variogram(v, vgm(1, "Bes", 5, 5))

## 3) fit the model using one of the possible fitting criteria

plot(v, v.fit1, pch = 19, main="Exponential model")
plot(v, v.fit2, pch = 19, main="Pentaspherical model")
plot(v, v.fit3, pch = 19, main="Bessel model")

rm(resid_w2, v, v.fit1, v.fit2, v.fit3, coff)
