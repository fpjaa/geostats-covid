###----------------------------------###
### Thesis: ASP data + Geostatistics ###
###----------------------------------###

## Setup: Dataset building ####

## Clear the workspace
rm(list=ls())

## Load packages

library(readr)  # Read tsv for national means
library(openxlsx)  # Read excel for national totals
library(dplyr)  # Data management
library(geojsonR)  # Load geojson points
#library(sp)  # Data management
#library(gstat)  # Krigging
library(yaImpute)  # Approx nearest neighbors

#### Load data to complete ----

setwd('/home/fpjaa/Documents/GitHub/geostats-covid/')

data <- read.csv("ASPdataset.csv", header=TRUE, stringsAsFactors=FALSE)
data <- data[, -c(25, 26, 27, 28)]
data <- data[!is.na(data$cases_density_first_wave),]

# Exclude french islands
data <- data[substr(data$NUTS,1,3)!="FRY",]

data$country = substr(data$NUTS,1,2)

#### Load data by variable: National means ####

setwd('/home/fpjaa/Documents/GitHub/geostats-covid/eurostat_datasets/')

#import TSV file into data frame

air <- read_tsv('air_passengers.tsv')
air <- air[,c(1,ncol(air))]
colnames(air)[ncol(air)] <- 'air'

avail <- read_tsv('available_hospital_beds_nuts2.tsv')
avail <- avail[,c(1,ncol(avail)-1)]
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
employ <- employ[,c(1,ncol(employ)-7)]
colnames(employ)[ncol(employ)] <- 'employ'

farm <- read_tsv('farm_labour_force.tsv')
farm <- farm[,c(1,ncol(farm))]
colnames(farm)[ncol(farm)] <- 'farm'

health <- read_tsv('health_personnel_by_nuts2.tsv')
health <- health[,c(1,ncol(health)-3)]
colnames(health)[ncol(health)] <- 'health'

hosp <- read_tsv('hospital_discharges_resp_diseases_j00_to_j99_nuts2.tsv')
hosp <- hosp[,c(1,ncol(hosp))]
colnames(hosp)[ncol(hosp)] <- 'hosp'

life <- read_tsv('life_expectancy.tsv')
life <- life[,c(1,ncol(life))]
colnames(life)[ncol(life)] <- 'life'

longterm <- read_tsv('longterm_care_beds_per_hundred_thousand_nuts2.tsv')
longterm <- longterm[,c(1,20)]  # Useless, only non NAs in 2015 (23)
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
stock <- stock[,c(1,ncol(stock)-4)]
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

setwd('/home/fpjaa/Documents/GitHub/geostats-covid/')

gvagr <- read.xlsx('gvagr_2020.xlsx', sheet=3)
gvagr <- gvagr[7:420,c(1,ncol(gvagr)-3)]  #2019
colnames(gvagr) <- c('name', 'gvagr')
gvagr$gvagr <- as.numeric(gvagr$gvagr)

names <- read.csv('names.csv')
names <- data.frame(names[,c(1,2)])
colnames(names) <- c('NUTS', 'name')

regGVA <- merge(gvagr, names, by="name")
regGVA <- regGVA[!duplicated(regGVA$name),]
regGVA <- regGVA[,c(2,3)]
gva <- merge(data[,c(1,19)], regGVA, by="NUTS", all.x = TRUE)
colnames(regGVA) <- c('regGVA', 'geo')
data[,19] <- gva$gvagr

# Ricuperare BE
BE_causes <- causes[substr(causes$geo,1,2)=="BE",]
BE_pop <- popul[substr(popul$geo,1,2)=="BE",]
BE_causes <- merge(BE_causes, BE_pop, by="geo")
BE_causes$tot <- BE_causes$causes * BE_causes$popul
BE_causes$geo <- substr(BE_causes$geo,1,3)
BE_causes <- BE_causes[,-2] %>%
  group_by(geo) %>%
  summarise_all("sum", na.rm = TRUE)
BE_causes$causes <- BE_causes$tot / BE_causes$popul
# Fill BE data
data[substr(data$NUTS,1,2)=="BE", 4] <- BE_causes$causes

rm(BE_causes, BE_pop)

BE_compens <- compens[substr(compens$geo,1,2)=="BE",]
BE_compens$geo <- substr(BE_compens$geo,1,3)
BE_compens <- BE_compens %>%
  group_by(geo) %>%
  summarise_all("sum", na.rm = TRUE)
# Fill BE data
data[substr(data$NUTS,1,2)=="BE", 5] <- BE_compens$compens[1:3]
rm(BE_compens)

BE_life <- life[substr(life$geo,1,2)=="BE",]
BE_life$geo <- substr(BE_life$geo,1,3)
BE_life <- BE_life %>%
  group_by(geo) %>%
  summarise_all("mean", na.rm = TRUE)
data[substr(data$NUTS,1,2)=="BE",12] <- BE_life$life
rm(BE_life)

BE_gdp <- nama[substr(nama$geo,1,2)=="BE",]
BE_gdp$geo <- substr(BE_gdp$geo,1,3)
BE_gdp <- BE_gdp %>%
  group_by(geo) %>%
  summarise_all("sum", na.rm = TRUE)
data[substr(data$NUTS,1,2)=="BE",14] <- BE_gdp$nama[1:3]
rm(BE_gdp)

df_list <- list(air, avail, causes, compens, death, early, employ, farm, health,
                hosp, life, longterm, nama, partic, popd, popul, pupils, regGVA,
                stock, students, unempl, utilized, young)      
#merge all data frames together
country_nuts2 <- Reduce(function(x, y) merge(x, y, all=TRUE, by='geo'), df_list)

country_nuts2$country = substr(country_nuts2$geo,1,2)

means_by_country <- country_nuts2[, -1] %>%
  group_by(country) %>%
  summarise_all("mean", na.rm = TRUE)

rm(df_list, country_nuts2,
   air, avail, causes, compens, death, early, employ, farm, health,
   hosp, life, longterm, nama, partic, popd, popul, pupils, regGVA,
   stock, students, unempl, utilized, young, gvagr, gdp, gva, names)

#### Load data by variable: National totals ####

setwd('/home/fpjaa/Documents/GitHub/geostats-covid/datasets_by_country/')

air <- read.xlsx('air_passengers_by_country.xlsx', sheet=3)
air <- air[8:37,c(1,2,ncol(air))]
colnames(air) <- c('country', 'name', 'air')
air$air <- as.numeric(air$air)

avail <- read.xlsx('available_hosp_beds_by_country.xlsx', sheet=3)
avail <- avail[8:44,c(1,ncol(avail)-3)]
colnames(avail) <- c('name', 'available')
avail$available <- as.numeric(avail$available)

causes <- read.xlsx('causes_of_death_by_country.xlsx', sheet=3)
causes <- causes[10:43,c(1,ncol(causes)-1)]
colnames(causes) <- c('name', 'causes')
causes$causes <- as.numeric(causes$causes)

compens <- read.xlsx('compensation_by_country.xlsx', sheet=3)
compens <- compens[8:36,c(1,ncol(compens)-1)]
colnames(compens) <- c('name', 'compensation')
compens$compensation <- as.numeric(compens$compensation)

death <- read.xlsx('deaths_by_country.xlsx', sheet=3)
death <- death[7:43,c(1,2,ncol(death)-1)]
colnames(death) <- c('country', 'name', 'deaths')
death$deaths <- as.numeric(death$deaths)

early <- read.xlsx('early_leavers_by_country.xlsx', sheet=3)
early <- early[9:43,c(1,ncol(early)-1)]
colnames(early) <- c('name', 'early')
early$early <- as.numeric(early$early)

employ <- read.xlsx('employment_thous_by_country.xlsx', sheet=3)
employ <- employ[9:35,c(1,ncol(employ)-15)]
colnames(employ) <- c('name', 'employment')
employ$employment <- as.numeric(employ$employ)

farm <- read.xlsx('farm_labour_force_by_country.xlsx', sheet=3)
farm <- farm[7:37,c(1,ncol(farm))]
colnames(farm) <- c('name', 'farm')
farm$farm <- as.numeric(farm$farm)

health <- read.xlsx('health_personnel_by_country.xlsx', sheet=3)
health <- health[8:44,c(1,ncol(health)-7)]
colnames(health) <- c('name', 'health')
health$health <- as.numeric(health$health)

hosp <- read.xlsx('hospital_discharges_by_country.xlsx', sheet=3)
hosp <- hosp[11:46,c(1,ncol(hosp))]
colnames(hosp) <- c('name', 'discharges')
hosp$discharges <- as.numeric(hosp$discharges)

life <- read.xlsx('life_expectancy_by_country.xlsx', sheet=3)
life <- life[9:45,c(1,2,ncol(life)-1)]
colnames(life) <- c('country', 'name', 'life')
life$life <- as.numeric(life$life)

longterm <- read.xlsx('longterm_beds_by_country.xlsx', sheet=3)
longterm <- longterm[8:42,c(1,ncol(longterm)-7)]
colnames(longterm) <- c('name', 'longterm')
longterm$longterm <- as.numeric(longterm$longterm)

nama <- read.xlsx('nama_gdp_by_country.xlsx', sheet=3)
nama <- nama[7:39,c(1,ncol(nama)-3)] #2019
colnames(nama) <- c('name', 'nama')
nama$nama <- as.numeric(nama$nama)

partic <- read.xlsx('participation_rate_by_country.xlsx', sheet=3)
partic <- partic[9:43,c(1,ncol(partic)-1)]
colnames(partic) <- c('name', 'participation')
partic$participation <- as.numeric(partic$participation)

popd <- read.xlsx('pop_density_by_country.xlsx', sheet=3)
popd <- popd[7:43,c(1,2,ncol(popd)-1)]
colnames(popd) <- c('country', 'name', 'density')
popd$density <- as.numeric(popd$density)

popul <- read.xlsx('population_by_country.xlsx', sheet=3)
popul <- popul[9:45,c(1,2,ncol(popul)-1)]
colnames(popul) <- c('country', 'name', 'population')
popul$population <- as.numeric(popul$population)

pupils <- read.xlsx('pupils_by_country.xlsx', sheet=3)
pupils <- pupils[9:44,c(1,ncol(pupils)-1)]
colnames(pupils) <- c('name', 'pupils')
pupils$pupils <- as.numeric(pupils$pupils)

regGVA <- read.xlsx('real_gva_by_country.xlsx', sheet=3)
regGVA <- regGVA[7:38,c(1,ncol(regGVA)-1)]
colnames(regGVA) <- c('name', 'rgva')
regGVA$rgva <- as.numeric(regGVA$rgva)

stock <- read.xlsx('stock_of_vehicles_by_country.xlsx', sheet=3)
stock <- stock[8:30,c(1,2,ncol(stock)-4)]
colnames(stock) <- c('country', 'name', 'stock')
stock$stock <- as.numeric(stock$stock)

students <- read.xlsx('students_enrolled_by_country.xlsx', sheet=3)
students <- students[9:44,c(1,ncol(students)-1)]
colnames(students) <- c('name', 'students')
students$students <- as.numeric(students$students)

unempl <- read.xlsx('unemployment_rate_by_country.xlsx', sheet=3)
unempl <- unempl[9:35,c(1,ncol(unempl)-1)]
colnames(unempl) <- c('name', 'unemployment')
unempl$unemployment <- as.numeric(unempl$unemployment)

utilized <- read.xlsx('utilized_agricultural_by_country.xlsx', sheet=3)
utilized <- utilized[8:35,c(1,ncol(utilized))]
colnames(utilized) <- c('name', 'utilized')
utilized$utilized <- as.numeric(utilized$utilized)

young <- read.xlsx('young_neet_by_country.xlsx', sheet=3)
young <- young[11:45,c(1,ncol(young)-1)]
colnames(young) <- c('name', 'neet')
young$neet <- as.numeric(young$neet)

df_list <- list(air, death, life, popd, popul, stock)
#merge all data frames together
country_data <- Reduce(function(x, y) merge(x, y, all=TRUE, by=c('country', 'name')), df_list)

df_list1 <- list(country_data, avail, causes, compens, early, employ, farm, health,
                 hosp, longterm, nama, partic, pupils, regGVA,
                 students, unempl, utilized, young)
country_data <- Reduce(function(x, y) merge(x, y, all=TRUE, by='name'), df_list1)

rm(df_list,df_list1,
   air, avail, causes, compens, death, early, employ, farm, health,
   hosp, life, longterm, nama, partic, popd, popul, pupils, regGVA,
   stock, students, unempl, utilized, young)

country_data <- country_data %>%
  select(country, name, air, available, causes, compensation, deaths, early,
         employment, farm, health, discharges, life, longterm, nama,
         participation, density, population, pupils, rgva, stock, students,
         unemployment, utilized, neet)

#### Assessment for data filling: Preparation ----

my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
my.med <- function(x) ifelse( !all(is.na(x)), median(x, na.rm=T), NA)
my.mea <- function(x) ifelse( !all(is.na(x)), mean(x, na.rm=T), NA)

err <- data.frame(matrix(ncol = 24, nrow = 9))
colnames(err)[1] <- 'type'
colnames(err)[2:24] <- colnames(country_data)[3:25]

#### Fill NAs: Trial by national mean ----

data1 <- merge(data[,c(1,27)], means_by_country, by = 'country')

# Validation
errors <- data.frame(matrix(ncol = 24, nrow = 136))
errors[,1] <- data1[,2]
colnames(errors) <- colnames(data1)[2:25]
for (r in 1:136){
  for (c in 2:24){
    if (!is.na(data[r,c]) && !is.na(data1[r,c+1]) && data[r,c] != 0){
      errors[r,c] <- abs(data1[r,c+1] - data[r,c])/data[r,c]
    }
  }
}

err[1,1] <- 'mean_max'
err[1,2:24] <- apply(errors[,2:24], 2, my.max)
err[4,1] <- 'mean_med'
err[4,2:24] <- apply(errors[,2:24], 2, my.med)
err[7,1] <- 'mean_mea'
err[7,2:24] <- apply(errors[,2:24], 2, my.mea)

# CY or PT for longterm beds doesn't exist

errors$country = substr(errors$NUTS,1,2)

errors_c <- errors[,-1] %>%
  group_by(country) %>%
  summarise_all("mean", na.rm = TRUE)

matplot(t((errors_c[,-1])^(1/3)),
        ylab = "Cubic root errors", xlab = "Feature index", 
        col = rainbow(length(errors_c)),
        main="Country-mean errors by country", type="p", pch = 2:5)
legend("topright", legend = as.list(errors_c$country),
       col = rainbow(length(errors_c)), pch = 2:5, cex = 0.8, ncol = 6)


#### Fill NAs: Trial by population proportion ----

data1 <- merge(data[,c(1,27)], country_data, by = 'country')

# Validation
errors <- data.frame(matrix(ncol = 24, nrow = 136))
errors[,1] <- data1[,2]
colnames(errors) <- colnames(data1)[3:26]
for (r in 1:136){
  for (c in 2:24){
    if (!is.na(data[r,c]) && !is.na(data1[r,c+1]) && data[r,c] != 0){
      replace <- data1[r,c+2]*data[r,17]/data1[r,19]
      errors[r,c] <- abs(replace - data[r,c])/data[r,c]
    }
  }
}

err[2,1] <- 'pop_max'
err[2,2:24] <- apply(errors[,2:24], 2, my.max)
err[5,1] <- 'pop_med'
err[5,2:24] <- apply(errors[,2:24], 2, my.med)
err[8,1] <- 'pop_mea'
err[8,2:24] <- apply(errors[,2:24], 2, my.mea)

errors$country = substr(errors$name,1,2)

errors_c <- errors[,-1] %>%
  group_by(country) %>%
  summarise_all("mean", na.rm = TRUE)

matplot(t((errors_c[,-1])^(1/3)),
        ylab = "Cubic root errors", xlab = "Feature index", 
        col = rainbow(length(errors_c)),
        main="Population proportion errors by country", type="p", pch = 2:5)
legend("topright", legend = as.list(errors_c$country),
       col = rainbow(length(errors_c)), pch = 2:5, cex = 0.8, ncol = 6)

#### Add locations ####

setwd('/home/fpjaa/Documents/GitHub/geostats-covid/')
spdf <- FROM_GeoJson("NUTS_LB_2021_4326.geojson")

locations <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(locations) <- c('latitude', 'longitude', 'NUTS')
for (i in 1:2010){
  lon <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][1]
  lat <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][2]
  nuts <- spdf[["features"]][[i]][["properties"]][["NUTS_ID"]]
  new <- c(lat, lon, nuts)                       # Create new row
  locations[nrow(locations) + 1, ] <- new
}

dataset <- merge(data, locations, by = 'NUTS')

lost <- anti_join(data, locations, by="NUTS")

spdf <- FROM_GeoJson("NUTS_LB_2013_4326.geojson")

locations <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(locations) <- c('latitude', 'longitude', 'NUTS')
for (i in 1:1951){
  lon <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][1]
  lat <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][2]
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
for (i in 1:1931){
  lon <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][1]
  lat <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][2]
  nuts <- spdf[["features"]][[i]][["properties"]][["NUTS_ID"]]
  new <- c(lat, lon, nuts)                       # Create new row
  locations[nrow(locations) + 1, ] <- new
}

fill <- merge(lost, locations, by = 'NUTS')

dataset <- rbind(dataset, fill)

rm(fill, locations, lost, spdf, i, lat, lon, new, nuts)

dataset$latitude <- as.numeric(dataset$latitude)
dataset$longitude <- as.numeric(dataset$longitude)

#### Fill NAs: Trial by weighted mean by distance ----

amount <- 3  # Number of neighbors

# Validation
errors <- data.frame(matrix(ncol = 24, nrow = 136))
errors[,1] <- data1[,2]
colnames(errors) <- colnames(data1)[3:26]
for (n in data$NUTS){
  for (c in 2:24){
    if (!is.na(data[data$NUTS==n,c]) && data[data$NUTS==n,c] != 0){
      x_grid <- dataset[dataset$NUTS!=n,c(c,28,29)]  # Discard the value to evaluate
      x_grid <- x_grid[!is.na(x_grid[,1]),]  # Discard NAs
      x_newdata <- dataset[dataset$NUTS==n,c(c,28,29)]  # Value to validate
      # Approximate nearest-neighbor search
      # This command finds the k nearest points in x_grid for each point in x_newdata
      # In the output:
      #   The first k columns are the row numbers of the points
      #   The next k columns (k+1:2k) are the squared euclidean distances
      # Idea: First k to get the value, next k to have the weights
      knn.out <- ann(ref = as.matrix(x_grid[,c(2,3)]),
                     target = as.matrix(x_newdata[,c(2,3)]),
                     k = amount)
      values <- x_grid[knn.out$knnIndexDist[1,1:amount], 1]  # Col 1 with values
      weights <- 1/sqrt(knn.out$knnIndexDist[1,(amount+1):(2*amount)])
      replace <- 1/sum(weights) * values %*% weights
      errors[errors$name==n,c] <- abs(replace - x_newdata[1,1])/x_newdata[1,1]
    }
  }
}

err[3,1] <- 'dist_max'
err[3,2:24] <- apply(errors[,2:24], 2, my.max)
err[6,1] <- 'dist_med'
err[6,2:24] <- apply(errors[,2:24], 2, my.med)
err[9,1] <- 'dist_mea'
err[9,2:24] <- apply(errors[,2:24], 2, my.mea)

errors$country = substr(errors$name,1,2)

errors_c <- errors[,-1] %>%
  group_by(country) %>%
  summarise_all("mean", na.rm = TRUE)

matplot(t((errors_c[,-1])^(1/3)),
        ylab = "Cubic root errors", xlab = "Feature index", 
        col = rainbow(length(errors_c)),
        main="KNN errors by country", type="p", pch = c(2:5))
legend("topright", legend = as.list(errors_c$country),
       col = rainbow(length(errors_c)), pch = c(2:5), cex = 0.8, ncol = 6)

#### Before filling ----

# Previous NAs

missing <- is.na(data[, -c(1, 25, 26)])
missing <- rowSums(missing)

data$country = substr(data$NUTS,1,2)
countries <- merge(data[,c(1,27)], country_data[,c(1,2)], by = 'country')
countries$name <- replace(countries$name, countries$name=="Germany (until 1990 former territory of the FRG)", "Germany")

countries <- countries$name
missing <- data.frame(countries, missing)
colnames(missing) <- c('country', 'nans')
missing$nans <- as.integer(missing$nans)

missing <- missing %>%
  group_by(country) %>%
  summarise_all("max", na.rm = TRUE)

par(mfrow=c(1,1))
par(mar=c(5,7,4,3)+0.1)
barplot(height = missing$nans,
        names.arg = missing$country,
        main = "Maximum missing features in a region",
        xlab = "Missing values",
        ylab = "",
        col = "darkblue",
        las = 1,
        cex.names = 1,
        horiz = TRUE,
        xpd = FALSE)

missing <- is.na(data[, -c(1, 25, 26,27)])
missing <- colSums(missing)
missing <- data.frame(missing)

par(mfrow=c(1,1))
par(mar=c(5,10,4,3)+0.1)
barplot(height = missing$missing,
        names.arg = c("Air passengers", "Hospital beds", "Death rate",
                      "Compensation of employees", "Deaths (total)",
                      "Early leavers from ed.", "Employment worked hrs.",
                      "Farm labour force", "Health personnel",
                      "Respiratory discharges", "Life expectancy",
                      "Longterm care beds", "Gross domestic product",
                      "Ed. participation", "Population density", "Population",
                      "Pupils enrolled", "GVA growth", "Vehicles",
                      "Tertiary ed. students", "Unemployment rate",
                      "Utilised agricultural area", "NEET rate"),
        main = "Missing data by feature",
        xlab = "Missing values",
        ylab = "",
        col = "darkblue",
        las = 1,
        cex.names = 0.6,
        horiz = TRUE)

rm(errors, my.max, my.med, my.mea, errors_c)

# Compare errors

par(mar=c(5,3,4,3)+0.1)

plot(1:23, log(err[3,2:24]), ylab="Max log error", xlab="Feature", main="Approximation method comparison", type="p")
points(1:23, log(err[2,2:24]), col="red")
points(1:23, log(err[1,2:24]), col="blue")
legend("topright",legend = err[c(3,2,1),1],
       col = c("black", "red", "blue"), pch = 1, cex = 0.8, ncol = 3)

plot(1:23, log(err[6,2:24]), ylab="Median log error", xlab="Feature", main="Approximation method comparison", type="p")
points(1:23, log(err[5,2:24]), col="red")
points(1:23, log(err[4,2:24]), col="blue")
legend("bottomleft",legend = err[c(6,5,4),1],
       col = c("black", "red", "blue"), pch = 1, cex = 0.8)

plot(1:23, log(err[9,2:24]), ylab="Mean log error", xlab="Feature", main="Approximation method comparison", type="p")
points(1:23, log(err[8,2:24]), col="red")
points(1:23, log(err[7,2:24]), col="blue")
legend("topright",legend = err[c(9,8,7),1],
       col = c("black", "red", "blue"), pch = 1, cex = 0.8)

#### Fill NAs ----

# Winner by feature
## Air = None
## Available = KNN
## Causes = mean
## Compensation = pop
## 5 Deaths doesn't need filling
## Early = mean
## Employment = pop
## Farm doesn't need filling
## Health = pop
## 10 Discharges = pop
## Life = mean
## Longterm = KNN
## GDP = pop
## Participation = mean
## 15 Density = pop
## Population doesn't need filling
## Pupils = pop
## GVA = mean
## Stock = pop
## 20 Students = pop
## Unemployment = mean
## Utilized doesn't need filling
## NEETs = mean

missing <- is.na(data[, -c(1, 25, 26)])

data$country = substr(data$NUTS,1,2)
countries <- merge(data[,c(1,27)], country_data[,c(1,2)], by = 'country')
countries$name <- replace(countries$name, countries$name=="Germany (until 1990 former territory of the FRG)", "Germany")

countries <- countries$name
missing <- data.frame(countries, missing)

missing_matrix <- missing[,-25] %>%
  group_by(countries) %>%
  summarise_all("sum", na.rm = TRUE)

## Longterm = KNN -> MT warning but 0 NAs
## Density = pop -> 2 clusters (CY+EE+LU+LVMT have 0 errors), 0 NAs
## Stock = pop -> DE warning but 4/20 NAs
## Students = pop -> CY lower outlier but 0 NAs
## Unemployment = mean -> CY+HR lower outlier but 0 NAs
## NEETs = mean -> CY+EE lower outlier but 0 NAs

# Fill data: Using means by country
## causes of death, early leavers, life expectancy, participation, GVA, unemployment, NEET
data1 <- merge(data[,c(1,27)], means_by_country, by = 'country')
for (i in c(4,7,12,15,19,22,24)){
  data[,i] <- ifelse(is.na(data[,i]), data1[,i+1], data[,i])
}
# Fill data: Using weights by population
data1 <- merge(data[,c(1,27)], country_data, by = 'country')
for (r in 1:136){
  for (c in c(5,8,10,11,14,16,18,20,21)){
    if (is.na(data[r,c]) && !is.na(data1[r,c+1])){
      replace <- data1[r,c+2]*data[r,17]/data1[r,19]
      data[r,c] <- replace
    }
  }
}
# KNN: Available, long-term care beds
for (c in c(3,13)){
  x_grid <- dataset[!is.na(dataset[,c]),c(c,28,29)]  # Discard NAs
  x_newdata <- dataset[is.na(dataset[,c]),c(c,28,29)]  # Values to replace
  knn.out <- ann(ref = as.matrix(x_grid[,c(2,3)]),
                 target = as.matrix(x_newdata[,c(2,3)]),
                 k = amount)
  values <- matrix(x_grid[knn.out$knnIndexDist[,1:amount], 1], ncol=amount)
  weights <- 1/matrix(knn.out$knnIndexDist[,(amount+1):(2*amount)], ncol=amount)
  replace <- 1/apply(weights, 1, sum) * apply(values * weights, 1, sum)
  data[is.na(data[,c]),c] <- replace
}

rm(data1, means_by_country, c, r, i, replace,
   x_grid, x_newdata, n, knn.out, values, weights, amount)

# Fix data fill that is supposed to be integer
## Deaths (ok), discharges (to do), health personnel (to do),
## population (ok), pupils (to do), students (to do), stock (to do),
## farm labour force (to do) and utilised agricultural area (to do)
data$deaths <- as.integer(data$deaths)
data$health_personnel_by_nuts2 <- as.integer(data$health_personnel_by_nuts2)
data$hospital_discharges_resp_diseases_j00_to_j99_nuts2 <- as.integer(data$hospital_discharges_resp_diseases_j00_to_j99_nuts2)
data$population_nuts2 <- as.integer(data$population_nuts2)
data$stock_of_vehicles_by_category_and_nuts2 <- as.integer(data$stock_of_vehicles_by_category_and_nuts2)
data$students_enrolled_in_tertiary_education_by_education_level_programme_orientation_sex_and_nuts2 <- as.integer(data$students_enrolled_in_tertiary_education_by_education_level_programme_orientation_sex_and_nuts2)
data$pupils_and_students_enrolled_by_sex_age_and_nuts2 <- as.integer(data$pupils_and_students_enrolled_by_sex_age_and_nuts2)
data$farm_labour_force <- as.integer(data$farm_labour_force)
data$utilised_agricultural_area <- as.integer(data$utilised_agricultural_area)

# Check NAs that werent filled

missing <- is.na(data[, -c(1, 25, 26)])
missing <- rowSums(missing)

data$country = substr(data$NUTS,1,2)
countries <- merge(data[,c(1,27)], country_data[,c(1,2)], by = 'country')
countries$name <- replace(countries$name, countries$name=="Germany (until 1990 former territory of the FRG)", "Germany")

countries <- countries$name
missing <- data.frame(countries, missing)
colnames(missing) <- c('country', 'nans')
missing$nans <- as.integer(missing$nans)

missing <- missing %>%
  group_by(country) %>%
  summarise_all("max", na.rm = TRUE)

missing$nans <- as.integer(missing$nans)

par(mfrow=c(1,1))
par(mar=c(5,7,4,3)+0.1)
barplot(height = missing$nans,
        names.arg = missing$country,
        main = "Maximum missing features in a region",
        xlab = "Missing values",
        ylab = "",
        col = "darkblue",
        las = 1,
        cex.names = 1,
        horiz = TRUE,
        xlim = c(0,18))

missing <- is.na(data[, -c(1, 25, 26,27)])
missing <- colSums(missing)
missing <- data.frame(missing)

par(mfrow=c(1,1))
par(mar=c(5,10,4,3)+0.1)
barplot(height = missing$missing,
        names.arg = c("Air passengers", "Hospital beds", "Death rate",
                      "Compensation of employees", "Deaths (total)",
                      "Early leavers from ed.", "Employment worked hrs.",
                      "Farm labour force", "Health personnel",
                      "Respiratory discharges", "Life expectancy",
                      "Longterm care beds", "Gross domestic product",
                      "Ed. participation", "Population density", "Population",
                      "Pupils enrolled", "GVA growth", "Vehicles",
                      "Tertiary ed. students", "Unemployment rate",
                      "Utilised agricultural area", "NEET rate"),
        main = "Missing data by feature",
        xlab = "Missing values",
        ylab = "",
        col = "darkblue",
        las = 1,
        cex.names = 0.6,
        horiz = TRUE,
        xlim = c(0,49))

# Unsolvable cases:
## No national data in DE, error with other methods too high

rm(countries, missing, country_data, err, missing_matrix)

#### Add locations: Save dataset ####

spdf <- FROM_GeoJson("NUTS_LB_2021_4326.geojson")

locations <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(locations) <- c('latitude', 'longitude', 'NUTS')
for (i in 1:2010){
  lon <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][1]
  lat <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][2]
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
for (i in 1:1951){
  lon <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][1]
  lat <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][2]
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
for (i in 1:1931){
  lon <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][1]
  lat <- spdf[["features"]][[i]][["geometry"]][["coordinates"]][2]
  nuts <- spdf[["features"]][[i]][["properties"]][["NUTS_ID"]]
  new <- c(lat, lon, nuts)                       # Create new row
  locations[nrow(locations) + 1, ] <- new
}

fill <- merge(lost, locations, by = 'NUTS')

dataset <- rbind(dataset, fill)

rm(fill, locations, lost, spdf, i, lat, lon, new, nuts, data)

dataset$latitude <- as.numeric(dataset$latitude)
dataset$longitude <- as.numeric(dataset$longitude)

# Note there are islands far away that are european dominated
# In the dataset, when making a boxplot of latitude, we note some outliers
# under -40 that could be discarded since they are said islands,
# they are only 3 locations
# Similarly, some negative longitudes can be discarded,
# they are not the same locations, but an extra of 2 locations
## This will require to exclude 5 regions
dataset <- dataset[dataset$longitude>-40,]
# "FRY1" "FRY2" "FRY3"
dataset <- dataset[dataset$latitude>0,]
# "FRY4" "FRY5"

colnames(dataset) <- c("NUTS", "Air_passengers", "Hospital_beds", "Death_rate",
                       "Compensation_of_employees", "Deaths",
                       "Early_leavers_from_ed.", "Employment_worked_hrs.",
                       "Farm_labour_force", "Health_personnel",
                       "Respiratory_discharges", "Life_expectancy",
                       "Longterm_care_beds", "Gross_domestic_product",
                       "Ed._participation", "Population_density", "Population",
                       "Pupils_enrolled", "GVA_growth", "Vehicles",
                       "Tertiary_ed._students", "Unemployment_rate",
                       "Utilised_agricultural_area", "NEET_rate",
                       "Cases_density_1", "Cases_density_2", "Country",
                       "Latitude", "Longitude")

write_csv(dataset, 'dataset.csv')

## Setup: Data analysis ----

## Clear the workspace
rm(list=ls())

## Functions for graphics 
#v.f <- function(x, ...){100-cov.spatial(x, ...)}
#v.f.est<-function(x,C0, ...){C0-cov.spatial(x, ...)}

## Load packages

library(sf)  # SHP data management
library(dplyr)  # Data management
library(glmnet)  # LASSO selection
library(gstat)  # Geostatistics (variogram)
library(sp)  # Data management
library(rgeoda)  # LISA clustering
library(openxlsx)  # Read excel for national totals

## Load data

setwd('/home/fpjaa/Documents/GitHub/geostats-covid/')
dataset <- read.csv("dataset.csv", header=TRUE, stringsAsFactors=FALSE)
dataset <- dataset[,-c(2)]

## Set seed
set.seed(22072022)

#### Load map ----

setwd('/home/fpjaa/Documents/GitHub/geostats-covid/Polygons/')
nuts_polyg <- st_read("NUTS_RG_20M_2021_4326.shp")
# We have NUTS1 data: DE + BE (exclude all DE NUTS2 and BE1)
# Approach: Keep only NUTS2 and then replace the NUTS1 considered
exclude1 <- nuts_polyg[nuts_polyg$LEVL_CODE=="2" & nuts_polyg$CNTR_CODE=="DE",]
exclude1 <- exclude1$NUTS_ID
exclude2 <- nuts_polyg[nuts_polyg$LEVL_CODE=="2" & nuts_polyg$CNTR_CODE=="BE",]
exclude2 <- exclude2[exclude2$NUTS_ID != "BE10",]  # Do not exclude BE10
exclude2 <- exclude2$NUTS_ID
include <- nuts_polyg[nuts_polyg$LEVL_CODE=="1" & nuts_polyg$CNTR_CODE %in% c("DE", "BE"),]
include <- include[include$NUTS_ID != "BE1",]  # Exclude BE1
nuts_polyg <- nuts_polyg[nuts_polyg$LEVL_CODE=="2",]
nuts_polyg <- nuts_polyg[!(nuts_polyg$NUTS_ID %in% c(exclude1, exclude2)),]
nuts_polyg <- rbind(nuts_polyg, include)
colnames(nuts_polyg)[1] <- "NUTS"

rm(exclude1, exclude2, include)

lost <- anti_join(dataset, nuts_polyg, by="NUTS")

nuts_polyg1 <- st_read("NUTS_RG_20M_2013_4326.shp")
colnames(nuts_polyg1)[1] <- "NUTS"

nuts_polyg1 <- merge(nuts_polyg1, lost[, c(1,27)], by = 'NUTS')

lost <- anti_join(lost, nuts_polyg1, by="NUTS")

nuts_polyg <- rbind(nuts_polyg[,-c(6:8)], nuts_polyg1[,-c(7)])

nuts_polyg1 <- st_read("NUTS_RG_20M_2006_4326.shp")
colnames(nuts_polyg1)[1] <- "NUTS"

nuts_polyg1 <- merge(nuts_polyg1, lost[, c(1,27)], by = 'NUTS')

nuts_polyg <- rbind(nuts_polyg, nuts_polyg1[,-c(7)])

lost <- anti_join(lost, nuts_polyg1, by="NUTS")

rm(lost, nuts_polyg1)

#write_csv(nuts_polyg[,c(1,4)], 'names.csv')

setwd('/home/fpjaa/Documents/GitHub/geostats-covid/')

#### Map plots ----

# Uncoloured map
par(mfrow=c(1,1))
par(mar=c(2,4,2,4)+0.1)  # BLTR
plot(nuts_polyg$geometry, xlim=c(-20,30), ylim=c(25,60))

# Waves 1 and 2 initial configuration
my_colors <- c(heat.colors(6, alpha = 1))[c(6,5,4,3,2)]
nuts_polyg_tagged <- merge(nuts_polyg[,c(1,7)], dataset[,c('NUTS', 'Cases_density_1', 'Cases_density_2')], by="NUTS", all=TRUE)

mybreaks <- seq(from = min(dataset$Cases_density_1)-0.000001,
                to = max(dataset$Cases_density_1)+0.00001,
                length.out = 6)

tags <- cut(nuts_polyg_tagged$Cases_density_1, mybreaks)

nuts_polyg_tagged$mycolourscheme <- my_colors[findInterval(nuts_polyg_tagged$Cases_density_1, vec = mybreaks)]

# Wave 1 plot
plot(nuts_polyg_tagged$geometry, col = nuts_polyg_tagged$mycolourscheme,
     xlim=c(-18,20), ylim=c(25,63))
legend("topleft", legend = levels(tags), col = my_colors, pch=1, cex=0.9,
       title="Cases density (wave 1)")

# Wave 2 settings counting on work from wave 1
mybreaks <- seq(from = min(dataset$Cases_density_2)-0.0000001,
                to = max(dataset$Cases_density_2)+0.00001,
                length.out = 6)

tags <- cut(nuts_polyg_tagged$Cases_density_2, mybreaks)

mycolourscheme <- my_colors[findInterval(nuts_polyg_tagged$Cases_density_2, vec = mybreaks)]

# Wave 2 plot
plot(nuts_polyg_tagged$geometry, col = mycolourscheme,
     xlim=c(-18,20), ylim=c(25,63))
legend("topleft", legend = levels(tags), col = my_colors, pch=1, cex=0.9,
       title="Cases density (wave 2)")

dataset <- na.omit(dataset)

# Waves 1 and 2 initial configuration
my_colors <- c(heat.colors(6, alpha = 1))[c(6,5,4,3,2)]
nuts_polyg_tagged <- merge(nuts_polyg[,c(1,7)], dataset[,c('NUTS', 'Cases_density_1', 'Cases_density_2')], by="NUTS", all=TRUE)

mybreaks <- seq(from = min(dataset$Cases_density_1)-0.000001,
                to = max(dataset$Cases_density_1)+0.00001,
                length.out = 6)

tags <- cut(nuts_polyg_tagged$Cases_density_1, mybreaks)

nuts_polyg_tagged$mycolourscheme <- my_colors[findInterval(nuts_polyg_tagged$Cases_density_1, vec = mybreaks)]

# Wave 1 plot
plot(nuts_polyg_tagged$geometry, col = nuts_polyg_tagged$mycolourscheme,
     xlim=c(-18,20), ylim=c(25,63))
legend("topleft", legend = levels(tags), col = my_colors, pch=1, cex=0.9,
       title="Cases density (wave 1)")

# Wave 2 settings counting on work from wave 1
mybreaks <- seq(from = min(dataset$Cases_density_2)-0.0000001,
                to = max(dataset$Cases_density_2)+0.00001,
                length.out = 6)

tags <- cut(nuts_polyg_tagged$Cases_density_2, mybreaks)

mycolourscheme <- my_colors[findInterval(nuts_polyg_tagged$Cases_density_2, vec = mybreaks)]

# Wave 2 plot
plot(nuts_polyg_tagged$geometry, col = mycolourscheme,
     xlim=c(-18,20), ylim=c(25,63))
legend("topleft", legend = levels(tags), col = my_colors, pch=1, cex=0.9,
       title="Cases density (wave 2)")

rm(my_colors, mybreaks, mycolourscheme, tags)

#### Plots of 112 rows ----

## Transformations

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

par(mfrow=c(1,2))
par(mar=c(2,6,2,4)+0.1)  # BLTR
# histogram
hist(dataset$Cases_density_1, breaks=16, col="grey", main='Histogram of wave 1 cases', prob = TRUE, xlab = 'Cases density')
# highly skewed, transform to the log
#hist(log10(dataset$Cases_density_1+1), breaks=16, col="grey", main='Histogram of log(W1_cases+1)', prob = TRUE, xlab = 'log(W1_cases)')
# power transform
#hist(sqrt(dataset$Cases_density_1), breaks=16, col="grey", main='Histogram of sqrt(W1_cases)', prob = TRUE, xlab = 'sqrt(W1_cases)')
# logit transform
hist(qlogis(dataset$Cases_density_1), breaks=16, col="grey", main='Histogram of logit cases', prob = TRUE, xlab = 'logit(W1_cases)')
# Z transform
#w1.z <- (dataset$Cases_density_1 - mean(dataset$Cases_density_1))/sd(dataset$Cases_density_1)
#hist(w1.z, breaks=16, col="grey", main='Histogram of Z(W1_cases)', prob = TRUE, xlab = 'Z(W1_cases)')
# power transform
#hist((dataset$Cases_density_1)^(1/3), breaks=16, col="grey", main='Histogram of (W1_cases)^1/3', prob = TRUE, xlab = '(W1_cases)^1/3')

par(mfrow=c(1,2))
# histogram
hist(dataset$Cases_density_2, breaks=16, col="grey", main='Histogram of wave 2 cases', prob = TRUE, xlab = 'Cases density')
# highly skewed, transform to the log
#hist(log10(dataset$Cases_density_2+1), breaks=16, col="grey", main='Histogram of log(W2_cases+1)', prob = TRUE, xlab = 'log(W2_cases)')
# power transform
#hist(sqrt(dataset$Cases_density_2), breaks=16, col="grey", main='Histogram of sqrt(W2_cases)', prob = TRUE, xlab = 'sqrt(W2_cases)')
# logit transform
#hist(qlogis(dataset$Cases_density_2), breaks=16, col="grey", main='Histogram of logit(W2_cases)', prob = TRUE, xlab = 'logit(W2_cases)')
# Z transform
w2.z <- (dataset$Cases_density_2 - mean(dataset$Cases_density_2))/sd(dataset$Cases_density_2)
hist(w2.z, breaks=16, col="grey", main='Histogram of Z(W2_cases)', prob = TRUE, xlab = 'Z(W2_cases)')
# power transform
#hist((dataset$Cases_density_2)^(1/3), breaks=16, col="grey", main='Histogram of (W2_cases)^1/3', prob = TRUE, xlab = '(W2_cases)^1/3')


# https://chrischizinski.github.io/SNR_R_Group/2016-08-10-Data-Transformations

# Instead of transforming, could do logistic regression (with LASSO)
## diagnostics for logistic regression are different from OLS regression
## we'll work with the residuals so it can be problematic

rm(w2.z)

## Regions by country

setwd('/home/fpjaa/Documents/GitHub/geostats-covid/datasets_by_country/')

names <- read.xlsx('air_passengers_by_country.xlsx', sheet=3)
names <- names[8:37,c(1,2)]
colnames(names) <- c('Country', 'Name')

counter <- merge(dataset[,c('NUTS', 'Country')], names, by = "Country")
counter$Name <- replace(counter$Name, counter$Name=="Germany (until 1990 former territory of the FRG)", "Germany")
par(mfrow = c(1,1))
par(mar=c(4,8,4,4)+0.1)  # BLTR
barplot(table(counter$Name),
        main = "Regions by country",
        xlab = "Regions",
        ylab = "",
        col = "darkblue",
        las = 1,
        cex.names = 1,
        horiz = TRUE,
        xpd = FALSE)

rm(counter, names)

#### LASSO ----

coeff2dt <- function(fitobject, s) {
  coeffs <- coef(fitobject, s) 
  coeffs.dt <- data.frame(name = coeffs@Dimnames[[1]][coeffs@i + 1], coefficient = coeffs@x) 
  
  # reorder the variables in term of coefficients
  return(coeffs.dt[order(coeffs.dt$coefficient, decreasing = T),])
}

#### Wave 1 ----

setwd('/home/fpjaa/Documents/GitHub/geostats-covid/Wave1')

dataset$response1 <- qlogis(dataset$Cases_density_1)

# Build the matrix of predictors
options(na.action='na.omit')
#options(na.action='na.pass')
x <- model.matrix(response1 ~ #Air_passengers+
                    Hospital_beds+
                    Death_rate+
                    Compensation_of_employees+
                    Deaths+
                    Early_leavers_from_ed.+
                    Employment_worked_hrs.+
                    Farm_labour_force+
                    Health_personnel+
                    Respiratory_discharges+
                    Life_expectancy+
                    Longterm_care_beds+
                    Gross_domestic_product+
                    Ed._participation+
                    Population_density+
                    Population+
                    Pupils_enrolled+
                    GVA_growth+
                    Vehicles+
                    Tertiary_ed._students+
                    Unemployment_rate+
                    Utilised_agricultural_area+
                    NEET_rate, data=dataset)[,-1]

# Build the vector of response
y <- dataset$response1

# Let's set a grid of candidate lambda's for the estimate
lambda.grid <- c(0,10^seq(2,-3,length=100))
fit.lasso <- glmnet(x,y, lambda = lambda.grid) # default: alpha=1 -> lasso

# Let's set lambda via cross validation
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) # default: 10-fold CV

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

par(mfrow=c(1,1))
par(mar=c(4,6,4,4)+0.1)  # BLTR
plot(fit.lasso, xvar='lambda', label=TRUE, col = rainbow(dim(x)[2]))
labs <- gsub("_", " ", dimnames(x)[[2]])
legend('topright', labs, col =  rainbow(dim(x)[2]), lty=1, cex=0.7, ncol=2)
abline(v = log(bestlam.lasso), col = 'black', lty = 3)

plot(cv.lasso)

coeffs.table <- coeff2dt(fitobject = cv.lasso, s = 'lambda.min')

barplot(coeffs.table$coefficient[1:(length(coeffs.table[,1])-1)],
        col = rainbow(length(coeffs.table[,1])-1),
        main = "Coefficients of selected model")
labs <- gsub("_", " ", coeffs.table$name[1:(length(coeffs.table[,1])-1)])
legend('topright', labs, col =  rainbow(length(coeffs.table[,1])-1), lty=1, cex=0.8, ncol=2)

#### Wave 1 residuals -----------------

fitted_values <- predict(cv.lasso, x, s = 'lambda.min')

dataset$residuals1 <- y - fitted_values

shapiro.test(dataset$residuals1)

# Diagnostic plots
par(mar=c(6,6,4,4)+0.1)  # BLTR

hist(dataset$residuals1, breaks = 15,
     main = "Histogram of model residuals",
     xlab = "Residuals")

qqnorm(dataset$residuals1)
qqline(dataset$residuals1)

plot(x=fitted_values, y=dataset$residuals1,
     main="Residuals vs Fitted", xlab="Fitted", ylab="Residuals")
abline(h=0, col = 'black', lty = 3)

std.res <- (dataset$residuals1 - mean(dataset$residuals1))/sd(dataset$residuals1)
plot(x=fitted_values, y=sqrt(abs(std.res)),
     main="Scale-Location", xlab="Fitted", ylab="Sqrt. Abs. Std. Res.")
abline(h=mean(sqrt(abs(std.res))), col = 'red', lty = 3)

H <- x %*% solve(t(x) %*% x) %*% t(x)
plot(x=diag(H), y=std.res,
     main="Residuals vs Leverage", xlab="Leverage", ylab="Std. Residuals")

#find R-Squared
sst <- sum((y - mean(y))^2)
sse <- sum((fitted_values - y)^2)
rsq <- 1 - sse/sst
rsq

resid_w1 <- dataset[, c('NUTS', 'residuals1', 'Latitude', 'Longitude')]

# Map plot
# Residuals configuration
mybreaks <- c(min(resid_w1$residuals1, na.rm = TRUE)-0.0001,
              min(resid_w1$residuals1, na.rm = TRUE)/2,
              0,
              max(resid_w1$residuals1, na.rm = TRUE)/2,
              max(resid_w1$residuals1, na.rm = TRUE)+0.0001)
my_colors <- c('#0000ff', '#6495ed', '#f08080', '#ff0000')

nuts_polyg_tagged <- merge(nuts_polyg[,c(1,7)], resid_w1[,c('NUTS', 'residuals1')], by="NUTS", all=TRUE)
tags <- cut(nuts_polyg_tagged$residuals1, mybreaks)
mycolourscheme <- my_colors[findInterval(nuts_polyg_tagged$residuals1, vec = mybreaks)]

# Wave 1 plot
par(mfrow=c(1,1))
par(mar=c(2,4,2,4)+0.1)  # BLTR
plot(nuts_polyg_tagged$geometry, col = mycolourscheme,
     xlim=c(-18,20), ylim=c(25,63))
legend("topleft", legend = levels(tags), col = my_colors, pch=1, cex=0.9,
       title="Residuals (wave 1)")

# Transformed response configuration
mybreaks <- seq(min(dataset$response1)-0.0001,
                max(dataset$response1)+0.0001,
                length.out = 5)
my_colors <- heat.colors(5, rev=TRUE)

nuts_polyg_tagged <- merge(nuts_polyg[,c(1,7)], dataset[,c('NUTS', 'response1')], by="NUTS", all=TRUE)
tags <- cut(nuts_polyg_tagged$response1, mybreaks)
mycolourscheme <- my_colors[findInterval(nuts_polyg_tagged$response1, vec = mybreaks)]

# Wave 1 plot
par(mfrow=c(1,1))
par(mar=c(2,4,2,4)+0.1)  # BLTR
plot(nuts_polyg_tagged$geometry, col = mycolourscheme,
     xlim=c(-18,20), ylim=c(25,63))
legend("topleft", legend = levels(tags), col = my_colors, pch=1, cex=0.9,
       title="Response (wave 1)")

resid_w1$Latitude <- as.numeric(resid_w1$Latitude)
resid_w1$Longitude <- as.numeric(resid_w1$Longitude)
resid_w1$residuals1 <- as.numeric(resid_w1$residuals1)

# Extract data in case of anisotropy
write.csv(dataset[, c('NUTS', 'Latitude', 'Longitude', 'Cases_density_1', 'response1', 'residuals1')], 'wave1.csv')

rm(coeffs.table, cv.lasso, fit.lasso, fitted_values, x, y,
   bestlam.lasso, lambda.grid, sst, sse, rsq, std.res,
   nuts_polyg_tagged, labs, my_colors, mybreaks, mycolourscheme, tags)

#### Variogram modeling ----

resid_w1 <- read.csv("wave1.csv", header=TRUE, stringsAsFactors=FALSE)
resid_w1 <- resid_w1[, c('Latitude', 'Longitude', 'residuals1')]
resid_w1$Latitude <- as.numeric(resid_w1$Latitude)
resid_w1$Longitude <- as.numeric(resid_w1$Longitude)
resid_w1$residuals1 <- as.numeric(resid_w1$residuals1)
coordinates(resid_w1) <- c('Latitude','Longitude')
proj4string(resid_w1) <- CRS("+init=epsg:4326")
#resid_proj <- spTransform(resid_w1, CRS("+proj=longlat +datum=WGS84"))

plot(variogram(residuals1 ~ 1, data=resid_w1), pch=19, main = 'Sample Variogram')

plot(variogram(residuals1 ~ Longitude, data=resid_w1), pch=19, main = 'Residual Variogram')

#plot(variogram(residuals1 ~ Latitude, data=resid_w1), pch=19, main = 'Residual Variogram')

#plot(variogram(residuals1 ~ Longitude+Latitude, data=resid_w1), pch=19, main = 'Residual Variogram')

coff <- 1200

plot(variogram(residuals1 ~ 1, data=resid_w1,
               cutoff = coff, width = coff/15), pch=19, main = paste('Sample Variogram, cutoff =',coff))

plot(variogram(residuals1 ~ Longitude, data=resid_w1,
               cutoff = coff, width = coff/15), pch=19, main = paste('Residual Variogram, cutoff =',coff))

#plot(variogram(residuals1 ~ Latitude, data=resid_w1, cutoff = coff, width = coff/15), pch=19, main = paste('Residual Variogram, cutoff =',coff))

#plot(variogram(residuals1 ~ Longitude+Latitude, data=resid_w1, cutoff = coff, width = coff/15), pch=19, main = paste('Residual Variogram, cutoff =',coff))

plot(variogram(residuals1 ~ Longitude, data=resid_w1,
               alpha = c(0, 45, 90, 135),
               cutoff = coff, width = coff/15), pch=19, main = 'Directional Residual Variogram')

#plot(variogram(residuals1 ~ Latitude, data=resid_w1, alpha = c(0, 45, 90, 135), cutoff = coff, width = coff/15), pch=19, main = 'Directional Residual Variogram')

#plot(variogram(residuals1 ~ Longitude+Latitude, data=resid_w1, alpha = c(0, 45, 90, 135), cutoff = coff, width = coff/15), pch=19, main = 'Directional Residual Variogram')

#plot(variogram(residuals1 ~ 1, data=resid_w1, alpha = c(0, 45, 90, 135), cutoff = coff, width = coff/15), pch=19, main = 'Directional Sample Variogram')

#plot(variogram(residuals1 ~ Longitude, data=resid_w1, cutoff = coff, width = coff/15, map = TRUE))

## weighted least squares fitting a variogram model to the sample variogram
## STEPS:
## 1) choose a suitable model
v <- variogram(residuals1 ~ Longitude, data=resid_w1,
               cutoff = coff, width = coff/15)

#vgm()
## 2) choose suitable initial values for partial sill, range & nugget
v.fit1 <- fit.variogram(v, vgm(1, "Exp", 500, 500))
v.fit2 <- fit.variogram(v, vgm(1, "Sph", 500, 500))
v.fit3 <- fit.variogram(v, vgm(1, "Gau", 500, 500))

## 3) fit the model using one of the possible fitting criteria

plot(v, v.fit1, pch = 19, main="Exponential model")
plot(v, v.fit2, pch = 19, main="Spherical model")
plot(v, v.fit3, pch = 19, main="Gaussian model")

## Problem: Anisotropy: alpha = 0, 45, 90, 135

v <- variogram(residuals1 ~ Longitude, data=resid_w1,
               cutoff = coff, width = coff/15, alpha = c(0, 45, 90, 135))

plot(v, v.fit1, pch = 19, main="Exponential model")
plot(v, v.fit2, pch = 19, main="Spherical model")
plot(v, v.fit3, pch = 19, main="Gaussian model")

# Goodness of Fit: Residual sum of squares
c(attributes(v.fit1)$SSErr, attributes(v.fit2)$SSErr, attributes(v.fit3)$SSErr)

# Make sure they converged
c(attributes(v.fit1)$singular, attributes(v.fit2)$singular, attributes(v.fit3)$singular)

# calculate generalised least squares residuals
par(mfrow = c(1,3))

g1 <- gstat(NULL, "exp", residuals1 ~ Longitude,
           data = resid_w1, model = v.fit1)
blue1 <- predict(g1, newdata = resid_w1, BLUE = TRUE)
blue1$blue.res <- resid_w1$residuals1 - blue1$exp.pred
exp.box <- boxplot(blue1$blue.res, main = "GLS residuals for Exp model")
summary(blue1$blue.res)

g2 <- gstat(NULL, "sph", residuals1 ~ Longitude,
            data = resid_w1, model = v.fit2)
blue2 <- predict(g2, newdata = resid_w1, BLUE = TRUE)
blue2$blue.res <- resid_w1$residuals1 - blue2$sph.pred
sph.box <- boxplot(blue2$blue.res, main = "GLS residuals for Sph model")
summary(blue2$blue.res)

g3 <- gstat(NULL, "gau", residuals1 ~ Longitude,
            data = resid_w1, model = v.fit3)
blue3 <- predict(g3, newdata = resid_w1, BLUE = TRUE)
blue3$blue.res <- resid_w1$residuals1 - blue3$gau.pred
gau.box <- boxplot(blue3$blue.res, main = "GLS residuals for Gau model")
summary(blue3$blue.res)

rm(resid_w1, v, v.fit1, v.fit2, v.fit3, coff,
   blue1, blue2, blue3, g1, g2, g3, exp.box, sph.box, gau.box)

#### LISA setting ----

dataset <- read.csv("wave1.csv", header=TRUE, stringsAsFactors=FALSE)
regions.raw <- merge(dataset, nuts_polyg[,c(1,7)], by="NUTS")
regions <- st_as_sf(regions.raw[,c('NUTS', 'geometry')])

# Queen criterion defines neighbors as spatial units sharing a common edge/vertex
queen_w <- queen_weights(regions)
summary(queen_w)
#weights_sparsity(queen_w)  # In summary
#get_neighbors(queen_w, idx = 1)  # Gets neighbors of 1st element, can check for each
#spatial_lag(queen_w, dataset['Cases_density_1'])  # Checks spatial lag of variable

# Rook criterion defines neighbors by the existence of a common edge between two spatial units
#rook_w <- rook_weights(regions)
#summary(rook_w)  # Not very different from Queen

# Distance-based: Get optimal critical distance to get neighbors
#dist_thres <- min_distthreshold(regions)  # Everyone has at least 1 neighbor
#dist_w <- distance_weights(regions, dist_thres)
#summary(dist_w)  # MANY neighbors

# Distance-based: KNN
#knn_w <- knn_weights(regions, 5)
#summary(knn_w)

### Wave 1 residuals ----

par(mfrow=c(1,1))
par(mar=c(1,2,4,2)+0.1)  # BLTR

lisa_q <- local_moran(queen_w, data.frame(regions.raw$residuals1))  # Default: 999 permutations
#fdr <- lisa_fdr(lisa_q, 0.05)  # False Discovery Rate, can use as cutoff for clusters

regions.raw$clusters <- lisa_clusters(lisa_q)
regions <- merge(data.frame(nuts_polyg), data.frame(regions.raw), by="NUTS", all=TRUE)
regions$clusters <- replace(regions$clusters, is.na(regions$clusters), 5)

lisa_colors <- lisa_colors(lisa_q)[c(1:5, 7, 6)]
lisa_labels <- lisa_labels(lisa_q)

plot(regions$geometry.x, 
     col=sapply(regions$clusters, function(x){return(lisa_colors[[x+1]])}), 
     border = "#333333", lwd=0.2, xlim=c(-20,30), ylim=c(25,60))
title(main = "Local Moran Map of 1st wave residuals")
legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex=0.9)

#lisa_r <- local_moran(rook_w, data.frame(regions.raw$residuals1))  # Default: 999 permutations
#fdr <- lisa_fdr(lisa, 0.05)  # False Discovery Rate, can use as cutoff for clusters

#regions.raw$clusters <- lisa_clusters(lisa_r)
#regions <- merge(data.frame(nuts_polyg), data.frame(regions.raw), by="NUTS", all=TRUE)
#regions$clusters <- replace(regions$clusters, is.na(regions$clusters), 5)

#lisa_colors <- lisa_colors(lisa_r)[c(1:5, 7, 6)]
#lisa_labels <- lisa_labels(lisa_r)

#plot(regions$geometry.x, col=sapply(regions$clusters, function(x){return(lisa_colors[[x+1]])}), border = "#333333", lwd=0.2, xlim=c(-20,30), ylim=c(25,60))
#title(main = "Local Moran Map of 1st wave residuals\n Rook neigbors criterion")
#legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex=0.8)

#lisa_d <- local_moran(dist_w, dataset['Cases_density_1'])  # Default: 999 permutations
#fdr <- lisa_fdr(lisa, 0.05)  # False Discovery Rate, can use as cutoff for clusters

#regions.raw$clusters <- lisa_clusters(lisa_d)
#regions <- merge(data.frame(nuts_polyg), data.frame(regions.raw), by="NUTS", all=TRUE)
#regions$clusters <- replace(regions$clusters, is.na(regions$clusters), 5)

#lisa_colors <- lisa_colors(lisa_d)[c(1:5, 7, 6)]
#lisa_labels <- lisa_labels(lisa_d)

#plot(regions$geometry.x, col=sapply(regions$clusters, function(x){return(lisa_colors[[x+1]])}), border = "#333333", lwd=0.2, xlim=c(-20,30), ylim=c(25,60))
#title(main = "Local Moran Map of 1st wave cases density\n Distance-based criterion")
#legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex=0.8)

#lisa_knn <- local_moran(knn_w, dataset['Cases_density_1'])  # Default: 999 permutations
#fdr <- lisa_fdr(lisa, 0.05)  # False Discovery Rate, can use as cutoff for clusters

#regions.raw$clusters <- lisa_clusters(lisa_knn)
#regions <- merge(data.frame(nuts_polyg), data.frame(regions.raw), by="NUTS", all=TRUE)
#regions$clusters <- replace(regions$clusters, is.na(regions$clusters), 5)

#lisa_colors <- lisa_colors(lisa_knn)[c(1:5, 7, 6)]
#lisa_labels <- lisa_labels(lisa_knn)

#plot(regions$geometry.x, col=sapply(regions$clusters, function(x){return(lisa_colors[[x+1]])}), border = "#333333", lwd=0.2, xlim=c(-20,30), ylim=c(25,60))
#title(main = "Local Moran Map of 1st wave cases density\n KNN criterion (K=5)")
#legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex=0.8)

### Wave 1 responses ----

par(mfrow=c(1,1))

lisa_q <- local_moran(queen_w, data.frame(regions.raw$response1))  # Default: 999 permutations

regions.raw$clusters <- lisa_clusters(lisa_q)
regions <- merge(data.frame(nuts_polyg), data.frame(regions.raw), by="NUTS", all=TRUE)
regions$clusters <- replace(regions$clusters, is.na(regions$clusters), 5)

lisa_colors <- lisa_colors(lisa_q)[c(1:5, 7, 6)]
lisa_labels <- lisa_labels(lisa_q)

plot(regions$geometry.x, 
     col=sapply(regions$clusters, function(x){return(lisa_colors[[x+1]])}), 
     border = "#333333", lwd=0.2, xlim=c(-20,30), ylim=c(25,60))
title(main = "Local Moran Map of 1st wave response")
legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex=0.9)

#lisa_r <- local_moran(rook_w, data.frame(regions.raw$response1))  # Default: 999 permutations

#regions.raw$clusters <- lisa_clusters(lisa_r)
#regions <- merge(data.frame(nuts_polyg), data.frame(regions.raw), by="NUTS", all=TRUE)
#regions$clusters <- replace(regions$clusters, is.na(regions$clusters), 5)

#lisa_colors <- lisa_colors(lisa_r)[c(1:5, 7, 6)]
#lisa_labels <- lisa_labels(lisa_r)

#plot(regions$geometry.x, col=sapply(regions$clusters, function(x){return(lisa_colors[[x+1]])}), border = "#333333", lwd=0.2, xlim=c(-20,30), ylim=c(25,60))
#title(main = "Local Moran Map of 1st wave response\n Rook neigbors criterion")
#legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex=0.8)

### Wave 1 cases ----

par(mfrow=c(1,1))

lisa_q <- local_moran(queen_w, data.frame(regions.raw$Cases_density_1))  # Default: 999 permutations

regions.raw$clusters <- lisa_clusters(lisa_q)
regions <- merge(data.frame(nuts_polyg), data.frame(regions.raw), by="NUTS", all=TRUE)
regions$clusters <- replace(regions$clusters, is.na(regions$clusters), 5)

lisa_colors <- lisa_colors(lisa_q)[c(1:5, 7, 6)]
lisa_labels <- lisa_labels(lisa_q)

plot(regions$geometry.x, 
     col=sapply(regions$clusters, function(x){return(lisa_colors[[x+1]])}), 
     border = "#333333", lwd=0.2, xlim=c(-20,30), ylim=c(25,60))
title(main = "Local Moran Map of 1st wave cases density")
legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex=0.9)

#lisa_r <- local_moran(rook_w, data.frame(regions.raw$Cases_density_1))  # Default: 999 permutations

#regions.raw$clusters <- lisa_clusters(lisa_r)
#regions <- merge(data.frame(nuts_polyg), data.frame(regions.raw), by="NUTS", all=TRUE)
#regions$clusters <- replace(regions$clusters, is.na(regions$clusters), 5)

#lisa_colors <- lisa_colors(lisa_r)[c(1:5, 7, 6)]
#lisa_labels <- lisa_labels(lisa_r)

#plot(regions$geometry.x, col=sapply(regions$clusters, function(x){return(lisa_colors[[x+1]])}), border = "#333333", lwd=0.2, xlim=c(-20,30), ylim=c(25,60))
#title(main = "Local Moran Map of 1st wave cases density\n Rook neigbors criterion")
#legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex=0.8)

#### Wave 2 ----

setwd('/home/fpjaa/Documents/GitHub/geostats-covid/Wave2')

dataset$response2 <- (dataset$Cases_density_2 - mean(dataset$Cases_density_2))/sd(dataset$Cases_density_2)
#(dataset$Cases_density_2)^(1/3)
#sqrt(dataset$Cases_density_2)

# Build the matrix of predictors
options(na.action='na.omit')
#options(na.action='na.pass')
x <- model.matrix(response2 ~ #Air_passengers+
                    Hospital_beds+
                    Death_rate+
                    Compensation_of_employees+
                    Deaths+
                    Early_leavers_from_ed.+
                    Employment_worked_hrs.+
                    Farm_labour_force+
                    Health_personnel+
                    Respiratory_discharges+
                    Life_expectancy+
                    Longterm_care_beds+
                    Gross_domestic_product+
                    Ed._participation+
                    Population_density+
                    Population+
                    Pupils_enrolled+
                    GVA_growth+
                    Vehicles+
                    Tertiary_ed._students+
                    Unemployment_rate+
                    Utilised_agricultural_area+
                    NEET_rate, data=dataset)[,-1]

# Build the vector of response
y <- dataset$response2

# Let's set a grid of candidate lambda's for the estimate
lambda.grid <- c(0,10^seq(2,-3,length=100))
fit.lasso <- glmnet(x,y, lambda = lambda.grid) # default: alpha=1 -> lasso

# Let's set lambda via cross validation
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) # default: 10-fold CV

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

par(mfrow=c(1,1))
par(mar=c(4,6,4,4)+0.1)  # BLTR
plot(fit.lasso, xvar='lambda', label=TRUE, col = rainbow(dim(x)[2]))
labs <- gsub("_", " ", dimnames(x)[[2]])
legend('topright', labs, col =  rainbow(dim(x)[2]), lty=1, cex=0.7, ncol=2)
abline(v = log(bestlam.lasso), col = 'black', lty = 3)

plot(cv.lasso)

coeffs.table <- coeff2dt(fitobject = cv.lasso, s = 'lambda.min')

barplot(coeffs.table$coefficient[1:(length(coeffs.table[,1])-1)],
        col = rainbow(length(coeffs.table[,1])-1),
        main = "Coefficients of selected model")
labs <- gsub("_", " ", coeffs.table$name[1:(length(coeffs.table[,1])-1)])
legend('topright', labs, col =  rainbow(length(coeffs.table[,1])-1), lty=1, cex=0.8, ncol=2)

#### Wave 2 residuals -----------------

fitted_values <- predict(cv.lasso, x, s = 'lambda.min')

dataset$residuals2 <- y - fitted_values

shapiro.test(dataset$residuals2)

par(mar=c(6,6,4,4)+0.1)  # BLTR

hist(dataset$residuals2, breaks = 15,
     main = "Histogram of model residuals",
     xlab = "Residuals")

qqnorm(dataset$residuals2)
qqline(dataset$residuals2)

plot(x=fitted_values, y=dataset$residuals2,
     main="Residuals vs Fitted", xlab="Fitted", ylab="Residuals")
abline(h=0, col = 'black', lty = 3)

std.res <- (dataset$residuals2 - mean(dataset$residuals2))/sd(dataset$residuals2)
plot(x=fitted_values, y=sqrt(abs(std.res)),
     main="Scale-Location", xlab="Fitted", ylab="Sqrt. Abs. Std. Res.")
abline(h=mean(sqrt(abs(std.res))), col = 'red', lty = 3)

H <- x %*% solve(t(x) %*% x) %*% t(x)
plot(x=diag(H), y=std.res,
     main="Residuals vs Leverage", xlab="Leverage", ylab="Std. Residuals")

#find R-Squared
sst <- sum((y - mean(y))^2)
sse <- sum((fitted_values - y)^2)
rsq <- 1 - sse/sst
rsq

resid_w2 <- dataset[, c('NUTS', 'residuals2', 'Latitude', 'Longitude')]

# Map plot
# Residuals configuration
mybreaks <- c(min(resid_w2$residuals2, na.rm = TRUE)-0.0001,
              min(resid_w2$residuals2, na.rm = TRUE)/2,
              0,
              max(resid_w2$residuals2, na.rm = TRUE)/2,
              max(resid_w2$residuals2, na.rm = TRUE)+0.0001)
my_colors <- c('#0000ff', '#6495ed', '#f08080', '#ff0000')

nuts_polyg_tagged <- merge(nuts_polyg[,c(1,7)], resid_w2[,c('NUTS', 'residuals2')], by="NUTS", all=TRUE)
tags <- cut(nuts_polyg_tagged$residuals2, mybreaks)
mycolourscheme <- my_colors[findInterval(nuts_polyg_tagged$residuals2, vec = mybreaks)]

# Wave 2 plot
par(mfrow=c(1,1))
par(mar=c(2,4,2,4)+0.1)  # BLTR
plot(nuts_polyg_tagged$geometry, col = mycolourscheme,
     xlim=c(-18,20), ylim=c(25,63))
legend("topleft", legend = levels(tags), col = my_colors, pch=1, cex=0.9,
       title="Residuals (wave 2)")

# Transformed response configuration
mybreaks <- seq(min(dataset$response2)-0.0001,
                max(dataset$response2)+0.0001,
                length.out = 5)
my_colors <- heat.colors(5, rev=TRUE)

nuts_polyg_tagged <- merge(nuts_polyg[,c(1,7)], dataset[,c('NUTS', 'response2')], by="NUTS", all=TRUE)
tags <- cut(nuts_polyg_tagged$response2, mybreaks)
mycolourscheme <- my_colors[findInterval(nuts_polyg_tagged$response2, vec = mybreaks)]

# Wave 2 plot
par(mfrow=c(1,1))
par(mar=c(2,4,2,4)+0.1)  # BLTR
plot(nuts_polyg_tagged$geometry, col = mycolourscheme,
     xlim=c(-18,20), ylim=c(25,63))
legend("topleft", legend = levels(tags), col = my_colors, pch=1, cex=0.9,
       title="Response (wave 2)")

resid_w2$Latitude <- as.numeric(resid_w2$Latitude)
resid_w2$Longitude <- as.numeric(resid_w2$Longitude)
resid_w2$residuals2 <- as.numeric(resid_w2$residuals2)

# Extract data in case of anisotropy
write.csv(dataset[, c('NUTS', 'Latitude', 'Longitude', 'Cases_density_2', 'response2', 'residuals2')], 'wave2.csv')

rm(coeffs.table, cv.lasso, fit.lasso, fitted_values, x, y,
   bestlam.lasso, lambda.grid, sse, sst, rsq, std.res,
   nuts_polyg_tagged, labs, my_colors, mybreaks, mycolourscheme, tags)

#### Variogram modeling ----

resid_w2 <- read.csv("wave2.csv", header=TRUE, stringsAsFactors=FALSE)
resid_w2 <- resid_w2[, c('Latitude', 'Longitude', 'residuals2')]
resid_w2$Latitude <- as.numeric(resid_w2$Latitude)
resid_w2$Longitude <- as.numeric(resid_w2$Longitude)
resid_w2$residuals1 <- as.numeric(resid_w2$residuals2)
coordinates(resid_w2) <- c('Latitude','Longitude')
proj4string(resid_w2) <- CRS("+init=epsg:4326")
#resid_proj <- spTransform(resid_w2, CRS("+proj=longlat +datum=WGS84"))

plot(variogram(residuals2 ~ 1, data=resid_w2), pch=19, main = 'Sample Variogram')

#plot(variogram(residuals2 ~ Longitude, data=resid_w2), pch=19, main = 'Residual Variogram')

plot(variogram(residuals2 ~ Latitude, data=resid_w2), pch=19, main = 'Residual Variogram')

coff <- 1100  # Second best: 1300

plot(variogram(residuals2 ~ 1, data=resid_w2,
               cutoff = coff, width = coff/15), pch=19, main = paste('Sample Variogram, cutoff =',coff))

plot(variogram(residuals2 ~ Latitude, data=resid_w2,
               cutoff = coff, width = coff/15), pch=19, main = paste('Residual Variogram, cutoff =',coff))

plot(variogram(residuals2 ~ Latitude, data=resid_w2,
               alpha = c(0, 45, 90, 135),
               cutoff = coff, width = coff/15), pch=19, main = 'Directional Residual Variogram')

#plot(variogram(residuals2 ~ Latitude, data=resid_w2, cutoff = coff, width = coff/15, map = TRUE))

## weighted least squares fitting a variogram model to the sample variogram
## STEPS:
## 1) choose a suitable model
v <- variogram(residuals2 ~ Latitude, data=resid_w2,
               cutoff = coff, width = coff/15)

#plot(v)
#vgm()
## 2) choose suitable initial values for partial sill, range & nugget
v.fit1 <- fit.variogram(v, vgm(1, "Exp", 500, 500))
v.fit2 <- fit.variogram(v, vgm(1, "Sph", 500, 500))
v.fit3 <- fit.variogram(v, vgm(1, "Gau", 500, 500))

## 3) fit the model using one of the possible fitting criteria

plot(v, v.fit1, pch = 19, main="Exponential model")
plot(v, v.fit2, pch = 19, main="Spherical model")
plot(v, v.fit3, pch = 19, main="Gaussian model")

## Problem: Anisotropy: alpha = 0, 45, 90, 135

v <- variogram(residuals2 ~ Latitude, data=resid_w2,
               cutoff = coff, width = coff/15, alpha = c(0, 45, 90, 135))

plot(v, v.fit1, pch = 19, main="Exponential model")
plot(v, v.fit2, pch = 19, main="Spherical model")
plot(v, v.fit3, pch = 19, main="Gaussian model")

# Goodness of Fit: Residual sum of squares, wins Sph
c(attributes(v.fit1)$SSErr, attributes(v.fit2)$SSErr, attributes(v.fit3)$SSErr)

# Make sure they converged
c(attributes(v.fit1)$singular, attributes(v.fit2)$singular, attributes(v.fit3)$singular)

# calculate generalised least squares residuals
par(mfrow = c(1,3))

g1 <- gstat(NULL, "exp", residuals2 ~ Latitude,
            data = resid_w2, model = v.fit1)
blue1 <- predict(g1, newdata = resid_w2, BLUE = TRUE)
blue1$blue.res <- resid_w2$residuals2 - blue1$exp.pred
exp.box <- boxplot(blue1$blue.res, main = "GLS residuals for Exp model")
summary(blue1$blue.res)

g2 <- gstat(NULL, "sph", residuals2 ~ Latitude,
            data = resid_w2, model = v.fit2)
blue2 <- predict(g2, newdata = resid_w2, BLUE = TRUE)
blue2$blue.res <- resid_w2$residuals2 - blue2$sph.pred
sph.box <- boxplot(blue2$blue.res, main = "GLS residuals for Sph model")
summary(blue2$blue.res)

g3 <- gstat(NULL, "gau", residuals2 ~ Latitude,
            data = resid_w2, model = v.fit3)
blue3 <- predict(g3, newdata = resid_w2, BLUE = TRUE)
blue3$blue.res <- resid_w2$residuals2 - blue3$gau.pred
gau.box <- boxplot(blue3$blue.res, main = "GLS residuals for Gau model")
summary(blue3$blue.res)

rm(resid_w2, v, v.fit1, v.fit2, v.fit3, coff,
   blue1, blue2, blue3, g1, g2, g3, exp.box, sph.box, gau.box)

#### LISA setting ----

dataset <- read.csv("wave2.csv", header=TRUE, stringsAsFactors=FALSE)
regions.raw <- merge(dataset, nuts_polyg[,c(1,7)], by="NUTS")
regions <- st_as_sf(regions.raw[,c('NUTS', 'geometry')])

# Queen criterion defines neighbors as spatial units sharing a common edge/vertex
queen_w <- queen_weights(regions)
summary(queen_w)
#weights_sparsity(queen_w)  # In summary
#get_neighbors(queen_w, idx = 1)  # Gets neighbors of 1st element, can check for each
#spatial_lag(queen_w, dataset['Cases_density_1'])  # Checks spatial lag of variable

# Rook criterion defines neighbors by the existence of a common edge between two spatial units
#rook_w <- rook_weights(regions)
#summary(rook_w)  # Not very different from Queen

# Distance-based: Get optimal critical distance to get neighbors
#dist_thres <- min_distthreshold(regions)  # Everyone has at least 1 neighbor
#dist_w <- distance_weights(regions, dist_thres)
#summary(dist_w)  # MANY neighbors

# Distance-based: KNN
#knn_w <- knn_weights(regions, 5)
#summary(knn_w)

### Wave 2 residuals ----

par(mfrow=c(1,1))
par(mar=c(1,2,4,2)+0.1)  # BLTR

lisa_q <- local_moran(queen_w, data.frame(regions.raw$residuals2))  # Default: 999 permutations
#fdr <- lisa_fdr(lisa_q, 0.05)  # False Discovery Rate, can use as cutoff for clusters

regions.raw$clusters <- lisa_clusters(lisa_q)
regions <- merge(data.frame(nuts_polyg), data.frame(regions.raw), by="NUTS", all=TRUE)
regions$clusters <- replace(regions$clusters, is.na(regions$clusters), 5)

lisa_colors <- lisa_colors(lisa_q)[c(1:5, 7, 6)]
lisa_labels <- lisa_labels(lisa_q)

plot(regions$geometry.x, 
     col=sapply(regions$clusters, function(x){return(lisa_colors[[x+1]])}), 
     border = "#333333", lwd=0.2, xlim=c(-20,30), ylim=c(25,60))
title(main = "Local Moran Map of 2nd wave residuals")
legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex=0.9)

#lisa_r <- local_moran(rook_w, data.frame(regions.raw$residuals2))  # Default: 999 permutations
#fdr <- lisa_fdr(lisa, 0.05)  # False Discovery Rate, can use as cutoff for clusters

#regions.raw$clusters <- lisa_clusters(lisa_r)
#regions <- merge(data.frame(nuts_polyg), data.frame(regions.raw), by="NUTS", all=TRUE)
#regions$clusters <- replace(regions$clusters, is.na(regions$clusters), 5)

#lisa_colors <- lisa_colors(lisa_r)[c(1:5, 7, 6)]
#lisa_labels <- lisa_labels(lisa_r)

#plot(regions$geometry.x, col=sapply(regions$clusters, function(x){return(lisa_colors[[x+1]])}), border = "#333333", lwd=0.2, xlim=c(-20,30), ylim=c(25,60))
#title(main = "Local Moran Map of 2nd wave residuals\n Rook neigbors criterion")
#legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex=0.8)

#lisa_d <- local_moran(dist_w, dataset['Cases_density_1'])  # Default: 999 permutations
#fdr <- lisa_fdr(lisa, 0.05)  # False Discovery Rate, can use as cutoff for clusters

#regions.raw$clusters <- lisa_clusters(lisa_d)
#regions <- merge(data.frame(nuts_polyg), data.frame(regions.raw), by="NUTS", all=TRUE)
#regions$clusters <- replace(regions$clusters, is.na(regions$clusters), 5)

#lisa_colors <- lisa_colors(lisa_d)[c(1:5, 7, 6)]
#lisa_labels <- lisa_labels(lisa_d)

#plot(regions$geometry.x, col=sapply(regions$clusters, function(x){return(lisa_colors[[x+1]])}), border = "#333333", lwd=0.2, xlim=c(-20,30), ylim=c(25,60))
#title(main = "Local Moran Map of 1st wave cases density\n Distance-based criterion")
#legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex=0.8)

#lisa_knn <- local_moran(knn_w, dataset['Cases_density_1'])  # Default: 999 permutations
#fdr <- lisa_fdr(lisa, 0.05)  # False Discovery Rate, can use as cutoff for clusters

#regions.raw$clusters <- lisa_clusters(lisa_knn)
#regions <- merge(data.frame(nuts_polyg), data.frame(regions.raw), by="NUTS", all=TRUE)
#regions$clusters <- replace(regions$clusters, is.na(regions$clusters), 5)

#lisa_colors <- lisa_colors(lisa_knn)[c(1:5, 7, 6)]
#lisa_labels <- lisa_labels(lisa_knn)

#plot(regions$geometry.x, col=sapply(regions$clusters, function(x){return(lisa_colors[[x+1]])}), border = "#333333", lwd=0.2, xlim=c(-20,30), ylim=c(25,60))
#title(main = "Local Moran Map of 1st wave cases density\n KNN criterion (K=5)")
#legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex=0.8)

### Wave 2 responses ----

par(mfrow=c(1,1))

lisa_q <- local_moran(queen_w, data.frame(regions.raw$response2))  # Default: 999 permutations

regions.raw$clusters <- lisa_clusters(lisa_q)
regions <- merge(data.frame(nuts_polyg), data.frame(regions.raw), by="NUTS", all=TRUE)
regions$clusters <- replace(regions$clusters, is.na(regions$clusters), 5)

lisa_colors <- lisa_colors(lisa_q)[c(1:5, 7, 6)]
lisa_labels <- lisa_labels(lisa_q)

plot(regions$geometry.x, 
     col=sapply(regions$clusters, function(x){return(lisa_colors[[x+1]])}), 
     border = "#333333", lwd=0.2, xlim=c(-20,30), ylim=c(25,60))
title(main = "Local Moran Map of 2nd wave response")
legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex=0.9)

#lisa_r <- local_moran(rook_w, data.frame(regions.raw$response2))  # Default: 999 permutations

#regions.raw$clusters <- lisa_clusters(lisa_r)
#regions <- merge(data.frame(nuts_polyg), data.frame(regions.raw), by="NUTS", all=TRUE)
#regions$clusters <- replace(regions$clusters, is.na(regions$clusters), 5)

#lisa_colors <- lisa_colors(lisa_r)[c(1:5, 7, 6)]
#lisa_labels <- lisa_labels(lisa_r)

#plot(regions$geometry.x, col=sapply(regions$clusters, function(x){return(lisa_colors[[x+1]])}), border = "#333333", lwd=0.2, xlim=c(-20,30), ylim=c(25,60))
#title(main = "Local Moran Map of 2nd wave response\n Rook neigbors criterion")
#legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex=0.8)

### Wave 2 cases ----

par(mfrow=c(1,1))

lisa_q <- local_moran(queen_w, data.frame(regions.raw$Cases_density_2))  # Default: 999 permutations

regions.raw$clusters <- lisa_clusters(lisa_q)
regions <- merge(data.frame(nuts_polyg), data.frame(regions.raw), by="NUTS", all=TRUE)
regions$clusters <- replace(regions$clusters, is.na(regions$clusters), 5)

lisa_colors <- lisa_colors(lisa_q)[c(1:5, 7, 6)]
lisa_labels <- lisa_labels(lisa_q)

plot(regions$geometry.x, 
     col=sapply(regions$clusters, function(x){return(lisa_colors[[x+1]])}), 
     border = "#333333", lwd=0.2, xlim=c(-20,30), ylim=c(25,60))
title(main = "Local Moran Map of 2nd wave cases density")
legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex=0.9)

#lisa_r <- local_moran(rook_w, data.frame(regions.raw$Cases_density_2))  # Default: 999 permutations

#regions.raw$clusters <- lisa_clusters(lisa_r)
#regions <- merge(data.frame(nuts_polyg), data.frame(regions.raw), by="NUTS", all=TRUE)
#regions$clusters <- replace(regions$clusters, is.na(regions$clusters), 5)

#lisa_colors <- lisa_colors(lisa_r)[c(1:5, 7, 6)]
#lisa_labels <- lisa_labels(lisa_r)

#plot(regions$geometry.x, col=sapply(regions$clusters, function(x){return(lisa_colors[[x+1]])}), border = "#333333", lwd=0.2, xlim=c(-20,30), ylim=c(25,60))
#title(main = "Local Moran Map of 2nd wave cases density\n Rook neigbors criterion")
#legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee", cex=0.8)
