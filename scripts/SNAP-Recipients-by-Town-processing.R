library(dplyr)
library(datapkg)
library(acs)
library(stringr)
library(reshape2)
library(data.table)
library(tidyr)
library(purrr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for SNAP Recipients by Town
# Created by Jenna Daly
# On 08/14/2018
#
##################################################################


#Get state data
geography=geo.make(state=09)
yearlist=c(2019:2019)
span = 5
col.names="pretty" 
key="ed0e58d2538fb239f51e01643745e83f380582d7"
options(scipen=999)

tables <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
races <- c("White Alone", "Black or African American Alone", "American Indian and Alaska Native Alone", 
           "Asian Alone", "Native Hawaiian and Other Pacific Islander", "Some Other Race Alone", 
           "Two or More Races", "White Alone Not Hispanic or Latino", "Hispanic or Latino")

state_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  inter_data <- data.table()
  for (j in seq_along(tables)) {
    tbl <- tables[j]
    race <- races[j]
    #needed to grab all columns for all years 
    variable =list()      
    for (k in seq_along(1:3)) {
     number = paste0("B22005", tbl, "_", sprintf("%03d",k))
     variable = c(variable, number)
     k=k+1
    }
    variable <- as.character(variable)       
    data <- acs.fetch(geography=geography, endyear=endyear, span=span, 
                      variable = variable, key=key)
    Sys.sleep(3)
    year <- data@endyear
    print(paste("Processing: ", year, race))
    year <- paste(year-4, year, sep="-")
    geo <- data@geography
    geo$NAME <- NULL
    total <- data[,1]
    total_num <- acsSum(data, c(1), "Total")
    total_received <- acsSum(data, c(2), "Received")
    percent.total.received <- divide.acs(total_received, total_num, method="proportion", verbose=T)
    total_not_received <- acsSum(data, c(3), "Not Received")    
    percent.total.not.received <- divide.acs(total_not_received, total_num, method="proportion", verbose=T)
    
    numberEstimates <- data.table(
            geo,
            estimate(total_num),  
            estimate(total_received),
            estimate(total_not_received),
            year,
            race,
            "Number",
            "SNAP Recipients"
        )
    numberMOES <- data.table(
            geo,
            standard.error(total_num) * 1.645,
            standard.error(total_received) * 1.645,
            standard.error(total_not_received) * 1.645,
            year,
            race,
            "Number",
            "Margins of Error"
        )
    numberNames <- c(
            "FIPS",
            "Total",
            "Received",
            "Not Received",
            "Year", 
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
         )
    setnames(numberEstimates, numberNames)
    setnames(numberMOES, numberNames)

    numbersData.melt <- melt(
            rbind(numberEstimates, numberMOES),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="SNAP",
            value.name="Value",
            value.factor = F
         )
    
   percentEstimate <- data.table(
                geo,
                estimate(percent.total.received),
                estimate(percent.total.not.received),
                year,
                race,
                "Percent",
                "SNAP Recipients"
            )
    percentMOES <- data.table(
                geo,
                standard.error(percent.total.received) * 1.645,
                standard.error(percent.total.not.received) * 1.645,
                year,
                race,
                "Percent",
                "Margins of Error"
            )
    percentNames <- c(
            "FIPS",
            "Received",
            "Not Received",
            "Year", 
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
        )
    setnames(percentEstimate, percentNames)
    setnames(percentMOES, percentNames)
    percentData.melt <- melt(
            rbind(percentEstimate, percentMOES),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="SNAP",
            value.name="Value",
            value.factor = F
         )    
    inter_data <- rbind(inter_data, numbersData.melt, percentData.melt)
  }
  state_data <- rbind(state_data, inter_data)
}

#Get town data
geography=geo.make(state=09, county="*", county.subdivision = "*")

town_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  inter_data <- data.table()
  for (j in seq_along(tables)) {
    tbl <- tables[j]
    race <- races[j]
    #needed to grab all columns for all years 
    variable =list()      
    for (k in seq_along(1:3)) {
     number = number=paste0("B22005", tbl, "_", sprintf("%03d",k))
     variable = c(variable, number)
     k=k+1
    }
    variable <- as.character(variable)       
    data <- acs.fetch(geography=geography, endyear=endyear, span=span, 
                      variable = variable, key=key)
    Sys.sleep(3)
    year <- data@endyear
    print(paste("Processing: ", year, race))
    year <- paste(year-4, year, sep="-")
    geo <- data@geography
    geo$county <- sprintf("%02d", geo$county)
    geo$county <- gsub("^", "090", geo$county)
    geo$FIPS <- paste0(geo$county, geo$countysubdivision)
    geo$state <- NULL
    geo$NAME <- NULL
    geo$countysubdivision <- NULL
    geo$county <- NULL     
    total <- data[,1]
    total_num <- acsSum(data, c(1), "Total")
    total_received <- acsSum(data, c(2), "Received")
    percent.total.received <- divide.acs(total_received, total_num, method="proportion", verbose=T)
    total_not_received <- acsSum(data, c(3), "Not Received")    
    percent.total.not.received <- divide.acs(total_not_received, total_num, method="proportion", verbose=T)
    
    numberEstimates <- data.table(
            geo,
            estimate(total_num),  
            estimate(total_received),
            estimate(total_not_received),
            year,
            race,
            "Number",
            "SNAP Recipients"
        )
    numberMOES <- data.table(
            geo,
            standard.error(total_num) * 1.645,
            standard.error(total_received) * 1.645,
            standard.error(total_not_received) * 1.645,
            year,
            race,
            "Number",
            "Margins of Error"
        )
    numberNames <- c(
            "FIPS",
            "Total",
            "Received",
            "Not Received",
            "Year", 
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
         )
    setnames(numberEstimates, numberNames)
    setnames(numberMOES, numberNames)

    numbersData.melt <- melt(
            rbind(numberEstimates, numberMOES),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="SNAP",
            value.name="Value",
            value.factor = F
         ) 
    
    percentEstimate <- data.table(
                geo,
                estimate(percent.total.received),
                estimate(percent.total.not.received),
                year,
                race,
                "Percent",
                "SNAP Recipients"
            )
    
    percentMOES <- data.table(
                geo,
                standard.error(percent.total.received) * 1.645,
                standard.error(percent.total.not.received) * 1.645,
                year,
                race,
                "Percent",
                "Margins of Error"
            )
    percentNames <- c(
            "FIPS",
            "Received",
            "Not Received",
            "Year", 
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
        )
    setnames(percentEstimate, percentNames)
    setnames(percentMOES, percentNames)
    percentData.melt <- melt(
            rbind(percentEstimate, percentMOES),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="SNAP",
            value.name="Value",
            value.factor = F
         )    
    inter_data <- rbind(inter_data, numbersData.melt, percentData.melt)
  }
  town_data <- rbind(town_data, inter_data)
}

#Merge in Towns by FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

town_data <- merge(towns, town_data, by="FIPS", all.x=T)

town_data <- town_data[town_data$Town != "Connecticut",]

state_data$Town <- "Connecticut"

dataset <- rbind(state_data, town_data)
dataset <- as.data.frame(dataset)

#Create total datasets (for Race = Total)
totals <- spread(dataset, Variable, Value)
totals_calc <- totals %>% 
  filter(!`Race/Ethnicity` %in% c("White Alone Not Hispanic or Latino", "Hispanic or Latino" ) & `Measure Type` == "Number")

totals_calc <- totals_calc %>% 
  group_by(Town, Year, SNAP) %>% 
  mutate(sum_est = sum(`SNAP Recipients`), 
         sum_moe = moe_sum(`Margins of Error`, `SNAP Recipients`))
totals_calc$`Race/Ethnicity` <- "All"

totals_calc <- unique(totals_calc %>% 
  select(-c(`Margins of Error`, `SNAP Recipients`)) %>% 
  rename(`SNAP Recipients` = sum_est, `Margins of Error` = sum_moe))

totals_calc <- totals_calc %>% 
  gather(New_Var, Value, 7:8) %>%  #gather value columns (Value and MOE, indices 7:8)
  unite(Combined, New_Var, SNAP) %>% #Create new column 'Combined' that takes combined New_Var and joins with original SNAP column 
  spread(Combined, Value) #spread out new names and return each value

#Calculate percents for Race = Total
totals_calc <- totals_calc %>% 
  mutate(pct_est = (`SNAP Recipients_Received` / `SNAP Recipients_Total`), 
         pct_moe = moe_prop(`SNAP Recipients_Received`, `SNAP Recipients_Total`, `Margins of Error_Received`, `Margins of Error_Total`))

totals_calc <- totals_calc %>% 
  select(FIPS, Year, `Measure Type`, `Race/Ethnicity`, Town, `SNAP Recipients_Received`, `Margins of Error_Received`, pct_est, pct_moe) %>% 
  rename(`SNAP Recipients_pct` = pct_est, `Margins of Error_pct` = pct_moe)

totals_calc <- gather(totals_calc, Variable, Value, 6:9, factor_key=FALSE)
totals_calc$`Measure Type`[grepl("pct", totals_calc$Variable)] <- "Percent"
#Remove all after "_" in Variable column
totals_calc$Variable <- gsub("\\_.*", "", totals_calc$Variable)

totals_calc <- as.data.frame(totals_calc)

#Only grab snap recipients
dataset <- dataset %>% 
  filter(SNAP == "Received") %>% 
  select(-SNAP)

dataset_final <- rbind(dataset, totals_calc)
dataset_final <- as.data.table(dataset_final)

#Final Additions, processing
dataset_final[,`:=`(
    Value = ifelse(`Measure Type` == "Number", round(Value, 2), round(Value*100, 2))
)]

#set final column order
dataset_final <- dataset_final %>% 
  select(Town, FIPS, Year, `Race/Ethnicity`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, FIPS, Year, `Race/Ethnicity`, `Measure Type`)

#Fix NaNs
dataset_final$Value[dataset_final$Value == "NaN"] <- 0

write.table(
    dataset_final,
    file.path("data", "snap-recipients-by-town-2019-only.csv"),
    sep = ",",
    row.names = F,
    na = "-9999"
)



