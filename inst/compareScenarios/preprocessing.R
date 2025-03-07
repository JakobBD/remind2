# reference models for historical ----

# Sometimes it is necessary to choose a single model for the historical data,
# e.g., calculating per capita variables. These reference models are defined here.
histRefModel <- c(
  "Population" = "WDI",
  "GDP|PPP pCap" = "James_IMF"
)

options(mip.histRefModel = histRefModel) # nolint

# calculate pCap variables ----

# For all variables in following table, add a new variable to data with the name
# "OldName pCap". Calculate its value by
#     OldValue * conversionFactor # nolint
# and set its unit to newUnit.
# The new variable "OldName pCap" will be available in the plot sections.
pCapVariables <- tribble(
  ~variable, ~newUnit, ~conversionFactor,
  "GDP|PPP", "kUS$2017/pCap", 1e6, # creates "GDP|PPP pCap" which is equal to reported variable "GDP|per capita|PPP"
  "GDP|MER", "kUS$2017/pCap", 1e6, # creates "GDP|MER pCap" which is equal to reported variable "GDP|per capita|MER"
  "FE", "GJ/yr/pCap", 1e9,
  "FE|CDR", "GJ/yr/pCap", 1e9,
  "FE|Transport", "GJ/yr/pCap", 1e9,
  "FE|Buildings", "GJ/yr/pCap", 1e9,
  "FE|Industry", "GJ/yr/pCap", 1e9,
  "FE|Buildings|non-Heating|Electricity|Conventional", "GJ/yr/pCap", 1e9,
  "FE|Buildings|Heating|Electricity|Heat pump", "GJ/yr/pCap", 1e9,
  "FE|Buildings|Heating|District Heating", "GJ/yr/pCap", 1e9,
  "FE|Buildings|Heating|Electricity|Resistance", "GJ/yr/pCap", 1e9,
  "FE|Buildings|Heating|Hydrogen", "GJ/yr/pCap", 1e9,
  "FE|Buildings|Heating|Gases", "GJ/yr/pCap", 1e9,
  "FE|Buildings|Heating|Liquids", "GJ/yr/pCap", 1e9,
  "FE|Buildings|Heating|Solids", "GJ/yr/pCap", 1e9,
  "FE|Buildings|Heating", "GJ/yr/pCap", 1e9,
  "FE|Buildings|Appliances and Light", "GJ/yr/pCap", 1e9,
  "FE|Buildings|Cooking and Water", "GJ/yr/pCap", 1e9,
  "FE|Buildings|Space Cooling", "GJ/yr/pCap", 1e9,
  "FE|Buildings|Space Heating", "GJ/yr/pCap", 1e9,
  "UE|Buildings", "GJ/yr/pCap", 1e9,
  "UE|Buildings|non-Heating|Electricity|Conventional", "GJ/yr/pCap", 1e9,
  "UE|Buildings|Heating|Electricity|Heat pump", "GJ/yr/pCap", 1e9,
  "UE|Buildings|Heating|District Heating", "GJ/yr/pCap", 1e9,
  "UE|Buildings|Heating|Electricity|Resistance", "GJ/yr/pCap", 1e9,
  "UE|Buildings|Heating|Hydrogen", "GJ/yr/pCap", 1e9,
  "UE|Buildings|Heating|Gases", "GJ/yr/pCap", 1e9,
  "UE|Buildings|Heating|Liquids", "GJ/yr/pCap", 1e9,
  "UE|Buildings|Heating|Solids", "GJ/yr/pCap", 1e9,
  "UE|Buildings|Heating", "GJ/yr/pCap", 1e9,
  "ES|Buildings|Floor Space", "m2/pCap", 1e9,
  "ES|Transport|Pass", "k pkm/yr/pCap", 1e6, # use kilo-passenger-kilometer to prevent too large numbers in the plots
  "ES|Transport|Pass|Road|LDV", "k pkm/yr/pCap", 1e6,
  "ES|Transport|Pass|non-LDV", "k pkm/yr/pCap", 1e6,
  "ES|Transport|Pass|Road|LDV|BEV", "k pkm/yr/pCap", 1e6,
  "ES|Transport|Pass|Road|LDV|FCEV", "k pkm/yr/pCap", 1e6,
  "ES|Transport|Pass|Road|LDV|Gases", "k pkm/yr/pCap", 1e6,
  "ES|Transport|Pass|Road|LDV|Hybrid electric", "k pkm/yr/pCap", 1e6,
  "ES|Transport|Pass|Road|LDV|Liquids", "k pkm/yr/pCap", 1e6,
  "ES|Transport|Freight", "k tkm/yr/pCap", 1e6,
  "ES|Transport|Freight|Road|BEV", "k tkm/yr/pCap", 1e6,
  "ES|Transport|Freight|Road|FCEV", "k tkm/yr/pCap", 1e6,
  "ES|Transport|Freight|Road|Gases", "k tkm/yr/pCap", 1e6,
  "ES|Transport|Freight|Road|Liquids", "k tkm/yr/pCap", 1e6,
  "Stock|Transport|Pass|Road|LDV", "cars per 1000 people", 1e9,
  "Stock|Transport|Pass|Road|LDV|BEV", "cars per 1000 people", 1e9,
  "Stock|Transport|Pass|Road|LDV|Hybrid electric", "cars per 1000 people", 1e9,
  "Stock|Transport|Pass|Road|LDV|FCEV", "cars per 1000 people", 1e9,
  "Stock|Transport|Pass|Road|LDV|Gases", "cars per 1000 people", 1e9,
  "Stock|Transport|Pass|Road|LDV|Liquids", "cars per 1000 people", 1e9,
  "Emi|GHG", "t CO2eq/yr/pCap", 1e6,
  "Emi|GHG|Gross|Energy|Supply|Electricity", "t CO2eq/yr/pCap", 1e6,
  "Emi|GHG|Gross|Energy|Supply|Non-electric", "t CO2eq/yr/pCap", 1e6,
  "Emi|GHG|Energy|Demand|Transport", "t CO2eq/yr/pCap", 1e6,
  "Emi|GHG|Energy|Demand|Buildings", "t CO2eq/yr/pCap", 1e6,
  "Emi|GHG|Gross|Energy|Demand|Industry", "t CO2eq/yr/pCap", 1e6,
  "Emi|GHG|Industrial Processes", "t CO2eq/yr/pCap", 1e6,
  "Emi|GHG|Agriculture", "t CO2eq/yr/pCap", 1e6,
  "Emi|GHG|Land-Use Change", "t CO2eq/yr/pCap", 1e6,
  "Emi|GHG|Waste", "t CO2eq/yr/pCap", 1e6,
  "Emi|CO2|CDR", "t CO2eq/yr/pCap", 1e6,
  "Emi|CO2|CDR|BECCS", "t CO2eq/yr/pCap", 1e6,
  "Emi|CO2|CDR|Synthetic Fuels CCS", "t CO2eq/yr/pCap", 1e6,
  "Emi|CO2|CDR|DACCS", "t CO2eq/yr/pCap", 1e6,
  "Emi|CO2|CDR|OAE", "t CO2eq/yr/pCap", 1e6,
  "Emi|CO2|CDR|EW", "t CO2eq/yr/pCap", 1e6,
  "Emi|CO2", "t CO2eq/yr/pCap", 1e6,
  "Emi|CO2|Land-Use Change", "t CO2eq/yr/pCap", 1e6,
  "Emi|CO2|Industrial Processes", "t CO2eq/yr/pCap", 1e6,
  "Emi|CO2|Energy|Demand|Transport", "t CO2eq/yr/pCap", 1e6,
  "Emi|CO2|Gross|Energy|Demand|Industry", "t CO2eq/yr/pCap", 1e6,
  "Emi|CO2|Energy|Demand|Buildings", "t CO2eq/yr/pCap", 1e6,
  "Emi|CO2|Gross|Energy|Supply|Non-electric", "t CO2eq/yr/pCap", 1e6,
  "Emi|CO2|Gross|Energy|Supply|Electricity", "t CO2eq/yr/pCap", 1e6)

dataPop <-
  data %>%
  filter(variable == "Population") %>%
  filter( # Choose unique Population variable per scenario.
    (scenario == "historical" & model == histRefModel["Population"]) |
      (scenario != "historical" & model == "REMIND")) %>%
  select(scenario, region, period, value) %>%
  mutate(
    population = value * 1e6, # unit originally is million, now is 1
    value = NULL)

dataPCap <-
  data %>%
  inner_join(pCapVariables, "variable") %>%
  left_join(dataPop, c("scenario", "region", "period")) %>%
  mutate(
    value = value / population * conversionFactor,
    variable = paste0(variable, " pCap"),
    varplus = paste0(varplus, " pCap"),
    unit = newUnit,
    newUnit = NULL, conversionFactor = NULL, population = NULL)

data <-
  data %>%
  bind_rows(dataPCap)

# calculate pGDP_PPP variables ----
dataGDP <-
  data %>%
  filter(variable == "GDP|PPP pCap") %>%
  filter( # Choose unique GDP|PPP pCap variable per scenario.
    (scenario == "historical" & model == histRefModel["GDP|PPP pCap"]) |
      (scenario != "historical" & model == "REMIND")) %>%
  select(scenario, region, period, value) %>%
  rename(gdp = value)

# For all variables in following table, add a new variable to data with the name
# "OldName pGDP_PPP". Calculate its value by
#     OldValue / (GDP|PPP pCap) * conversionFactor
# and set its unit to newUnit.
# The new variable "OldName pGDP_PPP" will be available in the plot sections.
pGdpVariables <- tribble(
  ~variable, ~newUnit, ~conversionFactor,
  "FE", "MJ/US$2017/pCap", 1e3,
  "FE|CDR", "MJ/US$2017/pCap", 1e3,
  "FE|Transport", "MJ/US$2017/pCap", 1e3,
  "FE|Buildings", "MJ/US$2017/pCap", 1e3,
  "FE|Industry", "MJ/US$2017/pCap", 1e3
)

dataPGdp <-
  data %>%
  inner_join(pGdpVariables, "variable") %>%
  left_join(dataGDP, c("scenario", "region", "period")) %>%
  mutate(
    value = value / gdp * conversionFactor,
    variable = paste0(variable, " pGDP_PPP"),
    varplus = paste0(varplus, " pGDP_PPP"),
    unit = newUnit,
    newUnit = NULL, conversionFactor = NULL, gdp = NULL
  )

data <-
  data %>%
  bind_rows(dataPGdp)

# backwards compatibility for https://github.com/pik-piam/remind2/pull/579
data <- data %>%
  mutate(variable = gsub("|Elec|", "|Electricity|", .data$variable, fixed = TRUE))


# remove preprocessing objects not to be used anymore ----
varNames <- c(
  "dataGDP", "dataPCap", "dataPGdp", "dataPop",
  "histRefModel", "pCapVariables", "pGdpVariables")
for (vn in varNames) if (exists(vn)) rm(list = vn)
rm(varNames)
rm(vn)
