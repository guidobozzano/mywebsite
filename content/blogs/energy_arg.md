---
categories:  
- ""    #the front matter should be like the one found in, e.g., blog2.md. It cannot be like the normal Rmd we used
- ""
date: "2023-06-15"
description: Energy usage by type in Argentina # the title that will show up once someone gets to this page
draft: false
image: energy1.jpg # save picture in \static\img\blogs. Acceptable formats= jpg, jpeg, or png . Your iPhone pics wont work

keywords: ""
slug: energy_arg # slug is the shorthand URL address... no spaces plz
title: Energy usage by type in Argentina
---



# Exploring energy usage by Country

We will get energy data from the Our World in Data website, and CO2 and GDP per capita emissions from the World Bank, using the `wbstats`package.


```r
# Download electricity data
url <- "https://nyc3.digitaloceanspaces.com/owid-public/data/energy/owid-energy-data.csv"

energy <- read_csv(url) %>% 
  filter(year >= 1990) %>% 
  drop_na(iso_code) %>% 
  select(1:3,
         biofuel = biofuel_electricity,
         coal = coal_electricity,
         gas = gas_electricity,
         hydro = hydro_electricity,
         nuclear = nuclear_electricity,
         oil = oil_electricity,
         other_renewable = other_renewable_exc_biofuel_electricity,
         solar = solar_electricity,
         wind = wind_electricity, 
         electricity_demand,
         electricity_generation,
         net_elec_imports,	# Net electricity imports, measured in terawatt-hours
         energy_per_capita,	# Primary energy consumption per capita, measured in kilowatt-hours	Calculated by Our World in Data based on BP Statistical Review of World Energy and EIA International Energy Data
         energy_per_gdp,	# Energy consumption per unit of GDP. This is measured in kilowatt-hours per 2011 international-$.
         per_capita_electricity, #	Electricity generation per capita, measured in kilowatt-hours
  ) 

# Download data for C02 emissions per capita https://data.worldbank.org/indicator/EN.ATM.CO2E.PC
co2_percap <- wb_data(country = "countries_only", 
                      indicator = "EN.ATM.CO2E.PC", 
                      start_date = 1990, 
                      end_date = 2022,
                      return_wide=FALSE) %>% 
  filter(!is.na(value)) %>% 
  #drop unwanted variables
  select(-c(unit, obs_status, footnote, last_updated)) %>% 
  rename(year = date,
         co2percap = value)


# Download data for GDP per capita  https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD
gdp_percap <- wb_data(country = "countries_only", 
                      indicator = "NY.GDP.PCAP.PP.KD", 
                      start_date = 1990, 
                      end_date = 2022,
                      return_wide=FALSE) %>% 
  filter(!is.na(value)) %>% 
  #drop unwanted variables
  select(-c(unit, obs_status, footnote, last_updated)) %>% 
  rename(year = date,
         GDPpercap = value)
```


```r
##1

# Filter data for Argentina since 2000
argentina_energy <- energy %>%
  filter(iso_code == "ARG", year >= 2000)

# Select the relevant columns for electricity generation
argentina_generation <- argentina_energy %>%
  select(year, coal, gas, hydro, nuclear, oil, other_renewable, solar, wind)

# Convert data from wide to long format
argentina_long <- argentina_generation %>%
  pivot_longer(cols = -year, names_to = "source", values_to = "generation")

# Create a stacked area chart for electricity generation
ggplot(argentina_long, aes(x = year, y = generation, fill = source)) +
  geom_area(colour = "grey90", alpha = 0.5, position = "fill") +
  labs(title = "Electricity Generation in Argentina (2000 onwards)",
       x = "Year",
       y = "Generation",
       fill = "Energy Source") +
  scale_fill_discrete()
```

<img src="/blogs/energy_arg_files/figure-html/unnamed-chunk-3-1.png" width="672" />


```r
##2

# Merge CO2 per capita and GDP per capita data using ISO code as the key
co2_gdp <- left_join(co2_percap, gdp_percap, by = c("iso2c","iso3c","country","year"))

co2_gdpf <- co2_gdp %>% 
  filter(!is.na(co2percap),!is.na(GDPpercap)) #Exclude missing values from the co2percap and GDPpercap variables

argentina_data <- co2_gdpf %>%
  filter(country == "Argentina") %>%
  mutate(year = as.factor(year))

ggplot(data = argentina_data, aes(x = GDPpercap, y = co2percap)) +
  geom_point() +
  geom_text(aes(label = year), vjust = -0.5, hjust = 0.5, size = 3) +
  labs(title = "CO2 per Capita vs GDP per Capita in Argentina",
       x = "GDP per Capita",
       y = "CO2 per Capita") +
  theme_economist()
```

<img src="/blogs/energy_arg_files/figure-html/unnamed-chunk-4-1.png" width="672" />


```r
##3

# Merge CO2 per capita and GDP per capita data using ISO code as the key
energ_gdp <- left_join(energy, gdp_percap, by = c("country","year"))

energ_gdpf <- energ_gdp %>% 
  filter(!is.na(energy_per_capita),!is.na(GDPpercap)) #Exclude missing values from the energy_per_capita and GDPpercap variables

argentina_data <- energ_gdpf %>%
  filter(country == "Argentina") %>%
  mutate(year = as.factor(year))

# Create the scatter plot
ggplot(data = argentina_data, aes(x = energy_per_capita, y = GDPpercap)) +
  geom_point() +
  geom_text(aes(label = year), vjust = -0.5, hjust = 0.5, size = 3) +
  labs(title = "Electricity Usage per Capita vs GDP per Capita",
       x = "Electricity Usage (kWh) per Capita/Day",
       y = "GDP per Capita") +
  theme_economist()
```

<img src="/blogs/energy_arg_files/figure-html/unnamed-chunk-5-1.png" width="672" />

The information above displays what type of energy Argentina uses per year, how do C02 per capita and Electricity usage per capita compare vs GDP per capita. 

The last few years in Argentina have been quie interesting in terms of energy usage. On the one hand with the exploration of 'Vaca Muerta' the Gas and Oil usage has not only increased but also we have began exporting. On the other hand, we can see the beginning of other renewable energy.

On the last graph we can see how Electricity usage has decreased as well as GDP per Capita. In essence this relates to the crisis the country has been going through in terms of fast currency depreciation, lower investments and lower GDP growth (+ the huge negative impact of COVID-19). This negative impacts have translated in an overall reduction in both GDP and Electricity usage since households are struggling to make ends meet and are unable to afford usage given the lower wages that are still prevalent in the country.
