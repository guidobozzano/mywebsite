---
categories:  
- ""    #the front matter should be like the one found in, e.g., blog2.md. It cannot be like the normal Rmd we used
- ""
date: "2021-09-30"
description: Mass shootings analysis in USA # the title that will show up once someone gets to this page
draft: false
image: spices.jpg # save picture in \static\img\blogs. Acceptable formats= jpg, jpeg, or png . Your iPhone pics wont work

keywords: ""
slug: mass_shootings # slug is the shorthand URL address... no spaces plz
title: Mass shootings analysis in USA
---



## Obtain the data
First we begin by reading the mass_shootings csv and saving it to a table, here you can find a glimpse of that csv to understand which dimensions, data types and data is stored in each column


```
## Rows: 125
## Columns: 14
## $ case                 <chr> "Oxford High School shooting", "San Jose VTA shoo…
## $ year                 <dbl> 2021, 2021, 2021, 2021, 2021, 2021, 2020, 2020, 2…
## $ month                <chr> "Nov", "May", "Apr", "Mar", "Mar", "Mar", "Mar", …
## $ day                  <dbl> 30, 26, 15, 31, 22, 16, 16, 26, 10, 6, 31, 4, 3, …
## $ location             <chr> "Oxford, Michigan", "San Jose, California", "Indi…
## $ summary              <chr> "Ethan Crumbley, a 15-year-old student at Oxford …
## $ fatalities           <dbl> 4, 9, 8, 4, 10, 8, 4, 5, 4, 3, 7, 9, 22, 3, 12, 5…
## $ injured              <dbl> 7, 0, 7, 1, 0, 1, 0, 0, 3, 8, 25, 27, 26, 12, 4, …
## $ total_victims        <dbl> 11, 9, 15, 5, 10, 9, 4, 5, 7, 11, 32, 36, 48, 15,…
## $ location_type        <chr> "School", "Workplace", "Workplace", "Workplace", …
## $ male                 <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, T…
## $ age_of_shooter       <dbl> 15, 57, 19, NA, 21, 21, 31, 51, NA, NA, 36, 24, 2…
## $ race                 <chr> NA, NA, "White", NA, NA, "White", NA, "Black", "B…
## $ prior_mental_illness <chr> NA, "Yes", "Yes", NA, "Yes", NA, NA, NA, NA, NA, …
```
Then we generate a bar chart that identifies the number of mass shooters associated with each race category. The bars are sorted from highest to lowest and each bar shows its number.


```r
#Create a dataframe that groups by race and I renamed the duplicated race categories so that the information is collapsed into one variable per race category. Then I collapse all the others into Other. Afterwards, I reorder the information using fct_reorder and get it ready to plot it

dfmass_shootings <- mass_shootings %>% 
  group_by(race) %>% 
  mutate(race = case_when(
    race %in% c("White", "white") ~ "White", 
    race %in% c("Black", "black") ~ "Black", 
    race %in% c("Asian", "Latino", "Native American", "") ~ race,
    TRUE ~ "Other"  # Collapse other races into "Other" category
  )) %>% 
  summarise(count=n()) %>% 
  mutate(race = fct_reorder(race, count, .desc = TRUE))

# Get unique race categories
unique_races <- unique(dfmass_shootings$race) #define a unique list of categories

# Generate a color palette for each race category
num_races <- length(unique_races)
custom_colors <- brewer.pal(num_races, "Set1")

# Generate the bar chart
chart <- ggplot(dfmass_shootings, aes(x = race, y = count, fill = race)) +
  geom_bar(stat = "identity") +
  theme_economist(base_family = "ITC Officina Sans", dkpanel = TRUE) +
  scale_fill_manual(values = custom_colors) +
  geom_text(aes(label = count), vjust = 1, colour = "black", size = 4.5) +
  labs(
    title = "Number of mass shooters by race",
    subtitle = "Total number in units",
    x = "Race",
    y = "Number of mass shooters",
    fill = "Race"
  )

chart
```

<img src="/blogs/mass_shootings_files/figure-html/unnamed-chunk-3-1.png" width="672" />

Now we want to understand the frequency per location of the mass shootings, to do so we generate a boxplot visualizing the number of total victims, by type of location, but removing the Las Vegas Strip massacre outlier from the dataset.


```r
#I create dfmass_shootings and exclude the Las Vegas Strip Massacre and re plot the data using the same code as the question above

dfmass_shootings <- mass_shootings %>% 
  filter(case !="Las Vegas Strip massacre")

# Get unique race categories
unique_location <- unique(dfmass_shootings$location_type)

# Generate a color palette for each race category
num_location <- length(unique_location)
custom_colors <- rainbow(num_location)

# Generate the bar chart
chart <- ggplot(dfmass_shootings, aes(x = location_type, y = total_victims, fill = location_type)) +
  geom_boxplot() +
  theme_economist(base_family = "ITC Officina Sans", dkpanel = TRUE) +
  scale_colour_economist() + 
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Number of Total Victims by Location",
    x = "Location",
    y = "Number of Total Victims",
    fill = "Location"
  )

chart
```

<img src="/blogs/mass_shootings_files/figure-html/unnamed-chunk-4-1.png" width="672" />
