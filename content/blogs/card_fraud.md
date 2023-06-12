---
categories:  
- ""    #the front matter should be like the one found in, e.g., blog2.md. It cannot be like the normal Rmd we used
- ""
date: "2021-09-30"
description: Card fraud analysis # the title that will show up once someone gets to this page
draft: false
image: spices.jpg # save picture in \static\img\blogs. Acceptable formats= jpg, jpeg, or png . Your iPhone pics wont work

keywords: ""
slug: card_fraud # slug is the shorthand URL address... no spaces plz
title: Card fraud analysis
---




# Exploring credit card fraud

We will be using a dataset with credit card transactions containing legitimate and fraud transactions. Fraud is typically well below 1% of all transactions, so a naive model that predicts that all transactions are legitimate and not fraudulent would have an accuracy of well over 99%-- pretty good, no? (well, not quite as we will see later in the course)

You can read more on credit card fraud on [Credit Card Fraud Detection Using Weighted Support Vector Machine](https://www.scirp.org/journal/paperinformation.aspx?paperid=105944)

The dataset we will use consists of credit card transactions and it includes information about each transaction including customer details, the merchant and category of purchase, and whether or not the transaction was a fraud.

## Obtain the data


```
## Rows: 671,028
## Columns: 14
## $ trans_date_trans_time <dttm> 2019-02-22 07:32:58, 2019-02-16 15:07:20, 2019-…
## $ trans_year            <dbl> 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2020, …
## $ category              <chr> "entertainment", "kids_pets", "personal_care", "…
## $ amt                   <dbl> 7.79, 3.89, 8.43, 40.00, 54.04, 95.61, 64.95, 3.…
## $ city                  <chr> "Veedersburg", "Holloway", "Arnold", "Apison", "…
## $ state                 <chr> "IN", "OH", "MO", "TN", "CO", "GA", "MN", "AL", …
## $ lat                   <dbl> 40.1186, 40.0113, 38.4305, 35.0149, 39.4584, 32.…
## $ long                  <dbl> -87.2602, -80.9701, -90.3870, -85.0164, -106.385…
## $ city_pop              <dbl> 4049, 128, 35439, 3730, 277, 1841, 136, 190178, …
## $ job                   <chr> "Development worker, community", "Child psychoth…
## $ dob                   <date> 1959-10-19, 1946-04-03, 1985-03-31, 1991-01-28,…
## $ merch_lat             <dbl> 39.41679, 39.74585, 37.73078, 34.53277, 39.95244…
## $ merch_long            <dbl> -87.52619, -81.52477, -91.36875, -84.10676, -106…
## $ is_fraud              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
```


-   What types of purchases are most likely to be instances of fraud? Consider category of merchants and produce a bar chart that shows % of total fraudulent transactions sorted in order.


```r
fraud_by_category <- card_fraud %>%
  group_by(category) %>%
  summarise(total_fraud = sum(is_fraud == 1), total_transactions = n()) %>%
  mutate(percentage = total_fraud / total_transactions * 100) %>%
  arrange(percentage)

# Add a column for coloring the bars
fraud_by_category <- fraud_by_category %>%
  mutate(color = ifelse(row_number() >= 11, "Top 4", "Other")) %>% 
  mutate(label = ifelse(row_number() >= 11, paste0(round(percentage, 1), "%"), ""))

# Define the predefined value for the horizontal line
predefined_value <- 0.6

# Generate the bar chart with colored bars and horizontal line
chart <- ggplot(fraud_by_category, aes(x = reorder(category, -percentage), y = percentage, fill = color)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = predefined_value, linetype = "dashed", color = "red") +
  geom_text(aes(label = label), vjust = -0.4) +
  geom_text(aes(x = Inf, y = predefined_value, label = paste0(predefined_value, "%")),
            hjust = 1, vjust = -0.5, color = "red") +
  labs(title = "Percentage of Fraudulent Transactions by Merchant Category",
       x = "Merchant Category",
       y = "Percentage (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Top 4" = "orange", "Other" = "lightgrey")) +
  guides(fill = FALSE)
```

```
## Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
## of ggplot2 3.3.4.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```r
chart
```

<img src="/blogs/card_fraud_files/figure-html/unnamed-chunk-3-1.png" width="672" />
From this chart we can observe that the most common merchant categories for fraud are shopping_net, grocery_pos, misc_net and shopping_pos. Having said that, fraudulent transactions tend to happen more online and in shopping stores such as clothing or multi purpose shops for example. Also, there tend to be fraudulent transactions in grocery stores
