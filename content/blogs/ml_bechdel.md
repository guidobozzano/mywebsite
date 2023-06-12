---
categories:  
- ""    #the front matter should be like the one found in, e.g., blog2.md. It cannot be like the normal Rmd we used
- ""
date: "2021-09-30"
description: Bechdel Test Predictor # the title that will show up once someone gets to this page
draft: false
image: spices.jpg # save picture in \static\img\blogs. Acceptable formats= jpg, jpeg, or png . Your iPhone pics wont work

keywords: ""
slug: ml_bechdel # slug is the shorthand URL address... no spaces plz
title: Bechdel Test Predictor
---



# The Bechdel Test

https://fivethirtyeight.com/features/the-dollar-and-cents-case-against-hollywoods-exclusion-of-women/

The [Bechdel test](https://bechdeltest.com) is a way to assess how women are depicted in Hollywood movies.  In order for a movie to pass the test:

1. It has to have at least two [named] women in it
2. Who talk to each other
3. About something besides a man

There is a nice article and analysis you can find here https://fivethirtyeight.com/features/the-dollar-and-cents-case-against-hollywoods-exclusion-of-women/
We have a sample of 1394 movies and we want to fit a model to predict whether a film passes the test or not.


```r
bechdel <- read_csv(here::here("data", "bechdel.csv")) %>% 
  mutate(test = factor(test)) 
```

```
## Rows: 1394 Columns: 10
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (4): title, test, rated, genre
## dbl (6): year, budget_2013, domgross_2013, intgross_2013, metascore, imdb_ra...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
glimpse(bechdel)
```

```
## Rows: 1,394
## Columns: 10
## $ year          <dbl> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 20…
## $ title         <chr> "12 Years a Slave", "2 Guns", "42", "47 Ronin", "A Good …
## $ test          <fct> Fail, Fail, Fail, Fail, Fail, Pass, Pass, Fail, Pass, Pa…
## $ budget_2013   <dbl> 2.00, 6.10, 4.00, 22.50, 9.20, 1.20, 1.30, 13.00, 4.00, …
## $ domgross_2013 <dbl> 5.3107035, 7.5612460, 9.5020213, 3.8362475, 6.7349198, 1…
## $ intgross_2013 <dbl> 15.8607035, 13.2493015, 9.5020213, 14.5803842, 30.424919…
## $ rated         <chr> "R", "R", "PG-13", "PG-13", "R", "R", "PG-13", "PG-13", …
## $ metascore     <dbl> 97, 55, 62, 29, 28, 55, 48, 33, 90, 58, 52, 78, 83, 53, …
## $ imdb_rating   <dbl> 8.3, 6.8, 7.6, 6.6, 5.4, 7.8, 5.7, 5.0, 7.5, 7.4, 6.2, 7…
## $ genre         <chr> "Biography", "Action", "Biography", "Action", "Action", …
```
How many films fail/pass the test, both as a number and as a %?


```r
#To answer this question I begin by using the bechdel data set

bechdel %>% 
  group_by(test) %>% #First I group_by the information by test to understand films by this group
  summarise(count=n()) %>% #Then I perform a count of the number of films that passed or failed the test
  mutate(prop = round(count/sum(count)*100,1)) #Last I mutate a new column with the proportion by the above groupin
```

```
## # A tibble: 2 × 3
##   test  count  prop
##   <fct> <int> <dbl>
## 1 Fail    772  55.4
## 2 Pass    622  44.6
```


## Movie scores

```r
ggplot(data = bechdel, aes(
  x = metascore,
  y = imdb_rating,
  colour = test
)) +
  geom_point(alpha = .3, size = 3) +
  scale_colour_manual(values = c("tomato", "olivedrab")) +
  labs(
    x = "Metacritic score",
    y = "IMDB rating",
    colour = "Bechdel test"
  ) +
 theme_light()
```

<img src="/blogs/ml_bechdel_files/figure-html/unnamed-chunk-3-1.png" width="672" />


# Split the data

```r
# **Split the data**

set.seed(123)

data_split <- initial_split(bechdel, # updated data
                           prop = 0.8, 
                           strata = test)

bechdel_train <- training(data_split) 
bechdel_test <- testing(data_split)
```

Check the counts and % (proportions) of the `test` variable in each set.

```r
#To check the counts and proportions I repeat the first code but I run it on the bechdel_train and bechdel_test datasets.
#From the tables below I can conclude that both the train and test datasets are rightfully distributed since the proportions of pass and test are the same between the two datasets and between the entire data set (55.4% for Fail and 44.6% for Pass)

bechdel_train %>% 
  group_by(test) %>% 
  summarise(count=n()) %>% 
  mutate(prop = round(count/sum(count)*100,1))
```

```
## # A tibble: 2 × 3
##   test  count  prop
##   <fct> <int> <dbl>
## 1 Fail    617  55.4
## 2 Pass    497  44.6
```

```r
bechdel_test %>% 
  group_by(test) %>% 
  summarise(count=n()) %>% 
  mutate(prop = round(count/sum(count)*100,1))
```

```
## # A tibble: 2 × 3
##   test  count  prop
##   <fct> <int> <dbl>
## 1 Fail    155  55.4
## 2 Pass    125  44.6
```

## Feature exploration

## Any outliers? 


```r
bechdel %>% 
  select(test, budget_2013, domgross_2013, intgross_2013, imdb_rating, metascore) %>% 

    pivot_longer(cols = 2:6,
               names_to = "feature",
               values_to = "value") %>% 
  ggplot()+
  aes(x=test, y = value, fill = test)+
  coord_flip()+
  geom_boxplot()+
  facet_wrap(~feature, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x=NULL,y = NULL)
```

<img src="/blogs/ml_bechdel_files/figure-html/unnamed-chunk-6-1.png" width="672" />

From the boxplots below I see that there seems to be an outlier on the budget_2013 variable that failed the test, on the domgross_2013 variable that passed and failed, and the intgross_2013 that passed and failed.
In addition there seems to be an outlier on the imdb_rating that failed

## Scatterplot - Correlation Matrix

Write a paragraph discussing the output of the following 

```r
bechdel %>% 
  select(test, budget_2013, domgross_2013, intgross_2013, imdb_rating, metascore)%>% 
  ggpairs(aes(colour=test), alpha=0.2)+
  theme_bw()
```

<img src="/blogs/ml_bechdel_files/figure-html/unnamed-chunk-7-1.png" width="672" />

To being with, there seems to be a positive correlation between imdb_rating and metascore (0.737). This positive correlation is equally stronger for movies that passed or failed the test. As both variables are a rating it makes sense they are positively correlated because it means that the movies they rate are rated in the same direction (i.e: if it has a high score in imdb_rating it will probably have a high score in metascore and viceversa. This also applies to bad scores)
Furthermore, both imdb_rating and metascore seem to have no correlation with the budget variable (-0.004 and -0.067 respectively). Therefore, from this information it looks like having a big or small budget does not necesarily mean that the rating ot metascore will be higher or lower.
In addition, in terms of revenues there's a strong correlation (0.938) between domestic and international revenues which means that if the movie performs well domestically usually will perform well internationally and viceversa

## Categorical variables

Write a paragraph discussing the output of the following 

```r
bechdel %>% 
  group_by(genre, test) %>%
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))
```

```
## `summarise()` has grouped output by 'genre'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 24 × 4
## # Groups:   genre [14]
##    genre     test      n  prop
##    <chr>     <fct> <int> <dbl>
##  1 Action    Fail    260 0.707
##  2 Action    Pass    108 0.293
##  3 Adventure Fail     52 0.559
##  4 Adventure Pass     41 0.441
##  5 Animation Fail     63 0.677
##  6 Animation Pass     30 0.323
##  7 Biography Fail     36 0.554
##  8 Biography Pass     29 0.446
##  9 Comedy    Fail    138 0.427
## 10 Comedy    Pass    185 0.573
## # ℹ 14 more rows
```

```r
bechdel %>% 
  group_by(rated, test) %>%
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))
```

```
## `summarise()` has grouped output by 'rated'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 10 × 4
## # Groups:   rated [5]
##    rated test      n  prop
##    <chr> <fct> <int> <dbl>
##  1 G     Fail     16 0.615
##  2 G     Pass     10 0.385
##  3 NC-17 Fail      5 0.833
##  4 NC-17 Pass      1 0.167
##  5 PG    Fail    115 0.561
##  6 PG    Pass     90 0.439
##  7 PG-13 Fail    283 0.529
##  8 PG-13 Pass    252 0.471
##  9 R     Fail    353 0.568
## 10 R     Pass    269 0.432
```

By analyzing the test pass rate by genre, I can conclude that Action movies have one of the highest fail rates (70%) given the large number of cases. There are also some particular genres that reach 100% but the sample is particularly small.
On the other hand, both Horror and Comedy perfom really well in terms of pass rates.

By analyzing the test pass rate by rates, I can conlude that PG-13 have the highest pass rates with 47%, followed by R movies. On the other hand, the G and PG movies seem to perform really bad in terms of test pass rates.

# Train first models. `test ~ metascore + imdb_rating`


```r
lr_mod <- logistic_reg() %>% 
  set_engine(engine = "glm") %>% 
  set_mode("classification")

lr_mod
```

```
## Logistic Regression Model Specification (classification)
## 
## Computational engine: glm
```

```r
tree_mod <- decision_tree() %>% 
  set_engine(engine = "C5.0") %>% 
  set_mode("classification")

tree_mod 
```

```
## Decision Tree Model Specification (classification)
## 
## Computational engine: C5.0
```


```r
lr_fit <- lr_mod %>% # parsnip model
  fit(test ~ metascore + imdb_rating, # a formula
    data = bechdel_train # dataframe
  )

tree_fit <- tree_mod %>% # parsnip model
  fit(test ~ metascore + imdb_rating, # a formula
    data = bechdel_train # dataframe
  )
```

## Logistic regression


```r
lr_fit %>%
  broom::tidy()
```

```
## # A tibble: 3 × 5
##   term        estimate std.error statistic  p.value
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)   2.80     0.494        5.68 1.35e- 8
## 2 metascore     0.0207   0.00536      3.86 1.13e- 4
## 3 imdb_rating  -0.625    0.100       -6.24 4.36e-10
```

```r
lr_preds <- lr_fit %>%
  augment(new_data = bechdel_train) %>%
  mutate(.pred_match = if_else(test == .pred_class, 1, 0))
```

### Confusion matrix


```r
lr_preds %>% 
  conf_mat(truth = test, estimate = .pred_class) %>% 
  autoplot(type = "heatmap")
```

<img src="/blogs/ml_bechdel_files/figure-html/unnamed-chunk-12-1.png" width="672" />


## Decision Tree

```r
tree_preds <- tree_fit %>%
  augment(new_data = bechdel) %>%
  mutate(.pred_match = if_else(test == .pred_class, 1, 0)) 
```


```r
tree_preds %>% 
  conf_mat(truth = test, estimate = .pred_class) %>% 
  autoplot(type = "heatmap")
```

<img src="/blogs/ml_bechdel_files/figure-html/unnamed-chunk-14-1.png" width="672" />

## Draw the decision tree


```r
draw_tree <- 
    rpart::rpart(
        test ~ metascore + imdb_rating,
        data = bechdel_train, # uses data that contains both birth weight and `low`
        control = rpart::rpart.control(maxdepth = 5, cp = 0, minsplit = 10)
    ) %>% 
    partykit::as.party()
plot(draw_tree)
```

<img src="/blogs/ml_bechdel_files/figure-html/unnamed-chunk-15-1.png" width="672" />

# Cross Validation

Run the code below. What does it return?


```r
set.seed(123)
bechdel_folds <- vfold_cv(data = bechdel_train, 
                          v = 3, 
                          strata = test)
bechdel_folds
```

```
## #  3-fold cross-validation using stratification 
## # A tibble: 3 × 2
##   splits            id   
##   <list>            <chr>
## 1 <split [742/372]> Fold1
## 2 <split [742/372]> Fold2
## 3 <split [744/370]> Fold3
```

```r
#The code below returns the number of folds we are going to train and test the data, the code creates 3 folds of roughly equal size that are used to train and then test the model
```

## `fit_resamples()`

Trains and tests a resampled model.


```r
lr_fit <- lr_mod %>%
  fit_resamples(
    test ~ metascore + imdb_rating,
    resamples = bechdel_folds
  )


tree_fit <- tree_mod %>%
  fit_resamples(
    test ~ metascore + imdb_rating,
    resamples = bechdel_folds
  )
```


## `collect_metrics()`

Unnest the metrics column from a tidymodels `fit_resamples()`

```r
collect_metrics(lr_fit)
```

```
## # A tibble: 2 × 6
##   .metric  .estimator  mean     n std_err .config             
##   <chr>    <chr>      <dbl> <int>   <dbl> <chr>               
## 1 accuracy binary     0.585     3  0.0143 Preprocessor1_Model1
## 2 roc_auc  binary     0.602     3  0.0190 Preprocessor1_Model1
```

```r
collect_metrics(tree_fit)
```

```
## # A tibble: 2 × 6
##   .metric  .estimator  mean     n std_err .config             
##   <chr>    <chr>      <dbl> <int>   <dbl> <chr>               
## 1 accuracy binary     0.554     3 0.00458 Preprocessor1_Model1
## 2 roc_auc  binary     0.524     3 0.0122  Preprocessor1_Model1
```



```r
tree_preds <- tree_mod %>% 
  fit_resamples(
    test ~ metascore + imdb_rating, 
    resamples = bechdel_folds,
    control = control_resamples(save_pred = TRUE) #<<
  )

# What does the data for ROC look like?
tree_preds %>% 
  collect_predictions() %>% 
  roc_curve(truth = test, .pred_Fail)  
```

```
## # A tibble: 9 × 3
##   .threshold specificity sensitivity
##        <dbl>       <dbl>       <dbl>
## 1   -Inf          0           1     
## 2      0.346      0           1     
## 3      0.430      0.0342      0.977 
## 4      0.554      0.137       0.890 
## 5      0.556      0.471       0.556 
## 6      0.569      0.761       0.272 
## 7      0.819      0.952       0.0762
## 8      0.867      0.990       0.0276
## 9    Inf          1           0
```

```r
# Draw the ROC
tree_preds %>% 
  collect_predictions() %>% 
  roc_curve(truth = test, .pred_Fail) %>% 
  autoplot()
```

<img src="/blogs/ml_bechdel_files/figure-html/unnamed-chunk-19-1.png" width="672" />


# Build a better training set with `recipes`

## Preprocessing options

- Encode categorical predictors
- Center and scale variables
- Handle class imbalance
- Impute missing data
- Perform dimensionality reduction 
- ... ...

## To build a recipe

1. Start the `recipe()`
1. Define the variables involved
1. Describe **prep**rocessing [step-by-step]

## Collapse Some Categorical Levels

Do we have any `genre` with few observations?  Assign genres that have less than 3% to a new category 'Other'


<img src="/blogs/ml_bechdel_files/figure-html/unnamed-chunk-20-1.png" width="672" />



```r
movie_rec <-
  recipe(test ~ .,
         data = bechdel_train) %>%
  
  # Genres with less than 5% will be in a catewgory 'Other'
    step_other(genre, threshold = .03) 
```
  

## Before recipe


```
## # A tibble: 14 × 2
##    genre           n
##    <chr>       <int>
##  1 Action        293
##  2 Comedy        254
##  3 Drama         213
##  4 Adventure      75
##  5 Animation      72
##  6 Crime          68
##  7 Horror         68
##  8 Biography      50
##  9 Mystery         7
## 10 Fantasy         5
## 11 Sci-Fi          3
## 12 Thriller        3
## 13 Documentary     2
## 14 Musical         1
```


## After recipe


```r
movie_rec %>% 
  prep() %>% 
  bake(new_data = bechdel_train) %>% 
  count(genre, sort = TRUE)
```

```
## # A tibble: 9 × 2
##   genre         n
##   <fct>     <int>
## 1 Action      293
## 2 Comedy      254
## 3 Drama       213
## 4 Adventure    75
## 5 Animation    72
## 6 Crime        68
## 7 Horror       68
## 8 Biography    50
## 9 other        21
```

## `step_dummy()`

Converts nominal data into numeric dummy variables


```r
movie_rec <- recipe(test ~ ., data = bechdel) %>%
  step_other(genre, threshold = .03) %>% 
  step_dummy(all_nominal_predictors()) 

movie_rec 
```

```
## 
```

```
## ── Recipe ──────────────────────────────────────────────────────────────────────
```

```
## 
```

```
## ── Inputs
```

```
## Number of variables by role
```

```
## outcome:   1
## predictor: 9
```

```
## 
```

```
## ── Operations
```

```
## • Collapsing factor levels for: genre
```

```
## • Dummy variables from: all_nominal_predictors()
```

## Let's think about the modelling 

## `step_novel()`

Adds a catch-all level to a factor for any new values not encountered in model training, which lets R intelligently predict new levels in the test set.


```r
movie_rec <- recipe(test ~ ., data = bechdel) %>%
  step_other(genre, threshold = .03) %>% 
  step_novel(all_nominal_predictors) %>% # Use *before* `step_dummy()` so new level is dummified
  step_dummy(all_nominal_predictors()) 
```


## `step_zv()`

Intelligently handles zero variance variables (variables that contain only a single value)


```r
movie_rec <- recipe(test ~ ., data = bechdel) %>%
  step_other(genre, threshold = .03) %>% 
  step_novel(all_nominal(), -all_outcomes()) %>% # Use *before* `step_dummy()` so new level is dummified
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_numeric(), -all_outcomes()) 
```


## `step_normalize()`

Centers then scales numeric variable (mean = 0, sd = 1)


```r
movie_rec <- recipe(test ~ ., data = bechdel) %>%
  step_other(genre, threshold = .03) %>% 
  step_novel(all_nominal(), -all_outcomes()) %>% # Use *before* `step_dummy()` so new level is dummified
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_numeric(), -all_outcomes())  %>% 
  step_normalize(all_numeric()) 
```


## `step_corr()`

Removes highly correlated variables


```r
movie_rec <- recipe(test ~ ., data = bechdel) %>%
  step_other(genre, threshold = .03) %>% 
  step_novel(all_nominal(), -all_outcomes()) %>% # Use *before* `step_dummy()` so new level is dummified
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_numeric(), -all_outcomes())  %>% 
  step_normalize(all_numeric())



movie_rec
```

```
## 
```

```
## ── Recipe ──────────────────────────────────────────────────────────────────────
```

```
## 
```

```
## ── Inputs
```

```
## Number of variables by role
```

```
## outcome:   1
## predictor: 9
```

```
## 
```

```
## ── Operations
```

```
## • Collapsing factor levels for: genre
```

```
## • Novel factor level assignment for: all_nominal(), -all_outcomes()
```

```
## • Dummy variables from: all_nominal(), -all_outcomes()
```

```
## • Zero variance filter on: all_numeric(), -all_outcomes()
```

```
## • Centering and scaling for: all_numeric()
```


# Define different models to fit


```r
## Model Building

# 1. Pick a `model type`
# 2. set the `engine`
# 3. Set the `mode`: regression or classification

# Logistic regression
log_spec <-  logistic_reg() %>%  # model type
  set_engine(engine = "glm") %>%  # model engine
  set_mode("classification") # model mode

# Show your model specification
log_spec
```

```
## Logistic Regression Model Specification (classification)
## 
## Computational engine: glm
```

```r
# Decision Tree
tree_spec <- decision_tree() %>%
  set_engine(engine = "C5.0") %>%
  set_mode("classification")

tree_spec
```

```
## Decision Tree Model Specification (classification)
## 
## Computational engine: C5.0
```

```r
# Random Forest
library(ranger)

rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")


# Boosted tree (XGBoost)
library(xgboost)
```

```
## 
## Attaching package: 'xgboost'
```

```
## The following object is masked from 'package:dplyr':
## 
##     slice
```

```r
xgb_spec <- 
  boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

# K-nearest neighbour (k-NN)
knn_spec <- 
  nearest_neighbor(neighbors = 4) %>% # we can adjust the number of neighbors 
  set_engine("kknn") %>% 
  set_mode("classification") 
```


# Bundle recipe and model with `workflows`



```r
log_wflow <- # new workflow object
 workflow() %>% # use workflow function
 add_recipe(movie_rec) %>%   # use the new recipe
 add_model(log_spec)   # add your model spec

# show object
log_wflow
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: logistic_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 5 Recipe Steps
## 
## • step_other()
## • step_novel()
## • step_dummy()
## • step_zv()
## • step_normalize()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Logistic Regression Model Specification (classification)
## 
## Computational engine: glm
```

```r
## A few more workflows

tree_wflow <-
 workflow() %>%
 add_recipe(movie_rec) %>% 
 add_model(tree_spec) 

rf_wflow <-
 workflow() %>%
 add_recipe(movie_rec) %>% 
 add_model(rf_spec) 

xgb_wflow <-
 workflow() %>%
 add_recipe(movie_rec) %>% 
 add_model(xgb_spec)

knn_wflow <-
 workflow() %>%
 add_recipe(movie_rec) %>% 
 add_model(knn_spec)
```

# Model Comparison


```r
## Evaluate Models

## Logistic regression results{.smaller}

log_res <- log_wflow %>% 
  fit_resamples(
    resamples = bechdel_folds, 
    metrics = metric_set(
      recall, precision, f_meas, accuracy,
      kap, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)) 
```

```
## → A | warning: glm.fit: algorithm did not converge
```

```
## There were issues with some computations   A: x1
```

```
## → B | warning: prediction from a rank-deficient fit may be misleading
```

```
## There were issues with some computations   A: x1
There were issues with some computations   A: x2   B: x1
## There were issues with some computations   A: x3   B: x2
## There were issues with some computations   A: x3   B: x3
```

```r
# Show average performance over all folds (note that we use log_res):
log_res %>%  collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator    mean     n std_err .config             
##   <chr>     <chr>        <dbl> <int>   <dbl> <chr>               
## 1 accuracy  binary      0.434      3  0.0150 Preprocessor1_Model1
## 2 f_meas    binary      0.346      3  0.0772 Preprocessor1_Model1
## 3 kap       binary     -0.0916     3  0.0140 Preprocessor1_Model1
## 4 precision binary      0.470      3  0.0244 Preprocessor1_Model1
## 5 recall    binary      0.290      3  0.0978 Preprocessor1_Model1
## 6 roc_auc   binary      0.450      3  0.0115 Preprocessor1_Model1
## 7 sens      binary      0.290      3  0.0978 Preprocessor1_Model1
## 8 spec      binary      0.614      3  0.0918 Preprocessor1_Model1
```

```r
# Show performance for every single fold:
log_res %>%  collect_metrics(summarize = FALSE)
```

```
## # A tibble: 24 × 5
##    id    .metric   .estimator .estimate .config             
##    <chr> <chr>     <chr>          <dbl> <chr>               
##  1 Fold1 recall    binary        0.184  Preprocessor1_Model1
##  2 Fold1 precision binary        0.463  Preprocessor1_Model1
##  3 Fold1 f_meas    binary        0.264  Preprocessor1_Model1
##  4 Fold1 accuracy  binary        0.430  Preprocessor1_Model1
##  5 Fold1 kap       binary       -0.0751 Preprocessor1_Model1
##  6 Fold1 sens      binary        0.184  Preprocessor1_Model1
##  7 Fold1 spec      binary        0.735  Preprocessor1_Model1
##  8 Fold1 roc_auc   binary        0.464  Preprocessor1_Model1
##  9 Fold2 recall    binary        0.485  Preprocessor1_Model1
## 10 Fold2 precision binary        0.515  Preprocessor1_Model1
## # ℹ 14 more rows
```

```r
## `collect_predictions()` and get confusion matrix{.smaller}

log_pred <- log_res %>% collect_predictions()

log_pred %>%  conf_mat(test, .pred_class) 
```

```
##           Truth
## Prediction Fail Pass
##       Fail  179  192
##       Pass  438  305
```

```r
log_pred %>% 
  conf_mat(test, .pred_class) %>% 
  autoplot(type = "mosaic") +
  geom_label(aes(
      x = (xmax + xmin) / 2, 
      y = (ymax + ymin) / 2, 
      label = c("TP", "FN", "FP", "TN")))
```

<img src="/blogs/ml_bechdel_files/figure-html/unnamed-chunk-31-1.png" width="672" />

```r
log_pred %>% 
  conf_mat(test, .pred_class) %>% 
  autoplot(type = "heatmap")
```

<img src="/blogs/ml_bechdel_files/figure-html/unnamed-chunk-31-2.png" width="672" />

```r
log_pred
```

```
## # A tibble: 1,114 × 7
##    id    .pred_class  .row .pred_Fail .pred_Pass test  .config             
##    <chr> <fct>       <int>      <dbl>      <dbl> <fct> <chr>               
##  1 Fold1 Pass            1   2.22e-16   1   e+ 0 Fail  Preprocessor1_Model1
##  2 Fold1 Pass            2   2.22e-16   1   e+ 0 Fail  Preprocessor1_Model1
##  3 Fold1 Pass            4   2.22e-16   1   e+ 0 Fail  Preprocessor1_Model1
##  4 Fold1 Pass            6   2.22e-16   1   e+ 0 Fail  Preprocessor1_Model1
##  5 Fold1 Pass            8   2.22e-16   1   e+ 0 Fail  Preprocessor1_Model1
##  6 Fold1 Fail            9   1   e+ 0   2.22e-16 Fail  Preprocessor1_Model1
##  7 Fold1 Pass           14   2.22e-16   1   e+ 0 Fail  Preprocessor1_Model1
##  8 Fold1 Pass           16   2.22e-16   1   e+ 0 Fail  Preprocessor1_Model1
##  9 Fold1 Pass           20   2.22e-16   1   e+ 0 Fail  Preprocessor1_Model1
## 10 Fold1 Pass           24   2.22e-16   1   e+ 0 Fail  Preprocessor1_Model1
## # ℹ 1,104 more rows
```

```r
## ROC Curve

log_pred %>% 
  group_by(id) %>% # id contains our folds
  roc_curve(test, .pred_Pass) %>% 
  autoplot()
```

<img src="/blogs/ml_bechdel_files/figure-html/unnamed-chunk-31-3.png" width="672" />

```r
## Decision Tree results

tree_res <-
  tree_wflow %>% 
  fit_resamples(
    resamples = bechdel_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 

tree_res %>%  collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n std_err .config             
##   <chr>     <chr>      <dbl> <int>   <dbl> <chr>               
## 1 accuracy  binary     0.588     3  0.0147 Preprocessor1_Model1
## 2 f_meas    binary     0.633     3  0.0244 Preprocessor1_Model1
## 3 kap       binary     0.163     3  0.0257 Preprocessor1_Model1
## 4 precision binary     0.623     3  0.0105 Preprocessor1_Model1
## 5 recall    binary     0.647     3  0.0473 Preprocessor1_Model1
## 6 roc_auc   binary     0.589     3  0.0151 Preprocessor1_Model1
## 7 sens      binary     0.647     3  0.0473 Preprocessor1_Model1
## 8 spec      binary     0.515     3  0.0393 Preprocessor1_Model1
```

```r
## Random Forest

rf_res <-
  rf_wflow %>% 
  fit_resamples(
    resamples = bechdel_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 

rf_res %>%  collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n std_err .config             
##   <chr>     <chr>      <dbl> <int>   <dbl> <chr>               
## 1 accuracy  binary     0.634     3 0.00727 Preprocessor1_Model1
## 2 f_meas    binary     0.702     3 0.00975 Preprocessor1_Model1
## 3 kap       binary     0.238     3 0.0177  Preprocessor1_Model1
## 4 precision binary     0.640     3 0.00999 Preprocessor1_Model1
## 5 recall    binary     0.780     3 0.0332  Preprocessor1_Model1
## 6 roc_auc   binary     0.665     3 0.0140  Preprocessor1_Model1
## 7 sens      binary     0.780     3 0.0332  Preprocessor1_Model1
## 8 spec      binary     0.453     3 0.0448  Preprocessor1_Model1
```

```r
## Boosted tree - XGBoost

xgb_res <- 
  xgb_wflow %>% 
  fit_resamples(
    resamples = bechdel_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 

xgb_res %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n std_err .config             
##   <chr>     <chr>      <dbl> <int>   <dbl> <chr>               
## 1 accuracy  binary     0.606     3 0.00256 Preprocessor1_Model1
## 2 f_meas    binary     0.657     3 0.0107  Preprocessor1_Model1
## 3 kap       binary     0.195     3 0.00120 Preprocessor1_Model1
## 4 precision binary     0.634     3 0.00426 Preprocessor1_Model1
## 5 recall    binary     0.684     3 0.0288  Preprocessor1_Model1
## 6 roc_auc   binary     0.635     3 0.00299 Preprocessor1_Model1
## 7 sens      binary     0.684     3 0.0288  Preprocessor1_Model1
## 8 spec      binary     0.509     3 0.0303  Preprocessor1_Model1
```

```r
## K-nearest neighbour

knn_res <- 
  knn_wflow %>% 
  fit_resamples(
    resamples = bechdel_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 
```

```
## → A | warning: While computing binary `precision()`, no predicted events were detected (i.e. `true_positive + false_positive = 0`). 
##                Precision is undefined in this case, and `NA` will be returned.
##                Note that 206 true event(s) actually occured for the problematic event level, 'Fail'.
## There were issues with some computations   A: x1
## There were issues with some computations   A: x1
```

```r
knn_res %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n  std_err .config             
##   <chr>     <chr>      <dbl> <int>    <dbl> <chr>               
## 1 accuracy  binary     0.518     3 0.0359   Preprocessor1_Model1
## 2 f_meas    binary     0.713     2 0.000120 Preprocessor1_Model1
## 3 kap       binary     0         3 0        Preprocessor1_Model1
## 4 precision binary     0.554     2 0.000145 Preprocessor1_Model1
## 5 recall    binary     0.667     3 0.333    Preprocessor1_Model1
## 6 roc_auc   binary     0.553     3 0.0284   Preprocessor1_Model1
## 7 sens      binary     0.667     3 0.333    Preprocessor1_Model1
## 8 spec      binary     0.333     3 0.333    Preprocessor1_Model1
```

```r
## Model Comparison

log_metrics <- 
  log_res %>% 
  collect_metrics(summarise = TRUE) %>%
  # add the name of the model to every row
  mutate(model = "Logistic Regression") 

tree_metrics <- 
  tree_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Decision Tree")

rf_metrics <- 
  rf_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Random Forest")

xgb_metrics <- 
  xgb_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "XGBoost")

knn_metrics <- 
  knn_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Knn")

# create dataframe with all models
model_compare <- bind_rows(log_metrics,
                           tree_metrics,
                           rf_metrics,
                           xgb_metrics,
                           knn_metrics) 

#Pivot wider to create barplot
  model_comp <- model_compare %>% 
  select(model, .metric, mean, std_err) %>% 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) 

# show mean are under the curve (ROC-AUC) for every model
model_comp %>% 
  arrange(mean_roc_auc) %>% 
  mutate(model = fct_reorder(model, mean_roc_auc)) %>% # order results
  ggplot(aes(model, mean_roc_auc, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
   geom_text(
     size = 3,
     aes(label = round(mean_roc_auc, 2), 
         y = mean_roc_auc + 0.08),
     vjust = 1
  )+
  theme_light()+
  theme(legend.position = "none")+
  labs(y = NULL)
```

<img src="/blogs/ml_bechdel_files/figure-html/unnamed-chunk-31-4.png" width="672" />

```r
## `last_fit()` on test set

# - `last_fit()`  fits a model to the whole training data and evaluates it on the test set. 
# - provide the workflow object of the best model as well as the data split object (not the training data). 
 
last_fit_xgb <- last_fit(xgb_wflow, 
                        split = data_split,
                        metrics = metric_set(
                          accuracy, f_meas, kap, precision,
                          recall, roc_auc, sens, spec))

last_fit_xgb %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 4
##   .metric   .estimator .estimate .config             
##   <chr>     <chr>          <dbl> <chr>               
## 1 accuracy  binary         0.568 Preprocessor1_Model1
## 2 f_meas    binary         0.630 Preprocessor1_Model1
## 3 kap       binary         0.114 Preprocessor1_Model1
## 4 precision binary         0.599 Preprocessor1_Model1
## 5 recall    binary         0.665 Preprocessor1_Model1
## 6 sens      binary         0.665 Preprocessor1_Model1
## 7 spec      binary         0.448 Preprocessor1_Model1
## 8 roc_auc   binary         0.610 Preprocessor1_Model1
```

```r
#Compare to training
xgb_res %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n std_err .config             
##   <chr>     <chr>      <dbl> <int>   <dbl> <chr>               
## 1 accuracy  binary     0.606     3 0.00256 Preprocessor1_Model1
## 2 f_meas    binary     0.657     3 0.0107  Preprocessor1_Model1
## 3 kap       binary     0.195     3 0.00120 Preprocessor1_Model1
## 4 precision binary     0.634     3 0.00426 Preprocessor1_Model1
## 5 recall    binary     0.684     3 0.0288  Preprocessor1_Model1
## 6 roc_auc   binary     0.635     3 0.00299 Preprocessor1_Model1
## 7 sens      binary     0.684     3 0.0288  Preprocessor1_Model1
## 8 spec      binary     0.509     3 0.0303  Preprocessor1_Model1
```

```r
## Variable importance using `{vip}` package

library(vip)

last_fit_xgb %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 10) +
  theme_light()
```

```
## Warning: `pull_workflow_fit()` was deprecated in workflows 0.2.3.
## ℹ Please use `extract_fit_parsnip()` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

<img src="/blogs/ml_bechdel_files/figure-html/unnamed-chunk-31-5.png" width="672" />

```r
## Final Confusion Matrix

last_fit_xgb %>%
  collect_predictions() %>% 
  conf_mat(test, .pred_class) %>% 
  autoplot(type = "heatmap")
```

<img src="/blogs/ml_bechdel_files/figure-html/unnamed-chunk-31-6.png" width="672" />

```r
## Final ROC curve
last_fit_xgb %>% 
  collect_predictions() %>% 
  roc_curve(test, .pred_Pass) %>% 
  autoplot()
```

<img src="/blogs/ml_bechdel_files/figure-html/unnamed-chunk-31-7.png" width="672" />

To conclude, the model that has the best classification is the Random Forest since it has the biggest accuracy with 66%
