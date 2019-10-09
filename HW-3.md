HW 3
================
Sam Loewen
10/7/2019

## Problem 1

The dataset `instacart` has 1384617 observations, with 15 variables. The
data tracks products and thier corresponding order, providing
information like `order_number`, `product_name`, and its corresponding
`aisle`. We can learn things like the preportion of products that were
reordered (0.5985944) or the average number of days since prior order
(17.0661259).

  - How many aisles are there, and which aisles are the most items
    ordered from?

<!-- end list -->

``` r
instacart %>% 
group_by (aisle_id) %>% 
  summarize (n_obs = n()) %>% 
arrange(desc(n_obs))
```

    ## # A tibble: 134 x 2
    ##    aisle_id  n_obs
    ##       <int>  <int>
    ##  1       83 150609
    ##  2       24 150473
    ##  3      123  78493
    ##  4      120  55240
    ##  5       21  41699
    ##  6      115  36617
    ##  7       84  32644
    ##  8      107  31269
    ##  9       91  26240
    ## 10      112  23635
    ## # ... with 124 more rows

There are 134 aisles, and aisles 83, 24, and 123 are the most ordered
from.

  - Make a plot that shows the number of items ordered in each aisle,
    limiting this to aisles with more than 10000 items ordered. Arrange
    aisles sensibly, and organize your plot so others can read it.

<!-- end list -->

``` r
instacart %>%
  group_by (aisle_id) %>% 
  summarize (n_obs = n()) %>% 
  filter(n_obs > 10000) %>% 
  arrange(aisle_id) %>% 
    ggplot(aes(x = aisle_id, y = n_obs, color=n_obs)) + 
    geom_point() +
    labs(title = "Number of items ordered per aisle", 
    x = "Aisle Number", 
    y = "NUmber items ordered")
```

![](HW-3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

  - Make a table showing the three most popular items in each of the
    aisles “baking ingredients”, “dog food care”, and “packaged
    vegetables fruits”. Include the number of times each item is ordered
    in your table.

<!-- end list -->

``` r
instacart %>% 
group_by (aisle, product_id) %>% 
  summarize (n_obs = n()) %>% 
filter (aisle == "baking ingredients" | aisle == "dog food care" | aisle == "packaged vegetables fruits") %>% 
top_n(3, n_obs) %>% 
knitr::kable()
```

| aisle                      | product\_id | n\_obs |
| :------------------------- | ----------: | -----: |
| baking ingredients         |       23405 |    387 |
| baking ingredients         |       23537 |    499 |
| baking ingredients         |       49533 |    336 |
| dog food care              |         722 |     30 |
| dog food care              |       17471 |     26 |
| dog food care              |       23329 |     28 |
| packaged vegetables fruits |       21903 |   9784 |
| packaged vegetables fruits |       27966 |   5546 |
| packaged vegetables fruits |       39275 |   4966 |

  - Make a table showing the mean hour of the day at which Pink Lady
    Apples and Coffee Ice Cream are ordered on each day of the week;
    format this table for human readers (i.e. produce a 2 x 7 table).

<!-- end list -->

``` r
instacart %>% 
group_by (product_name, order_dow) %>%
  summarize(mean_hour = mean(order_hour_of_day)) %>% 
filter (product_name == "Pink Lady Apples" | product_name == "Coffee Ice Cream") %>%
  pivot_wider(
    names_from = order_dow,
    values_from = mean_hour) %>% 
knitr::kable()  
```

| product\_name    |        0 |        1 |        2 |        3 |        4 |        5 |        6 |
| :--------------- | -------: | -------: | -------: | -------: | -------: | -------: | -------: |
| Coffee Ice Cream | 13.77419 | 14.31579 | 15.38095 | 15.31818 | 15.21739 | 12.26316 | 13.83333 |
| Pink Lady Apples | 13.44118 | 11.36000 | 11.70213 | 14.25000 | 11.55172 | 12.78431 | 11.93750 |

## Problem 2

First, do some data cleaning:

  - format the data to use appropriate variable names;
  - focus on the “Overall Health” topic
  - include only responses from “Excellent” to “Poor”
  - organize responses as a factor taking levels ordered from “Poor” to
    “Excellent”

<!-- end list -->

``` r
brfss_smart2010_1 =
  brfss_smart2010 %>% 
  janitor::clean_names() %>% 
  filter (topic == "Overall Health",
          response == "Excellent" | 
          response == "Very good" | 
          response == "Good" | 
          response == "Fair" |
          response == "Poor") %>% 
  mutate(response = factor(response, labels = c("Poor", "Fair", "Good", "Very Good", "Excellent")))
```

Using this dataset, do or answer the following (commenting on the
results of each):

  - In 2002, which states were observed at 7 or more locations? What
    about in 2010? Construct a dataset that is limited to Excellent
    responses, and contains, year, state, and a variable that averages
    the data\_value across locations within a state.

<!-- end list -->

``` r
brfss_smart2010_2 = 
brfss_smart2010_1 %>% 
  group_by(year, locationabbr, response) %>% 
  summarize (n_loc = n_distinct(locationdesc),
             mean_dv = mean(data_value, na.rm = TRUE)) %>% 
filter(year == 2002 | year == 2010,
       n_loc >= 7,
       response == "Excellent") %>% 
select (-response, -n_loc)
```

  - Make a “spaghetti” plot of this average value over time within a
    state (that is, make a plot showing a line for each state across
    years – the geom\_line geometry and group aesthetic will help).

<!-- end list -->

``` r
brfss_smart2010_1 %>% 
  group_by(year, locationabbr, response) %>% 
  summarize (n_loc = n_distinct(locationdesc),
             mean_dv = mean(data_value, na.rm = TRUE)) %>% 
  filter(n_loc >= 7, response == "Excellent") %>% 
  ggplot(aes(x=year, y=mean_dv, group = locationabbr, color = locationabbr)) + 
  geom_point() + geom_line() +
  labs(title = "Mean data value over time, by state", 
    x = "year", 
    y = "data value")
```

![](HW-3_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

  - Make a two-panel plot showing, for the years 2006, and 2010,
    distribution of data\_value for responses (“Poor” to “Excellent”)
    among locations in NY State.

<!-- end list -->

``` r
brfss_smart2010_1 %>% 
  filter(locationabbr == "NY", 
         year == 2006 | year == 2010) %>% 
  select (year, locationabbr, locationdesc, response, data_value) %>% 
ggplot(aes(x = response, y = data_value)) + 
  geom_boxplot() + facet_grid(. ~ year)  +
  labs(title = "Distribution of data values by repsonse level, NY", 
    x = "response", 
    y = "data value")
```

![](HW-3_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Problem 3

variables activity.\* are the activity counts for each minute of a
24-hour day starting at midnight.

  - Load, tidy, and otherwise wrangle the data. Your final dataset
    should include all originally observed variables and values; have
    useful variable names; include a weekday vs weekend variable; and
    encode data with reasonable variable classes. Describe the resulting
    dataset (e.g. what variables exist, how many observations, etc).

<!-- end list -->

``` r
accel = 
  read_csv("./data/accel_data.csv") %>% 
  janitor::clean_names() %>% 
  mutate(day = factor(day, labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
         weekend = as.numeric(if_else(day == "Saturday" | day == "Sunday", "1", "0"))) %>% 
  select (week, day_id, day, weekend, everything()) %>% 
arrange (week, day)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   day = col_character()
    ## )

    ## See spec(...) for full column specifications.

The dataset `accel` has 35 observations, with 1444 variables. The data
tracks one man’s activity every day for 5 weeks, reporting activity
every minute. A unit of activity is captued every minute and is
represented in our data set with varaibles `acitvity_1` through
`activity_1440`.

  - Traditional analyses of accelerometer data focus on the total
    activity over the day. Using your tidied dataset, aggregate accross
    minutes to create a total activity variable for each day, and create
    a table showing these totals. Are any trends apparent?

<!-- end list -->

``` r
accel %>% 
  group_by(day_id) %>% 
  mutate (daily_tot = sum(activity_1 : activity_1440)) %>% 
  select (week, day_id, day, weekend, daily_tot, everything())
```

    ## # A tibble: 35 x 1,445
    ## # Groups:   day_id [35]
    ##     week day_id day   weekend daily_tot activity_1 activity_2 activity_3
    ##    <dbl>  <dbl> <fct>   <dbl>     <dbl>      <dbl>      <dbl>      <dbl>
    ##  1     1      1 Mond~       0     3949.       88.4       82.2       64.4
    ##  2     1      2 Tues~       0     2278         1          1          1  
    ##  3     1      3 Wedn~       0        1         1          1          1  
    ##  4     1      4 Thur~       0    46665         1          1          1  
    ##  5     1      5 Frid~       0      941.       47.4       48.8       46.9
    ##  6     1      6 Satu~       1     3166.       64.8       59.5       73.7
    ##  7     1      7 Sund~       1     1126.       71.1      103.        68.5
    ##  8     2      8 Mond~       0   228150       675        542       1010  
    ##  9     2      9 Tues~       0    42486       291        335        393  
    ## 10     2     10 Wedn~       0     1849        64         11          1  
    ## # ... with 25 more rows, and 1,437 more variables: activity_4 <dbl>,
    ## #   activity_5 <dbl>, activity_6 <dbl>, activity_7 <dbl>,
    ## #   activity_8 <dbl>, activity_9 <dbl>, activity_10 <dbl>,
    ## #   activity_11 <dbl>, activity_12 <dbl>, activity_13 <dbl>,
    ## #   activity_14 <dbl>, activity_15 <dbl>, activity_16 <dbl>,
    ## #   activity_17 <dbl>, activity_18 <dbl>, activity_19 <dbl>,
    ## #   activity_20 <dbl>, activity_21 <dbl>, activity_22 <dbl>,
    ## #   activity_23 <dbl>, activity_24 <dbl>, activity_25 <dbl>,
    ## #   activity_26 <dbl>, activity_27 <dbl>, activity_28 <dbl>,
    ## #   activity_29 <dbl>, activity_30 <dbl>, activity_31 <dbl>,
    ## #   activity_32 <dbl>, activity_33 <dbl>, activity_34 <dbl>,
    ## #   activity_35 <dbl>, activity_36 <dbl>, activity_37 <dbl>,
    ## #   activity_38 <dbl>, activity_39 <dbl>, activity_40 <dbl>,
    ## #   activity_41 <dbl>, activity_42 <dbl>, activity_43 <dbl>,
    ## #   activity_44 <dbl>, activity_45 <dbl>, activity_46 <dbl>,
    ## #   activity_47 <dbl>, activity_48 <dbl>, activity_49 <dbl>,
    ## #   activity_50 <dbl>, activity_51 <dbl>, activity_52 <dbl>,
    ## #   activity_53 <dbl>, activity_54 <dbl>, activity_55 <dbl>,
    ## #   activity_56 <dbl>, activity_57 <dbl>, activity_58 <dbl>,
    ## #   activity_59 <dbl>, activity_60 <dbl>, activity_61 <dbl>,
    ## #   activity_62 <dbl>, activity_63 <dbl>, activity_64 <dbl>,
    ## #   activity_65 <dbl>, activity_66 <dbl>, activity_67 <dbl>,
    ## #   activity_68 <dbl>, activity_69 <dbl>, activity_70 <dbl>,
    ## #   activity_71 <dbl>, activity_72 <dbl>, activity_73 <dbl>,
    ## #   activity_74 <dbl>, activity_75 <dbl>, activity_76 <dbl>,
    ## #   activity_77 <dbl>, activity_78 <dbl>, activity_79 <dbl>,
    ## #   activity_80 <dbl>, activity_81 <dbl>, activity_82 <dbl>,
    ## #   activity_83 <dbl>, activity_84 <dbl>, activity_85 <dbl>,
    ## #   activity_86 <dbl>, activity_87 <dbl>, activity_88 <dbl>,
    ## #   activity_89 <dbl>, activity_90 <dbl>, activity_91 <dbl>,
    ## #   activity_92 <dbl>, activity_93 <dbl>, activity_94 <dbl>,
    ## #   activity_95 <dbl>, activity_96 <dbl>, activity_97 <dbl>,
    ## #   activity_98 <dbl>, activity_99 <dbl>, activity_100 <dbl>,
    ## #   activity_101 <dbl>, activity_102 <dbl>, activity_103 <dbl>, ...

``` r
#sums not right.

accel %>% 
  group_by(day_id) %>% 
  mutate (daily_tot = sum(activity_1:activity_1440)) %>%  
  ggplot(aes(x=day_id, y=daily_tot)) + 
  geom_point()
```

![](HW-3_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

  - Accelerometer data allows the inspection activity over the course of
    the day. Make a single-panel plot that shows the 24-hour activity
    time courses for each day and use color to indicate day of the week.
    Describe in words any patterns or conclusions you can make based on
    this graph.

<!-- end list -->

``` r
accel %>% 
  pivot_longer(
    activity_1:activity_1440,
    names_to = "minute",
    values_to = "activity_unit") %>% 
ggplot(aes(x=day_id, y=activity_unit, group = minute, color = day)) + 
  geom_point() + geom_line() +
  labs(title = "Activity over time", 
    x = "day", 
    y = "activity unit")
```

![](HW-3_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
