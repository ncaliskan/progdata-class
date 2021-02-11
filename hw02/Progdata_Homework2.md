Hw2
================
Neslihan Caliskan
2/8/2021

Load in your packages

``` r
library(gapminder)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.6     ✓ dplyr   1.0.4
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
```

Exercise 1

``` r
gapminder::gapminder 
```

    ## # A tibble: 1,704 x 6
    ##    country     continent  year lifeExp      pop gdpPercap
    ##    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
    ##  1 Afghanistan Asia       1952    28.8  8425333      779.
    ##  2 Afghanistan Asia       1957    30.3  9240934      821.
    ##  3 Afghanistan Asia       1962    32.0 10267083      853.
    ##  4 Afghanistan Asia       1967    34.0 11537966      836.
    ##  5 Afghanistan Asia       1972    36.1 13079460      740.
    ##  6 Afghanistan Asia       1977    38.4 14880372      786.
    ##  7 Afghanistan Asia       1982    39.9 12881816      978.
    ##  8 Afghanistan Asia       1987    40.8 13867957      852.
    ##  9 Afghanistan Asia       1992    41.7 16317921      649.
    ## 10 Afghanistan Asia       1997    41.8 22227415      635.
    ## # … with 1,694 more rows

``` r
#Exercise 1.1 & 1.2
#In order to filter by multiple rows you need to use the "%in%" function!
#You can add more criteria by inserting "&" in the middle.
#You can select multiple columns simply by insterting a comma in between them! 
gapminder %>% 
  filter(country %in% c("Afghanistan","Australia","Austria") & year > 1970 & year < 1980) %>% 
  select(country, gdpPercap) 
```

    ## # A tibble: 6 x 2
    ##   country     gdpPercap
    ##   <fct>           <dbl>
    ## 1 Afghanistan      740.
    ## 2 Afghanistan      786.
    ## 3 Australia      16789.
    ## 4 Australia      18334.
    ## 5 Austria        16662.
    ## 6 Austria        19749.

``` r
gapminder %>% 
  mutate(diffLifeExp = lifeExp - lag(lifeExp)) %>% 
  filter(diffLifeExp < lifeExp )
```

    ## # A tibble: 1,703 x 7
    ##    country     continent  year lifeExp      pop gdpPercap diffLifeExp
    ##    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>       <dbl>
    ##  1 Afghanistan Asia       1957    30.3  9240934      821.      1.53  
    ##  2 Afghanistan Asia       1962    32.0 10267083      853.      1.66  
    ##  3 Afghanistan Asia       1967    34.0 11537966      836.      2.02  
    ##  4 Afghanistan Asia       1972    36.1 13079460      740.      2.07  
    ##  5 Afghanistan Asia       1977    38.4 14880372      786.      2.35  
    ##  6 Afghanistan Asia       1982    39.9 12881816      978.      1.42  
    ##  7 Afghanistan Asia       1987    40.8 13867957      852.      0.968 
    ##  8 Afghanistan Asia       1992    41.7 16317921      649.      0.852 
    ##  9 Afghanistan Asia       1997    41.8 22227415      635.      0.0890
    ## 10 Afghanistan Asia       2002    42.1 25268405      727.      0.366 
    ## # … with 1,693 more rows

``` r
gapminder %>% 
  group_by(country) %>% 
  summarize(max(gdpPercap))
```

    ## # A tibble: 142 x 2
    ##    country     `max(gdpPercap)`
    ##  * <fct>                  <dbl>
    ##  1 Afghanistan             978.
    ##  2 Albania                5937.
    ##  3 Algeria                6223.
    ##  4 Angola                 5523.
    ##  5 Argentina             12779.
    ##  6 Australia             34435.
    ##  7 Austria               36126.
    ##  8 Bahrain               29796.
    ##  9 Bangladesh             1391.
    ## 10 Belgium               33693.
    ## # … with 132 more rows

``` r
#A better way to write to code to make it look neater
gapminder %>% 
  select(country, gdpPercap) %>% 
  group_by(country) %>% 
  filter(gdpPercap == max(gdpPercap))
```

    ## # A tibble: 142 x 2
    ## # Groups:   country [142]
    ##    country     gdpPercap
    ##    <fct>           <dbl>
    ##  1 Afghanistan      978.
    ##  2 Albania         5937.
    ##  3 Algeria         6223.
    ##  4 Angola          5523.
    ##  5 Argentina      12779.
    ##  6 Australia      34435.
    ##  7 Austria        36126.
    ##  8 Bahrain        29796.
    ##  9 Bangladesh      1391.
    ## 10 Belgium        33693.
    ## # … with 132 more rows

``` r
gapminder %>% 
  filter(country == "Canada") %>% 
  ggplot +
  aes(x = gdpPercap, y = lifeExp) + 
  geom_point(alpha = 0.5) +
  scale_x_log10("GDP per capita", labels = scales::dollar_format()) +
  theme_bw() +
  scale_y_continuous("Life Expectancy") +
  ggtitle("Life Expectancy vs GP per Capita for Canada")
```

![](Progdata_Homework2_files/figure-gfm/Exercise%201.5-1.png)<!-- -->

Exercise 2

``` r
library(tidyverse)
library(dplyr)
library(palmerpenguins)
```

Pick two quantitative variables to
explore

``` r
#I couldn't get the first half of this chunk work can you help me with it?
# palmerpenguins::penguins %>% 
#   select(flipper_length_mm, body_mass_g) %>% 
#   summarise(
#     Mean_Length = round(flipper_length_mm, na.rm = TRUE),
#     Mean_BodyMass = round(body_mass_g, na.rm = TRUE),
#     SD_FLength = sd(flipper_length_mm, na.rm = TRUE),
#     SD_BodyMass = sd(body_mass_g, na.rm = TRUE),
#     )
palmerpenguins::penguins %>% 
  select(bill_length_mm, bill_depth_mm) %>% 
  summarise(
    meanlength = mean(bill_length_mm, na.rm = T),
    sdlength = sd(bill_length_mm, na.rm = T),
    meandepth = mean(bill_depth_mm, na.rm = T),
    sddepth = sd(bill_depth_mm, na.rm = T),
  )
```

    ## # A tibble: 1 x 4
    ##   meanlength sdlength meandepth sddepth
    ##        <dbl>    <dbl>     <dbl>   <dbl>
    ## 1       43.9     5.46      17.2    1.97

``` r
#Scatterplot
palmerpenguins::penguins %>% 
  ggplot +
  aes(x = bill_length_mm, y = bill_depth_mm) +
  geom_point(alpha = 0.5) +
  scale_x_log10("Bill Length") +
  ylab("Bill Depth")
```

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](Progdata_Homework2_files/figure-gfm/Exercise%202.1-1.png)<!-- -->

``` r
#Sample size info
palmerpenguins::penguins %>% 
  select(species, body_mass_g) %>% 
  summarize(rows = n())
```

    ## # A tibble: 1 x 1
    ##    rows
    ##   <int>
    ## 1   344

``` r
#One categorical and one quantitative variable to explore
palmerpenguins::penguins %>% 
  select(species, body_mass_g) %>% 
  group_by(species) %>% 
  summarise(sdmass = sd(body_mass_g, na.rm = T))
```

    ## # A tibble: 3 x 2
    ##   species   sdmass
    ## * <fct>      <dbl>
    ## 1 Adelie      459.
    ## 2 Chinstrap   384.
    ## 3 Gentoo      504.

``` r
#Visualize the data
palmerpenguins::penguins %>% 
  ggplot() +
  geom_jitter(aes(x = species, 
                  y = body_mass_g,
                  alpha = .5,
                  colour = species))
```

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](Progdata_Homework2_files/figure-gfm/Exercise%202.2-1.png)<!-- -->
