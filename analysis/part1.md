Economic Mobility Analysis Part 1
================
Lewis Tu
2025-05-02

  

## Part 1: Exploratory Data Analysis using the Opportunity Atlas

  

1.  Looking up my hometown on the Opportunity Atlas and Social Capital
    Atlas

<div class="figure" style="text-align: center">

<img src="belmont_all.png" alt="Upward Mobility at p25 for Belmont, MA Pooling All Races and Genders" width="50%" />
<p class="caption">

Upward Mobility at p25 for Belmont, MA Pooling All Races and Genders
</p>

</div>

<div class="figure" style="text-align: center">

<img src="middlesex_all.png" alt="Mobility Trends at p25 for Middlesex County, MA Pooling All Races and Genders" width="50%" />
<p class="caption">

Mobility Trends at p25 for Middlesex County, MA Pooling All Races and
Genders
</p>

</div>

Among children of all races and genders born in 25th percentile parent
income households in Middlesex County, there was a 4.2% decrease in
household income between 1978 and 1992 cohorts, indicating that upward
mobility has decreased over time.

<div class="figure" style="text-align: center">

<img src="belmont_social.png" alt="Economic Connectedness for Zip Code 02478" width="50%" />
<p class="caption">

Economic Connectedness for Zip Code 02478
</p>

</div>

ZIP 02478 (Belmont, MA) ranks at the 100th percentile in terms of
economic connectedness, with 73.1% of friends of low-income people
having high incomes. It exhibits relatively low cohesiveness, with 9.5%
(32nd percentile) of friends also being friends with each other. On the
other hand, civic engagement is strong, with volunteering rates at 10.6%
(83rd percentile).

2.  1.  Summary statistics

There exist observations with missing values for each of the 5
variables, as indicated by the pct_na column.

2.  2.  Why can these upward mobility statistics be negative or above
        100 in these data?

Linear models assume a linear relationship between independent and
dependent variables, and are not restricted to realistic bounds, which
can lead to unrealistic predictions that are negative or above 100. This
might be more likely when the data follows a nonlinear trend, or if
there are extreme outliers.

2.  3.  Histogram of **kfr_pooled_pooled_p25** using all Census tracts
        in the U.S.

``` r
#Histogram using ggplot
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
library(oiplot)

#Histogram
ggplot(atlas, aes(x=kfr_pooled_pooled_p25, y = after_stat(density))) +
  geom_histogram() +
  labs(x = "Absolute Mobility at the 25th percentile", y = "Density") +
  oi_style()
```

    ## Warning in oi_style(): 'oi_style' is deprecated.
    ## Use 'theme_oi' instead.
    ## See help("Deprecated")

    ## `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

    ## Warning: Removed 1189 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](part1_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

This histogram is roughly symmetric and bell-shaped, peaking at around
40. Almost all tracts are within the 25-70 range, with most falling
between about 30-45.

3.  Do kids in Belmont, MA have better or worse chances of climbing the
    income ladder than the average child in America? In Massachusetts?
    In Middlesex County?

``` r
# 2010 FIPS codes
home_tract <- 357600
home_state <- 25 #MA
home_county <- 017 #Middlesex

# My tract
my_tract <- subset(atlas, state == home_state & county == home_county & tract == home_tract)
my_tract_value <- mean(my_tract$kfr_pooled_pooled_p25, na.rm = TRUE)

# My county
my_county <- subset(atlas, state == home_state & county == home_county)
county_mean <- mean(my_county$kfr_pooled_pooled_p25, na.rm = TRUE)

# My state
my_state <- subset(atlas, state == home_state)
state_mean <- mean(my_state$kfr_pooled_pooled_p25, na.rm = TRUE)

# United States
us_mean <- mean(atlas$kfr_pooled_pooled_p25, na.rm = TRUE)

# Compare
compare_df <- data.frame(
  Region = c("My Census Tract", "Middlesex County", "MA", "US"),
  KFR_Mean = c(my_tract_value, county_mean, state_mean, us_mean)
)

compare_df
```

    ##             Region KFR_Mean
    ## 1  My Census Tract 48.18592
    ## 2 Middlesex County 49.45877
    ## 3               MA 46.40324
    ## 4               US 42.85813

Kids from my census tract have a better chance of climbing the income
ladder than the average child in America and the average child in
Massachusetts. However, they have a lower chance of climbing the income
ladder than the average child in Middlesex County, MA.

4.  What is the standard deviation of **kfr_pooled_pooled_p25** in
    Middlesex county? Is it larger or smaller than the standard
    deviation across tracts in Massachusetts? Across tracts in the
    entire U.S.?

``` r
# County SD
sd_county <- sd(my_county$kfr_pooled_pooled_p25, na.rm = TRUE)

# State SD
sd_state <- sd(my_state$kfr_pooled_pooled_p25, na.rm = TRUE)

# US SD
sd_us <- sd(atlas$kfr_pooled_pooled_p25, na.rm = TRUE)

# Compare
sd_compare <- data.frame(
  Region = c("Middlesex County", "MA", "US"),
  SD = c(sd_county, sd_state, sd_us)
)

sd_compare
```

    ##             Region       SD
    ## 1 Middlesex County 5.829882
    ## 2               MA 6.514794
    ## 3               US 7.126422

Middlesex County has the lowest standard deviation across the three
regions, followed by Massachusetts, then the United States. Lower
standard deviation indicates that there’s less variability of
**kfr_pooled_pooled_p25** in the corresponding subset of the data.

5.  1.  Regression of **kfr_pooled_pooled_p25** on factor variables for
        **HOLC_grade**. W

``` r
# Linear regression
mod1 <- lm(kfr_pooled_pooled_p25 ~ factor(HOLC_grade), data = atlas)
coeftest(mod1, vcov = vcovHC(mod1, type="HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                     Estimate Std. Error  t value              Pr(>|t|)    
    ## (Intercept)         45.75933    0.27294 167.6517 < 0.00000000000000022 ***
    ## factor(HOLC_grade)2 -2.88808    0.31790  -9.0847 < 0.00000000000000022 ***
    ## factor(HOLC_grade)3 -5.17620    0.29349 -17.6365 < 0.00000000000000022 ***
    ## factor(HOLC_grade)4 -9.04786    0.29461 -30.7109 < 0.00000000000000022 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

As the coefficients for the lower ratings are negative and statistically
significant, the regression results indicate that the lower the HOLC
grade of a Census tract from the 1930s, the less upward mobility there
is for children born in the 1980s.

5.  2.  The relationship demonstrated in the previous section could
        reflect *compositional differences* across neighborhoods (e.g.,
        HOLC D grade neighborhoods are still highly segregated even
        today). As a first step in assessing whether this explanation is
        consistent with the data, we run a regression of
        **share_black2020** on factor variables for **HOLC_grade** to
        document any differences in racial composition. Is racial
        composition is a potential confounding variable?

``` r
# Linear regression
mod2 <- lm(share_black2020 ~ factor(HOLC_grade), data = atlas)
coeftest(mod2, vcov = vcovHC(mod2, type="HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                      Estimate Std. Error t value              Pr(>|t|)    
    ## (Intercept)         0.1458426  0.0073381 19.8747 < 0.00000000000000022 ***
    ## factor(HOLC_grade)2 0.0835805  0.0093108  8.9767 < 0.00000000000000022 ***
    ## factor(HOLC_grade)3 0.1068811  0.0082995 12.8781 < 0.00000000000000022 ***
    ## factor(HOLC_grade)4 0.1957045  0.0088261 22.1735 < 0.00000000000000022 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

As the coefficients for HOLC grades B, C, and D are all positive and
statistically significant, the regression results indicate that the
lower the HOLC grade of a Census tract from the 1930s, the greater the
Black share of the population in 2020. Therefore racial composition is a
potential confounding variable, meaning that it is plausible that racial
composition explains some of the gap in upward mobility across different
HOLC grades.

5.  3.  Next, we test for if the relationship between upward mobility
        and HOLC grade is still there *holding fixed race*. To do this,
        we run regressions of **kfr_black_pooled_p25** and
        **kfr_white_pooled_p25** on factor variables for **HOLC_grade**.

``` r
# Black pool
mod3 <- lm(kfr_black_pooled_p25 ~ factor(HOLC_grade), data = atlas)
coeftest(mod3, vcov = vcovHC(mod3, type="HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                     Estimate Std. Error  t value              Pr(>|t|)    
    ## (Intercept)         34.73301    0.33715 103.0185 < 0.00000000000000022 ***
    ## factor(HOLC_grade)2 -0.54788    0.37362  -1.4664                0.1426    
    ## factor(HOLC_grade)3 -1.75601    0.35224  -4.9852          0.0000006315 ***
    ## factor(HOLC_grade)4 -3.36549    0.34854  -9.6561 < 0.00000000000000022 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# White pool
mod4 <- lm(kfr_white_pooled_p25 ~ factor(HOLC_grade), data = atlas)
coeftest(mod4, vcov = vcovHC(mod4, type="HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                     Estimate Std. Error  t value              Pr(>|t|)    
    ## (Intercept)         50.84968    0.29913 169.9909 < 0.00000000000000022 ***
    ## factor(HOLC_grade)2 -2.65115    0.34975  -7.5801   0.00000000000003756 ***
    ## factor(HOLC_grade)3 -5.16886    0.32826 -15.7462 < 0.00000000000000022 ***
    ## factor(HOLC_grade)4 -7.92148    0.34703 -22.8266 < 0.00000000000000022 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Racial composition definitely cannot be a confounder in this analysis
because the regressions focus exclusively on upward mobility for one
race (either Black children or White children only). Thus racial
composition cannot explain any of the results we see.

The coefficients in the regression results are negative and
statistically significant, which indicates that the lower the HOLC
ratings for a Census tract, the worse the upward mobility is for both
Black and White children. This suggests that redlining affects upward
mobility for children of all races, and that factors other than racial
composition play a role in the upward mobility gap across HOLC grade
levels.

5.  4.  Regressions of other variables (homeownership2010, vegetation,
        and extreme_heat) on factor variables for HOLC_grade.

``` r
# Home ownership
mod5 <- lm(homeownership2010 ~ factor(HOLC_grade), data = atlas)
coeftest(mod5, vcov = vcovHC(mod5, type="HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                       Estimate Std. Error t value              Pr(>|t|)    
    ## (Intercept)          0.6201309  0.0067176  92.314 < 0.00000000000000022 ***
    ## factor(HOLC_grade)2 -0.1243361  0.0079563 -15.627 < 0.00000000000000022 ***
    ## factor(HOLC_grade)3 -0.2129818  0.0072575 -29.346 < 0.00000000000000022 ***
    ## factor(HOLC_grade)4 -0.3044947  0.0072556 -41.967 < 0.00000000000000022 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Vegetation
mod6 <- lm(vegetation ~ factor(HOLC_grade), data = atlas)
coeftest(mod6, vcov = vcovHC(mod6, type="HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                       Estimate Std. Error t value              Pr(>|t|)    
    ## (Intercept)         -0.0746250  0.0031205 -23.914 < 0.00000000000000022 ***
    ## factor(HOLC_grade)2 -0.0772057  0.0041199 -18.740 < 0.00000000000000022 ***
    ## factor(HOLC_grade)3 -0.0985446  0.0035628 -27.660 < 0.00000000000000022 ***
    ## factor(HOLC_grade)4 -0.1460192  0.0037781 -38.649 < 0.00000000000000022 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Extreme Heat
mod7 <- lm(extreme_heat ~ factor(HOLC_grade), data = atlas)
coeftest(mod7, vcov = vcovHC(mod7, type="HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                     Estimate Std. Error t value              Pr(>|t|)    
    ## (Intercept)          3.70303    0.12856 28.8030 < 0.00000000000000022 ***
    ## factor(HOLC_grade)2  1.45947    0.14827  9.8435 < 0.00000000000000022 ***
    ## factor(HOLC_grade)3  1.83075    0.13693 13.3700 < 0.00000000000000022 ***
    ## factor(HOLC_grade)4  2.21008    0.14086 15.6903 < 0.00000000000000022 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Based on the sign of the coefficients for each variable and their
statistical significance, we find that the lower the HOLC grade for a
Census tract, the lower the homeownership rate, the lower the
vegetation, and the higher the extreme heat. This suggests that the
redlining has had lasting consequences across multiple dimensions, as
tracts with low HOLC grades continue to have economic and environmental
disparities. This likely drives part of the differences in mobility
across HOLC grades.

6.  1.  Note that currently, the EPA’s National Ambient Air Quality
        Standards (NAAQS) for annual average $\text{PM}_{2.5}$ levels is
        12 micrograms per cubic meter of air $(\mu g/m^3)$. What
        fraction of Census Tracts in the U.S. had pollution above this
        threshold in 1982? 1990? 2010? Has air pollution worsened or
        improved over time?

``` r
# EPA threshold
threshold <- 12

frac_1982 <- mean(atlas$pm25_1982 > threshold, na.rm = TRUE)
frac_1990 <- mean(atlas$pm25_1990 > threshold, na.rm = TRUE)
frac_2010 <- mean(atlas$pm25_2010 > threshold, na.rm = TRUE)

pollution_compare <- data.frame(
  Year = c("1982", "1990", "2010"),
  Fraction_Above_Threshold = c(frac_1982, frac_1990, frac_2010)
)

pollution_compare
```

    ##   Year Fraction_Above_Threshold
    ## 1 1982                0.9294327
    ## 2 1990                0.8411749
    ## 3 2010                0.1185486

The proportion of Census tracts with pollution above the threshold has
clearly decreased over time, so air pollution has improved over time.

6.  2.  In 1982 was air quality in Middlesex county better or worse than
        other parts of United States? What has happened over time?

``` r
# Middlesex County (my_county subset)
frac_1982_home <- mean(my_county$pm25_1982 > threshold, na.rm = TRUE)
frac_1990_home <- mean(my_county$pm25_1990 > threshold, na.rm = TRUE)
frac_2010_home <- mean(my_county$pm25_2010 > threshold, na.rm = TRUE)

pollution_middlesex <- data.frame(
  Year = c("1982", "1990", "2010"),
  Fraction_Above_Threshold_Middlesex = c(frac_1982_home, frac_1990_home, frac_2010_home)
)

pollution_middlesex
```

    ##   Year Fraction_Above_Threshold_Middlesex
    ## 1 1982                          1.0000000
    ## 2 1990                          0.9968454
    ## 3 2010                          0.0000000

In 1982 the air quality in my home county (Middlesex) was significantly
worse, with all tracts having pollution above the threshold. It didn’t
get much better in 1990, with nearly all tracts having pollution above
the threshold. In 1982 and 1990, Middlesex county performed below the
national average. But in 2010, we had no tracts with pollution above the
threshold, doing better than the national average. Therefore, it appears
that air pollution in Middlesex county has significantly improved
between 1990 and 2010.

7.  1.  New variable for the difference in Absolute Mobility at the 25th
        Percentile for white and Black men, defined as
        **kir_white_male_p25** minus **kir_black_male_p25**.

``` r
atlas$kir_diff_p25 <- atlas$kir_white_male_p25 - atlas$kir_black_male_p25
```

7.  2.  Relationship between the new variable above and the poverty rate
        in 1990 (**poor_share1990**) visualized with a binned scatter
        plot.

``` r
ggplot(atlas, aes(x = poor_share1990, y = kir_diff_p25)) +
  stat_smooth(method = "lm", se = FALSE) +
  stat_binmean(n=20, geom = "point") +
  labs(x = "Poverty Rate in 1990",
       y = "Difference in Absolute Mobility at the 25th Percentile for white and Black men",
       title = "Difference in Mobility across race vs 1990 Poverty Rate")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 52350 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 52350 rows containing non-finite outside the scale range
    ## (`stat_binmean()`).

![](part1_files/figure-gfm/unnamed-chunk-17-1.png)<!-- --> 7. c. Are
racial disparities between white and Black men bigger or smaller in more
affluent neighborhoods?

The binned scatter plot displays a clearly negative correlation between
the two variables - as 1990 Poverty Rate increases, the difference in
upward mobility between white and Black men decreases. Therefore racial
disparities between white and Black men are bigger in more affluent
neighborhoods.

7.  4.  Hypothesis that might explain the previous result.

One hypothesis is that in affluent neighborhoods, there exist more
opportunities that increase upward mobility, but Black men are more
likely to be excluded from such opportunities due to discrimination,
meaning white men benefit significantly more from the affluence of a
neighborhood compared to Black men.

8.  1.  Merging the **atlas.dta** data set with the cross-walk data set
        **zip_tracts_xwalk.dta** that connects each Census tract with
        one or more zip codes.

``` r
xwalk <- read_dta("zip_tracts_xwalk.dta")

newdf <- left_join(
  atlas,
  xwalk,
  by = c("tract", "county", "state")
)
```

8.  2.  Collapsing the data to obtain a data set containing the weighted
        average of **kfr_pooled_pooled_p25** for each zip code. The
        grouping variable is **zip** and the weight is **zpoppct**, the
        percent of the zip code’s population contained in each Census
        tract.

``` r
atlas_zip <- newdf %>%
  group_by(zip) %>%
  summarize(
    kfr_zip = weighted.mean(kfr_pooled_pooled_p25, w = zpoppct, na.rm = TRUE)
  )
```

8.  3.  Merging the collapsed mobility data set with the social capital
        data **social_capital_zip.dta.**

``` r
social_cap <- read_dta("social_capital_zip.dta")

atlas_zip_sc <- left_join(
  atlas_zip,
  social_cap,
  by = "zip"
)
```

8.  4.  Calculating the correlation coefficient (not regression
        coefficient) between **kfr_pooled_pooled_p25** and each of the
        following measures of social capital: (i) economic connectedness
        (**ec_zip**), cohesiveness (**clustering_zip**), and civic
        engagement (**civic_organizations_zip**).

``` r
corr_ec <- cor(atlas_zip_sc$kfr_zip, atlas_zip_sc$ec_zip, use = "pairwise.complete.obs")
corr_clustering <- cor(atlas_zip_sc$kfr_zip, atlas_zip_sc$clustering_zip, use = "pairwise.complete.obs")
corr_civic <- cor(atlas_zip_sc$kfr_zip, atlas_zip_sc$civic_organizations_zip, use = "pairwise.complete.obs")

social_cap_compare <- data.frame(
  Social_Capital_Measure = c("Economic Connectedness", "Cohesiveness", "Civic Engagement"),
  Correlation_Coefficient = c(corr_ec, corr_clustering, corr_civic)
)

social_cap_compare
```

    ##   Social_Capital_Measure Correlation_Coefficient
    ## 1 Economic Connectedness             0.693258083
    ## 2           Cohesiveness            -0.007367058
    ## 3       Civic Engagement            -0.008678115

8.  5.  Which measure of social capital is most strongly related to
        upward mobility?

The measure of social capital most strongly related to upward mobility
is clearly Economic Connectedness, as its correlation coefficient is
significantly higher than the correlation coefficients for the other
measures, which are effecitvely 0. Economic Connectedness also has the
only positive correlation coefficient.

9.  Hypothesis (to be tested in Part 2).

The variation in upward mobility across Middlesex County can be
partially explained by differences in public education quality and
economic connectedness. Towns such as Lexington, Belmont, and Acton have
historically had public schools with higher standardized test scores,
greater access to AP courses, and stronger college placement rates.
These factors likely contributed to greater economic mobility for
children growing up in these areas during the 1980s and 1990s.

Higher-quality education can attract more highly educated and wealthier
families, increasing economic connectedness as people would have greater
exposure to high-income people. This can create a self-reinforcing cycle
that increases differences in upward mobility across towns, where as
more affluent families move in to certain areas, property values rise,
making it harder for low-income families to live in such areas.
