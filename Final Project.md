Homework #6 
============================================================
_Huiting Ma_ 
 
 
**This homework will focus on the following parts:**
* <div id="Introduction1">[Introduction](#Introduction2)
* <div id="Data Cleaning1">[Data Cleaning](#Data Cleaning2)
* <div id="Data Aggregation and Visulation1">[Data Aggregation and Visulation](#Data Aggregation and Visulation2)
* <div id="Conclusion1">[Conclusion](#Conclusion2)
  



### <div id="Introduction2">[Introduction](#Introduction1)
This homework is to analyze the **Natural Disasters** around the world from the year 2000 to 2013. The natural disasters include earthquake, volcano and mass movement. My main focus is to identify which continents or countries have more natural disasters and whether the number of death, injuries, homeless and the population affected are correlated with the damaged amount. The dataset I am going to use is from EM-DAT, which is the International Disaster Database [here](http://www.emdat.be/database).

The definition of all variables can be found in the above website, which are:

- `NumDisaster` A unique disaster number for each event 
- `Country` Country (ies) in which the disaster has occurred
- `Year` When the disaster occurred. 
- `NumKilled` Persons confirmed as dead and persons missing and presumed dead (official figures when available)
- `NumInjured` People suffering from physical injuries, trauma or an illness requiring medical treatment as a direct result of a disaster
- `NumHomeless` People needing immediate assistance for shelter
- `NumAffected` People requiring immediate assistance during a period of emergency; it can also include displaced or evacuated people
- `TotalDamUSD` Several institutions have developed methodologies to quantify these losses in their specific domain. However, there is no standard procedure to determine a global figure for economic impact. Estimated damages are given (000') US$

### <div id="Data Cleaning2">[Data Cleaning](#Data Cleaning1)


```r
library(plyr)
library(xtable)
library(ggplot2)
library(reshape2)
```


Supercheck whether data has imported correctly

```r
NaturalDisaster <- read.csv("disaster.csv") 
str(NaturalDisaster)
```

```
## 'data.frame':	1648 obs. of  9 variables:
##  $ Country    : Factor w/ 188 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ Year       : int  2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 ...
##  $ NumDisaster: int  6 6 16 9 3 14 13 7 4 5 ...
##  $ NumKilled  : int  594 485 4083 137 18 582 382 296 1334 101 ...
##  $ NumInjured : int  0 20 1391 4 40 44 185 20 182 86 ...
##  $ NumAffected: int  2582228 204425 302279 500 2800 37901 2225515 26755 452602 62521 ...
##  $ NumHomeless: int  0 250 10000 4250 2700 6775 8210 3480 180 3250 ...
##  $ TotalDamUSD: int  50 10 0 0 0 5050 0 0 0 20000 ...
##  $ Continent  : Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
```


Learn from Jennie, to define a function for converting and printing to HTML table.

```r
htmlPrint <- function(x, ...,
                      digits = 0, include.rownames = FALSE) {
  print(xtable(x, digits = digits, ...), type = 'html',
        include.rownames = include.rownames, ...)
}
```


Try to count the number of natural disaster over time on different continents

```r
numCountByYear <- daply(NaturalDisaster,~Year + 
                          Continent, summarize, 
                        TotalCount = sum(NumDisaster))
numCountByYear <- as.data.frame(numCountByYear)
htmlPrint(numCountByYear)
```

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Sat Oct 19 19:00:44 2013 -->
<TABLE border=1>
<TR> <TH> Africa </TH> <TH> Americas </TH> <TH> Asia </TH> <TH> Europe </TH> <TH> Oceania </TH>  </TR>
  <TR> <TD align="right"> 122 </TD> <TD align="right"> 101 </TD> <TD align="right"> 195 </TD> <TD align="right"> 95 </TD> <TD align="right"> 11 </TD> </TR>
  <TR> <TD align="right"> 114 </TD> <TD align="right"> 97 </TD> <TD align="right"> 164 </TD> <TD align="right"> 53 </TD> <TD align="right"> 19 </TD> </TR>
  <TR> <TD align="right"> 111 </TD> <TD align="right"> 116 </TD> <TD align="right"> 187 </TD> <TD align="right"> 90 </TD> <TD align="right"> 20 </TD> </TR>
  <TR> <TD align="right"> 83 </TD> <TD align="right"> 89 </TD> <TD align="right"> 146 </TD> <TD align="right"> 51 </TD> <TD align="right"> 17 </TD> </TR>
  <TR> <TD align="right"> 85 </TD> <TD align="right"> 89 </TD> <TD align="right"> 169 </TD> <TD align="right"> 39 </TD> <TD align="right"> 19 </TD> </TR>
  <TR> <TD align="right"> 100 </TD> <TD align="right"> 95 </TD> <TD align="right"> 186 </TD> <TD align="right"> 99 </TD> <TD align="right"> 14 </TD> </TR>
  <TR> <TD align="right"> 118 </TD> <TD align="right"> 73 </TD> <TD align="right"> 183 </TD> <TD align="right"> 51 </TD> <TD align="right"> 18 </TD> </TR>
  <TR> <TD align="right"> 108 </TD> <TD align="right"> 103 </TD> <TD align="right"> 159 </TD> <TD align="right"> 66 </TD> <TD align="right"> 9 </TD> </TR>
  <TR> <TD align="right"> 104 </TD> <TD align="right"> 96 </TD> <TD align="right"> 147 </TD> <TD align="right"> 33 </TD> <TD align="right"> 12 </TD> </TR>
  <TR> <TD align="right"> 93 </TD> <TD align="right"> 84 </TD> <TD align="right"> 142 </TD> <TD align="right"> 42 </TD> <TD align="right"> 19 </TD> </TR>
  <TR> <TD align="right"> 100 </TD> <TD align="right"> 109 </TD> <TD align="right"> 142 </TD> <TD align="right"> 70 </TD> <TD align="right"> 15 </TD> </TR>
  <TR> <TD align="right"> 76 </TD> <TD align="right"> 97 </TD> <TD align="right"> 150 </TD> <TD align="right"> 18 </TD> <TD align="right"> 11 </TD> </TR>
  <TR> <TD align="right"> 68 </TD> <TD align="right"> 85 </TD> <TD align="right"> 147 </TD> <TD align="right"> 64 </TD> <TD align="right"> 11 </TD> </TR>
  <TR> <TD align="right"> 19 </TD> <TD align="right"> 16 </TD> <TD align="right"> 48 </TD> <TD align="right"> 11 </TD> <TD align="right"> 5 </TD> </TR>
   </TABLE>


Seems 2013 does not have a lot of observations, Let's drop 2013.

```r
NDisaster <- droplevels(subset(NaturalDisaster,Year != "2013"))
table(NDisaster$Year) #Check whether 2013 has dropped
```

```
## 
## 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 
##  128  120  128  118  122  139  106  134  121  114  135  105  120
```


Try to count the number of countries in each continents

```r
numCountries <- ddply(NDisaster, ~Continent, summarize, numCoutries = length(unique(Country)))
htmlPrint(numCountries)
```

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Sat Oct 19 19:00:44 2013 -->
<TABLE border=1>
<TR> <TH> Continent </TH> <TH> numCoutries </TH>  </TR>
  <TR> <TD> Africa </TD> <TD align="right"> 50 </TD> </TR>
  <TR> <TD> Americas </TD> <TD align="right"> 37 </TD> </TR>
  <TR> <TD> Asia </TD> <TD align="right"> 45 </TD> </TR>
  <TR> <TD> Europe </TD> <TD align="right"> 40 </TD> </TR>
  <TR> <TD> Oceania </TD> <TD align="right"> 16 </TD> </TR>
   </TABLE>


Based on the graph, it is found that "Oceania" does not have a lot of data, drop it!

```r
NDisaster <- droplevels(subset(NaturalDisaster,Continent != "Oceania"))
table(NDisaster$Continent) #Check whether "Oceania" has dropped
```

```
## 
##   Africa Americas     Asia   Europe 
##      501      339      399      316
```


Also, I found there are some missing data for Number of affected. Let's drop these data!

```r
NDisaster <- droplevels(subset(NDisaster,NumAffected != "0"))
str(NDisaster)
```

```
## 'data.frame':	1280 obs. of  9 variables:
##  $ Country    : Factor w/ 165 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ Year       : int  2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 ...
##  $ NumDisaster: int  6 6 16 9 3 14 13 7 4 5 ...
##  $ NumKilled  : int  594 485 4083 137 18 582 382 296 1334 101 ...
##  $ NumInjured : int  0 20 1391 4 40 44 185 20 182 86 ...
##  $ NumAffected: int  2582228 204425 302279 500 2800 37901 2225515 26755 452602 62521 ...
##  $ NumHomeless: int  0 250 10000 4250 2700 6775 8210 3480 180 3250 ...
##  $ TotalDamUSD: int  50 10 0 0 0 5050 0 0 0 20000 ...
##  $ Continent  : Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
```



Since there also a lot of missing data for Total damage, let's drop these values.

```r
Disaster <- droplevels(subset(NDisaster,TotalDamUSD != "0"))
str(Disaster)
```

```
## 'data.frame':	561 obs. of  9 variables:
##  $ Country    : Factor w/ 121 levels "Afghanistan",..: 1 1 1 1 1 2 2 3 3 3 ...
##  $ Year       : int  2000 2001 2005 2009 2011 2002 2004 2001 2002 2005 ...
##  $ NumDisaster: int  6 6 14 5 4 3 2 1 3 4 ...
##  $ NumKilled  : int  594 485 582 101 83 7 3 921 48 34 ...
##  $ NumInjured : int  0 20 44 86 115 0 0 423 20 5 ...
##  $ NumAffected: int  2582228 204425 37901 62521 1753000 192110 2500 45000 2285 70 ...
##  $ NumHomeless: int  0 250 6775 3250 9700 0 0 0 0 1750 ...
##  $ TotalDamUSD: int  50 10 5050 20000 142000 17500 173 300000 1500 7256 ...
##  $ Continent  : Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 4 4 1 1 1 ...
```


Since I want to fit **linear regression model** in the second script, it is necessary to make sure
all countries should have multiple observations. I will delete the countries with only 2 or less
observations.


```r
NumObsCountry <- ddply(Disaster, ~Country, summarize, numobs = length(Year))
CountryDrop <- droplevels(subset(NumObsCountry,!(numobs > 2)))
Disaster <- droplevels(subset(Disaster,
                              !(Country %in% CountryDrop$Country)))
```



```r
DropNumObsCountry <- ddply(Disaster, ~Country, summarize, numobs = length(Year))
htmlPrint(DropNumObsCountry)
```

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Sat Oct 19 19:00:44 2013 -->
<TABLE border=1>
<TR> <TH> Country </TH> <TH> numobs </TH>  </TR>
  <TR> <TD> Afghanistan </TD> <TD align="right"> 5 </TD> </TR>
  <TR> <TD> Algeria </TD> <TD align="right"> 7 </TD> </TR>
  <TR> <TD> Argentina </TD> <TD align="right"> 7 </TD> </TR>
  <TR> <TD> Bangladesh </TD> <TD align="right"> 4 </TD> </TR>
  <TR> <TD> Belize </TD> <TD align="right"> 4 </TD> </TR>
  <TR> <TD> Bolivia </TD> <TD align="right"> 7 </TD> </TR>
  <TR> <TD> Bosnia-Hercegovenia </TD> <TD align="right"> 3 </TD> </TR>
  <TR> <TD> Brazil </TD> <TD align="right"> 11 </TD> </TR>
  <TR> <TD> Bulgaria </TD> <TD align="right"> 3 </TD> </TR>
  <TR> <TD> Cambodia </TD> <TD align="right"> 5 </TD> </TR>
  <TR> <TD> Canada </TD> <TD align="right"> 7 </TD> </TR>
  <TR> <TD> Chile </TD> <TD align="right"> 9 </TD> </TR>
  <TR> <TD> China P Rep </TD> <TD align="right"> 14 </TD> </TR>
  <TR> <TD> Colombia </TD> <TD align="right"> 5 </TD> </TR>
  <TR> <TD> Costa Rica </TD> <TD align="right"> 6 </TD> </TR>
  <TR> <TD> Cuba </TD> <TD align="right"> 6 </TD> </TR>
  <TR> <TD> Czech Rep </TD> <TD align="right"> 3 </TD> </TR>
  <TR> <TD> Dominican Rep </TD> <TD align="right"> 6 </TD> </TR>
  <TR> <TD> Ecuador </TD> <TD align="right"> 5 </TD> </TR>
  <TR> <TD> El Salvador </TD> <TD align="right"> 5 </TD> </TR>
  <TR> <TD> France </TD> <TD align="right"> 8 </TD> </TR>
  <TR> <TD> Georgia </TD> <TD align="right"> 3 </TD> </TR>
  <TR> <TD> Germany </TD> <TD align="right"> 3 </TD> </TR>
  <TR> <TD> Greece </TD> <TD align="right"> 4 </TD> </TR>
  <TR> <TD> Guatemala </TD> <TD align="right"> 6 </TD> </TR>
  <TR> <TD> Haiti </TD> <TD align="right"> 6 </TD> </TR>
  <TR> <TD> Honduras </TD> <TD align="right"> 8 </TD> </TR>
  <TR> <TD> Hungary </TD> <TD align="right"> 5 </TD> </TR>
  <TR> <TD> India </TD> <TD align="right"> 13 </TD> </TR>
  <TR> <TD> Indonesia </TD> <TD align="right"> 14 </TD> </TR>
  <TR> <TD> Iran Islam Rep </TD> <TD align="right"> 9 </TD> </TR>
  <TR> <TD> Italy </TD> <TD align="right"> 6 </TD> </TR>
  <TR> <TD> Jamaica </TD> <TD align="right"> 6 </TD> </TR>
  <TR> <TD> Japan </TD> <TD align="right"> 12 </TD> </TR>
  <TR> <TD> Kazakhstan </TD> <TD align="right"> 5 </TD> </TR>
  <TR> <TD> Kenya </TD> <TD align="right"> 5 </TD> </TR>
  <TR> <TD> Korea Dem P Rep </TD> <TD align="right"> 6 </TD> </TR>
  <TR> <TD> Korea Rep </TD> <TD align="right"> 9 </TD> </TR>
  <TR> <TD> Madagascar </TD> <TD align="right"> 8 </TD> </TR>
  <TR> <TD> Malaysia </TD> <TD align="right"> 4 </TD> </TR>
  <TR> <TD> Mexico </TD> <TD align="right"> 13 </TD> </TR>
  <TR> <TD> Moldova Rep </TD> <TD align="right"> 4 </TD> </TR>
  <TR> <TD> Mongolia </TD> <TD align="right"> 3 </TD> </TR>
  <TR> <TD> Morocco </TD> <TD align="right"> 4 </TD> </TR>
  <TR> <TD> Mozambique </TD> <TD align="right"> 6 </TD> </TR>
  <TR> <TD> Myanmar </TD> <TD align="right"> 5 </TD> </TR>
  <TR> <TD> Nepal </TD> <TD align="right"> 5 </TD> </TR>
  <TR> <TD> Nicaragua </TD> <TD align="right"> 3 </TD> </TR>
  <TR> <TD> Nigeria </TD> <TD align="right"> 7 </TD> </TR>
  <TR> <TD> Pakistan </TD> <TD align="right"> 8 </TD> </TR>
  <TR> <TD> Panama </TD> <TD align="right"> 3 </TD> </TR>
  <TR> <TD> Paraguay </TD> <TD align="right"> 3 </TD> </TR>
  <TR> <TD> Peru </TD> <TD align="right"> 3 </TD> </TR>
  <TR> <TD> Philippines </TD> <TD align="right"> 14 </TD> </TR>
  <TR> <TD> Poland </TD> <TD align="right"> 5 </TD> </TR>
  <TR> <TD> Puerto Rico </TD> <TD align="right"> 3 </TD> </TR>
  <TR> <TD> Romania </TD> <TD align="right"> 4 </TD> </TR>
  <TR> <TD> Russia </TD> <TD align="right"> 11 </TD> </TR>
  <TR> <TD> South Africa </TD> <TD align="right"> 7 </TD> </TR>
  <TR> <TD> Spain </TD> <TD align="right"> 9 </TD> </TR>
  <TR> <TD> Sri Lanka </TD> <TD align="right"> 8 </TD> </TR>
  <TR> <TD> Sudan </TD> <TD align="right"> 3 </TD> </TR>
  <TR> <TD> Switzerland </TD> <TD align="right"> 4 </TD> </TR>
  <TR> <TD> Taiwan (China) </TD> <TD align="right"> 9 </TD> </TR>
  <TR> <TD> Tajikistan </TD> <TD align="right"> 11 </TD> </TR>
  <TR> <TD> Thailand </TD> <TD align="right"> 11 </TD> </TR>
  <TR> <TD> Turkey </TD> <TD align="right"> 7 </TD> </TR>
  <TR> <TD> Ukraine </TD> <TD align="right"> 7 </TD> </TR>
  <TR> <TD> United Kingdom </TD> <TD align="right"> 8 </TD> </TR>
  <TR> <TD> United States </TD> <TD align="right"> 14 </TD> </TR>
  <TR> <TD> Venezuela </TD> <TD align="right"> 5 </TD> </TR>
  <TR> <TD> Viet Nam </TD> <TD align="right"> 13 </TD> </TR>
  <TR> <TD> Zimbabwe </TD> <TD align="right"> 4 </TD> </TR>
   </TABLE>


Yes, countries with only few observations have been dropped. The last thing I want to do is 
to reorder continents based on the total nunmber of disasters happened.


```r
Disaster <- within(Disaster, Continent <- reorder(Continent, NumDisaster, sum))
Disaster <- arrange(Disaster,Continent)
```


### <div id="Data Aggregation and Visulation2">[Data Aggregation and Visulation](#Data Aggregation and Visulation1)

In this stage, dataset has been sucessfully cleaned to the format that I want. Let us get into 
**Exploratory Data Analysis** stage.

**First, try to get the spread of death within the continents.**


```r
spreaddeath <- ddply(Disaster, ~ Continent, summarize,
                     sdNumKilled = sd(NumKilled), 
                     iqrNumKilled = IQR(NumKilled))
htmlPrint(arrange(spreaddeath, sdNumKilled))
```

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Sat Oct 19 19:00:44 2013 -->
<TABLE border=1>
<TR> <TH> Continent </TH> <TH> sdNumKilled </TH> <TH> iqrNumKilled </TH>  </TR>
  <TR> <TD> Africa </TD> <TD align="right"> 264 </TD> <TD align="right"> 147 </TD> </TR>
  <TR> <TD> Europe </TD> <TD align="right"> 6802 </TD> <TD align="right"> 42 </TD> </TR>
  <TR> <TD> Asia </TD> <TD align="right"> 17676 </TD> <TD align="right"> 634 </TD> </TR>
  <TR> <TD> Americas </TD> <TD align="right"> 18674 </TD> <TD align="right"> 126 </TD> </TR>
   </TABLE>


It can be seen that there are a lot of variation for the number of killed people in natural disasters.


```r
newspread <- melt(spreaddeath, id="Continent")
ggplot(newspread, aes(x = Continent, y = value, colour = variable)) + 
     geom_point() + geom_line(aes(x=as.numeric(Continent)))+ ylab("spread") +
     ggtitle("Measure of Spread")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 


Try to produce the maximum and minimum statistics for 
different variables in all continents for all continents

```r
maxmean<- ddply(Disaster, ~ Continent, summarize, 
                meanDamage = mean(TotalDamUSD),
                maxDamage= max(TotalDamUSD),
                meanAffPop = mean(NumAffected),
                maxAffPop = max(NumAffected),
                meanInjured = mean(NumInjured),
                maxInjured = max(NumInjured),
                meanDeath = mean(NumKilled),
                maxDeath = max(NumKilled),
                meanHomeless = mean(NumHomeless),
                maxHomeless = max(NumHomeless)
)
htmlPrint(arrange(maxmean,maxDamage))
```

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Sat Oct 19 19:00:44 2013 -->
<TABLE border=1>
<TR> <TH> Continent </TH> <TH> meanDamage </TH> <TH> maxDamage </TH> <TH> meanAffPop </TH> <TH> maxAffPop </TH> <TH> meanInjured </TH> <TH> maxInjured </TH> <TH> meanDeath </TH> <TH> maxDeath </TH> <TH> meanHomeless </TH> <TH> maxHomeless </TH>  </TR>
  <TR> <TD> Africa </TD> <TD align="right"> 118076 </TD> <TD align="right"> 779000 </TD> <TD align="right"> 824687 </TD> <TD align="right"> 7015029 </TD> <TD align="right"> 128 </TD> <TD align="right"> 1200 </TD> <TD align="right"> 203 </TD> <TD align="right"> 921 </TD> <TD align="right"> 17465 </TD> <TD align="right"> 258450 </TD> </TR>
  <TR> <TD> Europe </TD> <TD align="right"> 1316447 </TD> <TD align="right"> 17137601 </TD> <TD align="right"> 87556 </TD> <TD align="right"> 2600000 </TD> <TD align="right"> 183 </TD> <TD align="right"> 5850 </TD> <TD align="right"> 1328 </TD> <TD align="right"> 55844 </TD> <TD align="right"> 1364 </TD> <TD align="right"> 30000 </TD> </TR>
  <TR> <TD> Americas </TD> <TD align="right"> 4122132 </TD> <TD align="right"> 159060330 </TD> <TD align="right"> 537394 </TD> <TD align="right"> 13390150 </TD> <TD align="right"> 4103 </TD> <TD align="right"> 577520 </TD> <TD align="right"> 1682 </TD> <TD align="right"> 229549 </TD> <TD align="right"> 14865 </TD> <TD align="right"> 800000 </TD> </TR>
  <TR> <TD> Asia </TD> <TD align="right"> 3576366 </TD> <TD align="right"> 212520000 </TD> <TD align="right"> 12551989 </TD> <TD align="right"> 342023353 </TD> <TD align="right"> 8179 </TD> <TD align="right"> 368719 </TD> <TD align="right"> 3563 </TD> <TD align="right"> 166691 </TD> <TD align="right"> 137750 </TD> <TD align="right"> 5003500 </TD> </TR>
   </TABLE>


After summarizing some detailed information about different 
variables in the dataset. Now, let's discouver more interesting areas.
Let's identify which varibales play an important role to predict 
the damaged amount from natural disasters.

First, Let us to fit the full model.

```r
FullModel <- lm(TotalDamUSD ~ NumDisaster + NumKilled + NumInjured +
                NumAffected +NumHomeless, data = Disaster)
summary(FullModel)
```

```
## 
## Call:
## lm(formula = TotalDamUSD ~ NumDisaster + NumKilled + NumInjured + 
##     NumAffected + NumHomeless, data = Disaster)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -38666229  -2518125   -747136    166490 206946995 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.14e+06   9.13e+05   -1.25     0.21    
## NumDisaster  6.34e+05   1.14e+05    5.58  4.1e-08 ***
## NumKilled    1.62e+01   5.72e+01    0.28     0.78    
## NumInjured   6.87e+01   2.67e+01    2.57     0.01 *  
## NumAffected -1.12e-02   2.68e-02   -0.42     0.68    
## NumHomeless -2.98e+00   1.94e+00   -1.54     0.12    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14100000 on 480 degrees of freedom
## Multiple R-squared:  0.111,	Adjusted R-squared:  0.102 
## F-statistic:   12 on 5 and 480 DF,  p-value: 5.73e-11
```


Based on the summmary, it can be shown that number of disasters 
played a significant role to determine the damaged amount. 
Therefore, I will pay more attention on whether
number of disasters also play an essential role in country level.

```r
mFun <- function(x) {
  model <- lm(TotalDamUSD ~ NumDisaster, x)
  estCoefs <- c(coef(model))
  # estSE <- c(se.coef(model))
  ## 2 means 2nd row (we do not want to test intercept), 
  ## 4 means the 4th column, which corresponding to p-value
  p_value <- summary(model)$coefficients[2, 4]  
  names(estCoefs) <- c("intercept", "NumDisaster")
  #names(estSE) <- c("SE(intercept)", "SE(NumDisaster)")
  names(p_value) <- "p-value (NumDisaster)"
  return (c(estCoefs,p_value))
}

mCoefs <- ddply(Disaster, ~Country, mFun)
print(mCoefs)
```

```
##                Country  intercept NumDisaster p-value (NumDisaster)
## 1          Afghanistan  8.053e+04  -6.730e+03             0.4589563
## 2              Algeria  4.192e+05  -7.655e+04             0.1692596
## 3            Argentina  3.517e+05   9.989e+03             0.9027709
## 4           Bangladesh  2.029e+06  -5.878e+04             0.8119140
## 5               Belize  1.625e+05  -1.631e+04             0.9353574
## 6              Bolivia -3.106e+04   5.384e+04             0.4841940
## 7  Bosnia-Hercegovenia  1.750e+05  -1.750e+04             0.8234795
## 8               Brazil  1.189e+06  -8.193e+04             0.4775022
## 9             Bulgaria -5.774e+05   1.292e+05             0.4567365
## 10            Cambodia  6.630e+05  -3.225e+05             0.0972609
## 11              Canada  2.467e+06  -3.347e+05             0.1638766
## 12               Chile  2.343e+07  -5.622e+06             0.1073906
## 13         China P Rep -3.802e+06   8.040e+05             0.5965933
## 14            Colombia  2.341e+06  -2.951e+05             0.2403580
## 15          Costa Rica  2.170e+05  -7.750e+04             0.1518295
## 16                Cuba -7.023e+05   6.903e+05             0.1475787
## 17           Czech Rep  6.762e+05   9.878e+04             0.9499379
## 18       Dominican Rep -5.039e+04   4.236e+04             0.2893147
## 19             Ecuador  6.549e+05  -1.396e+05             0.2795469
## 20         El Salvador  1.035e+06  -5.659e+04             0.8160431
## 21              France  8.089e+05   2.077e+05             0.7096798
## 22             Georgia  4.560e+05  -1.810e+05             0.3963003
## 23             Germany -6.612e+06   2.648e+06             0.4851640
## 24              Greece  1.117e+06  -1.241e+05             0.8102967
## 25           Guatemala  2.901e+05   2.358e+04             0.9005996
## 26               Haiti -8.410e+05   4.622e+05             0.4348501
## 27            Honduras -2.172e+04   2.292e+04             0.0363515
## 28             Hungary  4.467e+05  -2.117e+05             0.2633967
## 29               India -7.494e+05   1.670e+05             0.0618948
## 30           Indonesia -3.486e+02   7.951e+04             0.5044540
## 31      Iran Islam Rep  5.403e+04   2.293e+04             0.4979471
## 32               Italy  5.122e+06   1.184e+05             0.9373062
## 33             Jamaica  5.036e+05  -1.196e+05             0.6064703
## 34               Japan -1.466e+07   5.848e+06             0.4642779
## 35          Kazakhstan  1.184e+05  -5.018e+04             0.3660053
## 36               Kenya  7.958e+04  -8.492e+03             0.5442917
## 37     Korea Dem P Rep  7.045e+06  -2.994e+06             0.0666164
## 38           Korea Rep  2.211e+06  -2.341e+05             0.5290848
## 39          Madagascar  2.744e+05  -4.604e+04             0.2454068
## 40            Malaysia  6.645e+05  -6.805e+04             0.8137809
## 41              Mexico  5.382e+04   2.409e+05             0.5019379
## 42         Moldova Rep  3.872e+05  -1.837e+05             0.4608119
## 43            Mongolia -3.228e+04   3.987e+04             0.1960214
## 44             Morocco  1.492e+05  -5.214e+04             0.4775352
## 45          Mozambique -1.892e+05   6.052e+04             0.1149816
## 46             Myanmar  7.859e+06  -3.859e+06             0.0007387
## 47               Nepal -3.327e+03   4.499e+03             0.4487414
## 48           Nicaragua  1.029e+03  -3.571e+00             0.8789623
## 49             Nigeria  1.686e+05  -1.621e+04             0.4834093
## 50            Pakistan  1.565e+06   1.748e+05             0.6953090
## 51              Panama  1.233e+04  -3.250e+03             0.5332411
## 52            Paraguay  3.908e+04  -1.005e+04             0.1058932
## 53                Peru  2.143e+05   2.142e+04             0.8789929
## 54         Philippines -2.169e+05   3.705e+04             0.0136360
## 55              Poland  7.441e+05   1.539e+04             0.9875127
## 56         Puerto Rico  2.947e+05  -2.300e+04             0.9329778
## 57             Romania -6.334e+05   2.083e+05             0.1153793
## 58              Russia  8.353e+05  -6.734e+03             0.9125249
## 59        South Africa  2.898e+05  -2.456e+04             0.2677210
## 60               Spain  4.345e+05  -6.019e+04             0.5198923
## 61           Sri Lanka -2.065e+04   9.909e+04             0.4732060
## 62               Sudan -5.600e+04   9.343e+04             0.2020026
## 63         Switzerland  1.290e+06  -2.100e+05             0.6977024
## 64      Taiwan (China) -1.030e+05   1.062e+05             0.1380400
## 65          Tajikistan  2.199e+05  -3.533e+04             0.5734894
## 66            Thailand  2.047e+04   6.859e+05             0.7949296
## 67              Turkey  1.282e+06  -1.839e+05             0.3572760
## 68             Ukraine -8.176e+04   2.405e+05             0.3854931
## 69      United Kingdom -1.747e+06   1.117e+06             0.2348985
## 70       United States  5.639e+07  -8.762e+05             0.6313770
## 71           Venezuela  4.700e+04   8.167e+03             0.7943047
## 72            Viet Nam  2.013e+05   3.811e+04             0.4116813
## 73            Zimbabwe  1.164e+05  -1.343e+04             0.8139364
```

**Question:** For this part, I did not use `htmlPrint()` since I do not know how to control
decimals in r markdown. R markdown automatically round numbers.

Based on above table, we found that even though number of disasters play an critical role in the world level, 
it does not significant in country level. It might because the individual country has limited sample size. 
Thus, the standard error is huge.


**The next topic that I would like to focus one is how is the number of disasters changing over 
time on different continents.To begin with, let us plot the number of disasters in different continents.**


```r
ggplot(Disaster,aes(x=NumDisaster, fill= Continent)) + facet_wrap(~Continent)+
  geom_bar(binwidth = 2, color = "black") +
  ggtitle("Number of Disasters in Diffferent Continents")
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19.png) 


Now, let us look at the number of disasters changing over time on diffferent continents.


```r
p <- ggplot(Disaster, aes(x = Year, y = NumDisaster, color = Year))+ 
  geom_jitter() + facet_wrap(~ Continent) + 
  geom_line(stat = "summary", fun.y = "mean", col = "red", lwd = 1) +
  ggtitle("How is Number of Disasters Changing over Time on Diffferent Continents") + 
  scale_x_continuous(name = "Year", breaks = seq(min(Disaster$Year), 
                                                 max(Disaster$Year), by = 2))  + 
  xlab("Year") + ylab("Number of Disasters")
print(p)
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20.png) 


Try boxplot for the year 2000, 2005 and 2010

```r
ggplot(subset(Disaster, Year %in% c(2000,2005,2010)), aes(x = factor(Year), 
                                                          y = NumDisaster, 
                                                          fill = Continent), 
       groups = Continent) + geom_boxplot(alpha = 0.2, outlier.colour= "red") 
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21.png) 


Finally, let us try another plot about the number of disasters changing over 
time on diffferent continents

```r
low_number= 10
ggplot(Disaster, aes(x = Year, y = NumDisaster, 
                        colour = NumDisaster <= low_number)) + 
  geom_jitter(position = position_jitter(width = .2)) + 
  facet_wrap(~ Continent) + 
  ggtitle(paste("NumDisaster <= ", low_number)) + 
  theme(plot.title = element_text(face="bold")) + 
  scale_colour_discrete(name="",breaks=c("FALSE", "TRUE"),
                        labels=c("Death > 5", "Death <= 5"))+
  ggtitle("How is Number of Disasters Changing over Time on Diffferent Continents")
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22.png) 


Based on the above graph, it can be seen that Americas and Asia have a lot of natural disastes than other continents.
Let's have a close look at these two continents.

Delete other continents.

```r
AsiaAmerica <- subset(Disaster,Continent == c("Asia", "Americas"))
str(AsiaAmerica)
```

```
## 'data.frame':	173 obs. of  9 variables:
##  $ Country    : Factor w/ 73 levels "Afghanistan",..: 3 3 3 5 5 6 6 6 6 8 ...
##  $ Year       : int  2001 2007 2012 2000 2007 2001 2003 2007 2011 2001 ...
##  $ NumDisaster: int  7 3 3 1 1 2 4 4 2 5 ...
##  $ NumKilled  : int  35 19 25 14 0 50 143 116 52 98 ...
##  $ NumInjured : int  150 0 30 570 0 5 40 0 0 196 ...
##  $ NumAffected: int  254800 130000 17500 62000 20000 357250 48190 824723 92825 1001750 ...
##  $ NumHomeless: int  2000 5000 2000 0 0 0 10300 0 0 12500 ...
##  $ TotalDamUSD: int  765000 40000 10000 277460 14847 121000 1000 590000 20000 45000 ...
##  $ Continent  : Factor w/ 4 levels "Africa","Europe",..: 3 3 3 3 3 3 3 3 3 3 ...
```


**Here we go, we have only samples from Asia and America.The first thing that I would like to do is
to look at dots scatterplot of number of natural disaster over year for China. Why?**


```r
ggplot(subset(Disaster, Country == "China P Rep"), aes(x = Year, y = NumKilled)) + 
  geom_line() + xlab("Year") + ylab("Number killed") +
  ggtitle("How is Number of People Killed over time in People's Repubic of China") +
  scale_x_continuous(name = "Year", breaks = seq(min(NDisaster$Year), 
                                                 max(NDisaster$Year), by = 2)) 
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24.png) 


The graph shows that more than 80,000 were killed by natural disaster 
in the year of 2008 in China. That was Sichuan Earthquake. At that time, 
I was also in Sichuan and experienced this catastrophe. I feel sorry for 
those people who lost their lives during this disaster.Since I experienced 
this natural disaster, I would think this is an important plot for me!!

**Next, I would like to evaluate whether there is a relationship between affected population and death across continents.**

The question is that whether more people got affected will lead more people be killed? Since some natural disasters caused a lot of people lost lives, 
I used log transformation (**Question** Log transformation may not make sense?)


```r
ggplot(NDisaster, aes(x = NumAffected, y = NumKilled, color = Continent)) + 
  geom_point() + scale_x_log10() + scale_y_log10()+
  ggtitle("How NumKilled related to NumAffected across Continents") 
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25.png) 

Based on the graph, it can be seen that with the increase of number of people got affected,
there is a slightly increasing trend for the number of people got killed.

### <div id="Conclusion2">[Conclusion](#Conclusion1)
In conclusion, the number of disasters played a significant role to determine the total damaged amount in population level but not in country level. In addition, with the increase of number of people got affected,
there is a slightly increasing trend for the number of people got killed.
