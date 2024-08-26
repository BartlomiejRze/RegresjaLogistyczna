> acs_ny$binary <- with(acs_ny, FamilyIncome >= 150000)

> library(useful)
> ggplot(acs_ny, aes(x=FamilyIncome))
> ggplot(acs_ny, aes(x=FamilyIncome))+geom_density(fill="grey", color="grey")
> ggplot(acs_ny, aes(x=FamilyIncome))+geom_density(fill="grey", color="grey")+geom_vline(xintercept = 150000)
> ggplot(acs_ny, aes(x=FamilyIncome)) + geom_density(fill='grey') + geom_vline(xintercept = 150000) + scale_x_continuous(label=multiple.dollar, limits = c(0, 1000000))
Warning message:
Removed 13 rows containing non-finite values (`stat_density()`). 
> help("glm")

> income <- glm(binary ~ HouseCosts + NumWorkers + OwnRent + NumBedrooms + FamilyType, data = acs_ny, family = binomial(link="logit"))
> income

Call:  glm(formula = binary ~ HouseCosts + NumWorkers + OwnRent + NumBedrooms + 
    FamilyType, family = binomial(link = "logit"), data = acs_ny)

Coefficients:
        (Intercept)           HouseCosts           NumWorkers      OwnRentOutright        OwnRentRented          NumBedrooms  
         -5.7377781            0.0007398            0.5610784            1.7723810           -0.8885524            0.2338601  
FamilyTypeMale Head    FamilyTypeMarried  
          0.3335577            1.4050481  

Degrees of Freedom: 22744 Total (i.e. Null);  22737 Residual
Null Deviance:	    22810 
Residual Deviance: 18070 	AIC: 18090

> summary(income)

Call:
glm(formula = binary ~ HouseCosts + NumWorkers + OwnRent + NumBedrooms + 
    FamilyType, family = binomial(link = "logit"), data = acs_ny)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.8452  -0.6246  -0.4231  -0.1743   2.9503  

Coefficients:
                      Estimate Std. Error z value Pr(>|z|)    
(Intercept)         -5.738e+00  1.185e-01 -48.421   <2e-16 ***
HouseCosts           7.398e-04  1.724e-05  42.908   <2e-16 ***
NumWorkers           5.611e-01  2.588e-02  21.684   <2e-16 ***
OwnRentOutright      1.772e+00  2.075e-01   8.541   <2e-16 ***
OwnRentRented       -8.886e-01  1.002e-01  -8.872   <2e-16 ***
NumBedrooms          2.339e-01  1.683e-02  13.895   <2e-16 ***
FamilyTypeMale Head  3.336e-01  1.472e-01   2.266   0.0235 *  
FamilyTypeMarried    1.405e+00  8.704e-02  16.143   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 22808  on 22744  degrees of freedom
Residual deviance: 18073  on 22737  degrees of freedom
AIC: 18089

Number of Fisher Scoring iterations: 6 #liczba iteracji algorytmu Fishera

> coef(income)
        (Intercept)          HouseCosts          NumWorkers     OwnRentOutright       OwnRentRented         NumBedrooms 
      -5.7377781209        0.0007397992        0.5610784482        1.7723810390       -0.8885523627        0.2338600719 
FamilyTypeMale Head   FamilyTypeMarried 
       0.3335577219        1.4050480683

> coefficients(income)
        (Intercept)          HouseCosts          NumWorkers     OwnRentOutright       OwnRentRented         NumBedrooms 
      -5.7377781209        0.0007397992        0.5610784482        1.7723810390       -0.8885523627        0.2338600719 
FamilyTypeMale Head   FamilyTypeMarried 
       0.3335577219        1.4050480683
#uzyskujemy ten sam wynik

> deviance(income)
[1] 18073.32

> library(coefplot)
> coefplot(income1)
> ggplot(acs_ny, aes(x=NumChildren)) + geom_histogram(binwidth = 1)
> children <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent, data = acs_ny, family = poisson(link = "log"))
> summary(children)

Call:
glm(formula = NumChildren ~ FamilyIncome + FamilyType + OwnRent, 
    family = poisson(link = "log"), data = acs_ny)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.9950  -1.3235  -1.2045   0.9464   6.3781  

Coefficients:
                      Estimate Std. Error z value Pr(>|z|)    
(Intercept)         -3.257e-01  2.103e-02 -15.491  < 2e-16 ***
FamilyIncome         5.420e-07  6.572e-08   8.247  < 2e-16 ***
FamilyTypeMale Head -6.298e-02  3.847e-02  -1.637    0.102    
FamilyTypeMarried    1.440e-01  2.147e-02   6.707 1.98e-11 ***
OwnRentOutright     -1.974e+00  2.292e-01  -8.611  < 2e-16 ***
OwnRentRented        4.086e-01  2.067e-02  19.773  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 35240  on 22744  degrees of freedom
Residual deviance: 34643  on 22739  degrees of freedom
AIC: 61370

Number of Fisher Scoring iterations: 5

> coefplot(children)
> z <- (acs_ny$NumChildren - children$fitted.values) / sqrt(children$fitted.values)
> sum(z^2) / children$df.residual
[1] 1.469747 #współczynnik rosproszenia o wartości 2 oznacza nadmierne rozproszenie 
> # p wartość dla nadmiernego rozproszenia
> pchisq(sum(z^2), children$df.residual)
[1] 1
> #rozkład pascala, ujemny rozkład dwumianowy
> children2 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent, data = acs_ny, family = quasipoisson(link = "log"))


> ggplot(wifi, aes(x = x, y = y, color = Distance ))
> ggplot(wifi, aes(x = x, y = y, color = Distance )) + geom_point()
> ggplot(wifi, aes(x = x, y = y, color = Distance )) + geom_point() + scale_color_gradient2(low = "black", mid = "gray", high = "white", midpoint = mean(wifi$Distance))
> wifiM1 <- nls(Distance ~ sqrt((betaX - x)^2 + (betaY - y)^2), data = wifi, start = list(betaX = 50, betaY = 50))
> summary(wifiM1)

Formula: Distance ~ sqrt((betaX - x)^2 + (betaY - y)^2)

Parameters:
      Estimate Std. Error t value Pr(>|t|)    
betaX   17.851      1.289   13.85   <2e-16 ***
betaY   52.906      1.476   35.85   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 13.73 on 198 degrees of freedom

Number of iterations to convergence: 6 
Achieved convergence tolerance: 3.846e-06


