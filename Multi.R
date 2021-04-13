library(tidyverse)

#Dataset for mtcars
mt=mtcars

summary(mt)

plot(mpg~hp,data=mt)  #lesser the hp more the mpg
plot(mpg~wt,mt)#lesser the wt more the mpg


# lets models thdata

mtmodel=lm(mpg~hp+wt,data=mt)
summary(mtmodel)

R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
Copyright (C) 2020 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> library(tidyverse)
-- Attaching packages -------------------------------- tidyverse 1.3.0 --
  v ggplot2 3.3.3     v purrr   0.3.4
v tibble  3.1.0     v dplyr   1.0.5
v tidyr   1.1.3     v stringr 1.4.0
v readr   1.4.0     v forcats 0.5.1
-- Conflicts ----------------------------------- tidyverse_conflicts() --
  x dplyr::filter() masks stats::filter()
x dplyr::lag()    masks stats::lag()
Warning messages:
  1: package ‘tidyverse’ was built under R version 4.0.4 
2: package ‘ggplot2’ was built under R version 4.0.4 
3: package ‘tibble’ was built under R version 4.0.4 
4: package ‘tidyr’ was built under R version 4.0.4 
5: package ‘readr’ was built under R version 4.0.4 
6: package ‘dplyr’ was built under R version 4.0.4 
7: package ‘forcats’ was built under R version 4.0.4 
> mt=mtcars
> library(tidyverse)
> mt=mtcars
> summary(mt)
mpg             cyl             disp             hp       
Min.   :10.40   Min.   :4.000   Min.   : 71.1   Min.   : 52.0  
1st Qu.:15.43   1st Qu.:4.000   1st Qu.:120.8   1st Qu.: 96.5  
Median :19.20   Median :6.000   Median :196.3   Median :123.0  
Mean   :20.09   Mean   :6.188   Mean   :230.7   Mean   :146.7  
3rd Qu.:22.80   3rd Qu.:8.000   3rd Qu.:326.0   3rd Qu.:180.0  
Max.   :33.90   Max.   :8.000   Max.   :472.0   Max.   :335.0  
drat             wt             qsec             vs        
Min.   :2.760   Min.   :1.513   Min.   :14.50   Min.   :0.0000  
1st Qu.:3.080   1st Qu.:2.581   1st Qu.:16.89   1st Qu.:0.0000  
Median :3.695   Median :3.325   Median :17.71   Median :0.0000  
Mean   :3.597   Mean   :3.217   Mean   :17.85   Mean   :0.4375  
3rd Qu.:3.920   3rd Qu.:3.610   3rd Qu.:18.90   3rd Qu.:1.0000  
Max.   :4.930   Max.   :5.424   Max.   :22.90   Max.   :1.0000  
am              gear            carb      
Min.   :0.0000   Min.   :3.000   Min.   :1.000  
1st Qu.:0.0000   1st Qu.:3.000   1st Qu.:2.000  
Median :0.0000   Median :4.000   Median :2.000  
Mean   :0.4062   Mean   :3.688   Mean   :2.812  
3rd Qu.:1.0000   3rd Qu.:4.000   3rd Qu.:4.000  
Max.   :1.0000   Max.   :5.000   Max.   :8.000  
> plot(mt)
> plot(mpg~hp,data=mt)
> plot(mpg~wt,mt)
> mtmodel=lm(mpg~hp+wt,data=mt)
> summary(mtmodel)
# 
# Call:
#   lm(formula = mpg ~ hp + wt, data = mt)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -3.941 -1.600 -0.182  1.050  5.854 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 37.22727    1.59879  23.285  < 2e-16 ***
#   hp          -0.03177    0.00903  -3.519  0.00145 ** 
#   wt          -3.87783    0.63273  -6.129 1.12e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.593 on 29 degrees of freedom
# Multiple R-squared:  0.8268,	Adjusted R-squared:  0.8148


# Lets try model with few more parameter and asses the model

result=lm(mpg~hp+wt+gear+disp+vs+cyl+carb,mt)

summary(result)
# Multiple R-squared:  0.8531,	Adjusted R-squared:  0.8103 



# lets remove the model with higest p value (vs)
result1=result=lm(mpg~hp+wt+gear+disp+cyl+carb,mt)
summary(result1)

# Multiple R-squared:  0.8527,	Adjusted R-squared:  0.8173  decreased


result2=lm(mpg~hp+wt+gear+cyl+carb,mt)
summary(result2)


# best rsquared is result with 85.31% accurancy



# let make a data set for train and test 
library(caTools)
newdata=sample.split(mt,SplitRatio = 0.7)

train=subset(mt,newdata==T)
test=subset(mt,newdata==F)

# predicting values for train dataset
train$mpgpred=predict(result,train)
train


# predicting values for test dataset

test$mpgpred=predict(result,test)
test


# let find the accurancy by mape 
# mean absolute precent error

mape=mean(abs((test$mpgpred-test$mpg))/test$mpg)
mape
# mean the error is just 7% and 93% accuraccy 

# let plot
plot(result)
