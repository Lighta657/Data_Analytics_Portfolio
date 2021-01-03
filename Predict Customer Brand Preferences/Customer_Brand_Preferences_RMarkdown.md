R Notebook
================

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.1     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lattice)
library(caret)
```

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
set.seed(2241)
```

``` r
CompleteResponses <- read_csv("C:/Users/Lighta/Desktop/Data_Analytics/Course3/Task2/CompleteResponses.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   salary = col_double(),
    ##   age = col_double(),
    ##   elevel = col_double(),
    ##   car = col_double(),
    ##   zipcode = col_double(),
    ##   credit = col_double(),
    ##   brand = col_character()
    ## )

``` r
#EDA
CompleteResponses[,"brand"]<-lapply(CompleteResponses[,"brand"],factor)
summary(CompleteResponses)
```

    ##      salary            age            elevel           car       
    ##  Min.   : 20000   Min.   :20.00   Min.   :0.000   Min.   : 1.00  
    ##  1st Qu.: 52082   1st Qu.:35.00   1st Qu.:1.000   1st Qu.: 6.00  
    ##  Median : 84950   Median :50.00   Median :2.000   Median :11.00  
    ##  Mean   : 84871   Mean   :49.78   Mean   :1.983   Mean   :10.52  
    ##  3rd Qu.:117162   3rd Qu.:65.00   3rd Qu.:3.000   3rd Qu.:15.75  
    ##  Max.   :150000   Max.   :80.00   Max.   :4.000   Max.   :20.00  
    ##     zipcode          credit        brand     
    ##  Min.   :0.000   Min.   :     0   Acer:3744  
    ##  1st Qu.:2.000   1st Qu.:120807   Sony:6154  
    ##  Median :4.000   Median :250607              
    ##  Mean   :4.041   Mean   :249176              
    ##  3rd Qu.:6.000   3rd Qu.:374640              
    ##  Max.   :8.000   Max.   :500000

``` r
ggplot(CompleteResponses, aes(x = age, y = salary, colour = brand)) +
geom_point(alpha=0.5)+scale_colour_manual(name="Brand", values=c("blue", "green"), labels=c("Acer","Sony"))+labs(title="Frequency Distribution in Salary by Age by Brand group ", x="Age", y="Salary, US$")+theme(plot.title = element_text(hjust = 0.5))
```

![](Customer_Brand_Preferences_RMarkdown_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#Training/testing sets
inTraining<-createDataPartition(CompleteResponses$brand,p=.75,list=FALSE)
training<-CompleteResponses[inTraining, ]
```

    ## Warning: The `i` argument of ``[`()` can't be a matrix as of tibble 3.0.0.
    ## Convert to a vector.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
testing<-CompleteResponses[-inTraining, ]
```

``` r
#C5.0 Automatic grid
fitControl<-trainControl(method="repeatedcv", number=10, repeats=1)
CFiveFit1<-train(brand~.,data=training,method="C5.0",trControl=fitControl,tuneLength=2)
CFiveFit1
```

    ## C5.0 
    ## 
    ## 7424 samples
    ##    6 predictor
    ##    2 classes: 'Acer', 'Sony' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold, repeated 1 times) 
    ## Summary of sample sizes: 6682, 6682, 6681, 6681, 6683, 6681, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   model  winnow  trials  Accuracy   Kappa    
    ##   rules  FALSE    1      0.8402516  0.6790312
    ##   rules  FALSE   10      0.9186521  0.8270460
    ##   rules   TRUE    1      0.8426742  0.6847924
    ##   rules   TRUE   10      0.9222863  0.8343464
    ##   tree   FALSE    1      0.8370170  0.6620611
    ##   tree   FALSE   10      0.9191893  0.8280644
    ##   tree    TRUE    1      0.8395743  0.6681312
    ##   tree    TRUE   10      0.9217480  0.8341333
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final values used for the model were trials = 10, model = rules and
    ##  winnow = TRUE.

``` r
plot(varImp(CFiveFit1),top=5)
```

![](Customer_Brand_Preferences_RMarkdown_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
#C5.0 Random search
fitControlRS<-trainControl(method="repeatedcv", number=10, repeats=1,search="random")
CFiveFitRS<-train(brand~.,data=training,method="C5.0",trControl=fitControlRS)
CFiveFitRS
```

    ## C5.0 
    ## 
    ## 7424 samples
    ##    6 predictor
    ##    2 classes: 'Acer', 'Sony' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold, repeated 1 times) 
    ## Summary of sample sizes: 6682, 6682, 6681, 6683, 6681, 6681, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   winnow  trials  Accuracy   Kappa    
    ##   FALSE    4      0.9123126  0.8114926
    ##   FALSE   11      0.9175688  0.8243671
    ##    TRUE   20      0.9203977  0.8308570
    ## 
    ## Tuning parameter 'model' was held constant at a value of rules
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final values used for the model were trials = 20, model = rules and
    ##  winnow = TRUE.

``` r
plot(varImp(CFiveFitRS),top=5)
```

![](Customer_Brand_Preferences_RMarkdown_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#C5.0 manual grid
CFGrid<-expand.grid(trials=10,model="rules",winnow=FALSE)
system.time(CFFit<-train(brand~.,data=training,method="C5.0",trControl=fitControl,tuneGrid=CFGrid))
```

    ##    user  system elapsed 
    ##    5.90    0.00    5.91

``` r
CFFit
```

    ## C5.0 
    ## 
    ## 7424 samples
    ##    6 predictor
    ##    2 classes: 'Acer', 'Sony' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold, repeated 1 times) 
    ## Summary of sample sizes: 6682, 6681, 6683, 6681, 6682, 6682, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.9181043  0.8253788
    ## 
    ## Tuning parameter 'trials' was held constant at a value of 10
    ## Tuning
    ##  parameter 'model' was held constant at a value of rules
    ## Tuning
    ##  parameter 'winnow' was held constant at a value of FALSE

``` r
plot(varImp(CFFit),top=5)
```

![](Customer_Brand_Preferences_RMarkdown_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
RFGrid<-expand.grid(mtry=c(1,2,3,4,5))
fitControl<-trainControl(method="repeatedcv", number=10, repeats=1)
system.time(RFFit<-train(brand~.,data=training,method="rf",trControl=fitControl,tuneGrid=RFGrid))
```

    ##    user  system elapsed 
    ##  117.63    7.52  125.18

``` r
#getModelInfo("rf")
RFFit
```

    ## Random Forest 
    ## 
    ## 7424 samples
    ##    6 predictor
    ##    2 classes: 'Acer', 'Sony' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold, repeated 1 times) 
    ## Summary of sample sizes: 6682, 6682, 6682, 6681, 6682, 6682, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##   1     0.8553355  0.6806830
    ##   2     0.9213344  0.8332522
    ##   3     0.9195837  0.8291809
    ##   4     0.9189104  0.8277152
    ##   5     0.9139269  0.8168914
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 2.

``` r
plot(varImp(RFFit),top=5)
```

![](Customer_Brand_Preferences_RMarkdown_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
#Testing
preds<-predict(CFiveFitRS,testing)
summary(preds)
```

    ## Acer Sony 
    ##  862 1612

``` r
confusionMatrix(data=preds,reference=testing$brand)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction Acer Sony
    ##       Acer  805   57
    ##       Sony  131 1481
    ##                                           
    ##                Accuracy : 0.924           
    ##                  95% CI : (0.9129, 0.9341)
    ##     No Information Rate : 0.6217          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.8359          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.015e-07       
    ##                                           
    ##             Sensitivity : 0.8600          
    ##             Specificity : 0.9629          
    ##          Pos Pred Value : 0.9339          
    ##          Neg Pred Value : 0.9187          
    ##              Prevalence : 0.3783          
    ##          Detection Rate : 0.3254          
    ##    Detection Prevalence : 0.3484          
    ##       Balanced Accuracy : 0.9115          
    ##                                           
    ##        'Positive' Class : Acer            
    ## 

``` r
#Unseen data - incomplete data
SurveyIncomplete <- read_csv("C:/Users/Lighta/Desktop/Data_Analytics/Course3/Task2/SurveyIncomplete.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   salary = col_double(),
    ##   age = col_double(),
    ##   elevel = col_double(),
    ##   car = col_double(),
    ##   zipcode = col_double(),
    ##   credit = col_double(),
    ##   brand = col_double()
    ## )

``` r
SurveyIncomplete[,"brand"]<-lapply(SurveyIncomplete[,"brand"],factor)
summary(SurveyIncomplete)
```

    ##      salary            age            elevel           car      
    ##  Min.   : 20000   Min.   :20.00   Min.   :0.000   Min.   : 1.0  
    ##  1st Qu.: 52590   1st Qu.:35.00   1st Qu.:1.000   1st Qu.: 6.0  
    ##  Median : 86221   Median :50.00   Median :2.000   Median :11.0  
    ##  Mean   : 85794   Mean   :49.94   Mean   :2.009   Mean   :10.6  
    ##  3rd Qu.:118535   3rd Qu.:65.00   3rd Qu.:3.000   3rd Qu.:16.0  
    ##  Max.   :150000   Max.   :80.00   Max.   :4.000   Max.   :20.0  
    ##     zipcode          credit       brand   
    ##  Min.   :0.000   Min.   :     0   0:4937  
    ##  1st Qu.:2.000   1st Qu.:122311   1:  63  
    ##  Median :4.000   Median :250974           
    ##  Mean   :4.038   Mean   :249546           
    ##  3rd Qu.:6.000   3rd Qu.:375653           
    ##  Max.   :8.000   Max.   :500000

``` r
#Predict using unseen data
preds1<-predict(CFiveFitRS,SurveyIncomplete, type="raw")
```

``` r
#Savings preds as .csv
write.csv(preds1,file="C:\\Users\\Lighta\\Desktop\\Data_Analytics\\Course3\\Task2\\C3T2\\predictions.csv")
```

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you
execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk
or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*.

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or
by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output
will be saved alongside it (click the *Preview* button or press
*Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the
editor. Consequently, unlike *Knit*, *Preview* does not run any R code
chunks. Instead, the output of the chunk when it was last run in the
editor is displayed.
