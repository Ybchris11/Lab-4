Lab \#4
================

``` r
library(ggplot2)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
require(class)
```

    ## Loading required package: class

``` r
require(caret)
```

    ## Loading required package: caret
    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
load("BRFSS2022_rev.RData")
brfss22$Age_midpt <- fct_recode(brfss22$X_AGEG5YR, "21" = "Age 18 to 24",
                                "27" = "Age 25 to 29", "32" = "Age 30 to 34",
                                "37" = "Age 35 to 39", "42" = "Age 40 to 44",
                                "47" = "Age 45 to 49", "52" = "Age 50 to 54",
                                "57" = "Age 55 to 59", "62" = "Age 60 to 64",
                                "67" = "Age 65 to 69", "72" = "Age 70 to 74",
                                "77" = "Age 75 to 79", "82" = "Age 80 or older",
                                NULL = "Dont know/Refused/Missing")
brfss22$Age_midpt <- as.numeric(levels(brfss22$Age_midpt))[brfss22$Age_midpt]
brfss22$Educ_number <- fct_recode(brfss22$EDUCA, 
                                  "0" = "Never attended school or only kindergarten", 
                                  "4.5" = "Grades 1 through 8 (Elementary)",
                                  "10" = "Grades 9 through 11 (Some high school)",
                                  "12" = "Grade 12 or GED (High school graduate)",
                    "14" = "College 1 year to 3 years (Some college or technical school)",
                    "16" = "College 4 years or more (College graduate)",
                    NULL = "Refused" )
brfss22$Educ_number <- as.numeric(levels(brfss22$Educ_number))[brfss22$Educ_number]

ACEdidntask <- (as.numeric(is.na(brfss22$ACEDEPRS)) + 
                        as.numeric(is.na(brfss22$ACEDRINK)) +
                        as.numeric(is.na(brfss22$ACEDRUGS)) +
                        as.numeric(is.na(brfss22$ACEPRISN)) +
                        as.numeric(is.na(brfss22$ACEDIVRC)) +
                        as.numeric(is.na(brfss22$ACEPUNCH)) +
                        as.numeric(is.na(brfss22$ACEHURT1)) +
                        as.numeric(is.na(brfss22$ACESWEAR)) +
                        as.numeric(is.na(brfss22$ACETOUCH)) )
select_ACE <- (ACEdidntask == 0) & !is.na(brfss22$MENTHLTH) # with zero missing values for any of ACE questions and not missing MENTLHLTH
brfss_ACE <- subset(brfss22, select_ACE)
```

``` r
summary(brfss_ACE$MENTHLTH[brfss_ACE$ACETOUCH == "Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually"])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   0.000   3.642   3.000  30.000

``` r
summary(brfss_ACE$MENTHLTH[brfss_ACE$ACETOUCH == "once"])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   2.000   6.705  10.000  30.000

``` r
summary(brfss_ACE$MENTHLTH[brfss_ACE$ACETOUCH == "more than once"])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   3.000   9.094  15.000  30.000

``` r
sd(brfss_ACE$MENTHLTH[brfss_ACE$ACETOUCH == "Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually"], na.rm = TRUE)
```

    ## [1] 7.712734

``` r
sd(brfss_ACE$MENTHLTH[brfss_ACE$ACETOUCH == "once"], na.rm = TRUE)
```

    ## [1] 9.799491

``` r
sd(brfss_ACE$MENTHLTH[brfss_ACE$ACETOUCH == "more than once"], na.rm = TRUE)
```

    ## [1] 11.11077

``` r
summary(brfss_ACE$ACETOUCH) # N in each group
```

    ## Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually 
    ##                                                                                                                      40223 
    ##                                                                                                                       once 
    ##                                                                                                                       1901 
    ##                                                                                                             more than once 
    ##                                                                                                                       3614 
    ##                                                                                                         dont know not sure 
    ##                                                                                                                        193 
    ##                                                                                                                    refused 
    ##                                                                                                                       1151

``` r
# is there an easier way?!?
library(plyr)
```

    ## ------------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## ------------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

``` r
summary1 <- ddply(brfss_ACE,.(ACETOUCH), summarize, mean_mentalhealth = mean(MENTHLTH), 
                  sd_mentalhealth = sd(MENTHLTH), n_obs = sum(!is.na(MENTHLTH)) )
summary1
```

    ##                                                                                                                     ACETOUCH
    ## 1 Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually
    ## 2                                                                                                                       once
    ## 3                                                                                                             more than once
    ## 4                                                                                                         dont know not sure
    ## 5                                                                                                                    refused
    ##   mean_mentalhealth sd_mentalhealth n_obs
    ## 1          3.642369        7.712734 40223
    ## 2          6.705418        9.799491  1901
    ## 3          9.094079       11.110770  3614
    ## 4          6.896373       10.287008   193
    ## 5          5.990443       10.162544  1151

``` r
summary2 <- ddply(brfss_ACE,.(MENTHLTH >0), summarize, 
                  zero_ACETOUCH = sum(ACETOUCH == "Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually"), 
                  once_ACETOUCH = sum(ACETOUCH == "once"), 
                  mult_ACETOUCH = sum(ACETOUCH == "more than once") )
summary2
```

    ##   MENTHLTH > 0 zero_ACETOUCH once_ACETOUCH mult_ACETOUCH
    ## 1        FALSE         26334           859          1370
    ## 2         TRUE         13889          1042          2244

``` r
brfss_ACE$ACETOUCH_recode <- fct_recode(brfss_ACE$ACETOUCH, 
                                        "0" = "Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually",
                                "0.5" = "once", 
                                "1" = "more than once",
                                NULL = "dont know not sure",
                                NULL = "refused"
)
brfss_ACE$ACEHURT_recode <- fct_recode(brfss_ACE$ACEHURT1, 
                                        "0" = "Adverse Childhood Exper, never: Not including spanking, (before age 18), how often did a parent or adult in your home ever hit, beat, kick, or physically hurt you in any way",
                                "0.5" = "once", 
                                "1" = "more than once",
                                NULL = "dont know not sure",
                                NULL = "refused"
)
brfss_ACE$ACETOUCH_recode <- as.numeric(levels(brfss_ACE$ACETOUCH_recode))[brfss_ACE$ACETOUCH_recode]
brfss_ACE$ACEHURT_recode <- as.numeric(levels(brfss_ACE$ACEHURT_recode))[brfss_ACE$ACEHURT_recode]

brfss_ACE$MENTHLTH_recode <- cut(brfss_ACE$MENTHLTH, breaks = c(-1,0,1,5,10,15,31))
summary(brfss_ACE$MENTHLTH_recode)
```

    ##  (-1,0]   (0,1]   (1,5]  (5,10] (10,15] (15,31] 
    ##   29340    1537    6932    2735    2010    4528

``` r
# create a function to standardize
standardize_varb_to01 <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE)  )
}
```

``` r
X1 <- standardize_varb_to01(brfss_ACE$Age_midpt)
X2 <- standardize_varb_to01(brfss_ACE$Educ_number)
X3 <- brfss_ACE$ACETOUCH_recode
X4 <- brfss_ACE$ACEHURT_recode
# you could add more X variables...
Y <- brfss_ACE$MENTHLTH_recode

nonmissingobs <- complete.cases(Y,X1,X2,X3,X4)

X1 <- subset(X1, nonmissingobs)
X2 <- subset(X2, nonmissingobs)
X3 <- subset(X3, nonmissingobs)
X4 <- subset(X4, nonmissingobs)
dat_use <- data.frame(X1,X2,X3,X4)
Y <- subset(Y, nonmissingobs)
```

``` r
set.seed(1234)
NN_obs <- length(Y)
select1 <- (runif(NN_obs) < 0.6)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- Y[select1]
true_data <- Y[!select1]
```

``` r
for (indx in seq(1, 9, by= 2)) {
 pred_y <- knn3Train(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
 num_correct_labels <- sum(pred_y == true_data)
 correct_rate <- num_correct_labels/length(true_data)
 print(c(indx,correct_rate))
}
```

    ## [1] 1.0000000 0.6165127
    ## [1] 3.0000000 0.6179121
    ## [1] 5.0000000 0.6183039
    ## [1] 7.000000 0.618248
    ## [1] 9.0000000 0.6193675

``` r
summary(brfss22)
```

    ##        X_STATE                          X_METSTAT               X_URBSTAT     
    ##  Washington: 26152   Metropolitan counties   :318082   urban counties:380732  
    ##  New York  : 17800   nonmetropolitan counties:117642   rural counties: 54992  
    ##  Minnesota : 16821   NA's                    :  9408   NA's          :  9408  
    ##  Ohio      : 16487                                                            
    ##  Maryland  : 16418                                                            
    ##  Texas     : 14245                                                            
    ##  (Other)   :337209                                                            
    ##                                MSCODE          CHILDREN    
    ##  in central city                  : 29393   Min.   : 0.00  
    ##  in county containing central city: 16030   1st Qu.: 0.00  
    ##  in suburb near city              : 16068   Median : 0.00  
    ##  outside MSA                      : 32395   Mean   : 0.49  
    ##  NA's                             :351246   3rd Qu.: 1.00  
    ##                                             Max.   :82.00  
    ##                                             NA's   :14464  
    ##                             MARITAL      
    ##  Married                        :227424  
    ##  Never married                  : 80001  
    ##  Divorced                       : 57516  
    ##  Widowed                        : 48019  
    ##  A member of an unmarried couple: 18668  
    ##  (Other)                        : 13496  
    ##  NA's                           :     8  
    ##                                                           EDUCA       
    ##  College 4 years or more (College graduate)                  :187496  
    ##  College 1 year to 3 years (Some college or technical school):120252  
    ##  Grade 12 or GED (High school graduate)                      :108990  
    ##  Grades 9 through 11 (Some high school)                      : 16954  
    ##  Grades 1 through 8 (Elementary)                             :  8381  
    ##  (Other)                                                     :  3054  
    ##  NA's                                                        :     5  
    ##           VETERAN3                                   X_PRACE2     
    ##  Yes a veteran: 53211   White                            :351032  
    ##  No           :386272   Black or African American        : 41522  
    ##  NA's         :  5649   Asian                            : 14836  
    ##                         American Indian or Alaskan Native: 10147  
    ##                         Refused                          :  8593  
    ##                         (Other)                          : 18989  
    ##                         NA's                             :    13  
    ##                      X_HISPANC                X_AGEG5YR          RENTHOM1     
    ##  yes Hispanic             : 42917   Age 65 to 69   : 47099   own home:310708  
    ##  no                       :396631   Age 60 to 64   : 44511   rent    :108332  
    ##  dont know refused missing:  5584   Age 70 to 74   : 43472   other   : 21463  
    ##                                     Age 55 to 59   : 36821   NA's    :  4629  
    ##                                     Age 80 or older: 36251                    
    ##                                     Age 50 to 54   : 33644                    
    ##                                     (Other)        :203334                    
    ##                EMPLOY1      
    ##  Employed for wages:186004  
    ##  Retired           :137083  
    ##  Self-employed     : 38768  
    ##  Unable to work    : 26737  
    ##  A homemaker       : 17477  
    ##  (Other)           : 27823  
    ##  NA's              : 11240  
    ##                                                 INCOME3      
    ##  Less than $75,000 ($50,000 to less than $75,000)   : 59148  
    ##  Less than $150,000 ($100,000 to less than $150,000): 50330  
    ##  Less than $100,000 ($75,000 to less than $100,000) : 48436  
    ##  Refused                                            : 47001  
    ##  Less than $50,000 ($35,000 to less than $50,000)   : 46831  
    ##  (Other)                                            :180454  
    ##  NA's                                               : 12932  
    ##                  FOODSTMP     
    ##  got food stamps SNAP: 25323  
    ##  no                  :226638  
    ##  NA's                :193171  
    ##                               
    ##                               
    ##                               
    ##                               
    ##                                                                                               SDHFOOD1     
    ##  Never                                                                                            :203654  
    ##  Rarely                                                                                           : 20768  
    ##  Sometimes                                                                                        : 17258  
    ##  Always the food that you bought not last, and you didn\x92t have money to get more, in last 12 mo:  4824  
    ##  Usually                                                                                          :  4603  
    ##  (Other)                                                                                          :  1722  
    ##  NA's                                                                                             :192303  
    ##     SEXVAR                      BIRTHSEX     
    ##  Male  :209239   male sex at birth  : 37441  
    ##  Female:235893   female sex at birth: 41456  
    ##                  NA's               :366235  
    ##                                              
    ##                                              
    ##                                              
    ##                                              
    ##                         SOMALE                             SOFEMALE     
    ##  Gay                       :  2939   Lesbian or Gay            :  2318  
    ##  Straight, that is, not gay:112679   Straight, that is, not gay:123813  
    ##  Bisexual                  :  2570   Bisexual                  :  5507  
    ##  Something else            :  1974   Something else            :  2789  
    ##  I dont know the answer    :  1057   I dont know the answer    :  1760  
    ##  Refused                   :  2813   Refused                   :  3443  
    ##  NA's                      :321100   NA's                      :305502  
    ##                                    TRNSGNDR     
    ##  Yes, Transgender, male-to-female      :   499  
    ##  Yes, Transgender, female to male      :   515  
    ##  Yes, Transgender, gender nonconforming:   589  
    ##  No                                    :258106  
    ##  Dont know/Not Sure                    :   811  
    ##  Refused                               :  3041  
    ##  NA's                                  :181571  
    ##                       HADSEX            GENHLTH          PHYSHLTH     
    ##  yes had sex in last 6 mo: 14744   Very good:148444   Min.   : 0.000  
    ##  no                      :  4231   Good     :143598   1st Qu.: 0.000  
    ##  dont know not sure      :    66   Excellent: 71878   Median : 0.000  
    ##  refused                 :   835   Fair     : 60273   Mean   : 4.348  
    ##  NA's                    :425256   Poor     : 19741   3rd Qu.: 3.000  
    ##                                    (Other)  :  1195   Max.   :30.000  
    ##                                    NA's     :     3   NA's   :10927   
    ##     MENTHLTH                          LSATISFY     
    ##  Min.   : 0.000   Very satisfied with life:114252  
    ##  1st Qu.: 0.000   Satisfied               :123445  
    ##  Median : 0.000   Dissatisfied            : 10758  
    ##  Mean   : 4.383   Very dissatisfied       :  3062  
    ##  3rd Qu.: 5.000   Dont know/Not sure      :  1864  
    ##  Max.   :30.000   Refused                 :  1107  
    ##  NA's   :9067     NA's                    :190644  
    ##                                     EMTSUPRT     
    ##  Always get social and emotional support:118012  
    ##  Usually                                : 77907  
    ##  Sometimes                              : 33813  
    ##  Rarely                                 : 10835  
    ##  Never                                  :  9379  
    ##  (Other)                                :  4195  
    ##  NA's                                   :190991  
    ##                                       SDHISOLT     
    ##  Never                                    :106160  
    ##  Rarely                                   : 70617  
    ##  Sometimes                                : 53072  
    ##  Usually                                  : 13178  
    ##  Always feel socially isolated from others:  8098  
    ##  (Other)                                  :  2665  
    ##  NA's                                     :191342  
    ##                                                                                                                             SDHSTRE1     
    ##  Never                                                                                                                          : 94681  
    ##  Rarely                                                                                                                         : 69465  
    ##  Sometimes                                                                                                                      : 55913  
    ##  Usually                                                                                                                        : 17179  
    ##  Always feels tense, restless, nervous, or anxious, or is unable to sleep at night because his/her mind is troubled all the time: 12295  
    ##  (Other)                                                                                                                        :  1678  
    ##  NA's                                                                                                                           :193921  
    ##                                   ADDEPEV3     
    ##  Yes ever told had depressive disorder: 91410  
    ##  No                                   :350910  
    ##  NA's                                 :  2812  
    ##                                                
    ##                                                
    ##                                                
    ##                                                
    ##                              PRIMINSR     
    ##  health ins thr employer or union:161388  
    ##  Medicare                        :135848  
    ##  private plan                    : 36931  
    ##  Medicaid                        : 29072  
    ##  no coverage of any type         : 23018  
    ##  (Other)                         : 58871  
    ##  NA's                            :     4  
    ##                                                                     CHECKUP1     
    ##  last routine checkup within past year (anytime less than 12 months ago):350944  
    ##  Within past 2 years (1 year but less than 2 years ago)                 : 41919  
    ##  Within past 5 years (2 years but less than 5 years ago)                : 24882  
    ##  5 or more years ago                                                    : 19079  
    ##  Dont know Not sure                                                     :  5063  
    ##  (Other)                                                                :  3242  
    ##  NA's                                                                   :     3  
    ##                            FLUSHOT7     
    ##  Yes got flu shot in last 12 mo:209256  
    ##  No                            :188755  
    ##  NA's                          : 47121  
    ##                                         
    ##                                         
    ##                                         
    ##                                         
    ##                                 COVIDPOS     
    ##  Yes had med prof tell positive test:110877  
    ##  No                                 :270055  
    ##  tested positive at home wo med prof: 13436  
    ##  NA's                               : 50764  
    ##                                              
    ##                                              
    ##                                              
    ##                                           COVIDSMP     
    ##  Yes had covid symptoms for more than 3 months: 26783  
    ##  No                                           : 94596  
    ##  NA's                                         :323753  
    ##                                                        
    ##                                                        
    ##                                                        
    ##                                                        
    ##                                                                                                        COVIDPRM     
    ##  Tiredness or fatigue                                                                                      :  7072  
    ##  Difficulty breathing or shortness of breath                                                               :  4772  
    ##  Loss of taste or smell                                                                                    :  4234  
    ##  Difficulty thinking or concentrating or forgetfulness/memory problems (sometimes referred to as brain fog):  2564  
    ##  Some other symptom                                                                                        :  2511  
    ##  (Other)                                                                                                   :  5562  
    ##  NA's                                                                                                      :418417  
    ##                          COVIDVA1                 COVIDNU1     
    ##  Yes had at least 1 covid vax:124818   had 1 covid vax:  6712  
    ##  No                          : 27011   2 covid vax    : 36926  
    ##  NA's                        :293303   3 covid vax    : 57488  
    ##                                        4 covid vax    : 23051  
    ##                                        NA's           :320955  
    ##                                                                
    ##                                                                
    ##                         EXERANY2         SLEPTIM1      Height_inches  
    ##  Yes exercised in past month:337559   Min.   : 1.000   Min.   :24.00  
    ##  No                         :106480   1st Qu.: 6.000   1st Qu.:64.00  
    ##  NA's                       :  1093   Median : 7.000   Median :67.00  
    ##                                       Mean   : 7.023   Mean   :67.06  
    ##                                       3rd Qu.: 8.000   3rd Qu.:70.00  
    ##                                       Max.   :24.000   Max.   :97.00  
    ##                                       NA's   :5453     NA's   :32468  
    ##     WEIGHT2           X_BMI5              X_BMI5CAT     
    ##  Min.   :  32.0   Min.   :12.02   Underweight  :  6778  
    ##  1st Qu.: 150.0   1st Qu.:24.13   Normal Weight:116976  
    ##  Median : 178.0   Median :27.44   Overweight   :139995  
    ##  Mean   : 183.3   Mean   :28.53   Obese        :132577  
    ##  3rd Qu.: 210.0   3rd Qu.:31.75   NA's         : 48806  
    ##  Max.   :1230.0   Max.   :99.64                         
    ##  NA's   :44257    NA's   :48806                         
    ##                                  SMOKE100                    SMOKDAY2     
    ##  yes smoked at least 100 cigs in life:164217   smoke every day   : 36003  
    ##  no                                  :245955   smoke some days   : 13938  
    ##  dont know not sure                  :  2297   not at all        :113774  
    ##  refused                             :   886   Dont know Not Sure:   165  
    ##  NA's                                : 31777   Refused           :   173  
    ##                                                NA's              :281079  
    ##                                                                           
    ##                             ECIGNOW2     
    ##  never used e-cigarettes in life:311988  
    ##  use every day                  : 10382  
    ##  use some days                  : 11734  
    ##  not at all right now           : 75368  
    ##  Dont know Not Sure             :   905  
    ##  Refused                        :  1176  
    ##  NA's                           : 33579  
    ##                            ALCDAY4          AVEDRNK3         DRNK3GE5     
    ##  none                          :187667   Min.   : 0.00    Min.   : 0.00   
    ##  1 in last month               : 31355   1st Qu.: 1.00    1st Qu.: 0.00   
    ##  2 in last month               : 24184   Median : 2.00    Median : 0.00   
    ##  1 alcoholic drink in past week: 19043   Mean   : 2.24    Mean   : 1.34   
    ##  2 in week                     : 15797   3rd Qu.: 3.00    3rd Qu.: 1.00   
    ##  (Other)                       :126323   Max.   :76.00    Max.   :76.00   
    ##  NA's                          : 40763   NA's   :237372   NA's   :238769  
    ##     MARIJAN1                       FIREARM5     
    ##  Min.   : 0       yes firearms in house: 13839  
    ##  1st Qu.: 0       no                   : 23428  
    ##  Median : 0       NA's                 :407865  
    ##  Mean   : 2                                     
    ##  3rd Qu.: 0                                     
    ##  Max.   :30                                     
    ##  NA's   :351397                                 
    ##                                                                                           ACEDEPRS     
    ##  Yes, Adverse Childhood Exper, lived with someone who was depressed, mentally ill, or suicidal:  8800  
    ##  No                                                                                           : 38156  
    ##  dont know not sure                                                                           :   489  
    ##  refused                                                                                      :   841  
    ##  NA's                                                                                         :396846  
    ##                                                                                                        
    ##                                                                                                        
    ##                                                                                     ACEDRINK     
    ##  Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic: 11527  
    ##  No                                                                                     : 35692  
    ##  dont know not sure                                                                     :   206  
    ##  refused                                                                                :   806  
    ##  NA's                                                                                   :396901  
    ##                                                                                                  
    ##                                                                                                  
    ##                                                                                                                   ACEDRUGS     
    ##  Yes, Adverse Childhood Exper, lived with someone who used illegal street drugs or who abused prescription medications:  4691  
    ##  No                                                                                                                   : 42448  
    ##  dont know not sure                                                                                                   :   307  
    ##  refused                                                                                                              :   774  
    ##  NA's                                                                                                                 :396912  
    ##                                                                                                                                
    ##                                                                                                                                
    ##                                                                                                                                               ACEPRISN     
    ##  Yes, Adverse Childhood Exper, lived with someone who served time or was sentenced to serve time in a prison, jail, or other correctional facility:  3549  
    ##  No                                                                                                                                               : 43701  
    ##  dont know not sure                                                                                                                               :   178  
    ##  refused                                                                                                                                          :   766  
    ##  NA's                                                                                                                                             :396938  
    ##                                                                                                                                                            
    ##                                                                                                                                                            
    ##                                                         ACEDIVRC     
    ##  Yes, Adverse Childhood Exper, parents separated or divorced: 12107  
    ##  No                                                         : 34367  
    ##  dont know not sure                                         :   269  
    ##  parents never married                                      :   632  
    ##  refused                                                    :   797  
    ##  NA's                                                       :396960  
    ##                                                                      
    ##                                                                                                                                 ACEPUNCH     
    ##  Adverse Childhood Exper, never: How often did your parents or adults in your home ever slap, hit, kick, punch or beat each other up: 38842  
    ##  once                                                                                                                               :  1795  
    ##  more than once                                                                                                                     :  5816  
    ##  dont know not sure                                                                                                                 :   672  
    ##  refused                                                                                                                            :  1004  
    ##  NA's                                                                                                                               :397003  
    ##                                                                                                                                              
    ##                                                                                                                                                                           ACEHURT1     
    ##  Adverse Childhood Exper, never: Not including spanking, (before age 18), how often did a parent or adult in your home ever hit, beat, kick, or physically hurt you in any way: 35092  
    ##  once                                                                                                                                                                         :  2939  
    ##  more than once                                                                                                                                                               :  8628  
    ##  dont know not sure                                                                                                                                                           :   350  
    ##  refused                                                                                                                                                                      :  1068  
    ##  NA's                                                                                                                                                                         :397055  
    ##                                                                                                                                                                                        
    ##                                                                                                                         ACESWEAR     
    ##  Adverse Childhood Exper, never: How often did a parent or adult in your home ever swear at you, insult you, or put you down: 30266  
    ##  once                                                                                                                       :  2393  
    ##  more than once                                                                                                             : 13722  
    ##  dont know not sure                                                                                                         :   573  
    ##  refused                                                                                                                    :  1075  
    ##  NA's                                                                                                                       :397103  
    ##                                                                                                                                      
    ##                                                                                                                        ACETOUCH     
    ##  Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually: 40941  
    ##  once                                                                                                                      :  1940  
    ##  more than once                                                                                                            :  3677  
    ##  dont know not sure                                                                                                        :   202  
    ##  refused                                                                                                                   :  1212  
    ##  NA's                                                                                                                      :397160  
    ##                                                                                                                                     
    ##                                              CIMEMLOS     
    ##  experienced confusion memory loss in last 12 mo :  7003  
    ##  no                                              : 56945  
    ##  NA's                                            :381184  
    ##                                                           
    ##                                                           
    ##                                                           
    ##                                                           
    ##                                                                                                                                                  CDHOUSE      
    ##  Never                                                                                                                                               :  3642  
    ##  Sometimes                                                                                                                                           :  1650  
    ##  Rarely                                                                                                                                              :  1272  
    ##  Usually                                                                                                                                             :   364  
    ##  Always, as a result of confusion or memory loss, how often have you given up day-to-day household activities or chores you used to do, in last 12 mo:   357  
    ##  (Other)                                                                                                                                             :   168  
    ##  NA's                                                                                                                                                :437679  
    ##                                                 CDASSIST     
    ##  Never                                              :  3954  
    ##  Rarely                                             :  1447  
    ##  Sometimes                                          :  1354  
    ##  Always need assist because confusion or memory loss:   304  
    ##  Usually                                            :   281  
    ##  (Other)                                            :    91  
    ##  NA's                                               :437701  
    ##                                                                      CDSOCIAL     
    ##  Never                                                                   :  3726  
    ##  Rarely                                                                  :  1387  
    ##  Sometimes                                                               :  1313  
    ##  Always confusion or memory loss interfere with work or social activities:   458  
    ##  Usually                                                                 :   410  
    ##  (Other)                                                                 :   121  
    ##  NA's                                                                    :437717  
    ##                                                                   CAREGIV1     
    ##  provided care to family or friend with disability or health condition: 19662  
    ##  no                                                                   : 78174  
    ##  caregiving recipient died in past 30 days                            :   177  
    ##  NA's                                                                 :347119  
    ##                                                                                
    ##                                                                                
    ##                                                                                
    ##                        CRGVREL4        Age_midpt      Educ_number  
    ##  Mother                    :  3986   Min.   :21.00   Min.   : 0    
    ##  Non-relative/Family friend:  3002   1st Qu.:42.00   1st Qu.:12    
    ##  Husband                   :  2243   Median :57.00   Median :14    
    ##  Child                     :  1968   Mean   :55.06   Mean   :14    
    ##  Wife                      :  1904   3rd Qu.:72.00   3rd Qu.:16    
    ##  (Other)                   :  6531   Max.   :82.00   Max.   :16    
    ##  NA's                      :425498   NA's   :9079    NA's   :2383

``` r
brfss22$income_midpoint <- fct_recode(brfss22$INCOME3, "7500" = "Household income less than $10,000",
                                      "12500" = "Less than $15,000 ($10,000 to less than $15,000)",
                                      "17500" = "Less than $20,000 ($15,000 to less than $20,000) ", # fukkin extra space!
                                      "22500" = "Less than $25,000 ($20,000 to less than $25,000) ",
                                      "30000" = "Less than $35,000 ($25,000 to less than $35,000) ",
                                      "42500" = "Less than $50,000 ($35,000 to less than $50,000) ",
                                      "62500" = "Less than $75,000 ($50,000 to less than $75,000)",
                                      "87500" = "Less than $100,000 ($75,000 to less than $100,000)",
                                      "125000" = "Less than $150,000 ($100,000 to less than $150,000)",
                                      "175000" = "Less than $200,000 ($150,000 to less than $200,000)",
                                      "210000" = "$200,000 or more",
                                      NULL = "Dont know/Not sure",
                                      NULL = "Refused")
```

``` r
ACEdidntask <- (as.numeric(is.na(brfss22$ACEDEPRS)) + 
                        as.numeric(is.na(brfss22$ACEDRINK)) +
                        as.numeric(is.na(brfss22$ACEDRUGS)) +
                        as.numeric(is.na(brfss22$ACEPRISN)) +
                        as.numeric(is.na(brfss22$ACEDIVRC)) +
                        as.numeric(is.na(brfss22$ACEPUNCH)) +
                        as.numeric(is.na(brfss22$ACEHURT1)) +
                        as.numeric(is.na(brfss22$ACESWEAR)) +
                        as.numeric(is.na(brfss22$ACETOUCH)) )
select_ACE <- (ACEdidntask == 0) & !is.na(brfss22$MENTHLTH) # with zero missing values for any of ACE questions and not missing MENTLHLTH
brfss_ACE <- subset(brfss22, select_ACE)
```

``` r
summary(brfss_ACE)
```

    ##          X_STATE                        X_METSTAT              X_URBSTAT    
    ##  Florida     :9418   Metropolitan counties   :29613   urban counties:37562  
    ##  Virginia    :7844   nonmetropolitan counties:17469   rural counties: 9520  
    ##  Iowa        :7726                                                          
    ##  South Dakota:7002                                                          
    ##  Oregon      :4936                                                          
    ##  Arkansas    :4100                                                          
    ##  (Other)     :6056                                                          
    ##                                MSCODE         CHILDREN      
    ##  in central city                  : 3114   Min.   : 0.0000  
    ##  in county containing central city: 1413   1st Qu.: 0.0000  
    ##  in suburb near city              : 2485   Median : 0.0000  
    ##  outside MSA                      : 4883   Mean   : 0.4771  
    ##  NA's                             :35187   3rd Qu.: 0.0000  
    ##                                            Max.   :82.0000  
    ##                                            NA's   :362      
    ##                             MARITAL     
    ##  Married                        :24509  
    ##  Divorced                       : 6224  
    ##  Widowed                        : 5904  
    ##  Separated                      :  828  
    ##  Never married                  : 7484  
    ##  A member of an unmarried couple: 1773  
    ##  Refused                        :  360  
    ##                                                           EDUCA      
    ##  Never attended school or only kindergarten                  :   59  
    ##  Grades 1 through 8 (Elementary)                             :  802  
    ##  Grades 9 through 11 (Some high school)                      : 1758  
    ##  Grade 12 or GED (High school graduate)                      :12105  
    ##  College 1 year to 3 years (Some college or technical school):13673  
    ##  College 4 years or more (College graduate)                  :18507  
    ##  Refused                                                     :  178  
    ##           VETERAN3                                  X_PRACE2    
    ##  Yes a veteran: 6529   White                            :39343  
    ##  No           :40433   Black or African American        : 3096  
    ##  NA's         :  120   American Indian or Alaskan Native: 1682  
    ##                        Refused                          : 1030  
    ##                        Asian                            :  671  
    ##                        No race choice given             :  554  
    ##                        (Other)                          :  706  
    ##                      X_HISPANC               X_AGEG5YR         RENTHOM1    
    ##  yes Hispanic             : 2833   Age 65 to 69   : 5288   own home:34931  
    ##  no                       :43804   Age 70 to 74   : 5033   rent    : 9750  
    ##  dont know refused missing:  445   Age 60 to 64   : 4794   other   : 2017  
    ##                                    Age 80 or older: 4478   NA's    :  384  
    ##                                    Age 75 to 79   : 3933                   
    ##                                    Age 55 to 59   : 3894                   
    ##                                    (Other)        :19662                   
    ##                EMPLOY1     
    ##  Employed for wages:19004  
    ##  Retired           :16396  
    ##  Self-employed     : 4275  
    ##  Unable to work    : 2856  
    ##  A homemaker       : 1662  
    ##  (Other)           : 2462  
    ##  NA's              :  427  
    ##                                                 INCOME3     
    ##  Less than $75,000 ($50,000 to less than $75,000)   : 6995  
    ##  Less than $50,000 ($35,000 to less than $50,000)   : 5688  
    ##  Less than $100,000 ($75,000 to less than $100,000) : 5546  
    ##  Less than $150,000 ($100,000 to less than $150,000): 5181  
    ##  Refused                                            : 4833  
    ##  (Other)                                            :18838  
    ##  NA's                                               :    1  
    ##                  FOODSTMP    
    ##  got food stamps SNAP: 1948  
    ##  no                  :17187  
    ##  NA's                :27947  
    ##                              
    ##                              
    ##                              
    ##                              
    ##                                                                                               SDHFOOD1    
    ##  Never                                                                                            :15371  
    ##  Rarely                                                                                           : 1664  
    ##  Sometimes                                                                                        : 1335  
    ##  Always the food that you bought not last, and you didn\x92t have money to get more, in last 12 mo:  383  
    ##  Usually                                                                                          :  348  
    ##  (Other)                                                                                          :   73  
    ##  NA's                                                                                             :27908  
    ##     SEXVAR                     BIRTHSEX                            SOMALE     
    ##  Male  :21875   male sex at birth  : 4883   Gay                       :  203  
    ##  Female:25207   female sex at birth: 5214   Straight, that is, not gay: 9159  
    ##                 NA's               :36985   Bisexual                  :  195  
    ##                                             Something else            :  126  
    ##                                             I dont know the answer    :   63  
    ##                                             Refused                   :  201  
    ##                                             NA's                      :37135  
    ##                        SOFEMALE    
    ##  Lesbian or Gay            :  145  
    ##  Straight, that is, not gay:10366  
    ##  Bisexual                  :  424  
    ##  Something else            :  188  
    ##  I dont know the answer    :  104  
    ##  Refused                   :  254  
    ##  NA's                      :35601  
    ##                                    TRNSGNDR    
    ##  Yes, Transgender, male-to-female      :   33  
    ##  Yes, Transgender, female to male      :   32  
    ##  Yes, Transgender, gender nonconforming:   33  
    ##  No                                    :20982  
    ##  Dont know/Not Sure                    :   48  
    ##  Refused                               :  287  
    ##  NA's                                  :25667  
    ##                       HADSEX                      GENHLTH         PHYSHLTH     
    ##  yes had sex in last 6 mo: 3117   Excellent           : 6946   Min.   : 0.000  
    ##  no                      :  813   Very good           :15556   1st Qu.: 0.000  
    ##  dont know not sure      :   16   Good                :15611   Median : 0.000  
    ##  refused                 :  172   Fair                : 6669   Mean   : 4.464  
    ##  NA's                    :42964   Poor                : 2191   3rd Qu.: 4.000  
    ##                                   Dont know - Not Sure:   82   Max.   :30.000  
    ##                                   Refused             :   27   NA's   :836     
    ##     MENTHLTH                          LSATISFY    
    ##  Min.   : 0.000   Very satisfied with life: 9092  
    ##  1st Qu.: 0.000   Satisfied               : 8950  
    ##  Median : 0.000   Dissatisfied            :  785  
    ##  Mean   : 4.255   Very dissatisfied       :  269  
    ##  3rd Qu.: 4.000   Dont know/Not sure      :   93  
    ##  Max.   :30.000   Refused                 :   71  
    ##                   NA's                    :27822  
    ##                                     EMTSUPRT    
    ##  Always get social and emotional support: 9182  
    ##  Usually                                : 5948  
    ##  Sometimes                              : 2566  
    ##  Rarely                                 :  822  
    ##  Never                                  :  510  
    ##  (Other)                                :  211  
    ##  NA's                                   :27843  
    ##                                       SDHISOLT    
    ##  Never                                    : 8294  
    ##  Rarely                                   : 5163  
    ##  Sometimes                                : 4014  
    ##  Usually                                  : 1000  
    ##  Always feel socially isolated from others:  602  
    ##  (Other)                                  :  149  
    ##  NA's                                     :27860  
    ##                                                                                                                             SDHSTRE1    
    ##  Never                                                                                                                          : 7452  
    ##  Rarely                                                                                                                         : 5359  
    ##  Sometimes                                                                                                                      : 4089  
    ##  Usually                                                                                                                        : 1222  
    ##  Always feels tense, restless, nervous, or anxious, or is unable to sleep at night because his/her mind is troubled all the time:  887  
    ##  (Other)                                                                                                                        :   68  
    ##  NA's                                                                                                                           :28005  
    ##                                   ADDEPEV3    
    ##  Yes ever told had depressive disorder: 9336  
    ##  No                                   :37487  
    ##  NA's                                 :  259  
    ##                                               
    ##                                               
    ##                                               
    ##                                               
    ##                              PRIMINSR    
    ##  Medicare                        :16017  
    ##  health ins thr employer or union:15997  
    ##  private plan                    : 4005  
    ##  Medicaid                        : 2748  
    ##  no coverage of any type         : 2147  
    ##  CHAMPUS                         : 1985  
    ##  (Other)                         : 4183  
    ##                                                                     CHECKUP1    
    ##  last routine checkup within past year (anytime less than 12 months ago):37966  
    ##  Within past 2 years (1 year but less than 2 years ago)                 : 3966  
    ##  Within past 5 years (2 years but less than 5 years ago)                : 2358  
    ##  5 or more years ago                                                    : 1992  
    ##  Dont know Not sure                                                     :  516  
    ##  Never                                                                  :  239  
    ##  Refused                                                                :   45  
    ##                            FLUSHOT7    
    ##  Yes got flu shot in last 12 mo:24689  
    ##  No                            :21942  
    ##  NA's                          :  451  
    ##                                        
    ##                                        
    ##                                        
    ##                                        
    ##                                 COVIDPOS    
    ##  Yes had med prof tell positive test:12902  
    ##  No                                 :32480  
    ##  tested positive at home wo med prof: 1476  
    ##  NA's                               :  224  
    ##                                             
    ##                                             
    ##                                             
    ##                                           COVIDSMP    
    ##  Yes had covid symptoms for more than 3 months: 3369  
    ##  No                                           :10670  
    ##  NA's                                         :33043  
    ##                                                       
    ##                                                       
    ##                                                       
    ##                                                       
    ##                                                                                                        COVIDPRM    
    ##  Tiredness or fatigue                                                                                      :  869  
    ##  Difficulty breathing or shortness of breath                                                               :  651  
    ##  Loss of taste or smell                                                                                    :  558  
    ##  Some other symptom                                                                                        :  316  
    ##  Difficulty thinking or concentrating or forgetfulness/memory problems (sometimes referred to as brain fog):  311  
    ##  (Other)                                                                                                   :  664  
    ##  NA's                                                                                                      :43713  
    ##                          COVIDVA1                COVIDNU1    
    ##  Yes had at least 1 covid vax:12135   had 1 covid vax:  771  
    ##  No                          : 3202   2 covid vax    : 3876  
    ##  NA's                        :31745   3 covid vax    : 5561  
    ##                                       4 covid vax    : 1890  
    ##                                       NA's           :34984  
    ##                                                              
    ##                                                              
    ##                         EXERANY2        SLEPTIM1      Height_inches  
    ##  Yes exercised in past month:35061   Min.   : 1.000   Min.   :36.00  
    ##  No                         :11930   1st Qu.: 6.000   1st Qu.:64.00  
    ##  NA's                       :   91   Median : 7.000   Median :67.00  
    ##                                      Mean   : 7.078   Mean   :67.22  
    ##                                      3rd Qu.: 8.000   3rd Qu.:70.00  
    ##                                      Max.   :24.000   Max.   :96.00  
    ##                                      NA's   :454      NA's   :1243   
    ##     WEIGHT2           X_BMI5              X_BMI5CAT    
    ##  Min.   :  62.0   Min.   :12.11   Underweight  :  724  
    ##  1st Qu.: 150.0   1st Qu.:24.37   Normal Weight:12149  
    ##  Median : 180.0   Median :27.76   Overweight   :15481  
    ##  Mean   : 186.4   Mean   :28.86   Obese        :15742  
    ##  3rd Qu.: 210.0   3rd Qu.:32.12   NA's         : 2986  
    ##  Max.   :1135.0   Max.   :95.66                        
    ##  NA's   :2597     NA's   :2986                         
    ##                                  SMOKE100                   SMOKDAY2    
    ##  yes smoked at least 100 cigs in life:19668   smoke every day   : 4537  
    ##  no                                  :27060   smoke some days   : 1646  
    ##  dont know not sure                  :  247   not at all        :13458  
    ##  refused                             :  107   Dont know Not Sure:   19  
    ##                                               Refused           :    8  
    ##                                               NA's              :27414  
    ##                                                                         
    ##                             ECIGNOW2                               ALCDAY4     
    ##  never used e-cigarettes in life:36662   none                          :22094  
    ##  use every day                  : 1146   1 in last month               : 3637  
    ##  use some days                  : 1238   2 in last month               : 2917  
    ##  not at all right now           : 7804   1 alcoholic drink in past week: 2088  
    ##  Dont know Not Sure             :  106   30 in last month              : 1923  
    ##  Refused                        :  126   2 in week                     : 1673  
    ##                                          (Other)                       :12750  
    ##     AVEDRNK3         DRNK3GE5         MARIJAN1     
    ##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
    ##  1st Qu.: 1.000   1st Qu.: 0.000   1st Qu.: 0.000  
    ##  Median : 2.000   Median : 0.000   Median : 0.000  
    ##  Mean   : 2.275   Mean   : 1.393   Mean   : 2.219  
    ##  3rd Qu.: 3.000   3rd Qu.: 1.000   3rd Qu.: 0.000  
    ##  Max.   :76.000   Max.   :55.000   Max.   :30.000  
    ##  NA's   :23028    NA's   :23086    NA's   :28825   
    ##                   FIREARM5    
    ##  yes firearms in house:  908  
    ##  no                   : 1244  
    ##  NA's                 :44930  
    ##                               
    ##                               
    ##                               
    ##                               
    ##                                                                                           ACEDEPRS    
    ##  Yes, Adverse Childhood Exper, lived with someone who was depressed, mentally ill, or suicidal: 8612  
    ##  No                                                                                           :37255  
    ##  dont know not sure                                                                           :  455  
    ##  refused                                                                                      :  760  
    ##                                                                                                       
    ##                                                                                                       
    ##                                                                                                       
    ##                                                                                     ACEDRINK    
    ##  Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic:11260  
    ##  No                                                                                     :34886  
    ##  dont know not sure                                                                     :  193  
    ##  refused                                                                                :  743  
    ##                                                                                                 
    ##                                                                                                 
    ##                                                                                                 
    ##                                                                                                                   ACEDRUGS    
    ##  Yes, Adverse Childhood Exper, lived with someone who used illegal street drugs or who abused prescription medications: 4591  
    ##  No                                                                                                                   :41482  
    ##  dont know not sure                                                                                                   :  291  
    ##  refused                                                                                                              :  718  
    ##                                                                                                                               
    ##                                                                                                                               
    ##                                                                                                                               
    ##                                                                                                                                               ACEPRISN    
    ##  Yes, Adverse Childhood Exper, lived with someone who served time or was sentenced to serve time in a prison, jail, or other correctional facility: 3466  
    ##  No                                                                                                                                               :42739  
    ##  dont know not sure                                                                                                                               :  165  
    ##  refused                                                                                                                                          :  712  
    ##                                                                                                                                                           
    ##                                                                                                                                                           
    ##                                                                                                                                                           
    ##                                                         ACEDIVRC    
    ##  Yes, Adverse Childhood Exper, parents separated or divorced:11824  
    ##  No                                                         :33647  
    ##  dont know not sure                                         :  258  
    ##  parents never married                                      :  608  
    ##  refused                                                    :  745  
    ##                                                                     
    ##                                                                     
    ##                                                                                                                                 ACEPUNCH    
    ##  Adverse Childhood Exper, never: How often did your parents or adults in your home ever slap, hit, kick, punch or beat each other up:38067  
    ##  once                                                                                                                               : 1752  
    ##  more than once                                                                                                                     : 5695  
    ##  dont know not sure                                                                                                                 :  629  
    ##  refused                                                                                                                            :  939  
    ##                                                                                                                                             
    ##                                                                                                                                             
    ##                                                                                                                                                                           ACEHURT1    
    ##  Adverse Childhood Exper, never: Not including spanking, (before age 18), how often did a parent or adult in your home ever hit, beat, kick, or physically hurt you in any way:34431  
    ##  once                                                                                                                                                                         : 2890  
    ##  more than once                                                                                                                                                               : 8439  
    ##  dont know not sure                                                                                                                                                           :  331  
    ##  refused                                                                                                                                                                      :  991  
    ##                                                                                                                                                                                       
    ##                                                                                                                                                                                       
    ##                                                                                                                         ACESWEAR    
    ##  Adverse Childhood Exper, never: How often did a parent or adult in your home ever swear at you, insult you, or put you down:29709  
    ##  once                                                                                                                       : 2357  
    ##  more than once                                                                                                             :13476  
    ##  dont know not sure                                                                                                         :  538  
    ##  refused                                                                                                                    : 1002  
    ##                                                                                                                                     
    ##                                                                                                                                     
    ##                                                                                                                        ACETOUCH    
    ##  Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually:40223  
    ##  once                                                                                                                      : 1901  
    ##  more than once                                                                                                            : 3614  
    ##  dont know not sure                                                                                                        :  193  
    ##  refused                                                                                                                   : 1151  
    ##                                                                                                                                    
    ##                                                                                                                                    
    ##                                              CIMEMLOS    
    ##  experienced confusion memory loss in last 12 mo : 2084  
    ##  no                                              :16156  
    ##  NA's                                            :28842  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                                                                                                                  CDHOUSE     
    ##  Never                                                                                                                                               : 1071  
    ##  Sometimes                                                                                                                                           :  469  
    ##  Rarely                                                                                                                                              :  397  
    ##  Usually                                                                                                                                             :  118  
    ##  Always, as a result of confusion or memory loss, how often have you given up day-to-day household activities or chores you used to do, in last 12 mo:  104  
    ##  (Other)                                                                                                                                             :   42  
    ##  NA's                                                                                                                                                :44881  
    ##                                                 CDASSIST    
    ##  Never                                              : 1185  
    ##  Rarely                                             :  426  
    ##  Sometimes                                          :  374  
    ##  Always need assist because confusion or memory loss:  104  
    ##  Usually                                            :   90  
    ##  (Other)                                            :   22  
    ##  NA's                                               :44881  
    ##                                                                      CDSOCIAL    
    ##  Never                                                                   : 1107  
    ##  Rarely                                                                  :  421  
    ##  Sometimes                                                               :  364  
    ##  Usually                                                                 :  142  
    ##  Always confusion or memory loss interfere with work or social activities:  134  
    ##  (Other)                                                                 :   33  
    ##  NA's                                                                    :44881  
    ##                                                                   CAREGIV1    
    ##  provided care to family or friend with disability or health condition: 2662  
    ##  no                                                                   : 9920  
    ##  caregiving recipient died in past 30 days                            :   28  
    ##  NA's                                                                 :34472  
    ##                                                                               
    ##                                                                               
    ##                                                                               
    ##                        CRGVREL4       Age_midpt      Educ_number   
    ##  Mother                    :  577   Min.   :21.00   Min.   : 0.00  
    ##  Non-relative/Family friend:  451   1st Qu.:42.00   1st Qu.:12.00  
    ##  Husband                   :  291   Median :62.00   Median :14.00  
    ##  Child                     :  266   Mean   :56.75   Mean   :13.94  
    ##  Wife                      :  240   3rd Qu.:72.00   3rd Qu.:16.00  
    ##  (Other)                   :  836   Max.   :82.00   Max.   :16.00  
    ##  NA's                      :44421   NA's   :759     NA's   :178    
    ##  income_midpoint
    ##  62500  : 6995  
    ##  42500  : 5688  
    ##  87500  : 5546  
    ##  125000 : 5181  
    ##  30000  : 4801  
    ##  (Other):10248  
    ##  NA's   : 8623

``` r
brfss_ACE$ACEPUNCH_recode <- fct_recode(brfss_ACE$ACEPUNCH, 
                                        "0" = "Adverse Childhood Exper, never: How often did your parents or adults in your home ever slap, hit, kick, punch or beat each other up",
                                "0.5" = "once", 
                                "1" = "more than once",
                                NULL = "dont know not sure",
                                NULL = "refused"
)

brfss_ACE$ACEPUNCH_recode <- as.numeric(levels(brfss_ACE$ACEPUNCH_recode))[brfss_ACE$ACEPUNCH_recode]
```

``` r
summary(brfss_ACE$ACEPUNCH_recode)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  0.0000  0.0000  0.0000  0.1444  0.0000  1.0000    1568

``` r
brfss_ACE$ACESWEAR_recode <- fct_recode(brfss_ACE$ACESWEAR, 
                                        "0" = "Adverse Childhood Exper, never: How often did a parent or adult in your home ever swear at you, insult you, or put you down",
                                "0.5" = "once", 
                                "1" = "more than once",
                                NULL = "dont know not sure",
                                NULL = "refused"
)

brfss_ACE$ACESWEAR_recode <- as.numeric(levels(brfss_ACE$ACESWEAR_recode))[brfss_ACE$ACESWEAR_recode]
```

``` r
summary(brfss_ACE$ACESWEAR_recode)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  0.0000  0.0000  0.0000  0.3218  1.0000  1.0000    1540

``` r
brfss_ACE$ACEDRUGS_recode <- fct_recode(brfss_ACE$ACEDRUGS, 
                                        "0" = "Yes, Adverse Childhood Exper, lived with someone who used illegal street drugs or who abused prescription medications",
                                "0.5" = "No", 
                                NULL = "dont know not sure",
                                NULL = "refused"
)

brfss_ACE$ACEDRUGS_recode <- as.numeric(levels(brfss_ACE$ACEDRUGS_recode))[brfss_ACE$ACEDRUGS_recode]
```

``` r
summary(brfss_ACE$ACEDRUGS_recode)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  0.0000  0.5000  0.5000  0.4502  0.5000  0.5000    1009

``` r
brfss_ACE$MENTHLTH_recode <- cut(brfss_ACE$MENTHLTH, breaks = c(-1,0,1,5,10,15,31))
summary(brfss_ACE$MENTHLTH_recode)
```

    ##  (-1,0]   (0,1]   (1,5]  (5,10] (10,15] (15,31] 
    ##   29340    1537    6932    2735    2010    4528
