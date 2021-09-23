library(tidyverse)
library(fixest)

###################################### Wald Estimator
######################################
install.packages("masteringmetrics")

data("ak91", package = "masteringmetrics")
### Simple OLS

reg_ols<-feols(lnw~s, data=ak91, se="hetero")
summary(reg_ols)
############### One additional year of schooling increases wages by 7.09%

### First Stage

### The Instrument is a dummy variable that equals to one if QOB==1
ak91$instrument<-ifelse(ak91$qob==1,1,0)

ak91%>%group_by(instrument)%>%summarize(wages=mean(lnw), schooling=mean(s))

RF<-mean(ak91$lnw[ak91$instrument==1])-mean(ak91$lnw[ak91$instrument==0])
FS<-mean(ak91$s[ak91$instrument==1])-mean(ak91$s[ak91$instrument==0])

Wald=RF/FS

Wald

############# 2SLS estimates

##### Manually -> don't do it!!!

first_stage<-feols(s~instrument, data=ak91, se="hetero")
ak91$s_hat<-first$fitted.values

second_stage<-feols(lnw~s_hat, data=ak91, se="hetero")
summary(second_stage)


##### Let the software do it for you

### Without controls

ivreg<-feols(lnw~1|s~instrument, data=ak91, se="hetero")
summary(ivreg)


### With controls

ivreg2<-feols(lnw~1+factor(yob)+factor(sob)|s~instrument, data=ak91, se="hetero")
summary(ivreg2)

### With controls and multiple instruments
ak91$instrument2<-ifelse(ak91$qob==2,1,0)
ak91$instrument3<-ifelse(ak91$qob==3,1,0)

ivreg3<-feols(lnw~1+factor(yob)+factor(sob)|s~instrument+instrument2+instrument3, data=ak91, se="hetero")
summary(ivreg3)
