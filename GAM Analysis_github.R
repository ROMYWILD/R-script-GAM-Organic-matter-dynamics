library("itsadug")
library("mgcv")

#creates a logical string indicating when a new time series starts based on the information of sampling time (when T=1) within each of the streams (S).
data1 <- start_event(data1, column="T", event=c("S"))


#Calculate GAM m1
#Response variable= Organic matter standing crop (OM), fixed factor= Stream (S) nested into landuse type (LU).
m1<- gam(OM ~ LU/S
         + s(T, by=LU),
         family=Gamma(link="log"),
         data=data1
#Calculate start value of model m1
rho.m1 <- start_value_rho(m1, plot=TRUE) 

#Recalculate model m1 including the correlation structure
m1<- gam(OM ~ LU/S
            + s(T, by=LU),
            family=Gamma(link="log"),
            data=data1,
            AR.start=data1$start.event, rho=rho.m1)

summary(m1)

##Ordering the factor landuse(LU)
#create factor OFLU
data1$OFLU <- as.factor(data1$LU)
# change factor to ordered factor:
data1$OFLU <- as.ordered(data1$OFLU)
# change contrast to treatment coding (difference curves)
contrasts(data1$OFLU) <- 'contr.treatment'
# Inspect contrasts:
contrasts(data1$OFLU)

#calculate model with the ordered factor OFLU
m2<- gam(BM ~ LU/S
         + s(T)
         +s(T,by=OFLU),
         family=Gamma(link="log"),
         data=data1)
#calculate start value for m2
rho.m2 <- start_value_rho(m2, plot=TRUE) 
#recalculate m2 with the correlation structure
m2<- gam(BM ~ LU/BK
            + s(T)
            +s(T,by=OFLU),
            family=Gamma(link="log"),
            data=Enviro_BM_BK_T_K,
            AR.start=data1$start.event, rho=rho.m2)

#calculating a glmer model to carry out multiple comparisons between streams (S) using the lsmeans function
library(lme4)
library(lsmeans)
glmer_1<-glmer(OM ~ S + (1 + T | S), data = data1, family = Gamma(link = "log"))
lsmeans(glmer_1, pairwise ~ S, adjust= "bonferroni")

