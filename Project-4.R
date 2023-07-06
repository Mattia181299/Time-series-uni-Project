################### Project Time Series ####################

install.packages("rugarch")

install.packages("rumidas")

################### Import the libraries ###################

library(rugarch)

library(rumidas)

library(readxl)

library(tseries)

library(tidyverse)

library(xts)

################### Import the dataset #################

getwd()

setwd("D:/Documenti/MAGISTRALE/TIME SERIES/PROJECT")

#Let's import the data set

data <- 
  na.exclude(read_excel("energydata+covid.xlsx",sheet=1, skip=4))

data2 <- read_excel("energydata+covid.xlsx",sheet=2, skip=4)

data1 <- data[-36,]

######## First let's analize if we can use the data as we have them by
#analyzing the stationarity ###

adf.test(data$WTI)

# with this we see that with the returns we have the results we wanted

adf.test(diff(log(data$WTI)))

jarque.bera.test(diff(log(data$WTI)))

######## Let's plot all the variables that we have ###########

graphics.off()

??plot.xts

graphics <- function (data) {
  par(mfrow=c(1,2))
  ts.plot(data)
  ts.plot(diff(log(data)))
}

graphics(data$WTI)
graphics(data$Europe.Brent)
graphics(data$Heating.Oil)
graphics(data$Propane)
graphics(data$Gasoline)
graphics(data$Kerosene)

############## Let's do all the garch model that we are interested in #################

################################# spec model ###################

spec1<-ugarchspec(variance.model = list(model="eGARCH",
                 garchOrder=c(1,1)), mean.model = 
                   list(armaOrder=c(0,0)), 
                 distribution.model = "norm")

spec2<-ugarchspec(variance.model = list(model="eGARCH",
              garchOrder=c(1,1)), mean.model = 
                   list(armaOrder=c(0,0)), 
                 distribution.model = "snorm")

spec3<- ugarchspec(variance.model = list(model="eGARCH",
                garchOrder=c(1,1)), mean.model = 
                     list(armaOrder=c(0,0)), 
                   distribution.model = "std")

spec4 <- ugarchspec(variance.model = list(model="eGARCH",
                    garchOrder=c(1,1)), mean.model = 
                      list(armaOrder=c(0,0)), 
                    distribution.model = "ged")

spec5 <-ugarchspec(variance.model = list(model="eGARCH",
                garchOrder=c(1,1)), mean.model = 
                    list(armaOrder=c(0,0)), 
                  distribution.model = "sstd")

spec6<-ugarchspec(variance.model = list(model="fGARCH",submodel="TGARCH",
                  garchOrder=c(1,1)), mean.model = 
                    list(armaOrder=c(0,0)), 
                  distribution.model = "norm")

spec7<- ugarchspec(variance.model = list(model="fGARCH", submodel="TGARCH",
                    garchOrder=c(1,1)), mean.model = 
                     list(armaOrder=c(0,0)), 
                   distribution.model = "snorm")

spec8 <- ugarchspec(variance.model = list(model="fGARCH", submodel="TGARCH",
                    garchOrder=c(1,1)), mean.model = 
                      list(armaOrder=c(0,0)), 
                    distribution.model = "std")

spec9<- ugarchspec(variance.model = list(model="fGARCH", submodel="TGARCH",
                  garchOrder=c(1,1)), mean.model = 
                     list(armaOrder=c(0,0)), 
                   distribution.model = "ged")

spec10<- ugarchspec(variance.model = list(model="fGARCH", submodel="TGARCH",
              garchOrder=c(1,1)), mean.model = 
               list(armaOrder=c(0,0)), 
             distribution.model = "sstd")


#####################LETS CHANGE ALL THE VARIABLES TO RETURNS ###############

WTI2 <- diff(log(data$WTI))
Ebd<- diff(log(data$Europe.Brent))
Hod<- diff(log(data$Heating.Oil))
Prd<-diff(log(data$Propane))
Gasd<-diff(log(data$Gasoline))
Kerd<-diff(log(data$Kerosene))

##################### FIT THE MODELS ############################

garchmodel <-function(data, spec){
  ugarchfit(data, spec=spec, out.sample = 20)
}

############# WTI RETURNS ###########

Wti_mod1<-garchmodel(WTI2, spec1)
Wti_mod2<-garchmodel(WTI2, spec3)
Wti_mod3<-garchmodel(WTI2, spec4)
Wti_mod4<-garchmodel(WTI2, spec5)
Wti_mod5<-garchmodel(WTI2, spec6)
Wti_mod6<-garchmodel(WTI2, spec7)
Wti_mod7<-garchmodel(WTI2, spec8)
Wti_mod8<-garchmodel(WTI2, spec9)
Wti_mod9<-garchmodel(WTI2, spec10)

######### EUROPE.BRENT ########

Ebd_mod1<-garchmodel(Ebd, spec1)
Ebd_mod2<-garchmodel(Ebd, spec2)
Ebd_mod3<-garchmodel(Ebd, spec3)
Ebd_mod4<-garchmodel(Ebd, spec4)
Ebd_mod5<-garchmodel(Ebd, spec5)
Ebd_mod6<-garchmodel(Ebd, spec6)
Ebd_mod7<-garchmodel(Ebd, spec7)
Ebd_mod8<-garchmodel(Ebd, spec8)
Ebd_mod9<-garchmodel(Ebd, spec9)
Ebd_mod10<-garchmodel(Ebd, spec10)

###########GASOLINE #########

Gasd_mod1<-garchmodel(Gasd, spec1)
Gasd_mod2<-garchmodel(Gasd, spec2)
Gasd_mod3<-garchmodel(Gasd, spec3)
Gasd_mod4<-garchmodel(Gasd, spec4)
Gasd_mod5<-garchmodel(Gasd, spec5)
Gasd_mod6<-garchmodel(Gasd, spec6)
Gasd_mod7<-garchmodel(Gasd, spec7)
Gasd_mod8<-garchmodel(Gasd, spec8)
Gasd_mod9<-garchmodel(Gasd, spec9)
Gasd_mod10<-garchmodel(Gasd, spec10)

############ HEATING.OIL ############

Hod_mod1<-garchmodel(Hod, spec1)
Hod_mod2<-garchmodel(Hod, spec2)
Hod_mod3<-garchmodel(Hod, spec3)
Hod_mod4<-garchmodel(Hod, spec4)
Hod_mod5<-garchmodel(Hod, spec5)
Hod_mod6<-garchmodel(Hod, spec6)
Hod_mod7<-garchmodel(Hod, spec7)
Hod_mod8<-garchmodel(Hod, spec8)
Hod_mod9<-garchmodel(Hod, spec9)
Hod_mod10<-garchmodel(Hod, spec10)

############# PROPANE ###############

Prd_mod1<-garchmodel(Prd, spec1)
Prd_mod2<-garchmodel(Prd, spec2)
Prd_mod3<-garchmodel(Prd, spec3)
Prd_mod4<-garchmodel(Prd, spec4)
Prd_mod5<-garchmodel(Prd, spec5)
Prd_mod6<-garchmodel(Prd, spec6)
Prd_mod7<-garchmodel(Prd, spec7)
Prd_mod8<-garchmodel(Prd, spec8)
Prd_mod9<-garchmodel(Prd, spec9)
Prd_mod10<-garchmodel(Prd, spec10)

############ KEROSENE #############

Kerd_mod1<-garchmodel(Kerd, spec1)
Kerd_mod2<-garchmodel(Kerd, spec2)
Kerd_mod3<-garchmodel(Kerd, spec3)
Kerd_mod4<-garchmodel(Kerd, spec4)
Kerd_mod5<-garchmodel(Kerd, spec5)
Kerd_mod6<-garchmodel(Kerd, spec6)
Kerd_mod7<-garchmodel(Kerd, spec7)
Kerd_mod8<-garchmodel(Kerd, spec8)
Kerd_mod9<-garchmodel(Kerd, spec9)
Kerd_mod10<-garchmodel(Kerd, spec10)

############## VaR TEST ###############

help(ugarchroll)

var_f<-function(Spec, data){
  var_t<- ugarchroll(Spec, data=data, n.ahead=1, n.start=130,
                     refit.every = 5, refit.window = "rolling",
                     calculate.VaR = TRUE, VaR.alpha = c(0.05))
  plot(var_t, which= 4, VaR.alpha=0.05)
  report(var_t, type="VaR", VaR.alpha=0.05, conf.level=0.95)
  print(class(var_t))
}

graphics.off()

########## let's do it for all the spec ###########

########## WTI returns #############

var_f(spec1,WTI2)
var_f(spec2,WTI2)
var_f(spec3,WTI2)
var_f(spec4,WTI2)
var_f(spec5,WTI2)
var_f(spec6,WTI2)
var_f(spec7,WTI2)
var_f(spec8,WTI2)
var_f(spec9,WTI2)
var_f(spec10,WTI2)

######################### EUROPE BRENT RETURNS ########################

var_f(spec1,Ebd)
var_f(spec2,Ebd)
var_f(spec3,Ebd)
var_f(spec4,Ebd)
var_f(spec5,Ebd)
var_f(spec6,Ebd)
var_f(spec7,Ebd)
var_f(spec8,Ebd)
var_f(spec9,Ebd)
var_f(spec10,Ebd)

######################## HEATING OIL RETURNS ########################

var_f(spec1,Hod)
var_f(spec2,Hod)
var_f(spec3,Hod)
var_f(spec4,Hod)
var_f(spec5,Hod)
var_f(spec6,Hod)
var_f(spec7,Hod)
var_f(spec8,Hod)
var_f(spec9,Hod)
var_f(spec10,Hod)

#########################



############################ MIDAS ##############################

d=data2$`Deaths USA`

Y2<-ts(d)

date2<-as.Date(data2$...1)

Death_weekly_<- apply.weekly(Y2, sum)

Death_weekly <- as.xts(coredata(Death_weekly_),order.by = date2,
                       
         by = "week", length.out = length(Death_weekly_))

############### Remove one data cause we are talking
# about returns #########################

d<- data$...1

d<- d[-1]

date <- as.Date(d)

################# Turn all the variables to time series ###################

WTI2_TS<- ts(WTI2)
Ebd_TS<- ts(Ebd)
Gasd_TS<-ts(Gasd)
Hod_TS<-ts(Hod)
Kerd_TS<-ts(Kerd)
Prd_TS<-ts(Prd)

################ apply weekly

WTI2_weeklysum<- apply.weekly(WTI2_TS, sum)
Ebd_weeklysum<- apply.weekly(Ebd_TS, sum)
Gasd_weeklysum<- apply.weekly(Gasd_TS, sum)
Hod_weeklysum<- apply.weekly(Hod_TS, sum)
Kerd_weeklysum<- apply.weekly(Kerd_TS, sum)
Prd_weeklysum<- apply.weekly(Prd_TS, sum)

################## xts ###########################

xtsfunction <- function(x){
  as.xts(coredata(x), order.by=date, 
          by = "week", length.out = length(x))
}

WTI2_weekly <- xtsfunction(WTI2_weeklysum)
Hod_weekly <- xtsfunction(Hod_weeklysum)
Ebd_weekly <- xtsfunction(Ebd_weeklysum)
Gasd_weekly <- xtsfunction(Gasd_weeklysum)
Kerd_weekly <- xtsfunction(Kerd_weeklysum)
Prd_weekly <- xtsfunction(Prd_weeklysum)

#############################################################

mv_mat <- function (data) {
  mv_into_mat(data['2020-03-16/'],
            diff(Death_weekly),
            K=2,type="weekly")
  }

mv_mat1<-mv_mat(WTI2_weekly)
mv_mat2<-mv_mat(Hod_weekly)
mv_mat3<-mv_mat(Kerd_weekly)
mv_mat4<-mv_mat(Prd_weekly)
mv_mat5<-mv_mat(Gasd_weekly)
mv_mat6<-mv_mat(Ebd_weekly)



mv_mat_ <- mv_into_mat(WTI2_weekly['2020-04-06/'],
                      diff(Death_weekly),
                      K=4,type="weekly")


fit<-ugmfit(model="GM",skew="YES",distribution="std",
            WTI2_weekly['2020-03-16/'],
            mv_mat2,K=2)
fit


fit2<-ugmfit(model="GM",skew="YES",distribution="norm",
            WTI2_weekly['2020-03-16/'],
            mv_mat2,K=2)

fit2

fit3<-ugmfit(model="GM",skew="YES",distribution="std",
            WTI2_weekly['2020-04-06/'],
            mv_mat_,K=4)

fit4<-ugmfit(model="GM",skew="YES",distribution="norm",
             WTI2_weekly['2020-04-06/'],
             mv_mat_,K=4)


summary.rumidas(fit3)

fit4$inf_criteria

fit$inf_criteria

multi_step_ahead_pred(fit,h=10)

names(fit)

fit$rob_coef_mat

adf.test(WTI2)

help(as.Date)

plot.ts(WTI2)

###################################################

#ugarchroll

models <- c("eGARCH", "tGARCH")
distributions <- c("norm", "std", "ged", "snorm", "sstd")
spec.comp <- list()
for( m in models ) {
  for( d in distributions ) {
    spec.comp[[paste( m, d, sep = "-" )]] <-
      ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                 variance.model = list(model = m, garchOrder = c(1, 1)),
                 distribution.model=d)
  }
}
specifications <- names( spec.comp )
roll.comp <- list()
for( s in specifications ){
  roll.comp[[s]] <- ugarchroll(spec = spec.comp[[s]], data = WTI2^2,
                               forecast.length = 130, refit.every=1)
}


roll.comb<- list()
for(s in specifications) {
  roll.comb[[s]]<-ugarchroll(spec=spec.comp[[s]],data=WTI2^2,
                             forecast.length = 120,refit.every = 5,
                             refit.window = "rolling")  
}

Var.test1<-list()
for(s in specifications){
  Var.test1[[s]] =VaRTest(alpha= 0.01,roll.comb[[s]]@forecast[["VaR"]][["realized"]],
                          roll.comb[[s]]@forecast[["VaR"]][["alpha(1%)"]])
} 
