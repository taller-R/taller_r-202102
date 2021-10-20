#=========================================#
# Elaborado por: Eduard Martinez
# Colaboradores: 
# Ultima modificacion: 19/09/2021
# Versión de R: 
#=========================================#

# configuracion inicial 
rm(list = ls()) # limpia el entorno de R
require(pacman)
p_load(tidyverse, # llamar y/o instalar las librerias de la clase
       broom, # tidy-coefficients
       mfx, # marginal effects
       margins,  # marginal effects
       estimatr, # robust standard errors
       lmtest, # HAC (Newey-West) standard errors
       fixest, # hdfe regressions (feols)
       modelsummary, # Coefplot with modelplot
       stargazer # export tables to latex 
)  

# importar datos
browseURL("https://www1.nyc.gov/site/tlc/about/tlc-trip-record-data.page") # source
browseURL("https://www1.nyc.gov/assets/tlc/downloads/pdf/data_dictionary_trip_records_green.pdf") # data dictionaries 
df = read.csv("https://nyc-tlc.s3.amazonaws.com/trip+data/green_tripdata_2020-12.csv")

# trip_distance: The elapsed trip distance in miles reported by the taximeter
# total_amount: The total amount charged to passengers. Does not include cash tips.
# payment_type: 1= Credit card 2= Cash 3= No charge 4= Dispute 5= Unknown 6= Voided trip
# passenger_count: The number of passengers in the vehicle.
# trip_type: 1= Street-hail 2= Dispatch

#=====================#
# 1. Basic Regression #
#=====================#

# lm function
?lm
lm(total_amount ~ trip_distance + passenger_count , data = df) 

# Linear regression
ols = lm(total_amount ~ trip_distance + passenger_count , data = df) 
ols %>% summary() 
summary(ols)$r.squared # R^2
summary(ols)$adj.r.squared # R^2 ajustado

# What is ols object?
View(ols)
ols$call # model
ols$coefficients # get coefficients
ols$na.action # rows's NA
ols$residuals # get residuals
summary(ols$residuals)
hist(ols$residuals)

# get predict values
ols %>% predict()
df$predict_ols = predict(object = ols,newdata=df)

#=================#
# 2. Subset datos #
#=================#

# remover outlayers
ggplot(data=df) + geom_point(aes(x=trip_distance,y=total_amount)) + theme_bw()

# subset data
df_s = subset(df,trip_distance<1000)
ggplot(data=df_s) + geom_point(aes(x=trip_distance,y=total_amount)) + theme_bw()

# new estimations
ols2 = lm(total_amount ~ trip_distance + passenger_count , data = df_s) 

# get "tidy" regression coefficients (broom library)
tidy(ols2, conf.int = TRUE)
glance(ols2)

#===========================#
# 3. Robust standard errors #
#===========================#

# Eicker-Huber-White robust standard errors (commonly referred to as “HC2”)
ols_robust = lm_robust(total_amount ~ trip_distance + passenger_count , data = df_s)
ols_robust %>% tidy(conf.int = TRUE)

# replicar resultados de Stata
ols_stata = lm_robust(total_amount ~ trip_distance + passenger_count , data = df_s , se_type = "stata")
ols_stata %>% tidy(conf.int = TRUE)

# Print the HAC VCOV
ols_hac = coeftest(ols, vcov = NeweyWest)
ols_hac %>% tidy(conf.int = TRUE)

# cluster standar errors
# ols_cluster = lm_robust(total_amount ~ trip_distance + passenger_count , data=df_s , clusters=passenger_count)
# ols_cluster

#==========================================#
# 4. Dummy variables and interaction terms #
#==========================================#

# categoricla variables
lm(total_amount ~ trip_distance + passenger_count + as.factor(payment_type), data=df_s)

# include interaction terms
cat("x1:x2 = x1 × x2")
cat("x1/x2 = x1 + x1:x2")
cat("x1*x2 = x1 + x2 + x1:x2")
lm(total_amount ~ trip_distance:passenger_count, data=df_s)
lm(total_amount ~ trip_distance/passenger_count, data=df_s)
lm(total_amount ~ trip_distance*passenger_count, data=df_s)

#=====================#
# 5. Marginal effects #
#=====================#

# make output var
df = df %>% mutate(pay_credit = ifelse(payment_type==1,1,0))

# logit
logit = glm(pay_credit ~ trip_distance + passenger_count , data = df , family = binomial(link = "logit")) 
logit %>% summary()

# probit
probit = glm(pay_credit ~ trip_distance + passenger_count , data = df , family = binomial(link = "probit")) 
probit %>% summary()

# marginal effects
logit_marg = margins(logit)
logit_marg %>% tidy(conf.int = TRUE)
probit_marg = margins(probit)
probit_marg %>% tidy(conf.int = TRUE)

#=================#
# 6. Presentation #
#=================#

# joint models (modelsummary)
msummary(list(ols, ols2 , ols_robust , ols_stata , ols_hac))

# export table
stargazer(ols, ols2,
          type= 'text',
          dep.var.labels = c('','Number of flights',''), 
          df = FALSE,
          digits = 3, 
          out = paste0('data_10/output/ols.text'))

# coefplot
mods = list('Logit' = logit_marg , 'Probit' = probit_marg)

modelplot(mods) + coord_flip() + 
labs(title = "Probability to pay with credit card" , subtitle = "Comparing models")


# coefplot with ggplot
db = tidy(ols2 , conf.int = TRUE)
db
ggplot(db , aes(x = estimate, y = term)) + theme_light() + 
geom_vline(aes(xintercept = 0),color="red",linetype="dashed",width=1) + 
geom_errorbar(width=.5, aes(xmin=conf.low, xmax=conf.high) , col="black" , show.legend = F) + 
geom_point(size = 3,show.legend = F , col="black") +
theme(axis.text = element_text(color = "black", size = 15)) + 
labs(y="",x="Effect on total amount")

# Prediction and model validation
ggplot(df_s, aes(x = trip_distance, y = total_amount)) +
geom_point(alpha = 0.7) +
geom_smooth(method="lm" , se=T) + theme_bw()



