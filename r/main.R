library(dplyr)
library(ggplot2)
library(sandwich)
library(lmtest)
library(lubridate)
library(stargazer)
library(xtable)
library(estimatr)

# compute classical taylor rule for our period in question
a_y = 0.5
a_pi = 0.5

taylor <- data.frame(date=deflator$observation_date)
taylor$gap <- gdp_real$GDPC1-gdp_pot$GDPPOT
taylor$optimal <- ( deflator$deflator + HLW_natural_r$r + a_pi*(deflator$deflator-2) + a_y*100*(taylor$gap)/gdp_pot$GDPPOT)
taylor$nogap <- ( deflator$deflator + HLW_natural_r$r + a_pi*(deflator$deflator-2) ) # optimal assuming a_y=0

# get quarterly fundsrates (yes this looks stupid)
fundsrate_qtr <-   fundsrate[ grepl("01-01$", as.character(fundsrate$observation_date)) |
                              grepl("04-01$", as.character(fundsrate$observation_date)) |
                              grepl("07-01$", as.character(fundsrate$observation_date)) |
                              grepl("10-01$", as.character(fundsrate$observation_date)),]
taylor$actual <- fundsrate_qtr$DFF

ggplot(taylor, aes(date)) +
  geom_line(aes(y=optimal, colour="Taylor at alpha_y=0.5")) +
  geom_line(aes(y=actual, colour="actual interest rate")) +
  geom_line(aes(y=nogap, colour="Taylor at alpha_y=0")) +
  theme_linedraw()

# evaluate if the taylor rule predicts actual policy
taylor$actual_2 <- dplyr::lag(taylor$actual, 2)
taylor$actual_4 <- dplyr::lag(taylor$actual, 4)
taylor$actual_8 <- dplyr::lag(taylor$actual, 8)

lmgap <- lm(data=taylor, actual ~ optimal+actual_2+actual_4+actual_8)
coeftest(lmgap, vcov=NeweyWest(lmgap))

lmnogap <- lm(data=taylor, actual ~ nogap+actual_2+actual_4+actual_8)
coeftest(lmnogap, vcov=NeweyWest(lmnogap))

# evaluate if the output gap is still significantly influencing central bank policy
chowgap <- lm(taylor$actual ~ deflator$deflator + HLW_natural_r$r + taylor$gap)
chownogap <- lm(taylor$actual ~ deflator$deflator + HLW_natural_r$r)
coeftest(chowgap, vcov=NeweyWest(chowgap))
coeftest(chownogap, vcov=NeweyWest(chownogap))
# Chow test (from 1985): F=32.5584642234, 159 DF, q=1
#           (from 2010): very significant

############################
# now using real-time data #
############################

# get real-time inflation 
rt_taylor <- data.frame(date=deflator$observation_date)
rt_taylor$deflator <- (rt_ngdp$d - rt_rgdp$d)
rt_taylor$rstar <- rt_rstar$rstar

rt_taylor$gap <- rt_rstar$gap 
rt_taylor$gap_1 <- lag(rt_rstar$gap)

rt_taylor$infdev <- rt_taylor$deflator-2

rt_taylor$actual <- taylor$actual

rt_taylor$zlb <- as.numeric(rt_taylor$actual < 0.25)

# gap is in % this time
rt_taylor$optimal <- ( rt_taylor$deflator + rt_taylor$rstar + a_pi*(rt_taylor$deflator-2) + a_y*rt_taylor$gap )
rt_taylor$nogap <- ( rt_taylor$deflator + rt_taylor$rstar + a_pi*(rt_taylor$deflator-2) ) # optimal assuming a_y=0

rt_taylor$actual_1 <- dplyr::lag(taylor$actual, 1)
rt_taylor$actual_2 <- dplyr::lag(taylor$actual, 2)
rt_taylor$actual_4 <- dplyr::lag(taylor$actual, 4)
rt_taylor$actual_8 <- dplyr::lag(taylor$actual, 8)

rt_taylor$actual_d <- rt_taylor$actual - rt_taylor$actual_1
rt_taylor$infdev_d <- rt_taylor$infdev - lag(rt_taylor$infdev)
rt_taylor$gap_d <- rt_taylor$gap - lag(rt_taylor$gap)

ggplot(rt_taylor, aes(date)) +
  geom_line(aes(y=optimal, colour="Taylor at alpha_y=0.5")) +
  geom_line(aes(y=actual, colour="actual interest rate")) +
  geom_line(aes(y=nogap, colour="Taylor at alpha_y=0")) +
  theme_linedraw()

# add a recession dummy variable as fed policy may be different during recessions
rt_taylor$recession <- as.numeric(rt_taylor$gap < 0)

# delineate some time periods to control for 
rt_taylor$greenspan <- as.numeric(rt_taylor$date < "2006-03-01")
rt_taylor$bernanke <- as.numeric(rt_taylor$date >= "2006-03-01" & rt_taylor$date < "2014-03-01")
rt_taylor$yellen <- as.numeric(rt_taylor$date >= "2014-03-01" & rt_taylor$date < "2018-03-01")
rt_taylor$powell <- as.numeric(rt_taylor$date >= "2018-03-01")
rt_taylor$covid <- as.numeric(rt_taylor$date >= "2020-03-01" & rt_taylor$date < "2023-03-01")

rt_chowgap <- lm(data=rt_taylor, actual ~ infdev + gap + gap*bernanke + gap*powell + gap*yellen + covid)
rt_chowgap_err <- sqrt(diag(NeweyWest(rt_chowgap)))
coeftest(rt_chowgap, vcov=NeweyWest(rt_chowgap))

rt_nls <- nls(data=rt_taylor,
              actual ~ rho*actual_1 + (1-rho)*(a0 + rstar + a1*infdev + a2*gap),
              start = list(rho=0.5, a0=0.5, a1=0.5, a2=0.5))
summary(rt_nls)

rt_chownogap <- lm(data=rt_taylor, actual ~ infdev + bernanke + powell + yellen + covid)
rt_chownogap_err <- sqrt(diag(NeweyWest(rt_chownogap)))
coeftest(rt_chownogap, vcov=NeweyWest(rt_chownogap))

## output
stargazer(rt_chownogap, rt_chowgap, se=list(rt_chownogap_err, rt_chowgap_err))

# Chow (from 1985): 2.03973509934
# Chow 
