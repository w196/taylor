library(dplyr)
library(ggplot2)
library(sandwich)
library(lmtest)

# compute classical taylor rule for our period in question
a_y = 0.5
a_pi = 0.5

taylor <- data.frame(date=deflator$observation_date)
taylor$gap <- gdp_real$GDPC1-gdp_pot$GDPPOT
taylor$optimal <- ( deflator$deflator + HLW_natural_r$r + a_pi*(deflator$deflator-2) + a_y*100*(taylor$gap)/gdp_pot$GDPPOT)
taylor$nogap <- ( deflator$deflator + HLW_natural_r$r + (deflator$deflator-2) ) # optimal assuming a_y=0
# get quarterly fundsrates (yes this is a dumb way of doing it)
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
#           (from 2010): very fucking significant
