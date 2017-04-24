library(readr)
library(fitdistrplus)
library(ggplot2)
library(dplyr)


wind_speed <- read_csv("6-hour_avr_wind_speed.csv", col_names = FALSE, skip = 1)
View(wind_speed$X1)
x<-as.numeric(wind_speed$X1)
hist(x,density = T)
x <- x[which(x>0)]
plot(x)
fit.weibull <- fitdist(x, "weibull")
fit.lognorm <- fitdist(x, "lnorm")
fit.exp <- fitdist(x, "exp")
fit.logis <- fitdist(x, "logis")
fit.gamma <- fitdist(x, "gamma", lower = c(0, 0))



den <- density(x)
dat <- data.frame(x = den$x, y = den$y)
ggplot(data = dat, aes(x = x, y = y)) + 
  geom_point(size = 3) +
  theme_classic()

fit.params <- fitdistr(x, "gamma", lower = c(0, 0))
fit.params <- fit.weibull
str(fit.params)
glimpse(fit.params)

ggplot(data = dat, aes(x = x,y = y)) + 
  geom_point(size = 3) +     
  geom_line(aes(x=dat$x, y=dgamma(dat$x,fit.params$estimate["shape"], fit.params$estimate["rate"])), color="red", size = 1) + 
  theme_classic()


# Plot using histograms

ggplot(data = dat) +
  geom_histogram(data = as.data.frame(x), aes(x=x, y=..density..)) +
  geom_line(aes(x=dat$x, y=dgamma(dat$x,fit.params$estimate["shape"], fit.params$estimate["rate"])), color="red", size = 1) + 
  theme_classic()


h <- hist(x, 1000, plot = FALSE)
t1 <- data.frame(x = h$mids, y = h$density)

ggplot(data = t1, aes(x = x, y = y)) + 
  geom_point(size = 3) +     
  geom_line(aes(x=t1$x, y=dgamma(t1$x,fit.params$estimate["shape"], fit.params$estimate["rate"])), color="red", size = 1) + 
  theme_classic()


# Compare fits graphically
# Weibull Gamma 
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "Gamma")
?denscomp
denscomp(list(fit.weibull, fit.gamma), fitcol = c("red", "blue"), legendtext = plot.legend, xlab="Wind Speed(m/s)")
qqcomp(list(fit.weibull, fit.gamma), fitcol = c("red", "blue"), legendtext = plot.legend)
cdfcomp(list(fit.weibull, fit.gamma), fitcol = c("red", "blue"), legendtext = plot.legend, xlab="Wind Speed(m/s)")
ppcomp(list(fit.weibull, fit.gamma), fitcol = c("red", "blue"), legendtext = plot.legend)


gofstat(list(fit.weibull, fit.gamma))


# Weibull LogNorm 
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "LogNorm")
denscomp(list(fit.weibull, fit.lognorm), fitcol = c("red", "blue"), legendtext = plot.legend, xlab="Wind Speed(m/s)")
qqcomp(list(fit.weibull, fit.lognorm), fitcol = c("red", "blue"), legendtext = plot.legend)
cdfcomp(list(fit.weibull, fit.lognorm), fitcol = c("red", "blue"), legendtext = plot.legend, xlab="Wind Speed(m/s)")
ppcomp(list(fit.weibull, fit.lognorm), fitcol = c("red", "blue"), legendtext = plot.legend)


# Weibull Logis 
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "Logis")
denscomp(list(fit.weibull, fit.logis), fitcol = c("red", "blue"), legendtext = plot.legend, xlab="Wind Speed(m/s)")
qqcomp(list(fit.weibull, fit.logis), fitcol = c("red", "blue"), legendtext = plot.legend)
cdfcomp(list(fit.weibull, fit.logis), fitcol = c("red", "blue"), legendtext = plot.legend, xlab="Wind Speed(m/s)")
ppcomp(list(fit.weibull, fit.logis), fitcol = c("red", "blue"), legendtext = plot.legend)


# 
gofstat(list(fit.weibull, fit.gamma,fit.logis,fit.lognorm))

plot.legend <- c("Weibull", "Logis","LogNorm","Gamma")
ppcomp(list(fit.weibull, fit.logis, fit.lognorm, fit.gamma), fitcol = c("red", "blue","green","maroon"), legendtext = plot.legend)
par(mfrow = c(1,1))
denscomp(list(fit.weibull, fit.logis, fit.lognorm, fit.gamma), fitcol = c("red", "blue","purple","maroon"), legendtext = plot.legend, xlab="Wind Speed(m/s)")
