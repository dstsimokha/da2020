# Ramsey test

# RESET - Ramsey test
# H0: there is no missing regressors
# H1: there are missing regressors

# ## How to:
# Define model \[\hat{wage}_i = -2.5 + 0.6school + 0.16 exper\] 
# build auxiliary regression model 


# # Informational criteria
# If RSS bigger and k - number of regressors - bigger - AIC/BIC will be lower.

# Akaike AIC = \[n ln(RSS/n) + 2k\]

# Schwartz BIC = \[n ln(RSS/n) + ln(n)k\]

library(memisc); library(lmtest); library(ggplot2); library(dplyr)
library(foreign); library(vcd); library(devtools); library(hexbin)
library(pander); library(sjPlot); library(knitr)
setwd("~/Documents/github/da2020/econometrics")

f = read.csv("flats_moscow.txt", sep = "\t", header = TRUE, dec = ".")
glimpse(f)

qplot(data = f, totsp, price)

(bg <- qplot(data = f, log(totsp), log(price)))
bg + geom_hex()

mosaic(data = f, ~walk+brick, shade = TRUE)

# Change columns format
f = mutate_each(f, "factor", walk, brick, floor, code)
glimpse(f)

qplot(data = f, log(price))
qplot(data = f, log(price), fill = brick)
qplot(data = f, log(price), fill = brick, position = "dodge")
qplot(data = f, log(price), fill = brick, geom = "density")
(g2 = qplot(data = f, log(price), fill = brick, geom = "density", alpha = 0.5))

g2 + facet_grid(walk~floor)
g2 + facet_grid(~floor)

# Models with dummy variables
model_0 = lm(data = f, log(price) ~ log(totsp))
model_1 = lm(data = f, log(price) ~ log(totsp) + brick)
# Model with interaction effect
# Sign ':' includes only variables interaction
model_2 = lm(data = f, log(price) ~ log(totsp) + brick + brick:log(totsp))

summary(model_0)
mtable(model_2)

# Sign '*' includes both variables and their interaction
model_2b = lm(data = f, log(price) ~ brick*log(totsp))
mtable(model_2, model_2b)

plot_model(model_2)

mtable(model_0, model_1, model_2)


# Predicting new data 
nw = data.frame(totsp = c(60, 60), brick = factor(c(1, 0)))
predict(model_2, newdata = nw)
exp(predict(model_2, newdata = nw))  # Because results in logarithmic scale
# Adding confidence intervals: for average flat
predict(model_2, newdata = nw, interval = "confidence")
exp(predict(model_2, newdata = nw, interval = "confidence"))
# Adding prediction intervals: for any possible flat
predict(model_2, newdata = nw, interval = "prediction")
exp(predict(model_2, newdata = nw, interval = "prediction"))


# Comparing models with F-test
waldtest(model_0, model_1)  # H0: true model_0 is rejected
waldtest(model_1, model_2)  # H0: true model_1 is rejected
waldtest(model_0, model_2)  # H0: true model_0 is rejected

(gg0 = qplot(data = f, log(totsp), log(price)))
gg0 + stat_smooth(method = "lm")
gg0 + stat_smooth(method = "lm") + facet_grid(~walk)
gg0 + aes(col = brick) + stat_smooth(method = "lm") + facet_grid(~walk)


# Dummy variables
f$nonbrick = memisc::recode(f$brick, 1 <- 0, 0 <- 1)
glimpse(f)
model_wrong = lm(data = f, log(price) ~ log(totsp) + brick + nonbrick)
summary(model_wrong)

# Comparing models with AIC & BIC - lower is better
mtable(model_0, model_1, model_2, 
       summary.stats = c("sigma", "R-squared", "F", "AIC", "BIC"))

# Ramsey test
resettest(model_2)  # if p-value < 0.05 = there are no missing variables


# Nano-experiment

