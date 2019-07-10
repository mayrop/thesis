



data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
# Fit the logistic regression model
model <- glm(diabetes ~., data = PimaIndiansDiabetes2, 
             family = binomial)
# Predict the probability (p) of diabete positivity
probabilities <- predict(model, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

mydata <- PimaIndiansDiabetes2 %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)


plot(fitted(model.beta),
     residuals(model.beta))


### More diagnostic plots: plot(model.beta)



data("GasolineYield", package="betareg")
GasolineYield
model <- betareg(yield ~ batch + temp, data=GasolineYield)
summary(model)
plot(model)

data("FoodExpenditure", package="betareg")
FoodExpenditureNew <- FoodExpenditure[order(FoodExpenditure$income),]
FoodExpenditureNew$prop <- FoodExpenditure$food/FoodExpenditure$income

FoodExpenditure <- FoodExpenditureNew

fe_lm <- lm(I(food/income) ~ income + persons, data=FoodExpenditure)
bptest(fe_lm)

fe_beta <- betareg(I(food/income) ~ income + persons, data=FoodExpenditure)
bptest(fe_beta)

fe_beta2 <- betareg(I(food/income) ~ income + persons | persons, data=FoodExpenditure)
bptest(fe_beta2)

summary(fe_beta)
summary(fe_beta2)

redblueblack <- hcl(c(0, 260, 0), c(90, 90, 0), c(40, 40, 0))

plot(I(food/income) ~ income, data = FoodExpenditure,
     xlab = "Household income", ylab = "Proportion of food expenditures",
     main = "Food expenditures data", type = "n", ylim = c(0.04, 0.57))

points(I(food/income) ~ income, data = FoodExpenditure, cex = persons / 1.5, pch = 19,
       col = rev(gray.colors(7))[persons])

points(I(food/income) ~ income, data = FoodExpenditure, cex = persons / 1.5)

lines(10:100, predict(fe_lm, 
                      newdata = data.frame(income = 10:100, persons = mean(FoodExpenditure$persons))),
      col = redblueblack[3], lwd = 2, lty = 2)
lines(10:100, predict(fe_beta, 
                      newdata = data.frame(income = 10:100, persons = mean(FoodExpenditure$persons))),
      col = redblueblack[2], lwd = 2, lty = 5)
lines(10:100, predict(fe_beta2, 
                      newdata = data.frame(income = 10:100, persons = mean(FoodExpenditure$persons))),
      col = redblueblack[1], lwd = 2)

abline(fe_lm)

#http://alexschell.github.io/emplogit.html
#https://bookdown.org/roback/bookdown-bysh/ch-logreg.html

empLogitPlot <- function(x, y, nclass = floor(sqrt(length(x)))) {
  require(arm)
  require(ggplot2)
  
  logit <- function (x, eps = 0.05) log((eps + x)/(1 - x + eps))
  
  binned.df <- as.data.frame(binned.resids(x = x, y = y, nclass = nclass)[[1]])
  
  p <- qplot(x = xbar, y = logit(ybar), data = binned.df, geom = c("point", "smooth"), method = "lm", se = FALSE) + 
    ylim(min(logit(binned.df$ybar)), max(logit(binned.df$ybar)))
  return(p)
}

emplogit = function(x, y, binsize = NULL, ci = FALSE, probit = FALSE,
                    prob = FALSE, main = NULL, xlab = "", ylab = ""){
  # x         vector with values of the independent variable
  # y         vector of binary responses
  # binsize   integer value specifying bin size (optional)
  # ci        logical value indicating whether to plot approximate
  #           confidence intervals (not supported as of 02/08/2015)
  # probit    logical value indicating whether to plot probits instead
  #           of logits
  # prob      logical value indicating whether to plot probabilities
  #           without transforming
  #
  # the rest are the familiar plotting options
  
  if (length(x) != length(y))
    stop("x and y lengths differ")
  if (any(y < 0 | y > 1))
    stop("y not between 0 and 1")
  if (length(x) < 100 & is.null(binsize))
    stop("Less than 100 observations: specify binsize manually")
  
  if (is.null(binsize)) binsize = min(round(length(x)/10), 50)
  
  if (probit){
    link = qnorm
    if (is.null(main)) main = "Empirical probits"
  } else {
    link = function(x) log(x/(1-x))
    if (is.null(main)) main = "Empirical logits"
  }
  
  sort = order(x)
  x = x[sort]
  y = y[sort]
  a = seq(1, length(x), by=binsize)
  b = c(a[-1] - 1, length(x))
  
  prob = xmean = ns = rep(0, length(a)) # ns is for CIs
  for (i in 1:length(a)){
    range = (a[i]):(b[i])
    prob[i] = mean(y[range])
    xmean[i] = mean(x[range])
    ns[i] = b[i] - a[i] + 1 # for CI 
  }
  
  extreme = (prob == 1 | prob == 0)
  prob[prob == 0] = min(prob[!extreme])
  prob[prob == 1] = max(prob[!extreme])
  
  g = link(prob) # logits (or probits if probit == TRUE)
  
  linear.fit = lm(g[!extreme] ~ xmean[!extreme])
  b0 = linear.fit$coef[1]
  b1 = linear.fit$coef[2]
  
  loess.fit = loess(g[!extreme] ~ xmean[!extreme])
  
  plot(xmean, g, main=main, xlab=xlab, ylab=ylab)
  abline(b0,b1)
  lines(loess.fit$x, loess.fit$fitted, lwd=2, lty=2)
}


set.seed(1234)
n = 2000
x1 = exp(rnorm(n/2, mean=0, sd=1))
x2 = exp(rnorm(n/2, mean=1, sd=1))
x = c(x1, x2)
y = c(rep(0, n/2), rep(1, n/2))

emplogit(x, y)
emplogit(log(republican$population_density_2010), as.numeric(republican$party_won) - 1)
empLogitPlot(log(republican$population_density_2010), as.numeric(republican$party_won) - 1)


plot(republican$age_under_5_percent_2014, log((as.numeric(!(as.numeric(republican$party_won) - 1)) + 1/2)/((as.numeric(republican$party_won) - 1) + 1/2)), xlab = "concentration", ylab = "logit")


republican$
  
  m1 = glm(y ~ x, family="binomial")
m2 = glm(y ~ log(x), family="binomial")



# there is a guy that is buying bottles of wine in batches
# wine weights different depending on the quality
# this guy only buys good quality but he thinks he is getting mixed good and bad quality
# he weighted 9 randomly bottles of wine he got and splitted them between good & bad

# question is, is there a significant difference in weights between the two groups?
# what's the prob that they are actually mixed?


#####################################3

good <- c(
  36.43,
  36.58,
  36.57,
  36.6,
  36.37,
  36.369
)

bad <- c(
  36.88,
  36.55,
  36.71
)

# test for equal variance
var.test(bad, good)
# p-value = 0.3894
# we can assume the variacnes of both samples are homogenous

# H0: Two means are the same
# H1: Two means are different
test <- t.test(good, bad, var.equal = TRUE)
test

# t = -2.5185, df = 7, p-value = 0.0399
# 95 percent confidence interval:
# -0.43948285 -0.01385048

p.2 <-power.t.test(n=9, delta=.2, sd=1, sig.level=.05, type='one.sample')
p.5 <- power.t.test(n=9, delta=.5, sd=1, sig.level=.05, type='one.sample')
p.8 <-power.t.test(n=9, delta=.8, sd=1, sig.level=.05, type='one.sample')

round(rbind(p.2=p.2$power, p.5=p.5$power, p.8=p.8$power), 2)  


wilcox.test(good, bad)

#####################################3