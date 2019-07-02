########################################################################

get_map_color <- function(lead_party, lead_votes) {
  if (lead_votes <= 0.1) {
    val <- 1
  } else if (lead_votes <= 0.2) {
    val <- 2
  } else if (lead_votes <= 0.3) {
    val <- 3
  } else {
    val <- 4  
  }
  
  if (lead_party == "democrat") {
    val <- val + 4
  }
  
  if (lead_party %in% c("democrat", "republican")) {
    return(val)
  }
  
  stop("error happened")
}



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