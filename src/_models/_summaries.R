# Goodness of test fit
good.fit <- HLTest(obj=my_models[["glm"]]$finalModel, g=6)
cbind(good.fit$observed, round(good.fit$expect, digits = 1))
good.fit

good.fit <- HLTest(obj=my_models[["glm_ltr"]]$finalModel, g=6)
cbind(good.fit$observed, round(good.fit$expect, digits = 1))
good.fit

#http://www.kimberlycoffey.com/blog/2016/7/16/compare-multiple-caret-run-machine-learning-models

importance(my_models$rf0$finalModel)

plot(my_models$rf0, metric = "Sens")
#monotonic
#SAS Certification Prep Guide: Statistical Business Analysis Using SAS9
plot.data <- as.data.frame(plot(my_models$rf$finalModel))
colnames(plot.data) <- c("Error")
plot.data$trees <- as.numeric(rownames(plot.data))
#http://www.kimberlycoffey.com/blog/2016/3/19/random-forest
# https://www.r-bloggers.com/using-rpart-to-figure-out-who-voted-for-trump/

library(rpart.plot)
fancyRpartPlot(my_models$rf$finalModel)

rpart.plot(my_models$rf$finalModel, main = "Winner candidate in county",
           extra = 104, split.suffix = "%?", branch = 1,
           fallen.leaves = FALSE, box.palette = "BuRd",
           branch.lty = 3, split.cex = 1.2,
           shadow.col = "gray", shadow.offset = 0.2)

rf.plot <- ggplot(plot.data, aes(x=plot.data$trees, y=plot.data$Error)) + geom_line(colour="#000099")
rf.plot <- rf.plot + xlab("Number of Decision Trees")
rf.plot <- rf.plot + ylab("Mean Squared Error")
rf.plot <- rf.plot + ggtitle("Mean Squared Error by Number of Decision Trees")
rf.plot
remove(rf.plot, plot.data)

emplogit <- function(df, var="x", response="y", bins=100) {
  labels <- (bins-1):0

  df$rank <- rank(-pull(df[,var]))
  df$bin <- labels[as.numeric(cut(df$rank, breaks=bins, labels=0:(bins-1)))]
  
  return(
    df %>% 
      group_by(bin) %>% 
      summarise(
        freq = n(),
        successes = sum(!!rlang::sym(response)),
        var := mean(!!rlang::sym(var))
      ) %>% 
      mutate(
        elogit=log((successes+0.5)/(freq-successes+0.5))
      ) %>% 
      arrange(
        bin
      )
  )
}


x <- myempplot(all, "edu_bachelor_pct_13", "response_binary")

plot(x$edu_bachelor_pct_13, x$elogit, type="o")
plot(x$bin, x$elogit, type="o")

m <- hoeff_spearman_matrix(as.matrix(all[,colnames(all) %in% predictors]), all$response_binary)

x <- sum(all$response_binary)
n <- nrow(all) - sum(all$response_binary)
all$emplogit <- with(all, log((x + 0.5) / (n-x+0.5)))

test2$rank<-rank(test2$test)


#Hoeffding's (1948) D statistics

plot(m$hoeffding_d_rank, m$spearman_rank)
text(m$hoeffding_d_rank + 0.45, m$spearman_rank + 0.45, labels=paste(rownames(m), m$spearman_rank, m$hoeffding_d_rank), cex= 0.4)

set.seed(1)
x <- seq(-10,10,length=200)
y <- x*sign(runif(200,-1,1))
plot(x,y)
hoeffd(x,y)