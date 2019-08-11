# Goodness of test fit
good.fit <- HLTest(obj=my_models[["glm"]]$finalModel, g=6)
cbind(good.fit$observed, round(good.fit$expect, digits = 1))
good.fit

good.fit <- HLTest(obj=my_models[["glm_ltr"]]$finalModel, g=6)
cbind(good.fit$observed, round(good.fit$expect, digits = 1))
good.fit



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