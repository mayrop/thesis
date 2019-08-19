# Goodness of test fit


x <- emplogit(all, "edu_bach_pct_13", "response_binary")

plot(x$var, x$elogit, type="o")
plot(x$bin, x$elogit, type="o")


