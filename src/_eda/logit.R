# Plot for logit function

library(latex2exp)

x = seq(-6, 6, length.out = 1000)
loadfonts(device = "win")


plot(
  x = x, 
  y = 1 / (1 + exp(-x)),
  type = "l",
  panel.first = grid(),
  col = "red",
  ylab = expression(sigma(z)),
  xlab = "z",
  main = "Sigmoid function"
)

text(x = 3.5, y = 0.1, labels = TeX('$\\sigma(z)  = \\frac{1}{1 + e^{-z}}$'), adj = NULL,
     pos = NULL, offset = 0)


# expression(paste(sigma(z), " = ", frac(1, 1 + exp(-z))))

abline(lwd=0.5, lty=2, v=0, col="black")

#########################################
# Cleaning global environment
rm(x)