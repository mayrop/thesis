# Creating confidence intervals plot

plot_model(
    my_models[["glm"]]$finalModel, 
    show.values = TRUE, 
    transform = NULL,
    title = "Log-Odds for the logistic regression model", 
    show.p = FALSE,
    value.offset = .3,
    value.size = 3, 
    line.size = 0.5, 
    dot.size = 1, 
    digits = 3
  ) + 
  ylim(-0.4, 0.5) + 
  theme_bw() +
  geom_hline(
    aes(yintercept = 0), 
    colour = "#BB0000", 
    linetype = "dashed",
    size = 0.3
  )