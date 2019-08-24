# Creating confidence intervals plot

plot_model(
    my_models[["glm"]]$finalModel, 
    show.values = TRUE, 
    transform = "exp",
    title = "Odds for the logistic regression model", 
    show.p = FALSE,
    value.offset = .3,
    value.size = 3, 
    line.size = 0.5, 
    dot.size = 1, 
    digits = 3
    
  ) + 
  ylim(0.8, 1.3) + 
  theme_bw() +
  geom_hline(
    aes(yintercept = 1), 
    colour = "#BB0000", 
    linetype = "dashed",
    size = 0.3
  )
