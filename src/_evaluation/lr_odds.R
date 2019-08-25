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
    digits = 3,
    terms = c(
      "age_o65_pct_14", 
      "edu_bach_pct_13", 
      "hsg_multiunit_pct_13", 
      "hsg_homeowner_rate_13", 
      "rh_white_nohisp_pct_14", 
      "rh_latino_pct_14",
      "rh_asian_pct_14",
      "rh_afroamerican_pct_14",
      "inc_pc_12_month_13",
      "veterans_pct_13",
      "females_pct_14"
    )
  ) + 
  ylim(0.75, 1.3) +  
  theme_bw() +
  geom_hline(
    aes(yintercept = 1), 
    colour = "#BB0000", 
    linetype = "dashed",
    size = 0.3
  )
