################################################## 
#
# Cochran's Q & McNemar's Test
hazel::eval.cochrans_q(
  as.numeric(test.data$response_factor), 
  as.data.frame(
    cbind(
      as.numeric(my_models[["glm"]]$evaluation$raw),
      as.numeric(my_models[["svm"]]$evaluation$raw),
      as.numeric(my_models[["rf"]]$evaluation$raw)
    )
  )
)

# Computing differences...
for (i in 1:(length(config$algorithms) - 1)) {
  for (j in (i+1):length(config$algorithms)) {
    comb <- c(names(config$algorithms)[i], names(config$algorithms)[j])
    index <- paste(comb, collapse = "_")
    
    my_results[["differences"]][[index]] = hazel::eval.mcnemar(
      as.numeric(test.data$response_factor),
      as.numeric(my_models[[comb[1]]]$evaluation$raw),
      as.numeric(my_models[[comb[2]]]$evaluation$raw),
      names = c(
        config$algorithms[[i]],
        config$algorithms[[j]]
      )
    )
    
    print(my_results[["differences"]][[index]])
  }
}

my_data_frame <- data.frame(rbind(
  my_results[["differences"]]$glm_svm$classifications,
  my_results[["differences"]]$glm_rf$classifications,
  my_results[["differences"]]$svm_rf$classifications
))

my_data_frame$names <- c(
  "LR (A) - SVM (B)", 
  "LR (A) - RF (B)", 
  "SVM (A) - RF (B)"
)

my_data_frame <- my_data_frame %>% 
  gather("group", "value", -names)

ggplot(
    my_data_frame, aes(names, value, fill = group)
  ) + 
  geom_bar(stat = "identity", position = "dodge2") + 
  scale_fill_manual(
    values = rev(config$theme$plots),
    name = "Predictions", 
    labels = c(
      "Both models predicted correctly", 
      "Only model A predicted correctly", 
      "Only model B predicted correctly", 
      "Both models predicted incorrectly"
    )
  ) +
  geom_text(
    aes(
      label = value
    ), 
    vjust = -0.5,
    position = position_dodge2(
      width = 0.9,
      padding = 2
    )
  ) +
  xlab("Algorithms Comparison") +
  ylab("# predictions") +
  scale_y_continuous(trans = log10_trans()) +
  theme_bw() + 
  theme(
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

#########################################
# Cleaning global environment
rm(comb)
rm(index)
rm(my_data_frame)