# Creating plot for RF Importance Variables

rf_importance <- data.frame(matrix(ncol=2, nrow=length(predictors)))
rf_importance[, 1] <- importance(my_models[["rf"]]$finalModel)[, 4]
rf_importance[, 2] <- rownames(importance(my_models[["rf"]]$finalModel))

rf_importance <- rf_importance[order(-rf_importance$X1),]
rf_importance <- rf_importance[1:10, ]
rf_importance <- rf_importance[order(rf_importance$X1),]

ggplot(
    rf_importance, aes(x =reorder(X2, X1), y = X1)
  ) + 
  geom_bar(
    stat = "identity", 
    position = "dodge", 
    colour = config$theme$parties_colors[1], 
    fill = config$theme$parties_colors[1],
    width = 0.5
  ) +
  coord_flip() +
  theme_bw() + 
  theme(
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 14),
    # reduce space between bars
    aspect.ratio = .77
  ) +
  geom_text(
    aes(
      label = as.integer(X1)
    ), 
    hjust = -0.25,
    position = position_dodge(
      width = 0.9
    ),
    size = 3.5
  ) +
  ylim(0, 128) +
  ylab("Mean Decrease Gini") +
  xlab("") +
  ggtitle("Variable importance plot")

#########################################
# Cleaning global environment
rm(rf_importance)