# TODO - Fix and improve

cols <- colnames(all)
cols_facts <- colnames(facts)

for (col in predictors) {
  if (length(which(cols==col))==0) {
    next
  }
  if (length(which(cols_facts==col))==0) {
    next
  }  
  
  plot(density(facts[,which(cols_facts==col)]), main=col)
}


plot_party_won_boxplot("housing_units_in_multiunit_13") 
plot_party_won_boxplot("race_white_no_hisp_pct_14") 
plot_party_won_boxplot("edu_bachelor_pct_13") 
plot_party_won_boxplot("race_asian_pct_14")
plot_party_won_boxplot("pop_foreign_pct_13")
plot_party_won_boxplot("log(pop_14)")
plot_party_won_boxplot("inc_med_househ_income_13")
plot_party_won_boxplot("housing_median_val_housing_units_13")
plot_party_won_boxplot("pop_other_lang_pct_13")
plot_party_won_boxplot("pop_foreign_pct_13")
plot_party_won_boxplot("log(pop_density_10)")

ggplot(all, aes_string(
  x = "party_won", y = "hsg_units_in_multiunit_13")) +
  geom_boxplot()

ggplot(all, aes_string(
  x = "party_won", y = "edu_bachelor_pct_13")) +
  geom_boxplot()

ggplot(all, aes_string(
  x = "party_won", y = "rh_white_no_hisp_pct_14")) +
  geom_boxplot()

fancyRpartPlot
rpart.plot(winner_rpart_2, main = "Winner candidate in county",
           extra = 104, split.suffix = "%?", branch = 1,
           fallen.leaves = FALSE, box.palette = "BuRd",
           branch.lty = 3, split.cex = 1.2,
           shadow.col = "gray", shadow.offset = 0.2)