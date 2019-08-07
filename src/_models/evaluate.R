stats <- rbindlist(my_metrics)
resamps <- resamples(my_resamples)

# The ideas and methods here are based on Hothorn et al. (2005) and Eugster et al. (2008).

bwplot(resamps, layout=c(3, 1), main="Metric comparison between different methods (CV train set)")
dotplot(resamps, metric = "Spec", main="Confidence Intervals for the different methods (Specificity) (CV train set)")
