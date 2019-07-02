
pairs_matrix <- republican %>%
  ungroup() %>%
  dplyr::select(
    -fips, -state, -state_abbreviation, -county, -candidate, -party, -total_votes, -votes, -lead_votes, -lead_party,
    -map_color, -area_name, -state_facts
  )

pairs(pairs_matrix[,c(1:7)], col=pairs_matrix$party_won)
pairs(pairs_matrix[,c(8:12)], col=pairs_matrix$party_won)
pairs(pairs_matrix[,c(13:17)], col=pairs_matrix$party_won)
pairs(pairs_matrix[,c(20:24)], col=pairs_matrix$party_won)

x <- pairs_matrix[,-c(2)]
y <- as.numeric(pairs_matrix[,]$party_won)

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=log(x), y=y, plot="density", scales=scales)
featurePlot(x=x, y=y, plot="density", scales=scales)

featurePlot(x=log(x[c(1,c(3:7))]), y=y, plot="pairs", scales=scales)
featurePlot(x=log(x[c(8:12)]), y=y, plot="pairs", scales=scales)
featurePlot(x=log(x[c(13:18)]), y=y, plot="pairs", scales=scales)
featurePlot(x=log(x[c(19:21)]), y=y, plot="pairs", scales=scales)

