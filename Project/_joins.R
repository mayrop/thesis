
all <- full_join(
  elections, facts_vars, by="fips"
)

# Missing values

# Counties with missing votes information
all[is.na(all$party) & all$state_facts != "AK",15:36]
# 2 counties

all[is.na(all$area_name) & all$state_abbreviation != "AK",]
# 1 county, Kansas City

# Removing incomplete rows
all <- all[complete.cases(all),]

republican <- all[all$party=="republican",]
democrat <- all[all$party=="democrat",]