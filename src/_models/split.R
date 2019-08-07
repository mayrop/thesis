###
#
# In this file we split data for performing our analysis
#
# @author Mayra Valdes @mayrop
# 
### 
set.seed(config$seed)

indices = createDataPartition(all$response_binary, p=0.6, list=FALSE)
train.data = all[indices, ]
test.data = all[-indices, ]

# Double checking proportions - Stratified Random Sample
proportions <- cbind(
  table(train.data$response_binary),  
  table(test.data$response_binary),
  prop.table(table(train.data$response_binary)),
  prop.table(table(test.data$response_binary))
)
colnames(proportions) <- c("Counts Train", "Counts Test", "Prop Train", "Prop Test")

print(proportions)

#########################################
# Cleaning global environment
rm(proportions)
rm(indices)