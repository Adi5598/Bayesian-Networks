library(bnlearn)
library(gtools)

######################### READING THE DATA ###############################

# Reference for reading table : https://www.datacamp.com/community/tutorials/r-data-import-tutorial
data <- read.csv('C:/Users/Aditya Aggarwal/Desktop/IIITD/sem 8/SC/Endsem/cleveland.csv')
# Reference for changing header names : https://stackoverflow.com/questions/46344879/r-read-csv-with-column-names-in-variables
variables <- c('age','sex','cp','trestbps','chol','fbs','restecg','thalach','exang','oldpeak','slope','ca','thal','target')
names(data) <- variables

######################### DEALING WITH CONTINUOUS VARIABLES ###############################

# Reference : https://rdrr.io/cran/gtools/man/quantcut.html
continuous_variables <- c('age','trestbps','chol','thalach','oldpeak')
for(var in continuous_variables){
  if(!is.factor(data$var)){
    data[,var] = as.numeric(unlist(data[,var]))
    data[,var] <- quantcut(data[,var],2) 
  }
}

# Since variables are considered integers we need to convert them into factors
for(var in variables){
  if(!is.factor(data$var)){
    data[,var] = as.factor(unlist(data[,var]))
  }
}

# Chosing a subset of variables : 'age',sex','fbs','restecg','exang','oldpeak','slope','ca','thal','target'
final_variables <- c(1,2,6,7,9,10,11,12,13,14)
data = data[,final_variables]

# Make the target variable into binary
#data[data[,'target']!=0,'target'] = 1 

#Splitting the data into training and testing.
training <- data[1:250,]
testing <- data[250:303,]

######################### BUILDING THE MODEL ###############################

# Reference : https://www.youtube.com/watch?v=6pl3m-UbUV4
alogorithms <- c('hc')
scores <- c('bic','aic')
for(algo in alogorithms){
  for(score in scores){
    print(algo)
    print(score)
    models <- boot.strength(data = training,R=1000,algorithm = algo,algorithm.args = list(score=score))
    best_models <- averaged.network(models,threshold = 0.2)
    fitted_model <- bn.fit(best_models,data = training,method = 'bayes')
    Learned_Bayesian_network <- graphviz.plot(fitted_model,highlight = list('fill'),layout = 'fdp',render = TRUE)
  }
}

######################## FINDING THE PROBABILITY ###############################
# We now want to test the structure on a tree network
# Reference : https://www.bnlearn.com/documentation/man/naive.bayes.html
Bayesian_tree = tree.bayes(training, "target")
tree_model = bn.fit(Bayesian_tree,training,method = 'bayes')
prediction = predict(tree_model, training)
table(prediction, training[, "target"])
fitted_model
