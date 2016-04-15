# load our data set
data(iris)

# make sure the ANN library is available
library(neuralnet)

# convert the labels into numeric labels and put them into a data frame
Species.numeric <- as.numeric(iris$Species)
iris.df <- data.frame(iris,Species.numeric)

# train a neural network with two hidden nodes
net <- neuralnet(Species.numeric ~ Sepal.Width+Sepal.Length+Petal.Width+Petal.Length,iris.df,hidden=4)

# display the ANN
#plot(net)

# report the ANN
net

# the training predictions from the ANN are numeric values, turn them into labels by rounding
result <- round(net$net.result[[1]])

# plot the confusion matrix
cm <- table(iris.df$Species.numeric,result)

print(cm)

# Initialize so the loops don't go haywire
tot.rec <- 0
tot.err <- 0

# Get total records and error.
for (i in 1:3) {
    for (j in 1:3) {
        if (i == j) {
            tot.rec <- tot.rec + cm[i,j]
        } else {
            tot.err <- tot.err + cm[i,j]
        }
    }
}

class.err <- tot.err/tot.rec
print(sprintf("Classification Error: %f", class.err))
