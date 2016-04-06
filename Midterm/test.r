df <- read.csv("riders.csv")

for (i in 1:5) {
	bs <- sample(x=df, size=5, replace=TRUE)
	print(bs)
}
