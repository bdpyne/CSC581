creeper.df <- read.csv("minecraft.csv")
creeper <- learner(creeper.df)
creeper(objects[[1,]])

biomed.df <- read.csv("biomed.csv")
biomed    <- learner(biomed.df)
biomed(objects[[1,]])

mammals.df <- read.csv("mammals.csv")
mammals   <- learner(mammals.df)
mammals(objects[[1,]])
