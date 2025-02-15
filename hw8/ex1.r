# I've set up some variables that you will need to leave unchanged
# for this assignment.
 
female = list()
male = list()
all = list()

female$n = 100
male$n = 100
all$n = female$n + male$n

female$ave.height = 163.2 # cm
female$ave.weight = 74.7 # kg

male$ave.height = 177.6 # cm
male$ave.weight = 88.3 # kg 

all$height.sd = 3 # cm
all$weight.sd = 5 # kg

all$young = 20
all$old = 40

# Setting the seed
# Note: this should be left alone as well and you shouldn't set
# the seed again in this file

set.seed(42)

# Now you need to implement some helper functions to simulate
# measuring independent variables (i.e., height, weight, and age)
# from a population.

# (1 point) Implement the heights function
# 
# Takes the following arguments:
#   n: the number of samples
#   ave: the average height
#   sd: the standard deviation from that average
#
# Returns a length n numeric vector of heights randomly drawn
# from the normal distribution specified by the input average
# and standard deviation.

heights = function(n, ave, sd) {
    return(rnorm(n, ave, sd))
}

# (0 points) The weights function is already implemented
# by the heights function.
weights = heights

# (1 point) Implement the ages function
# 
# Takes the following arguments:
#   n: the number of samples
#   min: the minimum age
#   max: the maximum age
#
# Returns a length n numeric vector of ages randomly drawn
# from the uniform distribution between the oldest and youngest
# person.

ages = function(n, min, max) {
    return(runif(n, min, max))
}

# (1 point) Implement the convert.heights function
# 
# Takes the following arguments:
#   heights: a numeric vector of heights
#
# Returns the vector of heights converted from from cm
# to inches.  You should convert centimeters to inches
# by multiplying by 0.393701.

convert.heights = function(heights) {
    return(heights * .393701)
}

# (1 point) Implement the convert.weights function
# 
# Takes the following arguments:
#   weights: a numeric vector of weights
#
# Returns the vector of weights converted from from kilograms
# to pounds.  You should convert kilograms to pounds
# by multiplying by 2.20462.

convert.weights = function(weights)  {
    return(weights * 2.20462)
}

# (2 points) Implement the compute.bmi function
# 
# Takes the following arguments:
#   heights: a numeric vector of heights (in cm)
#   weights: a numeric vector of weights (in kg)
#
# If heights and weights have the same number of elements, the
# function returns a numeric vector of bmi values.
#
# Otherwise the functions returns NULL.
#
# A person's body mass index (BMI) should be calculated as their
# weight in kg divided by their squared heights in m

compute.bmi = function(heights, weights)  {
    heights.m = heights / 100
    return(weights / (heights.m ** 2))
}

# Now you are going to create some datastructures using these functions
# and the variables I defined for you.

# (1 point) Create a dataframe female$df.base
# The names should be as follows:
#
# > names(female$df.base)
# [1] "height" "weight" "age"
# 
# where
#
#   height is the result of calling heights
#   weight is the result of calling weights
#   age is the result of calling ages
height = heights(female$n, female$ave.height, all$height.sd)
weight = weights(female$n, female$ave.weight, all$weight.sd)
age = ages(female$n, all$young, all$old)

female$df.base = data.frame(
    height, weight, age
)

# (1 point) Create a dataframe male$df.base
# same as above but for males
height = heights(male$n, male$ave.height, all$height.sd)
weight = weights(male$n, male$ave.weight, all$weight.sd)
age = ages(male$n, all$young, all$old)

male$df.base = data.frame(
    height, weight, age
)

# (1 point) Create a dataframe female$df
# The names should be as follows:
#
# > names(female$df)
# [1] "heights"        "weights"        "age"            "heights_in_in" 
# [5] "weights_in_lbs" "bmi"
#
# where
#
#   height, weight, and age come from female$df.base
#   heights_in_in, weights_in_lbs, and bmi result from
#    calling the corresponding functions you made above
#    on the relevant parts of female$df.base
height = female$df.base$height
weight = female$df.base$weight
age = female$df.base$age
heights_in_in = convert.heights(female$df.base$height)
weights_in_lbs = convert.weights(female$df.base$weight)
bmi = compute.bmi(height, weight)

female$df = data.frame(
    height, weight, age, heights_in_in, weights_in_lbs, bmi
)

# (1 point) Create a dataframe male$df
# same as above but for males
height = male$df.base$height
weight = male$df.base$weight
age = male$df.base$age
heights_in_in = convert.heights(male$df.base$height)
weights_in_lbs = convert.weights(male$df.base$weight)
bmi = compute.bmi(height, weight)

male$df = data.frame(
    height, weight, age, heights_in_in, weights_in_lbs, bmi
)

# (1 point) Create a dataframe all$df
# The first rows of this dataframe consist of female$df
# and the last rows consist of male$df.  In addition, your
# dataframe should have an additional column 'gender',
# which is a factor of 'f's and 'm's.
#
# > names(all$df)
# [1] "heights"        "weights"        "age"            "heights_in_in" 
# [5] "weights_in_lbs" "bmi"            "gender"
gender = rep("f", nrow(female$df))
females = cbind(female$df, gender)
gender = rep("m", nrow(male$df))
males = cbind(male$df, gender)

all$df = rbind(females, males)
all$df$gender = as.factor(all$df$gender)

# (2 points) Plot a scatterplot matrix from the first three columns of all$df
# Make sure that females are colored red and males are colored blue
#
# You many want to read more here:
#  http://www.stat.berkeley.edu/classes/s133/R-4a.html
cols=c("red", "blue")
plot(all$df[, 1:3], col=cols[all$df$gender])

# (2 points) Plot a scatterplot matrix from the first six columns of all$df
# Again make sure that females are colored red and males are colored blue

cols=c("red", "blue")
plot(all$df[, 1:6], col=cols[all$df$gender])

# (1 point) Create a prcomp object from all$df called pca
# You will need to remove non-numeric columns first

numeric.df = all$df[, sapply(all$df, is.numeric)]
pca = prcomp(numeric.df)


# (4 points) Plot data projected on its principal components
#
# In one figure create the following four plots:
#  all$df projected on to it 1st and 2nd components
#  all$df projected on to it 2nd and 3rd components
#  all$df projected on to it 1st and 3rd components
#  all$df projected on to it 3rd and 4th components
#
# Again make sure that females are colored red and males are colored blue

cols = c("red", "blue")
par(mfrow=c(2, 2))
plot(x=pca$x[, 1], y=pca$x[, 2], col=cols[all$df$gender],
     xlab="PC1", ylab="PC2")
plot(x=pca$x[, 2], y=pca$x[, 3], col=cols[all$df$gender],
     xlab="PC2", ylab="PC3")
plot(x=pca$x[, 1], y=pca$x[, 3], col=cols[all$df$gender],
     xlab="PC1", ylab="PC3")
plot(x=pca$x[, 3], y=pca$x[, 4], col=cols[all$df$gender],
     xlab="PC3", ylab="PC4")


# (1 point) Create a kmeans object using all$df
# set k to 2 and make sure that you use 10 different initial conditions

full.km = kmeans(numeric.df, centers=2, nstart=10)

# (1 point) Create a hclust object using all$df

distances = dist(numeric.df)
full.hclust = hclust(distances)

# (1 point) Find the labels using cutree with k=2

full.hclust.labels = cutree(full.hclust, k=2)
 
# (1 point) Create a kmeans object using just the first two columns of all$df
# set k to 2 and make sure that you use 10 different initial conditions

red.km = kmeans(numeric.df[, 1:2], centers=2, nstart=10)

# (1 point) Create a hclust object using just the first two columns of all$df

distances = dist(numeric.df[, 1:2])
red.hclust = hclust(distances)

# (1 point) Find the labels using cutree with k=2

red.hclust.labels = cutree(red.hclust, k=2)


# (4 point) Plot the data projected on its first two dimensions
# (i.e., height and weight).  
#
# In one figure create four such plots where the points are
# colored according to:
#  1. full.km$cluster and title this plot "kmeans (full)"
#  2. full.hclust.labels and title this plot "hclust (full)"
#  3. red.km$cluster and title this plot "kmeans (reduced)"
#  4. red.hclust.labels and title this plot "hclust (reduced)"
#
# Use red and blue again to color the two groups, but note that
# you shouldn't try to make sure the colors are necessarily coded
# to female and male.

cols=c("red", "blue")
par(mfrow=c(2, 2))
plot(all$df[, 1:2], col=cols[full.km$cluster], main="kmeans (full)")
plot(all$df[, 1:2], col=cols[full.hclust.labels], main="hclust (full)")
plot(all$df[, 1:2], col=cols[red.km$cluster], main="kmeans (reduced)")
plot(all$df[, 1:2], col=cols[red.hclust.labels], main="hclust (reduced)")
