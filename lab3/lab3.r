library(RUnit)
errMsg <- function(err) print(err)
babies.data <- read.csv('babies.csv')
load('lab3-tests.rda')

# Suppose you would like to compare the average birth weight for babies of
# smokers and babies of non-smokers. To reduce the variability of the
# groups being compared, you want to split the data based on one of the
# other variables (gestation, parity, age, height, or weight) before
# running your t-test. Implement a function that splits the data based on a
# given variable and cutoff value for that variable and then performs
# t-test in each group to compare the birthweights of smoker babies vs
# non-smoker babies. Your function should take the following arguments:
#
# <data>: any subset of the babies.data dataset
# <group.variable>: a string
#   containing the name of the variable that the data will be stratified by
#   (one of: 'gestation', 'parity', 'age', 'height', or 'weight')
# <group.cutoff>: a numeric value defining the boundary between the two
#   groups. One group should contain all observations with <group.variable>
#   less than or equal to <group.cutoff> while the other group should contain
#   all observations such that <group.value> is greater than <group.cutoff>.
#   You may assume that this value is specified by the user
#   in such a way that there are both smokers and non-smokers in each group.
# <test.alternative>: a character string that is one of c("two.sided",
#   "less", "greater") specifying the directionality of thet test
#
# Your function should return the following:
#
# <t.outputs>: a list containing two lists corresponding to the outputs of
#   each test (first list: the subset below the cutoff, second list: the
#   subset above the cutoff). Each of these lists should contain two
#   elements: the t-statistic as its first element and the p-value as its
#   second element.
# In addition, your function should return two plots (one for each group) in
#   the same window. Each plot should contain the densities for both non-smokers
#   (black) and smokers (red). Do not worry about any other parameters for
#   the plot.

stratifiedTest <- function(data, group.variable, group.cutoff, test.alternative) {

    stopifnot(group.variable %in% names(data)[2:6]) 

    var.data = data[, which(names(data) == group.variable)]
    lesser.group = data[var.data <= group.cutoff, ]
    greater.group = data[var.data > group.cutoff, ]
    lesser.smoker.bwt = lesser.group[lesser.group$smoke == 1, ]$bwt
    lesser.nonsmoker.bwt = lesser.group[lesser.group$smoke == 0, ]$bwt
    greater.smoker.bwt = greater.group[greater.group$smoke == 1, ]$bwt
    greater.nonsmoker.bwt = greater.group[greater.group$smoke == 0, ]$bwt
    
    lesser.smoker.density = density(lesser.smoker.bwt)
    lesser.nonsmoker.density = density(lesser.nonsmoker.bwt)
    greater.smoker.density = density(greater.smoker.bwt)
    greater.nonsmoker.density = density(greater.nonsmoker.bwt)
    par(mfrow=c(1, 2))
    plot(lesser.nonsmoker.density)
    lines(lesser.smoker.density, col="red")
    plot(greater.nonsmoker.density)
    lines(greater.smoker.density, col="red")
    
    t.test.lesser = t.test(lesser.smoker.bwt, lesser.nonsmoker.bwt,
        alternative=test.alternative)
    t.test.greater = t.test(greater.smoker.bwt, greater.nonsmoker.bwt,
        alternative=test.alternative)
    t.outputs = list()
    lesser.outputs = list()
    greater.outputs = list()
    lesser.outputs$t.stat = t.test.lesser$statistic
    lesser.outputs$p.val = t.test.lesser$p.value
    greater.outputs$t.stat = t.test.greater$statistic
    greater.outputs$p.val = t.test.greater$p.value
    t.outputs$lesser = lesser.outputs
    t.outputs$greater = greater.outputs
    return(t.outputs)
}

output.t1 <- stratifiedTest(babies.data, "height", 64, test.alternative="less")
tryCatch(checkEquals(stratified.test.t1, unname(unlist(output.t1))),
         error=function(err) errMsg(err))

output.t2 <- stratifiedTest(babies.data, "gestation", 280, test.alternative="less")
tryCatch(checkEquals(stratified.test.t2, unname(unlist(output.t2))),
         error=function(err) errMsg(err))
