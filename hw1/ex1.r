# Complete the implementation of the function "readRemove". This function should
# take the following arguments:
#
# <file.name>: a string giving the name of the file from which the data should
# be read
# <max.na>: the maximum number of variables that may be NA for a given
# observation (default=0)
# <sep>: the field separator character (eg. comma, white space, tab)
# <header>: a logical indicating whether or not first line of the file contains
# variable names (default=',')
# <na.strings>: the characther string used to represent NA observations in the
# file (default=NA)
#
# You function should return the following:
# <file.data>: a data frame containing the observation in the file given by
# the variable <file.name>. Any observations that have more NA variables than
# <max.na> should be removed from this data frame.

# returns FALSE if there are more NA's than max.NA, otherwise TRUE
check.amountNA <- function(vec, max.NA) {
    numNA = sum(is.na(vec))
    if (numNA > max.NA) {
        return(FALSE)
    }
    return(TRUE)
}

readRemove <- function(file.name, max.na=0, sep=',', header=F) {
    file.data = read.table(file.name, header=header, sep=sep)
    notTooManyNA = apply(file.data, 1, check.amountNA, max.na)
    file.data = file.data[notTooManyNA, ]
    return(file.data)
}

load('ex1_tests.Rda')
library(RUnit)
errMsg <- function(err) print(paste("ERROR:", err))
tryCatch(checkEquals(sum(is.na(readRemove('test.data', max.na=1, sep=';'))), sum(is.na(test.1))),
         error=function(err) errMsg(err))
tryCatch(checkIdentical(readRemove('babies.csv', sep=',', header=T), test.2),
         error=function(err) errMsg(err))




