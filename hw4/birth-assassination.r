library(RUnit)
errMsg <- function(err) print(err)
load('ba-tests.rda')

# The pdf in this folder contains detailed information of the process we
# are asking you to simulate. For this assignment, we have already
# implemented parts of functions for you. Pay close attention to where we
# ask for additional code and make sure your code returns the appropriate
# classes and variable names.  Good luck!

# Add the necessary code to implement the function "generateKids". Your
# function should take the following arguments:
#
# <lambda>: the rate parameter (per unit of time) for the Poisson process
#   describing child birthrate
# <kappa>: the parameter for the exponential distribution describing
#   assassination rate
# <parent>: a single row from a dataframe with variables <parent.id>,
#   <child.id>, <birth.date>, and <assassination.date>
#
# Your function should return:
#
# <kids>: a dataframe with the same variables as <parent>. The number of
#   children (i.e. rows of <kids>) should be generated from a  Poisson
#   process with rate proportional to the length of <parent>'s life (NOTE:
#   because children are born according to a Poisson process, their
#   <birth.date> also follows a specific distribution). Please generate the
#   variables for each row of <kids> as follows:
#   
#   1) <parent.id> for each child should be equal to the <child.id>
#   variable of <parent>
#   2) <child.id> should range from run from 1-nrow(<kids>) (we will change
#   this later in our simulation)
#   3) <birth.date> should follow the distibution of event times over a
#   fixed interval in a Poisson processR
#   4) <assassination.date> should occur X time units after <parent>'s
#   assassination date where X comes from an exponential distribution with
#   rate <kappa>


generateKids <- function(lambda, kappa, parent) {

    # Generate the random variable <n.kids> giving the number of children
    # born to <parent>. This value should come from a Poisson distribution
    # whose rate is proportional to the length of <parent>'s life

    # your code here
    parent.life = parent$assassination.date - parent$birth.date
    n.kids <- rpois(1, parent.life * lambda)
    
    if (n.kids) {

        # If any n.kids is non-zero, generate the dataframe <kids> containing
        # the necessary variables for each child. To do this, you will need to
        # randomly generate the following variables:
        # <kid.birthdays>: a random numeric vector giving the date of birth
        # for each child
        # <life.lengths>: a random numeric vector indicating the length of
        # each child's life
        kid.birthdays <- runif(n.kids, min=parent$birth.date,
                               max=parent$assassination.date)
        life.lengths <- rexp(n.kids, kappa)
        kids = sapply(seq(1:n.kids), function(x) {
            return(c(parent$child.id, x, kid.birthdays[x],
                     parent$assassination.date + life.lengths[x]))
        })
        kids = data.frame(t(kids))
        colnames(kids) = c("parent.id", "child.id", "birth.date", "assassination.date")
        return(kids)
    } else return(NULL) #return null if <parent> has no kids
}

set.seed(47)
tryCatch(checkEquals(generate.kids.t, generateKids(0.5, 1, test.df),
                     tolerance=1e-5), error=function(err) errMsg(err))


# Your above function generates children for one parent, storing them as a
# dataframe. We would like to combine the children born to all parents of
# one generation into a single dataframe that represents the next
# generation. Please implement the function "nextGeneration". Your function
# should take the following arguments:
#
# <lambda>: the rate parameter (per unit of time) for the Poisson process
#   describing birthrate
# <kappa>: the arameter for the exponential distribution describing
#   assassination rate  
# <parents>: a dataframe for which each row can be supplied as an argument
#   to the function "generateKids"
#
# Your function should return the following:
#
# <next.gen>: a single dataframe containing the children for each row of
#   <parents>. The <child.id> variable for this dataframe should be reset
#   to range from 1:nrow(next.gen) (i.e. no two children in a given
#   generation will have the same id)

nextGeneration <- function(lambda, kappa, parents) {

    # please create a list whose length is equal to the number of rows of
    # <parents>. Each element of this list should be a dataframe of the
    # kids born to the corresponding parent. Call this list <next.gen>

    next.gen <- lapply(seq(1:nrow(parents)), function(i) {
        return(generateKids(lambda, kappa, parents[i, ]))
    })


    # The following code removes any list element that has no entries
    # (i.e. those for which the parent had no children), and combines the
    # children into one dataframe. You do not need to add anything.
    no.births <- sapply(next.gen, is.null)
    next.gen <- next.gen[!no.births]
    next.gen <- do.call(rbind, lapply(next.gen, '['))

    # If <next.gen> is not null, change the <child.id> variable so that it
    # ranges from 1 to nrow(next.gen)

    if (!is.null(next.gen)) {
        next.gen$child.id = seq(1:nrow(next.gen))
    }
    
    return(next.gen)
}

set.seed(47)
tryCatch(checkEquals(next.generation.t, nextGeneration(0.5, 1,
                                                       generate.kids.t),
                     tolerance=1e-5), error=function(err) errMsg(err))


bAGen <- function(lambda, kappa, max.gen) {

    # The following creates an empty list whose elements represent each
    # generation 
    generation.list <- vector('list', length=(max.gen))

    # Please initialize the first generation. To do this, set the first
    # element of <generation.list> to be a dataframe with one row and the
    # following variables:
    # <parent.id> = 0
    # <child.id> = 0
    # <birth.date> = 0
    # <assassination.date> a random variable from an expoenetial
    # distribution with rate <kappa>

    first.gen = data.frame(0, 0, 0, rexp(1, kappa))
    colnames(first.gen) = c("parent.id", "child.id", "birth.date",
                "assassination.date")
    generation.list[[1]] = first.gen

    # Simulate the birth assassination process using your "nextGeneration"
    # function for up to <max.gen> generations. If the family dies out
    # (i.e. the object generated by "nextGeneration" is NULL), stop
    # simulating.

    for (gen in 1:(length(generation.list))) {
        curr.gen = generation.list[[gen]]
        next.gen = nextGeneration(lambda, kappa, curr.gen)
        if (is.null(next.gen)) {
            break
        }
        generation.list[[gen + 1]] = next.gen
    }

    # This removes extra list elements if the family died out. You do not
    # need to add anything here
    died.out <- sapply(generation.list, is.null)    
    return(generation.list[!died.out])
}

# Please run three simulations of bAGen (1000 iterations each). Set
# <lambda> = 0.1, <max.gen> = 10 for each and use <kappa> = 0.75, 0.5, and
# 0.2 for <sim.1>, <sim.2>, and <sim.3> respectively

set.seed(47)
sim.1 <- replicate(1000, bAGen(.1, .75, 10))
sim.2 <- replicate(1000, bAGen(.1, .5, 10))
sim.3 <- replicate(1000, bAGen(.1, .2, 10))


# Implement the function gensSurvived. Your function should take the
# following argument:
#
# <bagen.simulation>: a list whose elements correspond to one run of bAGen
#   (i.e. the outputs <sim.1>, <sim.2>, <sim.3>)
#
# Your function should return:
#
# <generations>: a numeric vector with the same length as
#   <bagen.simulation>. Each entry should give the number of generations
#   that survived in the corresponding simulation (so that the maximum
#   value will be <max.gen>)

gensSurvived <- function(bagen.simulation) {
    generations = sapply(bagen.simulation, length)
    return(generations)
}

# Use your gensSurvived function to create the following plot:
# In the same plot (not just the same window), plot densities for the
# number of surviving generations. The densities should be colored red (1),
# green (2), and blue (3) and a legend should be placed in the top
# right. The bandwidth for each density should be set to 
# 0.35. The x-axis should be labeled "generations survived" and the main
# title should be "B-A Simulation". The x-axis should range from 0 to 11.

num.1 = density(gensSurvived(sim.1), bw=.35)
num.2 = density(gensSurvived(sim.2), bw=.35)
num.3 = density(gensSurvived(sim.3), bw=.35)
plot(num.1, col="red", xlab="generations survived",
     main="B-A Simulation", xlim=c(0, 11))
lines(num.2, col="green")
lines(num.3, col="blue")
legend(x="topright", legend=c("sim.1", "sim.2", "sim.3"),
       lwd=1, col=c("red", "green", "blue"))
