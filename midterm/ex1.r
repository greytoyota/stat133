load("family.rda")

# After loading the dataset above, you will have the following
# objects:
#
# > ls()
# [1] "fage"    "fgender" "fheight" "fnames"  "fweight"

# (1 point) Create a numeric vector fbmi
# A person's body mass index (BMI) should be calculated as their
# weight divided by their squared heights multiplied by 703

fbmi = (fweight / (fheight ^ 2)) * 703

# (1 point) Create a logical vector foverWt
# A person should be considered overweight if their BMI is greater than 25

foverWt = fbmi > 25

# (1 point) Create a dataframe family
# The dataframe should contain fnames, fgender, fage, fheight, fweight, fbmi,
# and foverWt.  The names should be as follows:
#
# > names(family)
# [1] "name" "gender" "age" "height" "weight" "bmi" "overWt"

family = data.frame(fnames, fgender, fage, fheight, fweight, fbmi, foverWt)
colnames(family) <- c("name", "gender", "age", "height", "weight", "bmi", "overWt")

# (5 points) Plot age vs bmi

# For each individual in the family dataframe, plot their age (x-axis)
# against their bmi (y-axis).  Males should be represented with 'red'
# circles and females with 'blue' circles.  You may need to set pch to
# 'o'. The x-axis should range from 23 to 80 and your y-axis from 16 to
# 31. Finally put a legend in the 'topright' corner of the plot with a
# 'red' circle in front the label 'male' and a 'blue' circle in front the
# label 'female'. You may want to look at the documentation for R's 'plot'
# function

# your code here
status = factor(fgender)
cols = c("red", "blue")
plot(fage, fbmi, type="p", xlim=c(23, 80), ylim=c(16, 31), pch='o', col=cols[status],
     xlab="age", ylab="bmi")
legend(x="topright", legend=c("male", "female"), pch="o", col=cols)
