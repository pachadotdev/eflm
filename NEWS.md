# eflm 0.2

* Allows to use formulas such as `mpg ~ log(wt)` instead of having to create `mtcars$logwt <- log(mtcars$wt)`
* Implements some fixes from speedglm package to solve estimation bugs with categorical variables
* Adds the subset argument for a closer match with the stats package
* Makes all the arguments similar to the closest degree to lm() and glm()

# eflm 0.1

* This is the first version, which basically fits my need to obtain GLM (Quasi-Poisson) estimates
