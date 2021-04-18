# eflm 0.3

* All functions were re-written, by adapting from stats:: and new S3 methods when required
* Uses R's internal C code for all matrix decompositions
* Fixes rounding problem with broom's exponentiation

# eflm 0.2.2

* allows `family = gaussian`, etc instead of `family = gaussian()`, just like `glm()`
* supports `sandwich::vcovBS()`
* supports `subset = (cyl == 4)` and similar subsetting

# eflm 0.2.1

* Adds generic method for weights (i.e. now I can replicate the clotting example from glm)
* Starts adding vignettes with all lm/glm tests from R source code

# eflm 0.2

* Allows to use formulas such as `mpg ~ log(wt)` instead of having to create `mtcars$logwt <- log(mtcars$wt)`
* Implements some fixes from speedglm package to solve estimation bugs with categorical variables
* Adds the subset argument for a closer match with the stats package
* Makes all the arguments similar to the closest degree to lm() and glm()

# eflm 0.1

* This is the first version, which basically fits my need to obtain GLM (Quasi-Poisson) estimates
