#Overview#
This package enables the removal of training data from fitted R models while retaining predict functionality. The purged models are more portable as their memory footprints do not scale with the training sample size.

#Example Usage#
```r
x <- rnorm(1000)
y <- x + rnorm(1000)
unpurged.model <- lm(y ~ x)
purged.model <- purge(unpurged.model)
object.size(unpurged.model)
object.size(purged.model)
```

#Installation#
``` r
if (!require(devtools)) install.packages("devtools")
devtools:::install_github("massmutual/purge")
```
