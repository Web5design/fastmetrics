# fastmetrics: Performance metrics ported from scikit-learn to R

This R package ports the following metrics from the [scikit-learn](http://scikit-learn.org) machine learning package for Python:

Function | Description
--- | ---
`trapz` | Computes the area under a curve using the [trapezoidal rule](http://en.wikipedia.org/wiki/Trapezoidal_rule)
`precision_recall_curve` | Computes precision, recall, and threshold values
`roc_curve` | Computes true positive rates, false positive rates, and threshold values
`average_precision_score` | Computes the area under the precision-recall curve
`roc_auc_score` | Computes the area under the Receiver Operating Characteristic (ROC) curve

I've attempted a faithful port including most unit tests and error checking.  Comments and variable names are maintained where possible, roughly mapping to [Hadley's style guide](https://github.com/hadley/devtools/wiki/Style).

To install:

    git clone https://github.com/shwhalen/fastmetrics
    R CMD INSTALL fastmetrics

To use:

	library(fastmetrics)
	set.seed(0)
	labels <- c(rep(0, 50), rep(1, 50))
	predictions <- runif(100)
	roc_auc_score(labels, predictions)
	[1] 0.4624
