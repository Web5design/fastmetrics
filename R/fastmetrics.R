# fastmetrics: Performance metrics ported from scikit-learn
# Copyright (C) 2013 Sean Whalen

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see [http://www.gnu.org/licenses/].

trapz <- function(x, y, reorder = F) {
    if (reorder) {
        sorted_indices <- order(x, y)
        x <- x[sorted_indices]
        y <- y[sorted_indices]
    }
    widths      <- diff(x)
    heights     <- head(y, -1) + tail(y, -1)
    direction   <- ifelse(all(widths <= 0), -1, 1)
    direction * as.numeric(widths %*% heights) / 2
}

binary_clf_curve <- function(y_true, y_score, pos_label = NULL) {
    # ensure binary classification if pos_label is not specified
    stopifnot(length(unique(y_true)) == 2)
    if (is.null(pos_label)) {
        pos_label <- 1
    }

    # make y_true a boolean vector
    y_true              <- y_true == pos_label

    # sort scores and corresponding truth values
    desc_score_indices  <- rev(order(y_score))
    y_score             <- y_score[desc_score_indices]
    y_true              <- y_true[desc_score_indices]

    # y_score typically has many tied values. Here we extract
    # the indices associated with the distinct values. We also
    # concatenate a value for the end of the curve.
    distinct_value_indices  <- which(diff(y_score) != 0)
    threshold_idxs          <- c(distinct_value_indices, length(y_true))

    # accumulate the true positives with decreasing threshold
    tps <- cumsum(y_true)[threshold_idxs]
    fps <- threshold_idxs - tps
    list(fps, tps, y_score[threshold_idxs])
}

roc_curve <- function(y_true, y_score, pos_label = NULL) {
    points      <- binary_clf_curve(y_true, y_score, pos_label)
    fps         <- points[[1]]
    tps         <- points[[2]]
    thresholds  <- points[[3]]

    # add an extra threshold position if necessary
    if (length(tps) == 0 || fps[1] != 0) {
        tps <- c(0, tps)
        fps <- c(0, fps)
        thresholds <- c(head(thresholds, 1) + 1, thresholds)
    }

    fpr <- fps / tail(fps, 1)
    tpr <- tps / tail(tps, 1)
    list(fpr, tpr, thresholds)
}

precision_recall_curve <- function(y_true, y_score, pos_label = NULL) {
    points      <- binary_clf_curve(y_true, y_score, pos_label)
    fps         <- points[[1]]
    tps         <- points[[2]]
    thresholds  <- points[[3]]

    precision   <- tps / (tps + fps)
    recall      <- tps / tail(tps, 1)

    # stop when full recall attained
    # and reverse the outputs so recall is decreasing
    last_ind    <- which(tps == tail(tps, 1))[1]
    sl          <- last_ind:1

    precision   <- c(precision[sl], 1)
    recall      <- c(recall[sl], 0)
    thresholds  <- thresholds[sl]
    list(precision, recall, thresholds)
}

average_precision_score <- function(y_true, y_score, pos_label = NULL) {
    points <- precision_recall_curve(y_true, y_score, pos_label)
    trapz(points[[2]], points[[1]])
}

roc_auc_score <- function(y_true, y_score, pos_label = NULL) {
    points <- roc_curve(y_true, y_score, pos_label)
    trapz(points[[1]], points[[2]], reorder = T)
}

test_trapz <- function() {
    stopifnot(
        trapz(c(0, 1), c(0, 1)) == 0.5
    )
    stopifnot(
        trapz(c(1, 0), c(0, 1)) == 0.5
    )
    stopifnot(
        trapz(c(1, 0, 0), c(0, 1, 1)) == 0.5
    )
    stopifnot(
        trapz(c(0, 1), c(1, 1)) == 1.0
    )
    stopifnot(
        trapz(c(0, 0.5, 1), c(0, 0.5, 1)) == 0.5
    )
    stopifnot( # test duplicate values
        trapz(c(-2.0, 0.0, 0.0, 0.0, 1.0), c(2.0, 0.0, 0.5, 1.0, 1.0), reorder = T) == 3
    )
    stopifnot( # test duplicate values
        trapz(c(-2.0, 0.0, 0.0, 0.0, 1.0), c(2.0, 1.0, 0.0, 0.5, 1.0), reorder = T) == 3
    )
    stopifnot( # test duplicate values
        trapz(c(-2.0, 0.0, 0.0, 0.0, 1.0), c(2.0, 1.0, 0.5, 0.0, 1.0), reorder = T) == 3
    )
}

test_precision_recall_curve <- function() {
    expected_precision  <- c(1/2., 1/3., 1/2., 1., 1.)
    expected_recall     <- c(1., 1/2., 1/2., 1/2., 0.)
    expected_thresholds <- c(1, 2, 3, 4)
    points              <- precision_recall_curve(c(1, 0, 0, 1), c(1, 2, 3, 4))
    precision           <- points[[1]]
    recall              <- points[[2]]
    thresholds          <- points[[3]]
    stopifnot(
        sum(precision - expected_precision) < 1e-7
    )
    stopifnot(
        sum(recall - expected_recall) < 1e-7
    )
    stopifnot(
        sum(thresholds - expected_thresholds) < 1e-7
    )
}

test_roc_curve_end_points <- function() {
    set.seed(0)
    y_true      <- c(rep(0, 50), rep(1, 50))
    y_score     <- sample(0:2, 100, replace = T)
    points      <- roc_curve(y_true, y_score)
    fpr         <- points[[1]]
    tpr         <- points[[2]]
    thresholds  <- points[[3]]
    stopifnot(
        head(fpr, 1) == 0
    )
    stopifnot(
        tail(fpr, 1) == 1
    )
    stopifnot(
        length(fpr) == length(tpr)
    )
    stopifnot(
        length(fpr) == length(thresholds)
    )
}

test_average_precision_score <- function() {
    stopifnot( # test best
        average_precision_score(c(0, 1), c(0.0, 1.0)) == 1.0
    )
    stopifnot( # test worst
        average_precision_score(c(1, 0), c(0.0, 1.0)) == 0.25
    )
    stopifnot( # test alternate labels
        average_precision_score(c('z', 'z', 'a', 'a'), c(0.1, 0.4, 0.35, 0.8), pos_label = 'a') - 0.7916667 < 1e-7
    )
    stopifnot( # test random
        average_precision_score(c(1, 1, 1, 1, 0), c(0.025,  0.469,  0.418,  0.288,  0.032)) - 0.94374 < 1e-5
    )
    stopifnot( # test duplicate values
        average_precision_score(c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1), c(0, .1, .1, .4, .5, .6, .6, .9, .9, 1, 1)) == 1
    )
    stopifnot( # test ties
        average_precision_score(c(0, 1, 1), c(.5, .5, .6)) != 1
    )
}

test_roc_auc_score <- function() {
    stopifnot( # test best
        roc_auc_score(c(0, 1), c(0.0, 1.0)) == 1.0
    )
    stopifnot( # test worst
        roc_auc_score(c(1, 0), c(0.0, 1.0)) == 0.0
    )
    stopifnot( # test alternate labels
        roc_auc_score(c('z', 'z', 'a', 'a'), c(0.1, 0.4, 0.35, 0.8), pos_label = 'a') == 0.75
    )
    stopifnot( # test random
        roc_auc_score(c(0, 1, 0, 1, 1), c(0.025,  0.469,  0.418,  0.288,  0.032)) - 2/3. < 1e-7
    )
}
