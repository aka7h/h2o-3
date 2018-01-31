setwd(normalizePath(dirname(R.utils::commandArgs(asValues=TRUE)$"f")))
source("../../../scripts/h2o-r-test-setup.R")

test.glm.interactions_valid <- function() {

    df <- h2o.importFile(locate("smalldata/airlines/allyears2k_headers.zip"))

    XY <- names(df)[c(1,2,3,4,6,8,9,13,17,18,19,31)]
    interactions <- XY[c(7,9)]

    # Expand interactions in both testing & validation frame
    df.expanded <- h2o.cbind(.getExpanded(df[, interactions], interactions = interactions, T, F, T), df[, XY])

    df.expanded.split <- h2o.splitFrame(df.expanded, ratios = 0.9, seed = 32)
    df.expanded.train <- df.expanded.split[[1]]
    df.expanded.test <- df.expanded.split[[2]]

    # Build model with a validation frame and given set of interactions
    m1 <- h2o.glm(x = XY[-length(XY)], y = XY[length(XY)],
    training_frame = df.expanded.train[, XY], #validation_frame = df.expanded.test[, XY],
    interactions = interactions,
    lambda_search = TRUE, family="binomial", solver = "AUTO",
    early_stopping = FALSE, seed = 1234)

    # Build the GLM model
    m2 <- h2o.glm(x = 1:(length(df.expanded.train)-1), y = length(df.expanded.train),
    training_frame = df.expanded.train,# validation_frame = df.expanded.test,
    lambda_search = TRUE, family="binomial", solver = "AUTO",
    early_stopping = FALSE, seed = 1234)

    expect_equal(m1@model$training_metrics@metrics$AUC, m2@model$training_metrics@metrics$AUC, tolerance = 0.001, scale = 1)
    expect_equal(m1@model$training_metrics@metrics$logloss, m2@model$training_metrics@metrics$logloss, tolerance = 0.001, scale = 1)
}

doTest("Test GLM interactions with a validation frame", test.glm.interactions_valid)