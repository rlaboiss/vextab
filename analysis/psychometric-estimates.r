
### * Read the data frame with the responses
obj.stab <- read.csv ("obj-stab-resp.csv")

### * Get rid of NA in the object column
obj <- as.character (obj.stab$object)
obj [which (is.na (obj))] <- "none"
obj.stab$object <- factor (obj)

### * Generate a column with the numeric representation of responses
obj.stab$response.n <- as.numeric (obj.stab$response) - 1

### * Compose the data frame with all possible conditions
obj.stab.psycho <- aggregate (response ~ subject + experiment + background
                                  + stimulus + object.side + table.side + chair
                                  + object,
                              obj.stab,
                              function (x) NA,
                              na.action = na.pass)

### * Psychometric parameter estimation

### ** Load the needed library
### *** For the glmrob function
library (robustbase)
### *** For the HPDinterval function
library (coda)
### *** For the mvrnorm function
library (MASS)

### ** Function for extracting the parameters and the associated CI
get.psycho <- function (df) {
    ## *** Fit the model
    fm <- glmrob (response.n ~ angle, family = binomial, data = df)
    ## *** Get the coeeficientes
    cf <- coefficients (fm)
    ## *** Profile the fitted coefficients
    cv <- summary (fm)$cov.scaled
    x <- mvrnorm (10000, mu = cf, Sigma = cv)
    ## *** Return coefficients and HDI
    c ( -cf [1] / cf [2],
       as.vector (HPDinterval (as.mcmc (-x [, 1] / x [, 2]), prob = 0.95)),
       cf [2],
       as.vector (HPDinterval (as.mcmc (x [, 2]), prob = 0.95)))
}

### * Collect the data

### ** Initalize the parameter and CI vectors
threshold <- threshold.ci.inf <- threshold.ci.sup <-
     slope <- slope.ci.inf <- slope.ci.sup <- rep (NA, nrow (obj.stab.psycho))

### ** Store values for each condition
nb.cond <- nrow (obj.stab.psycho)
for (i in seq (1, nb.cond)) {

    ## *** Get the condition for each factor
    sb <- as.character (obj.stab.psycho [i, "subject"])
    st <- as.character (obj.stab.psycho [i, "stimulus"])
    bg <- as.character (obj.stab.psycho [i, "background"])
    co <- as.character (obj.stab.psycho [i, "chair"])
    ot <- as.character (obj.stab.psycho [i, "object"])
    ex <- as.character (obj.stab.psycho [i, "experiment"])
    os <- as.character (obj.stab.psycho [i, "object.side"])
    ts <- as.character (obj.stab.psycho [i, "table.side"])

    ## *** Create the specific data frame
    df <- subset (obj.stab, subject ==  sb
                            & background == bg
                            & stimulus == st
                            & chair == co
                            & experiment == ex
                            & object.side == os
                            & table.side == ts
                            & object == ot)

    ## *** Skip the lacking data
    if (all (is.na (df$response)))
        next

    ## *** Progress meter
    cat (sprintf ("\r%5d/%d: %s, %12s, %6s, %10s, %7s, %4s",
                  i, nb.cond, sb, ex, bg, st, co, ot))
    flush (stdout ())

    ## *** Compute parameters and store them
    ps <- get.psycho (df)
    threshold [i] <- ps [1]
    threshold.ci.inf [i] <- ps [2]
    threshold.ci.sup [i] <- ps [3]
    slope [i] <- ps [4]
    slope.ci.inf [i] <- ps [5]
    slope.ci.sup [i] <- ps [6]

}

### ** Clean the progress meter
cat ("\n")
flush (stdout ())

### ** Compose the final data frame
obj.stab.psycho <- data.frame (obj.stab.psycho,
                               threshold = threshold,
                               threshold.ci.inf = threshold.ci.inf,
                               threshold.ci.sup = threshold.ci.sup,
                               slope = slope,
                               slope.ci.inf = slope.ci.inf,
                               slope.ci.sup = slope.ci.sup)

### ** Save it
write.csv (obj.stab.psycho, file = "obj-stab-psycho.csv",
           quote = FALSE, row.names = FALSE)
