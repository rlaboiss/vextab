
### * Read the data frame with the responses
obj.stab <- read.csv ("obj-stab-resp.csv")
obj.stab$response.n <- as.numeric (obj.stab$response) - 1

### * Compose the data frame for storing the estimated parmaters

### ** First, compose the extensive comnbination of conditions
obj.stab.psycho <- expand.grid (subject = levels (obj.stab$subject),
                                background = c ("static", "vection"),
                                stimulus = c ("object", "horizontal",
                                              "egocentric"),
                                chair = c ("upright", "right", "left"),
                                object = c ("low", "mid", "high"))

### ** Second, eliminate the non-existent combinations
obj.stab.psycho <- subset (obj.stab.psycho,
                           ! ((background == "vection" & chair != "upright")
                               | ((stimulus == "horizontal"
                                   | stimulus == "egocentric")
                                   & (object == "mid" | object == "high"))))

### ** Third, set to NA the conditions with no objects
idx <- which (obj.stab.psycho$stimulus != "object"
              & obj.stab.psycho$object == "low")
obj.stab.psycho$object [idx] <- NA

### * Psychometric parameter estimation

### ** Load the needed library (for the bootstrap function)
library (car)

### ** Function for extracting the parameters and the associated CI
get.psycho <- function (df) {
    ## *** Fit the model
    fm <- glm (response.n ~ angle, family = binomial, data = df)
    ## *** Get the coeeficientes
    cf <- coefficients (fm)
    ## *** Do the bootstrap on the slope + threshold (NOT on glm coeffs)
    bs <- Boot (fm, function (m) {
        cf <- coef (m)
        c (-cf [1] / cf [2], cf [2])
    })
    ## *** Get confidence intervals
    ci <- confint (bs)
    ## *** Returning vector with results
    c ( - cf [1] / cf [2], ci [1, ], cf [2], ci [2, ])
}

### * Collect the data

### ** Initalize the parameter and CI vectors
(threshold <- threshold.ci.inf <- threshold.ci.sup <-
     slope <- slope.ci.inf <- slope.ci.sup <- rep (NA, nrow (obj.stab.psycho)))

### ** Store values for each condition
for (i in seq (1, nrow (obj.stab.psycho))) {
    ## *** Get the condition for each factor
    sb <- obj.stab.psycho [i, "subject"]
    st <- obj.stab.psycho [i, "stimulus"]
    bg <- obj.stab.psycho [i, "background"]
    co <- obj.stab.psycho [i, "chair"]
    ot <- obj.stab.psycho [i, "object"]

    ## *** Create the specific data frame
    df <- subset (obj.stab, subject ==  sb
                            & background == bg
                            & stimulus == st
                            & chair == co)
    if (st == "object")
        df <- subset (df, object == ot)

    ## *** Skip the lacking data
    if (all (is.na (df$response)))
        next

    ## *** Compute parameters and store them
    ps <- get.psycho (df)
    threshold [i] <- ps [1]
    threshold.ci.inf [i] <- ps [2]
    threshold.ci.sup [i] <- ps [3]
    slope [i] <- ps [4]
    slope.ci.inf [i] <- ps [5]
    slope.ci.sup [i] <- ps [6]

    ## *** Progress meter
    cat (sprintf ("\r%s, %6s, %10s, %7s, %4s", sb, bg, st, co, ot))
    flush (stdout ())
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

### * Boxplot the results

### ** All conditions, collapsed objects

### *** Threshold
pdf (file = "obj-stab-threshold-all.pdf")
par (mar = c (11, 4, 1, 1))
boxplot (threshold ~ chair * stimulus * background, data = obj.stab.psycho,
         las = 2, ylab = "threshold (degree)")
dev.off ()

### *** Slope
pdf (file = "obj-stab-slope-all.pdf")
par (mar = c (11, 4, 1, 1))
### **** Inclination at threshold is 25*a
boxplot (abs (25 * slope) ~ chair * stimulus * background, log = "y",
         data = obj.stab.psycho, las = 2, ylab = "slope (%/degree)")
dev.off ()

### ** Object condition
df <- droplevels (subset (obj.stab.psycho, stimulus == "object"))

### *** Threshold
pdf (file = "obj-stab-threshold-object.pdf")
par (mar = c (11, 4, 1, 1))
boxplot (threshold ~ object * chair * background, data = df,
         las = 2, ylab = "threshold (degree)")
dev.off ()

### *** Slope
pdf (file = "obj-stab-slope-object.pdf")
par (mar = c (11, 4, 1, 1))
### **** Inclination at threshold is 25*a
boxplot (abs (25 * slope) ~ object * chair * background, log = "y",
         data = df, las = 2, ylab = "slope (%/degree)")
dev.off ()

