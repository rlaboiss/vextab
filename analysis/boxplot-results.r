
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
