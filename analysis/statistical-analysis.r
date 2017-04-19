
### * Statistical analyses

### ** Load the necessary libraries
library (lme4)
library (lmerTest)
library (HDInterval)

### ** Function for computing confidence intervals of predicted values
ci.pred <- function (fit.model, new.data = NA, pred.fun = NA, nb.sim = 1000) {
    if (! is.function (pred.fun))
        pred.fun <- function (fit.model)
                        predict (fit.model, newdata = new.data, re.form = NA)
    n <- length (pred.fun (fit.model))
    b <- bootMer (fit.model, pred.fun, nsim = nb.sim)
    ci <- c ()
    for (i in seq (1, n))
        ci <- rbind (ci, hdi (b$t [, i]))
    return (list (ci = ci, t = b$t))
}

### ** Load the results
obj.stab.psycho <- read.csv ("obj-stab-psycho.csv")

### ** Transform the discrete factors chair and object into numeric
obj.stab.psycho$object.num <- c (1, -1, 0) [as.numeric(obj.stab.psycho$object)]
obj.stab.psycho$chair.num <- c (-1, 1, 0) [as.numeric (obj.stab.psycho$chair)]
obj.stab.psycho$table.side.num <- (c (-1, 0, 1)
                                   [as.numeric (obj.stab.psycho$table.side)])

### ** Boxplot parameters
obj.col <- c ("pink", "cyan", "wheat")
exp.col <- c ("aquamarine", "coral")
side.col <- c ("firebrick1", "deepskyblue")
boxplot.pars <- list (boxwex  = 0.5, bty = "n")
boxplot.ylab <- "threshold angle (degrees)"


### ** Plot labels

### *** Labels for the conditions
chair.lab <- c ("left tilt", "upright", "right tilt")
bg.lab <- c ("static periphery", "rotating periphery")
scene.lab <- c ("scene on the left", "scene on the right")
table.lab <- c ("table to the left", "table to the right")
com.lab <- c ("low COM", "mid COM", "high COM")

### *** Labels for the axes
dsvh.xlab <- expression (paste (Delta, "SVH (degrees)"))
dca.ylab = expression (paste (Delta, "CA (degrees)"))
ca.ylab <- "critical angle (degrees)"
svh.ylab <- "subjective visual horizontal (degrees)"

### ** Experiments

### *** Room 126

### **** Select the data
room.126 <- subset (obj.stab.psycho, experiment == "room-126")

### **** Drop subject S066 (it's Corinne Cian!))
room.126 <- subset (room.126, subject != "S066")

### **** Linear mixed models

## ***** Effect of chair inclination & object shape in static background
df.r126.chair.obj <- subset (room.126,
                             stimulus == "object" & background == "static")
fm.r126.chair.obj <- lmer (threshold ~ object.num * chair.num
                           + (1 | subject), df.r126.chair.obj)
df.r126.chair.obj$residuals <- residuals (fm.r126.chair.obj)

anova (fm.r126.chair.obj)
fixef (fm.r126.chair.obj)

pdf (file = "room-126-chair-object.pdf")
par (mar = c (4, 5, 0.5, 0))
for (i in c (1, 2)) {
    boxplot (threshold ~ object.num * chair.num, df.r126.chair.obj,
             frame = FALSE, las = 1, xlab = "", ylab = boxplot.ylab,
             xaxt = "n", pars = boxplot.pars,
             col = rep (obj.col, 3), add = (i == 2))
    if (i == 1)
        polygon (c (3.5, 6.5, 6.5, 3.5), c (0, 0, 100, 100),
                 col = "#eeeeee", border = NA)
}
axis (1, at = c (2, 5, 8), tick = FALSE, labels = chair.lab)
legend ("topright", inset = 0.05, pch = 22, pt.cex = 2, pt.bg = obj.col,
        legend = com.lab)
dummy <- dev.off ()

### ***** Effect of object shape and background in upright position
df.r126.bg.obj <- subset (room.126,
                          stimulus == "object" & chair == "upright")
fm.r126.bg.obj <- lmer (threshold ~ background * object.num
                        + (1 | subject), df.r126.bg.obj)
df.r126.bg.obj$residuals <- residuals (fm.r126.bg.obj)
anova (fm.r126.bg.obj)
fixef (fm.r126.bg.obj)

fe.r126.bg.obj <- fixef (fm.r126.bg.obj)
re.r126.bg.obj <- ranef (fm.r126.bg.obj)

pdf (file = "room-126-background-object.pdf")
par (mar = c (4, 5, 0.5, 0))
for (i in c (1, 2)) {
    boxplot (threshold ~ object.num * background, df.r126.bg.obj, frame = FALSE,
             las = 1, xlab = "", ylab = boxplot.ylab, xaxt = "n",
             pars = boxplot.pars, col = rep (obj.col, 2), add = (i == 2))
    if (i == 1)
        polygon (c (3.5, 6.5, 6.5, 3.5), c (0, 0, 100, 100),
                 col = "#eeeeee", border = NA)
}
axis (1, at = c (2, 5), tick = FALSE, labels = bg.lab)
legend ("topright", inset = 0.05, pch = 22, pt.cex = 2, pt.bg = obj.col,
        legend = com.lab)
dummy <- dev.off ()

### ***** Effect of background in upright position on horizontal estimation
df.r126.bg.hor <- subset (room.126,
                          stimulus == "horizontal" & chair == "upright")
fm.r126.bg.hor <- lmer (threshold ~ background + (1 | subject), df.r126.bg.hor)
df.r126.bg.hor$residuals <- residuals (fm.r126.bg.hor)
anova (fm.r126.bg.hor)
fixef (fm.r126.bg.hor)

fe.r126.bg.hor <- fixef (fm.r126.bg.hor)
re.r126.bg.hor <- ranef (fm.r126.bg.hor)

pdf (file = "room-126-background-horizontal.pdf")
par (mar = c (4, 5, 0.5, 0))
boxplot (threshold ~ background, df.r126.bg.hor, frame = FALSE,
         las = 1, xlab = "", ylab = boxplot.ylab,
         xaxt = "n", pars = list (boxwex  = 0.2, bty = "n"))
axis (1, at = c (1, 2), tick = FALSE, labels = bg.lab)
dummy <- dev.off ()

### ***** Efect of inclination in static background on horizontal estimation
df.r126.chair.hor <- subset (room.126,
              stimulus == "horizontal" & background == "static")
df.r126.chair.hor$object <- factor (as.character (df.r126.chair.hor$object))
fm.r126.chair.hor <- lmer (threshold ~ chair.num + (0 + chair.num | subject),
                           df.r126.chair.hor)
df.r126.chair.hor$residuals <- residuals (fm.r126.chair.hor)
anova (fm.r126.chair.hor)
fixef (fm.r126.chair.hor)

pdf (file = "room-126-chair-horizontal.pdf")
par (mar = c (4, 5, 0.5, 0))
boxplot (threshold ~ chair.num, df.r126.chair.hor, frame = FALSE,
         las = 1, xlab = "", ylab = boxplot.ylab,
         xaxt = "n", pars = list (boxwex  = 0.4, bty = "n"))
axis (1, at = seq (1, 3), tick = FALSE, labels = chair.lab)
dummy <- dev.off ()

### **** Plot random effects for horizontality vs. object stability models

pdf (file = "room-126-ranef-cor.pdf")
par (mar = c (4, 5, 0.5, 0))
plot (fe.r126.bg.hor [2] + re.r126.bg.hor$subject [, 1],
      fe.r126.bg.obj [2] + re.r126.bg.obj$subject [, 1],
      pch = 19, bty = "n", las = 1, xlim = c (0,7),
      xlab = dsvh.xlab, ylab = dca.ylab)
dummy <- dev.off ()

pdf (file = "room-126-ranef-cor-diag.pdf")
par (mar = c (4, 5, 0.5, 0))
plot (fe.r126.bg.hor [2] + re.r126.bg.hor$subject [, 1],
      fe.r126.bg.obj [2] + re.r126.bg.obj$subject [, 1],
      pch = 19, bty = "n", las = 1, xlim = c (0,7),
      xlab = dsvh.xlab, ylab = dca.ylab)
abline (0, 1, lty = 2, col = "gray", lwd = 2)
dummy <- dev.off ()

### **** Select representative subjects

### ***** Object stability experiment

### Get the data frame for the room-126 (background/object) experiment
### in condition "static"
df <- subset (df.r126.bg.obj, background == "static")
### Compute the differences in threshold high-mid and mid-low
ag <- aggregate (threshold ~ subject, df,
                 function (x) c(x[2] - x[3], x [3] - x [1]))
diff.t <- ag$threshold
### Get the size of the effect
diff.ca <- -fixef (fm.r126.bg.obj) [3]
### Find the subject
idx <- which.min ((diff.t [, 1] - diff.ca) ^ 2 + (diff.t [, 2] - diff.ca) ^ 2)
cat (sprintf ("Representative subject is %s\n", ag$subject [idx]))

### ***** Horizontality experiment

### Get the data frame for the room-126 (background/horizontal) experiment
### in condition "vection"
df <- subset (df.r126.bg.hor, background == "vection")
### Get the indivual thresholds
thres <- df$threshold
### Get the size of the effect
fe <- fixef (fm.r126.bg.hor) [2]
### Find the subject
idx <- which.min ((thres - fe) ^ 2)
cat (sprintf ("Representative subject is %s\n", df$subject [idx]))

### *** New screen

### **** Select the data
new.screen <- subset (obj.stab.psycho, experiment == "new-screen")
new.screen$subject <- factor (as.character (new.screen$subject))

### **** Select the same subjects from room 126 experiment
for (s in levels (new.screen$subject)) {
    new.screen <- rbind (new.screen,
                         subset (obj.stab.psycho, subject == s
                                                  & experiment == "room-126"
                                                  & stimulus == "horizontal"
                                                  & chair == "upright"))
}
new.screen$subject <- factor (as.character (new.screen$subject))
new.screen$experiment <- factor (as.character (new.screen$experiment),
                                 levels = c ("room-126", "new-screen"))


pdf (file = "room-126-new-screen.pdf")
par (mar = c (4, 5, 0.5, 0))
for (i in c (1, 2)) {
    boxplot (threshold ~ experiment * background, new.screen, frame = FALSE,
             las = 1, xlab = "", ylab = boxplot.ylab, col = rep (exp.col, 2),
             xaxt = "n", pars = list (boxwex  = 0.4, bty = "n"), add = (i == 2))
    if (i == 1)
        polygon (c (2.5, 4.5, 4.5, 2.5), c (-50, -50, 50, 50),
                 col = "#eeeeee", border = NA)
}
axis (1, at = c (1.5, 3.5), tick = FALSE, labels = bg.lab)
legend ("topleft", inset = 0.05, pch = 22, pt.cex = 2, pt.bg = exp.col,
        legend = c ("computer screen", "wall screen"))
dummy <- dev.off ()

### **** Fit model
fm <- lmer (threshold ~ experiment * background + (1 | subject), new.screen)
show (anova (fm))
show (fixef (fm))
show (ranef (fm))

### **** Results Exp. 1 (Fig. 2)

### ***** Points shapes & sizes + labels
obj.pch <- c (18, 16, 15) # diamond, circle, square
obj.cex <- c (2.5, 2, 2) # diamond, circle, square

### ***** Panel A
pred <- predict (fm.r126.bg.obj,
                 expand.grid (object.num  = c (-1, 0, 1),
                              background = c ("static", "vection")),
                 re.form = NA)
se <- aggregate (residuals ~ object.num * background,
                 df.r126.bg.obj,
                 function (x) sd (x) / sqrt (length (x)))$residuals
y.min <- 27.5
y.max <- 37
pdf (file = "Fig-2-A.pdf", width = 5, height = 5)
par (mar = c (2, 4, 1, 0))
plot (0, 0, xlim = c (0.5, 6.5), bty = "n", xaxt = "n", las = 1,
      ylim = c (y.min, y.max), xlab = "", ylab = ca.ylab, type = "n")
axis (1, at = c (2, 5), tick = FALSE, labels = bg.lab)
polygon (c (3.5, 6.5, 6.5, 3.5), c (-50, -50, 38, 38), col = "#eeeeee",
         border = NA)
points (pred, pch = obj.pch, cex = obj.cex)
for (i in seq (1, 6))
    lines (rep (i, 2), pred [i] + se [i] * c(-1, 1), lwd = 3)
legend ("bottomleft", inset = 0.02, pch = obj.pch, pt.cex = 0.75 * obj.cex,
        legend = c ("low GC", "mid GC", "high GC"))
par (xpd = NA)
text (-0.2, y.max, adj = c (0, 0), labels = "A", cex = 2)
dummy <- dev.off ()

### ***** Panel B
pred <- predict (fm.r126.bg.hor,
                 expand.grid (background = c ("static", "vection")),
                 re.form = NA)
se <- aggregate (residuals ~ background, df.r126.bg.hor,
                 function (x) sd (x) / sqrt (length (x)))$residuals
y.min <- -4
y.max <- 7
pdf (file = "Fig-2-B.pdf", width = 5, height = 4)
par (mar = c (4.5, 4, 2.0, 0))
plot (0, 0, xlim = c (0.5, 6.5), bty = "n", xaxt = "n", las = 1,
      ylim = c (y.min, y.max), xlab = "", ylab = svh.ylab, type = "n")
axis (1, at = c (2, 5), tick = FALSE, labels = bg.lab)
polygon (c (3.5, 6.5, 6.5, 3.5), c (-50, -50, 38, 38), col = "#eeeeee",
         border = NA)
for (i in seq (1, 2))
    lines (rep ((i - 1) * 3 + 2, 2), pred [i] + se [i] * c(-1, 1), lwd = 3)
points (c (2, 5), pred, pch = 21, cex = 1.8, bg = "white")
par (xpd = NA)
text (-0.2, y.max + 0.8, adj = c (0, -0.2), labels = "B", cex = 2)
dummy <- dev.off ()

### ***** Panel C
pred <- predict (fm.r126.chair.obj,
                 expand.grid (object.num  = c (-1, 0, 1),
                              chair.num = c (-1, 0, 1)),
                 re.form = NA)
se <- aggregate (residuals ~ object.num * chair.num,
                 df.r126.chair.obj,
                 function (x) sd (x) / sqrt (length (x)))$residuals
pdf (file = "Fig-2-C.pdf", width = 5, height = 5)
par (mar = c (2, 4, 1, 0))
y.min <- 27.5
y.max <- 37
plot (0, 0, xlim = c (0.5, 9.5), bty = "n", xaxt = "n", las = 1, type = "n",
      ylim = c (y.min, y.max), xlab = "", ylab = ca.ylab)
axis (1, at = c (2, 5, 8), tick = FALSE, labels = chair.lab)
polygon (c (3.5, 6.5, 6.5, 3.5), c (-50, -50, 38, 38), col = "#eeeeee",
         border = NA)
points (pred, pch = obj.pch, cex = obj.cex)
for (i in seq (1, 9))
    lines (rep (i, 2), pred [i] + se [i] * c(-1, 1), lwd = 3)
par (xpd = NA)
text (-0.2, y.max, adj = c (0, -0.2), labels = "C", cex = 2)
dummy <- dev.off ()

### ***** Panel D
pred <- predict (fm.r126.chair.hor,
                 expand.grid (chair.num = c (-1, 0, 1)),
                 re.form = NA)
se <- aggregate (residuals ~ chair.num, df.r126.chair.hor,
                 function (x) sd (x) / sqrt (length (x)))$residuals
y.min <- -4
y.max <- 7
pdf (file = "Fig-2-D.pdf", width = 5, height = 4)
par (mar = c (4.5, 5, 2.0, 0))
plot (0, 0, xlim = c (0.5, 9.5), bty = "n", xaxt = "n", las = 1,
      ylim = c (y.min, y.max), xlab = "", ylab = svh.ylab, type = "n")
axis (1, at = c (2, 5, 8), tick = FALSE, labels = chair.lab)
polygon (c (3.5, 6.5, 6.5, 3.5), c (-50, -50, 38, 38), col = "#eeeeee",
         border = NA)
for (i in seq (1, 3))
    lines (rep ((i - 1) * 3 + 2, 2), pred [i] + se [i] * c(-1, 1), lwd = 3)
points (c (2, 5, 8), pred, pch = 21, cex = 1.8, bg = "white")
par (xpd = NA)
text (-0.2, y.max + 0.8, adj = c (0, -0.2), labels = "D", cex = 2)
dummy <- dev.off ()

### ***** Compose Figure
system (paste ("pdfjam Fig-2-A.pdf Fig-2-C.pdf Fig-2-B.pdf Fig-2-D.pdf",
               "--no-landscape --frame true --nup 2x2 --frame false",
               "--outfile tmp.pdf"))
system ("pdfcrop --margins 10 tmp.pdf Fig-2.pdf")

### **** Correlation figure (Fig. 3)
df.hor <- subset(room.126, stimulus == "horizontal" & chair == "upright")
delta.hor <- aggregate (threshold ~ subject, df.hor, diff)
df.ca <- subset(room.126, stimulus == "object" & chair == "upright")
delta.ca <- aggregate (threshold ~ subject,
                       aggregate (threshold ~ subject * background, df.ca, mean),
                       diff)
pdf (file = "Fig-3.pdf", width = 5, height = 5)
par (mar = c (5, 4, 0, 0))
plot (delta.hor$threshold, delta.ca$threshold, bty = "n", las = 1, pch = 19,
      xlab = dsvh.xlab, ylab = dca.ylab, type = "n")
abline (0, 1, col = "gray", lwd = 2)
points (delta.hor$threshold, delta.ca$threshold, pch = 19)
points (mean (delta.hor$threshold), mean (delta.ca$threshold), pch = 18,
        col = "#ff000080", cex = 3)
dummy <- dev.off ()


### *** Scene mirror

### **** Select the data
scene.mirror <- subset (obj.stab.psycho, experiment == "scene-mirror")
scene.mirror$subject <- factor (as.character (scene.mirror$subject))

### **** Effect of object CG height and secene side

### ***** Extract the data
scene.mirror.obj <- subset (scene.mirror, stimulus == "object")

### ***** Plot the raw results
pdf (file = "scene-mirror-object.pdf")
par (mar = c (4, 5, 0.5, 0))
for (i in c (1, 2)) {
    boxplot (threshold ~ object.num * object.side, scene.mirror.obj,
             frame = FALSE, las = 1, xlab = "", ylab = boxplot.ylab,
             xaxt = "n", pars = boxplot.pars,
             col = rep (obj.col, 2), add = (i == 2))
    if (i == 1)
        polygon (c (3.5, 6.5, 6.5, 3.5), c (0, 0, 100, 100),
                 col = "#eeeeee", border = NA)
}
axis (1, at = c (2, 5), tick = FALSE, labels = scene.lab)
legend ("topright", inset = 0.05, pch = 22, pt.cex = 2, pt.bg = obj.col,
        legend = com.lab)
dummy <- dev.off ()

### ***** Fit the model
fm.scene.mirror <- lmer (threshold ~ object.num * table.side.num
                                     + (table.side.num | subject),
                         scene.mirror.obj)
show (anova (fm.scene.mirror))
show (fixef (fm.scene.mirror))
show (ranef (fm.scene.mirror))

fm.no.Intercept <- lmer (threshold ~ object.num * table.side.num
                                     + (0 + table.side.num | subject),
                         scene.mirror.obj)
fm.no.table.side.num <- lmer (threshold ~ object.num * table.side.num
                                          + (1 | subject),
                              scene.mirror.obj)
show (anova (fm.no.Intercept, fm.scene.mirror))
show (anova (fm.no.table.side.num, fm.scene.mirror))

### ***** Plot the results
pred <- predict (fm.scene.mirror, expand.grid (object.num = c (-1, 0, 1),
                                               table.side.num = c (-1, 1)),
                 re.form = NA)
scene.mirror.obj$residuals <- residuals (fm.scene.mirror)
se <- aggregate (residuals ~ object.num * table.side.num, scene.mirror.obj,
                 function (x) sd (x) / sqrt (length (x)))$residuals
y.min <- min (pred - se)
y.max <- max (pred + se)
pdf (file = "Fig-5-A.pdf", width = 5, height = 5)
par (mar = c (4.5, 5, 2.0, 0))
plot (0, 0, type = "n", xlim = c (0.5, 6.5), bty = "n", xaxt = "n", las = 1,
      ylim = c (y.min, y.max), xlab = "", ylab = ca.ylab)
axis (1, at = c (2, 5), tick = FALSE, labels = scene.lab)
polygon (c (3.5, 6.5, 6.5, 3.5), c (-50, -50, 38, 38), col = "#eeeeee",
         border = NA)
for (i in seq (1, 6))
    lines (rep (i, 2), pred [i] + se [i] * c(-1, 1), lwd = 3)
points (pred, pch = obj.pch, cex = obj.cex)
legend ("bottomleft", inset = 0.05, pch = obj.pch, pt.cex = 0.75 * obj.cex,
        legend = com.lab)
par (xpd = NA)
text (-0.2, y.max + 0.8, adj = c (0, -0.2), labels = "A", cex = 2)
dummy <- dev.off ()

### ***** Plot the BLUP
re <- ranef (fm.scene.mirror)$subject
n <- nrow (re)
fe <- fixef (fm.scene.mirror)

pdf (file = "Fig-5-B.pdf", width = 5, height = 5)
par (mar = c (5, 5.5, 2, 0.1))
x <- re [,1] + fe [1]
y <- -2 * (re [,2]  + fe [3])
y.min <- min (y)
y.max <- 18
plot (x, y, pch = 19, cex = 1.5, las = 1,  xlim = c (15, 40), col = "#00000080",
      bty = "n", xlab = "mean critical angle (degrees)",
      ylab = "left/right side effect (degrees)")
abline (h = -2 * fe [3], col = "#00000080", lwd = 2, lty = "21")
abline (v = fe [1], col = "#00000080", lwd = 2, lty = "21")
par (xpd = NA)
text (14, 16.5, adj = c (1, 0), labels = "B", cex = 2)
dummy <- dev.off ()

### ***** Compose the Fig. 5
system (paste ("pdfjam Fig-5-A.pdf Fig-5-B.pdf",
               "--no-landscape --frame true --nup 2x1 --frame false",
               "--outfile tmp.pdf"))
system ("pdfcrop --margins 10 tmp.pdf Fig-5.pdf")

### **** Effect of background on horizontal detection
scene.mirror.hor <- subset (scene.mirror, stimulus == "horizontal")
idx <- which (scene.mirror.hor$table.side == "right")
scene.mirror.hor$threshold [idx] <- -scene.mirror.hor$threshold [idx]

pdf (file = "scene-mirror-horizontal.pdf")
par (mar = c (4, 5, 0.5, 0))
for (i in c (1, 2)) {
    boxplot (threshold ~ table.side * background, scene.mirror.hor,
             frame = FALSE, las = 1, xlab = "", ylab = boxplot.ylab,
             col = rep (side.col, 2), xaxt = "n",
             pars = list (boxwex  = 0.4, bty = "n"), add = (i == 2))
    if (i == 1)
        polygon (c (2.5, 4.5, 4.5, 2.5), c (-50, -50, 50, 50),
                 col = "#eeeeee", border = NA)
}
axis (1, at = c (1.5, 3.5), tick = FALSE, labels = bg.lab)
legend ("topleft", inset = 0.05, pch = 22, pt.cex = 2, pt.bg = side.col,
        legend = table.lab)
dummy <- dev.off ()

fm <- lmer (threshold ~ table.side.num * background + (1 | subject),
            scene.mirror.hor)
show (anova (fm))
show (fixef (fm))
show (ranef (fm))

### *** Flip table

### **** Select the data
flip.table <- subset (obj.stab.psycho, experiment == "flip-table")
flip.table$subject <- factor (as.character (flip.table$subject))

### **** Effect of scene side and background on object stability
flip.table.obj <- subset (flip.table, stimulus == "object")

pdf (file = "flip-table-object.pdf")
par (mar = c (4, 5, 0.5, 0))
for (i in c (1, 2)) {
    boxplot (threshold ~ table.side * background, flip.table.obj,
             frame = FALSE, las = 1, xlab = "", ylab = boxplot.ylab,
             col = rep (side.col, 2), xaxt = "n",
             pars = list (boxwex  = 0.4, bty = "n"), add = (i == 2))
    if (i == 1)
        polygon (c (2.5, 4.5, 4.5, 2.5), c (-50, -50, 50, 50),
                 col = "#eeeeee", border = NA)
}
axis (1, at = c (1.5, 3.5), tick = FALSE, labels = bg.lab)
legend ("bottomleft", inset = 0.05, pch = 22, pt.cex = 2, pt.bg = side.col,
        legend = c ("table to the left", "table to the right"))
dummy <- dev.off ()

fm <- lmer (threshold ~ background * table.side + (1 | subject),
            flip.table.obj)
show (anova (fm))
show (fixef (fm))
show (ranef (fm))

### **** Effect of scene side and background on horizontal detection
flip.table.hor <- subset (flip.table, stimulus == "horizontal")
idx <- which (flip.table.hor$table.side == "right")
flip.table.hor$threshold [idx] <- -flip.table.hor$threshold [idx]

pdf (file = "flip-table-horizontal.pdf")
par (mar = c (4, 5, 0.5, 0))
for (i in c (1, 2)) {
    boxplot (threshold ~ background * table.side, flip.table.hor,
             frame = FALSE, las = 1, xlab = "", ylab = boxplot.ylab,
             col = rep (side.col, 2), xaxt = "n",
             pars = list (boxwex  = 0.4, bty = "n"), add = (i == 2))
    if (i == 1)
        polygon (c (2.5, 4.5, 4.5, 2.5), c (-50, -50, 50, 50),
                 col = "#eeeeee", border = NA)
}
axis (1, at = c (1.5, 3.5), tick = FALSE,
      labels = c ("table to the left", "table to the right"))
legend ("topleft", inset = 0.05, pch = 22, pt.cex = 2, pt.bg = side.col,
        legend = bg.lab)
dummy <- dev.off ()

fm <- t.test (threshold ~ background, paired = TRUE,
              data = subset (flip.table.hor, table.side == "left"))
show (fm)

fm <- t.test (threshold ~ background, paired = TRUE,
              data = subset (flip.table.hor, table.side == "right"))
show (fm)

### *** No table

### **** Select the data
no.table <- subset (obj.stab.psycho, experiment == "no-table")

### **** Plot the raw results
pdf (file = "no-table-object.pdf")
par (mar = c (4, 5, 0.5, 0))
for (i in c (1, 2)) {
    boxplot (threshold ~ object.num * background * table.side, no.table,
             frame = FALSE, las = 1, xlab = "", ylab = boxplot.ylab,
             xaxt = "n", pars = boxplot.pars,
             col = rep (obj.col, 2), add = (i == 2))
    if (i == 1)
        polygon (c (3.5, 6.5, 6.5, 3.5), c (0, 0, 100, 100),
                 col = "#eeeeee", border = NA)
}
axis (1, at = c (2, 5), tick = FALSE, labels = bg.lab)
legend ("topright", inset = 0.05, pch = 22, pt.cex = 2, pt.bg = obj.col,
        legend = com.lab)
dummy <- dev.off ()

fm <- lmer (threshold ~ background * object.num * table.side + (1 | subject),
            no.table)
show (anova (fm))
show (fixef (fm))
show (ranef (fm))

fe <- fixef (fm)

### **** Rudimentary plot of results
pdf (file = "no-table-object.pdf", width = 4, height = 5)
par (mar = c (4, 5, 0.5, 0))
plot (c (1, 1, 2, 2),
      c (fe[1], fe [1] + fe [2], fe [1] + fe [4],
         fe [1] + fe [2] + fe [4] + fe [6]),
      pch = 19, col = rep (c ("blue", "red"), 2), bty = "n",
      xlim = c (0.7, 2.3), cex = 1.3,
      las = 1, xaxt = "n", xlab = "", ylab = ca.ylab)
lines (c (1, 2), c (fe[1], fe [1] + fe [4]), col = "blue")
lines (c (1, 2), c (fe [1] + fe [2], fe [1] + fe [2] + fe [4] + fe [6]),
       col = "red")
axis (1, at = c (1, 2), labels = c ("table", "no table"))
legend ("right", col = c ("red", "blue"), legend = c ("rotating", "static"),
        pch = 19)
dummy <- dev.off ()

### ***** Plot the results
pred <- predict (fm, expand.grid (object.num = c (-1, 0, 1),
                                  background = c ("static", "vection"),
                                  table.side = c ("left", "none")),
                 re.form = NA)
no.table$residuals <- residuals (fm)
se <- aggregate (residuals ~ object.num * table.side * background,
                 no.table, function (x) sd (x) / sqrt (length (x)))$residuals
y.min <- min (pred - se)
y.max <- max (pred + se)
pdf (file = "Fig-6.pdf", width = 7, height = 5)
par (mar = c (4.5, 5, 4.5, 0), xpd = FALSE)
plot (0, 0, type = "n", xlim = c (0.5, 12.5), bty = "n", xaxt = "n", las = 1,
      ylim = c (y.min, y.max), xlab = "", ylab = ca.ylab)
axis (1, at = c (3.5, 9.5), labels = c ("with table", "without table"), line = 1)
for (i in c (1, 2)) {
    polygon (6 * (i - 1) + c (3.5, 6.5, 6.5, 3.5),
             c (0, 0, 100, 100), col = "#eeeeee", border = NA)
    axis (3, at = 6 * (i - 1) + c (2, 5), line = 1,
          labels = c ("static", "rotating"))
}
for (i in seq (1, 12))
    lines (rep (i, 2), pred [i] + se [i] * c(-1, 1), lwd = 3)
points (pred, pch = obj.pch, cex = obj.cex)
legend ("topleft", inset = 0.05, pch = obj.pch, pt.cex = 0.75 * obj.cex,
        legend = com.lab)
dummy <- dev.off ()
