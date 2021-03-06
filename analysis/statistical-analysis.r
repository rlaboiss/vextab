
### * Statistical analyses

### ** Load the necessary libraries
library (lme4)
library (lmerTest)
library (HDInterval)
library (shape)
library (car)

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
obj.stab.psycho$table.side.num <- (c (-0.5, 0, 0.5)
                                   [as.numeric (obj.stab.psycho$table.side)])

### ** Boxplot parameters
obj.col <- c ("pink", "cyan", "wheat")
exp.col <- c ("aquamarine", "coral")
side.col <- c ("firebrick1", "deepskyblue")
boxplot.pars <- list (boxwex  = 0.5, bty = "n")
boxplot.ylab <- "threshold angle (degrees)"


### ** Plot labels

### *** Labels for the conditions
chair.lab <- c ("Left Tilt", "Upright", "Right Tilt")
bg.lab <- c ("Static Surround", "Rotating Surround")
scene.lab <- c ("Scene on the Left", "Scene on the Right")
table.lab <- c ("Table to the Left", "Table to the Right")
com.lab <- c ("Low COM", "Mid COM", "High COM")

### *** Labels for the axes
dsvh.xlab <- expression (paste (Delta, "TU (degrees)"))
dca.ylab = expression (paste (Delta, "CA (degrees)"))
ca.ylab <- "Critical Angle (degrees)"
svh.ylab <- "Table Uprightness (degrees)"

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

anova (fm.r126.chair.obj)
fixef (fm.r126.chair.obj)
confint (fm.r126.chair.obj)

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
anova (fm.r126.bg.obj)
fixef (fm.r126.bg.obj)
confint (fm.r126.bg.obj)

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

### ***** Power analysis

### ****** Home-made simulation
### (Taken from the web somewhere.  Look at https://goo.gl/zOUgFy)
nsim <- 1000
p.value <- rep (NA, nsim)
sim.r126.bg.obj <- simulate (fm.r126.bg.obj, nsim = nsim)

cat ("Computing power effect for Exp. 1 (surround effect)\n")
flush.console ()

for (i in seq (1, nsim)) {
    df <- df.r126.bg.obj
    df$threshold <- sim.r126.bg.obj [, i]
    fm <- lmer (threshold ~ background * object.num + (1 | subject), df)
    p.value [i] <- anova (fm) [["Pr(>F)"]] [1]
    cat (sprintf ("\r%4d", i))
    flush.console ()
}

cat (sprintf ("Power for surround effect is %f\n",
              (nsim - length (which (p.value > 0.05))) / nsim))
flush.console ()

### ****** Using SIMR package
library (simr)
powerSim (fm.r126.bg.obj, test = fixed ("background"), nsim = 100)

### ***** Effect of background in upright position on horizontal estimation
df.r126.bg.hor <- subset (room.126,
                          stimulus == "horizontal" & chair == "upright")
fm.r126.bg.hor <- lmer (threshold ~ background + (1 | subject), df.r126.bg.hor)

anova (fm.r126.bg.hor)
fixef (fm.r126.bg.hor)
confint (fm.r126.bg.hor)

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

anova (fm.r126.chair.hor)
fixef (fm.r126.chair.hor)
confint (fm.r126.chair.hor)

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

### ***** General plot paramaters
obj.pch <- c (18, 16, 15) # diamond, circle, square
obj.cex <- c (2.5, 2, 2) # diamond, circle, square
gray.box <- "#eeeeee"
pdf.wd <- 4.5
pdf.ht <- 4.5

### ***** Panel A
nd <- expand.grid (object.num  = c (-1, 0, 1),
                   background = c ("static", "vection"))
pred <- predict (fm.r126.bg.obj, nd, re.form = NA)
ci <- ci.pred (fm.r126.bg.obj, nd)$ci
y.min <- 26
y.max <- 39
pdf (file = "Fig-2-a.pdf", width = pdf.wd, height = pdf.ht)
par (mar = c (2, 4, 1, 1))
plot (0, 0, xlim = c (0.5, 6.5), bty = "n", xaxt = "n", las = 1,
      ylim = c (y.min, y.max), xlab = "", ylab = ca.ylab, type = "n")
axis (1, at = c (2, 5), tick = FALSE, labels = bg.lab)
polygon (c (3.5, 6.5, 6.5, 3.5), c (-50, -50, y.max, y.max), col = gray.box,
         border = NA)
points (pred, pch = obj.pch, cex = obj.cex)
for (i in seq (1, 6))
    lines (rep (i, 2), ci [i, ], lwd = 3)
legend ("bottomleft", inset = c (0.05, 0.05), pch = obj.pch, bty = "n",
        pt.cex = 0.75 * obj.cex, legend = com.lab)
par (xpd = NA)
text (-0.2, y.max, adj = c (0, 0), labels = "a", cex = 2)
dummy <- dev.off ()

### ***** Panel C
nd <- expand.grid (object.num  = c (-1, 0, 1), chair.num = c (-1, 0, 1))
pred <- predict (fm.r126.chair.obj, nd, re.form = NA)
ci <- ci.pred (fm.r126.chair.obj, nd)$ci
pdf (file = "Fig-2-c.pdf", width = pdf.wd, height = pdf.ht)
par (mar = c (2, 4, 1, 1))
plot (0, 0, xlim = c (0.5, 9.5), bty = "n", xaxt = "n", las = 1, type = "n",
      ylim = c (y.min, y.max), xlab = "", ylab = ca.ylab)
axis (1, at = c (2, 5, 8), tick = FALSE, labels = chair.lab)
polygon (c (3.5, 6.5, 6.5, 3.5), c (-50, -50, y.max, y.max), col = gray.box,
         border = NA)
points (pred, pch = obj.pch, cex = obj.cex)
for (i in seq (1, 9))
    lines (rep (i, 2), ci [i, ], lwd = 3)
par (xpd = NA)
text (-0.2, y.max, adj = c (0, 0), labels = "c", cex = 2)
dummy <- dev.off ()

### ***** Panel B
nd <- expand.grid (background = c ("static", "vection"))
pred <- predict (fm.r126.bg.hor, nd, re.form = NA)
ci <- ci.pred (fm.r126.bg.hor, nd)$ci
y.min <- -5
y.max <- 8
pdf (file = "Fig-2-b.pdf", width = pdf.wd, height = 0.8 * pdf.ht)
par (mar = c (4.5, 4, 2.0, 1))
plot (0, 0, xlim = c (0.5, 6.5), bty = "n", xaxt = "n", las = 1,
      ylim = c (y.min, y.max), xlab = "", ylab = svh.ylab, type = "n")
axis (1, at = c (2, 5), tick = FALSE, labels = bg.lab)
polygon (c (3.5, 6.5, 6.5, 3.5), c (-50, -50, 38, 38), col = gray.box,
         border = NA)
for (i in seq (1, 2))
    lines (rep ((i - 1) * 3 + 2, 2), ci [i, ], lwd = 3)
points (c (2, 5), pred, pch = 21, cex = 1.8, bg = "white")
par (xpd = NA)
text (-0.2, y.max + 1.2, adj = c (0, -0.2), labels = "b", cex = 2)
dummy <- dev.off ()

### ***** Panel D
nd <- expand.grid (chair.num = c (-1, 0, 1))
pred <- predict (fm.r126.chair.hor, nd, re.form = NA)
ci <- ci.pred (fm.r126.chair.hor, nd)$ci
pdf (file = "Fig-2-d.pdf", width = pdf.wd, height = 0.8 * pdf.ht)
par (mar = c (4.5, 5, 2.0, 1))
plot (0, 0, xlim = c (0.5, 9.5), bty = "n", xaxt = "n", las = 1,
      ylim = c (y.min, y.max), xlab = "", ylab = svh.ylab, type = "n")
axis (1, at = c (2, 5, 8), tick = FALSE, labels = chair.lab)
polygon (c (3.5, 6.5, 6.5, 3.5), c (-50, -50, 38, 38), col = gray.box,
         border = NA)
for (i in seq (1, 3))
    lines (rep ((i - 1) * 3 + 2, 2), ci [i, ], lwd = 3)
points (c (2, 5, 8), pred, pch = 21, cex = 1.8, bg = "white")
par (xpd = NA)
text (-0.2, y.max + 1.2, adj = c (0, -0.2), labels = "d", cex = 2)
dummy <- dev.off ()

### ***** Compose Figure
system (paste ("pdfjam Fig-2-a.pdf Fig-2-c.pdf Fig-2-b.pdf Fig-2-d.pdf",
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
pdf (file = "Fig-3.pdf", width = pdf.wd, height = pdf.ht)
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
df.scene.mirror <- subset (obj.stab.psycho, experiment == "scene-mirror")
df.scene.mirror$subject <- factor (as.character (df.scene.mirror$subject))

### **** Effect of object CG height and secene side

### ***** Extract the data
df.scene.mirror.obj <- subset (df.scene.mirror, stimulus == "object")

### ***** Plot the raw results
pdf (file = "scene-mirror-object.pdf")
par (mar = c (4, 5, 0.5, 0))
for (i in c (1, 2)) {
    boxplot (threshold ~ object.num * object.side, df.scene.mirror.obj,
             frame = FALSE, las = 1, xlab = "", ylab = boxplot.ylab,
             xaxt = "n", pars = boxplot.pars,
             col = rep (obj.col, 2), add = (i == 2))
    if (i == 1)
        polygon (c (3.5, 6.5, 6.5, 3.5), c (0, 0, 100, 100),
                 col = gray.box, border = NA)
}
axis (1, at = c (2, 5), tick = FALSE, labels = scene.lab)
legend ("topright", inset = 0.05, pch = 22, pt.cex = 2, pt.bg = obj.col,
        legend = com.lab)
dummy <- dev.off ()

### ***** Fit the model
fm.scene.mirror.obj <- lmer (threshold ~ object.num * table.side.num
                                         + (1 | subject)
                                         + (0 + table.side.num | subject),
                             df.scene.mirror.obj)
show (anova (fm.scene.mirror.obj))
show (fixef (fm.scene.mirror.obj))
show (ranef (fm.scene.mirror.obj))

fm.no.Intercept <- lmer (threshold ~ object.num * table.side.num
                                     + (0 + table.side.num | subject),
                         df.scene.mirror.obj)
fm.no.table.side.num <- lmer (threshold ~ object.num * table.side.num
                                          + (1 | subject),
                              df.scene.mirror.obj)
show (anova (fm.no.Intercept, fm.scene.mirror.obj))
show (anova (fm.no.table.side.num, fm.scene.mirror.obj))

### ***** Plot the results
nd <- expand.grid (object.num = c (-1, 0, 1), table.side.num = c (-0.5, 0.5))
pred <- predict (fm.scene.mirror.obj, nd, re.form = NA)
ci <- ci.pred (fm.scene.mirror.obj, nd)$ci
y.min <- min (ci [, 1])
y.max <- max (ci [, 2])
pdf (file = "Fig-5-a.pdf", width = pdf.wd, height = pdf.ht)
par (mar = c (4.5, 5, 2.0, 0))
plot (0, 0, type = "n", xlim = c (0.5, 6.5), bty = "n", xaxt = "n", las = 1,
      ylim = c (y.min, y.max), xlab = "", ylab = ca.ylab)
axis (1, at = c (2, 5), tick = FALSE, labels = scene.lab)
polygon (c (3.5, 6.5, 6.5, 3.5), c (-50, -50, 42, 42), col = gray.box,
         border = NA)
for (i in seq (1, 6))
    lines (rep (i, 2), ci [i, ], lwd = 3)
points (pred, pch = obj.pch, cex = obj.cex)
legend ("bottomleft", inset = 0.05, pch = obj.pch, pt.cex = 0.75 * obj.cex,
        bty = "n", legend = com.lab)
par (xpd = NA)
text (-0.2, y.max + 0.8, adj = c (0, -0.2), labels = "a", cex = 2)
dummy <- dev.off ()

### ***** Plot the BLUP
re <- ranef (fm.scene.mirror.obj)$subject
n <- nrow (re)
fe <- fixef (fm.scene.mirror.obj)

pdf (file = "Fig-5-b.pdf", width = pdf.wd, height = pdf.ht)
par (mar = c (5, 5.5, 2, 0.1))
x <- re [,1] + fe [1]
y <- - (re [,2]  + fe [3])
show (max (y))
y.min <- min (y)
y.max <- 18
plot (x, y, pch = 19, cex = 1.5, las = 1,  xlim = c (20, 40), col = "#00000080",
      bty = "n", xlab = "Mean Critical Angle (degrees)",
      ylab = "Left/Right Side Effect (degrees)")
abline (h = -fe [3], col = "#00000080", lwd = 2, lty = "21")
abline (v = fe [1], col = "#00000080", lwd = 2, lty = "21")
par (xpd = NA)
text (20, 14.5, adj = c (1, 0), labels = "b", cex = 2)
dummy <- dev.off ()

### ***** Compose the Fig. 5
system (paste ("pdfjam Fig-5-a.pdf Fig-5-b.pdf",
               "--no-landscape --frame true --nup 2x1 --frame false",
               "--outfile tmp.pdf"))
system ("pdfcrop --margins 10 tmp.pdf Fig-5.pdf")

### **** Check age effect on the random factors

### ****** Plot the ranef with age as size of points
subjects <- read.csv ("cohort-info.csv")
age <- sapply (row.names (re),
               function (x)
                   subjects$age [which (x == as.character (subjects$subject))])
pdf (file = "scene-mirror-ranef-age.pdf")
par (xpd = NA)
plot (fe [1] + re [, 1], -(fe [3] + re [, 2]), bty = "n",
      las = 1, col = "#00000080", pch = 19,
      cex = 1 + 5 * (age - min (age)) / (max (age) - min (age)),
      xlab = "individual intercept effect (degrees)",
      ylab = "individual table-sode effect (degrees)")
legend.years <- c (20, 30)
legend.cex <- 1 + 5 * (legend.years - min (age)) / (max (age) - min (age))
legend ("topleft", inset = 0.2, pt.cex = legend.cex, pch = 1, cex = 1.8,
        legend = sapply (legend.years, function (x) sprintf ("%.0f years", x)),
        text.col = "#bbbbbb")
Arrows (19, 0, 19, -5, lwd = 2, col = "red")
text (19.5, -2.5, label = "- gravity", adj = c (0.5, 1), srt = 90, col = "red",
      cex = 1.2)
Arrows (19, 8, 19, 13, lwd = 2, col = "red")
text (19.5, 10.5, label = "+ gravity", adj = c (0.5, 1), srt = 90, col = "red",
      cex = 1.2)
Arrows (24, 15, 20, 15, lwd = 2, col = "red")
text (22, 15.5, label = "cautious", adj = c (0.5, 0), col = "red", cex = 1.2)
Arrows (32, 15, 36, 15, lwd = 2, col = "red")
text (34, 15.5, label = "risky", adj = c (0.5, 0), col = "red", cex = 1.2)
dummy <- dev.off ()

### ****** Pilai statistical test
fm.scene.mirror.age <- lm (cbind (fe [1] + re [, 1], -(fe [3] + re [, 2])) ~ age)
Manova (fm.scene.mirror.age)

### **** Effect of background on horizontal detection
df.scene.mirror.hor <- subset (df.scene.mirror, stimulus == "horizontal")
idx <- which (df.scene.mirror.hor$table.side == "right")
df.scene.mirror.hor$threshold [idx] <- -df.scene.mirror.hor$threshold [idx]

pdf (file = "scene-mirror-horizontal.pdf")
par (mar = c (4, 5, 0.5, 0))
for (i in c (1, 2)) {
    boxplot (threshold ~ table.side * background, df.scene.mirror.hor,
             frame = FALSE, las = 1, xlab = "", ylab = boxplot.ylab,
             col = rep (side.col, 2), xaxt = "n",
             pars = list (boxwex  = 0.4, bty = "n"), add = (i == 2))
    if (i == 1)
        polygon (c (2.5, 4.5, 4.5, 2.5), c (-50, -50, 50, 50),
                 col = gray.box, border = NA)
}
axis (1, at = c (1.5, 3.5), tick = FALSE, labels = bg.lab)
legend ("topleft", inset = 0.05, pch = 22, pt.cex = 2, pt.bg = side.col,
        legend = table.lab)
dummy <- dev.off ()

fm.scene.mirror.hor <- lmer (threshold ~ table.side.num * background
                                         + (1 | subject),
                             df.scene.mirror.hor)
show (anova (fm.scene.mirror.hor))
show (fixef (fm.scene.mirror.hor))
show (ranef (fm.scene.mirror.hor))

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
                 col = gray.box, border = NA)
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
                 col = gray.box, border = NA)
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
df.no.table <- subset (obj.stab.psycho, experiment == "no-table")

### **** Plot the raw results
pdf (file = "no-table-object.pdf")
par (mar = c (4, 5, 0.5, 0))
for (i in c (1, 2)) {
    boxplot (threshold ~ object.num * background * table.side, df.no.table,
             frame = FALSE, las = 1, xlab = "", ylab = boxplot.ylab,
             xaxt = "n", pars = boxplot.pars,
             col = rep (obj.col, 2), add = (i == 2))
    if (i == 1)
        polygon (c (3.5, 6.5, 6.5, 3.5), c (0, 0, 100, 100),
                 col = gray.box, border = NA)
}
axis (1, at = c (2, 5), tick = FALSE, labels = bg.lab)
legend ("topright", inset = 0.05, pch = 22, pt.cex = 2, pt.bg = obj.col,
        legend = com.lab)
dummy <- dev.off ()

fm.no.table <- lmer (threshold ~ background * object.num * table.side
                                 + (1 | subject),
                     df.no.table)
show (anova (fm.no.table))
show (fixef (fm.no.table))
show (ranef (fm.no.table))
show (confint (fm.no.table))

fe <- fixef (fm.no.table)

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
nd <- expand.grid (object.num = c (-1, 0, 1),
                   background = c ("static", "vection"),
                   table.side = c ("left", "none"))
pred <- predict (fm.no.table, nd, re.form = NA)
ci <- ci.pred (fm.no.table, nd)$ci
y.min <- min (ci [, 1])
y.max <- max (ci [, 2])
pdf (file = "Fig-6.pdf", width = 7, height = 5)
par (mar = c (4.5, 5, 4.5, 0), xpd = FALSE)
plot (0, 0, type = "n", xlim = c (0.5, 12.5), bty = "n", xaxt = "n", las = 1,
      ylim = c (y.min, y.max), xlab = "", ylab = ca.ylab)
group.axis <- function (pos, at, lab) {
    axis (pos, at = at, labels = rep ("", 2), line = 1)
    axis (pos, at = mean (at), tick = FALSE, line = 0.5, labels = lab)
}
for (i in c (1, 2)) {
    polygon (6 * (i - 1) + c (3.5, 6.5, 6.5, 3.5),
             c (0, 0, 100, 100), col = gray.box, border = NA)
    group.axis (3, at = 6 * (i - 1) + c (1, 3), "Static")
    group.axis (3, at = 6 * (i - 1) + c (4, 6), "Rotating")
}
group.axis (1, c (1, 6), "With Table")
group.axis (1, c (7, 12), "Without Table")
for (i in seq (1, 12))
    lines (rep (i, 2), ci [i, ], lwd = 3)
points (pred, pch = obj.pch, cex = obj.cex)
legend ("topleft", inset = c (0.05, 0), pch = obj.pch, pt.cex = 0.75 * obj.cex,
        bty = "n", legend = com.lab)
dummy <- dev.off ()

### ** Verify age effects for background effect in Exp. 1 and Exp. 3

df.age <- subset (obj.stab.psycho,
                  experiment %in% c ("room-126", "no-table")
                  & table.side == "left" & chair == "upright"
                  & subject != "S066" & stimulus == "object")
fm.age <- lmer (threshold ~ background * object + (background | subject),
                df.age)
re <- ranef (fm.age)$subject
age <- sapply (row.names (re),
               function (x)
                   subjects$age [which (x == as.character (subjects$subject))])
cor.test (re [, 1], age)
cor.test (re [, 2], age)
