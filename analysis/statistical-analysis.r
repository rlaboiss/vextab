
### * Statistical analyses

### ** Load the necessary libraries

library (lme4)
library (lmerTest)

### ** Load the resutls

### ** Boxplot parameters
obj.col <- c ("pink", "cyan", "wheat")
exp.col <- c ("aquamarine", "coral")
side.col <- c ("firebrick1", "deepskyblue")
boxplot.pars <- list (boxwex  = 0.5, bty = "n")
boxplot.ylab <- "threshold angle (degrees)"

obj.stab.psycho <- read.csv ("obj-stab-psycho.csv")

### ** Transform the discrete factor chair and object into numeric

obj.stab.psycho$chair.num <- c (-1, 1, 0) [as.numeric (obj.stab.psycho$chair)]
obj.stab.psycho$object.num <- c (1, -1, 0) [as.numeric(obj.stab.psycho$object)]

### ** Tests

### *** Linear mixed models

for (i in seq (1, 4)) {

    if (i == 1) {

        ## **** Efect of chair inclination & object shape in static background
        df <- subset (obj.stab.psycho,
                      stimulus == "object" & background == "static")
        fm <- lmer (threshold ~ object.num * chair.num + (1 | subject), df)

    } else if (i == 2) {

        ## **** Efect of object shape and background in upright position
        df <- subset (obj.stab.psycho,
                      stimulus == "object" & chair == "upright")
        fm <- lmer (threshold ~ background * object.num + (1 | subject), df)

    } else if (i == 3) {

        ## **** Efect of background in upright position on horizontal estimation
        df <- subset (obj.stab.psycho,
                      stimulus == "horizontal" & chair == "upright")
        fm <- lmer (threshold ~ background + (1 | subject), df)

    } else {

        ## **** Efect of inclination in static backaground on horizontal estimation
        df <- subset (obj.stab.psycho,
                      stimulus == "horizontal" & background == "static")
        fm <- lmer (threshold ~ chair.num + (1|subject), df)

    }

    show (anova (fm))
    show (rand (fm))
    show (fixef (fm))
    show (ranef (fm))

}

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
axis (1, at = c (1.5, 3.5), tick = FALSE,
      labels = c ("static background", "rotating background"))
legend ("topleft", inset = 0.05, pch = 22, pt.cex = 2, pt.bg = exp.col,
        legend = c ("computer screen", "wall screen"))
dummy <- dev.off ()

### **** Fit model
fm <- lmer (threshold ~ experiment * background + (1 | subject), new.screen)
show (anova (fm))
show (fixef (fm))
show (ranef (fm))
### *** Scene mirror

### **** Select the data
scene.mirror <- subset (obj.stab.psycho, experiment == "scene-mirror")
scene.mirror$subject <- factor (as.character (scene.mirror$subject))

### **** Transform the discrete factor object into numeric
scene.mirror$object.num <- c (1, -1, 0) [as.numeric (scene.mirror$object)]

### ***** Effect of object CG height and secene side
scene.mirror.obj <- subset (scene.mirror, stimulus == "object")

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
axis (1, at = c (2, 5), tick = FALSE,
      labels = c ("scene on the left", "scene on the right"))
legend ("topright", inset = 0.05, pch = 22, pt.cex = 2, pt.bg = obj.col,
        legend = c ("low object", "mid object", "high object"))
dev.off ()

fm <- lmer (threshold ~ object.num * table.side + (1 | subject),
            scene.mirror.obj)
show (anova (fm))
show (fixef (fm))
show (ranef (fm))
### ***** Effect of background on horizontal detection
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
axis (1, at = c (1.5, 3.5), tick = FALSE,
      labels = c ("static background", "rotating background"))
legend ("topleft", inset = 0.05, pch = 22, pt.cex = 2, pt.bg = side.col,
        legend = c ("table to the left", "table to the right"))
dummy <- dev.off ()

fm <- lmer (threshold ~ table.side * background + (1 | subject),
            scene.mirror.hor)
show (anova (fm))
show (fixef (fm))
show (ranef (fm))
### *** Flip table

### **** Select the data
flip.table <- subset (obj.stab.psycho, experiment == "flip-table")
flip.table$subject <- factor (as.character (flip.table$subject))

### ***** Effect of scene side and background on obejct stability
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
axis (1, at = c (1.5, 3.5), tick = FALSE,
      labels = c ("static background", "rotating background"))
legend ("bottomleft", inset = 0.05, pch = 22, pt.cex = 2, pt.bg = side.col,
        legend = c ("table to the left", "table to the right"))
dummy <- dev.off ()

fm <- lmer (threshold ~ background * table.side + (1 | subject),
            flip.table.obj)
show (anova (fm))
show (fixef (fm))
show (ranef (fm))
### ***** Effect of scene side and background on horizontal detection
flip.table.hor <- subset (flip.table, stimulus == "horizontal")
idx <- which (flip.table.hor$table.side == "right")
flip.table.hor$threshold [idx] <- -flip.table.hor$threshold [idx]

pdf (file = "flip-table-horizontal.pdf")
par (mar = c (4, 5, 0.5, 0))
for (i in c (1, 2)) {
    boxplot (threshold ~ table.side * background, flip.table.hor,
             frame = FALSE, las = 1, xlab = "", ylab = boxplot.ylab,
             col = rep (side.col, 2), xaxt = "n",
             pars = list (boxwex  = 0.4, bty = "n"), add = (i == 2))
    if (i == 1)
        polygon (c (2.5, 4.5, 4.5, 2.5), c (-50, -50, 50, 50),
                 col = "#eeeeee", border = NA)
}
axis (1, at = c (1.5, 3.5), tick = FALSE,
      labels = c ("static background", "rotating background"))
legend ("topleft", inset = 0.05, pch = 22, pt.cex = 2, pt.bg = side.col,
        legend = c ("table to the left", "table to the right"))
dummy <- dev.off ()

fm <- lmer (threshold ~ background * table.side + (1 | subject),
            flip.table.hor)
show (anova (fm))
show (fixef (fm))
show (ranef (fm))
