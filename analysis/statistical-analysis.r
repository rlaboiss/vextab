
### * Statistical analyses

### ** Load the necessary libraries

library (lme4)
library (lmerTest)

### ** Load the resutls

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

        ## **** Efect of inclination in satic backaground on horizontal estimation
        df <- subset (obj.stab.psycho,
                      stimulus == "horizontal" & background == "static")
        fm <- lmer (threshold ~ chair.num + (1|subject), df)

    }

    show (anova (fm))
    show (rand (fm))
    show (fixef (fm))
    show (ranef (fm))

}
