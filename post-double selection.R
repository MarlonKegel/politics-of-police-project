library(foreign)
library(haven)
library(tidyverse)
library(cobalt)
library(MatchIt)
library(stringr)
library(plm)
library(caret)
library(tidymodels)
library(VGAM)
library(car)
library(stargazer)
library(sandwich)
library(AER)
library(corrplot)
library(data.table)
library(zoo)
library(purrr)
library(sjPlot)
library(gt)
library(lme4)
library(visdat)
library(fixest)
library(glmnet)
library(doParallel)

############ FINAL PROJECT STAT3105 ###############


GSS_cont <- read.csv("/Users/marlonkegel/Desktop/uni/S8/Social Data Analysis II/final_project/data/GSS data/GSS_cont.csv")

GSS_cont <- GSS_cont %>% mutate(privsec = case_when(
  OCC10 %in% c(3930, 3945, 3840) ~ 1, 
  is.na(OCC10) ~ NA,
  T ~ 0
), 
cop = case_when(
  OCC10 %in% c(3700, 3710, 3800, 3820, 3850, 3860, 3870) | 
    ISCO08 %in% c(3355, 1112, 1349, 5412, 5413) ~ 1, 
  is.na(OCC10) & is.na(ISCO08) ~ NA, 
  T ~ 0
), 
boundary = case_when(
  cop == 1 ~ "cop", 
  privsec == 1 ~ "private security", 
  is.na(cop) & is.na(privsec) ~ NA, 
  T ~ "neither"
))


GSS_out <- read.csv("/Users/marlonkegel/Desktop/uni/S8/Social Data Analysis II/final_project/data/GSS data/GSS_out.csv")


simple <- read.csv("/Users/marlonkegel/Desktop/uni/S8/Social Data Analysis II/final_project/data/GSS data/simple.csv")
conts <- GSS_cont %>% select("ID_", "privsec", "boundary", "YEAR", "PADEG", "PAWRKSLF", "FAMILY16", "BORN", "PARBORN", "ATTEND", "HAPPY", "SATFIN", "PRESTG10", "PAPRES10")
simple <- simple %>% left_join(conts, by = c("ID_", "YEAR"))

conts <- colnames(simple)
simple <- GSS_cont %>% select(any_of(conts)) %>% relocate(privsec, boundary, .after = cop)


#simple <- simple %>% mutate(AGE2 = AGE^2, 
#                            LOGINC = log(REALINC))

#vis_dat(simple, warn_large_data = FALSE)
simple$AGE <- as.numeric(simple$AGE)
simple$PRESTG10 <- as.numeric(simple$PRESTG10)
simple$PAPRES10 <- as.numeric(simple$PAPRES10)

sum(simple$cop == 1, na.rm = T)
sum(simple$privsec == 1, na.rm = T)

sum(is.na(simple$PAPRES10))
sum(is.na(simple$PASEI10))

simple <- simple %>% select(!c(PASEI10, X))

controls <- simple %>% select(!c(1, 3, 4, 5))

### BIN all continuous variables for later

# YEAR
hist(simple$YEAR) 
length(levels(as.factor(simple$YEAR))) # maybe 20 bins
num_bins <- 15

breaks <- quantile(simple$YEAR, probs = 0:num_bins / num_bins)
labels <- as.character(as.vector(breaks[-(num_bins+1)]))

simple$YEAR_bin <- cut(simple$YEAR, breaks = breaks, labels = labels, include.lowest = TRUE)

plot(simple$YEAR_bin)
table(simple$YEAR_bin)

# AGE
hist(simple$AGE) # 
range(simple$AGE, na.rm = T)
length(levels(as.factor(simple$AGE)))

num_bins <- 20 # maybe 20 bins

breaks <- quantile(simple$AGE, probs = 0:num_bins / num_bins, na.rm = T)
labels <- as.character(as.vector(breaks[-(num_bins+1)]))

simple$AGE_bin <- cut(simple$AGE, breaks = breaks, labels = labels, include.lowest = TRUE)

plot(simple$AGE_bin)
table(simple$AGE_bin)



## REALINC

hist(simple$REALINC) # really skewed, but differences are important...
length(levels(as.factor(simple$REALINC)))
num_bins <- 30

breaks <- quantile(simple$REALINC, probs = 0:num_bins / num_bins, na.rm = T)
labels <- as.character(as.vector(round(breaks[-(num_bins+1)], digits = 0)))

simple$REALINC_bin <- cut(simple$REALINC, breaks = breaks, labels = labels, include.lowest = TRUE)

plot(simple$REALINC_bin)
table(simple$REALINC_bin)


## PRESTG10

hist(simple$PRESTG10) 
length(levels(as.factor(simple$PRESTG10)))
num_bins <- 15

breaks <- quantile(simple$PRESTG10, probs = 0:num_bins / num_bins, na.rm = T)
labels <- as.character(as.vector(breaks[-(num_bins+1)]))

simple$PRESTG10_bin <- cut(simple$PRESTG10, breaks = breaks, labels = labels, include.lowest = TRUE)

plot(simple$PRESTG10_bin)
table(simple$PRESTG10_bin)


## PAPRES10

hist(simple$PAPRES10) 
length(levels(as.factor(simple$PAPRES10)))
num_bins <- 14
  
breaks <- quantile(simple$PAPRES10, probs = 0:num_bins / num_bins, na.rm = T)
labels <- as.character(as.vector(breaks[-(num_bins+1)]))

simple$PAPRES10_bin <- cut(simple$PAPRES10, breaks = breaks, labels = labels, include.lowest = TRUE)

plot(simple$PAPRES10_bin)
table(simple$PAPRES10_bin)



## clean version for LASSO selection

simple_clean <- na.omit(simple)
sum(simple_clean$cop == 1)
sum(simple_clean$privsec == 1)

#vis_dat(simple_clean, warn_large_data = FALSE)


#### DUMMY ENCODING OF CATEGORICAL VARS

identify <- simple_clean %>% select(c(1, 2, 4, 5))
simple_clean <- simple_clean %>% select(!c(1, 2, 4, 5, 7, 9, 27, 28)) # won't use ID and excluded all continuous (keeping the binned ones)
simple_clean <- simple_clean %>% mutate(across(c(2:ncol(.)), as.character))

simple_clean$cop <- as.integer(simple_clean$cop)

blueprint <- recipe(cop ~ ., data = simple_clean) %>%
  
  step_string2factor(all_nominal_predictors()) %>% # as factors
  
  step_other(all_nominal_predictors(), threshold = 0.01, other = "other") %>%  # lumping categories that represent less than 1%
  
  step_dummy(all_nominal_predictors()) # dummy encoding all predictors
  
  # %>% step_nzv(all_predictors()) # removing predictors with near-zero variance


blueprint_prep <- prep(blueprint, training = simple_clean)

simple_clean <- bake(blueprint_prep, new_data = simple_clean)

simple_clean$ID_ <- identify$ID_
simple_clean$YEAR <- identify$YEAR
simple_clean$privsec <- identify$privsec
simple_clean$boundary <- identify$boundary


##########################################################################################################
##########################################################################################################


############### POST-DOUBLE SELECTION (PDS)

#### Defining the set of controls 

X <- simple_clean %>% select(!c(1, 172:175)) # omitting cop, ID_, privsec, and boundary
main_effects <- colnames(X)
X <- model.matrix( ~ .^2 - 1, data = X) # all 2-way interactions (with dummy that's fully sat)

X <- X[, colSums(X) > 0] # removing empty columns

intact_effects <- setdiff(colnames(X), main_effects) # identify interaction effects
X_int <- X[, intact_effects]

num_cores <- detectCores() - 2
chunks <- split(seq_len(ncol(X_int)), ceiling(seq_len(ncol(X_int)) / (ncol(X_int) / num_cores))) # parallel computation
nzv_list <- mclapply(chunks, function(indices) {
  sub_X <- X_int[, indices, drop = FALSE]
  nearZeroVar(sub_X, saveMetrics = TRUE, freqCut = 200, uniqueCut = 0.01)
}, mc.cores = num_cores)
nzv <- do.call(rbind, nzv_list)

X_int <- X_int[ ,!nzv$nzv] # remove interaction terms with near-zero variance

keep <- colnames(X_int)
keep <- c(main_effects, keep)

X <- X[ , keep] # keep all main effects and non-NZV interaction terms

ncol(X)

X <- Matrix(X, sparse = TRUE) # turn into sparse matrix for faster computation


###### Select all controls that affect TREATMENT (cop)

D1 <- simple_clean$cop # treatment 1

# LASSO the controls on treatment
registerDoParallel(cores = parallel::detectCores()) # parallel cores for faster computation
cv_d1 <- cv.glmnet(X, D1, alpha = 1, parallel = T)
beta_d1 <- coef(cv_d1, s = "lambda.min")[-1]
selected_d1 <- which(beta_d1 != 0)

cols_d1 <- colnames(X)[selected_d1] #selected variables

selected_main_d1 <- unlist(strsplit(cols_d1, ":"))
selected_main_d1 <- unique(selected_main_d1)  # all main effects

non_dummy_d1 <- unlist(strsplit(selected_main_d1, "_"))
non_dummy_d1 <- non_dummy_d1[!str_detect(non_dummy_d1, "X[0-9]+")]
non_dummy_d1 <- non_dummy_d1[!str_detect(non_dummy_d1, "bin")]
non_dummy_d1 <- unique(non_dummy_d1) # non-dummy variables

setdiff(colnames(controls), non_dummy_d1) # all of them...



###### Select all controls that affect TREATMENT #2 (private security)

D2 <- simple_clean$privsec # treatment 2

# LASSO the controls on treatment
registerDoParallel(cores = parallel::detectCores()) # parallel cores for faster computation
cv_d2 <- cv.glmnet(X, D2, alpha = 1, parallel = T)
beta_d2 <- coef(cv_d2, s = "lambda.min")[-1]
selected_d2 <- which(beta_d2 != 0)

cols_d2 <- colnames(X)[selected_d2] #selected variabled

selected_main_d2 <- unlist(strsplit(cols_d2, ":"))
selected_main_d2 <- unique(selected_main_d2)  # all main effects either selected or involved in any of the selected interactions

non_dummy_d2 <- unlist(strsplit(selected_main_d2, "_"))
non_dummy_d2 <- non_dummy_d2[!str_detect(non_dummy_d2, "X[0-9]+")]
non_dummy_d2 <- non_dummy_d2[!str_detect(non_dummy_d2, "bin")]
non_dummy_d2 <- unique(non_dummy_d2) # non-dummy variables

setdiff(colnames(controls), non_dummy_d2) # All but: "YEAR", "DEGREE", "RELIG", "PAWRKSLF", "FAMILY16", "BORN", "PARBORN"

##########################################################################################################
##########################################################################################################


####### Select all controls that affect OUTCOME

###### POLVIEWS
polviews <- GSS_out %>% select(c(ID_, YEAR, POLVIEWS)) %>% right_join(simple_clean, by = join_by(ID_, YEAR))
polviews <- na.omit(polviews)
polviews <- polviews %>% relocate(cop, privsec, boundary, .after = POLVIEWS)

sum(polviews$cop == 1)

## redefine control matrix (bc we lost some obs by omitting those with NA in POLVIEW)
X <- polviews[ ,(7:ncol(polviews))]
X <- model.matrix( ~ .^2 - 1, data = X) # all 2-way interactions (with dummy that's fully sat)
X <- X[ , keep] # keep all main effects and non-NZV interaction terms (this should be the same as above)

X <- Matrix(X, sparse = TRUE) # turn into sparse matrix for faster computation

Y <- polviews$POLVIEWS # outcome

## lasso the controls on the outcome
registerDoParallel(cores = parallel::detectCores()) # parallel cores for faster computation
cv_y <- cv.glmnet(X, Y, alpha = 1, parallel = T)
beta_y <- coef(cv_y, s = "lambda.min")[-1]
selected_y <- which(beta_y != 0)

cols_y <- colnames(X)[selected_y] # selected variables (interactions and main effects)

selected_main_y <- unlist(strsplit(cols_y, ":"))
selected_main_y <- unique(selected_main_y) # all variables involved whether main or interacted

non_dummy_y <- unlist(strsplit(selected_main_y, "_"))
non_dummy_y <- non_dummy_y[!str_detect(non_dummy_y, "X[0-9]+")]
non_dummy_y <- non_dummy_y[!str_detect(non_dummy_y, "bin")]
non_dummy_y <- unique(non_dummy_y) 

setdiff(colnames(controls), non_dummy_y) # All of them.


##### MATCHING! (mahalanobis)

polviews <- GSS_out %>% select(c(ID_, YEAR, POLVIEWS)) %>% right_join(simple, by = join_by(ID_, YEAR))
polviews <- na.omit(polviews) 

#non_dummy <- unique(non_dummy_d, non_dummy_y)
#formula <- paste(non_dummy, collapse = ") + factor(")

# matching on all of them, since all were selected...

m.out <- matchit(
  cop ~ factor(RACE) + factor(DEGREE) + factor(MARITAL) + factor(REG16) + factor(RELIG) + 
  PRESTG10 + PAPRES10 + factor(SEX) + factor(ATTEND) + AGE + 
  REALINC + factor(REGION) + factor(FAMILY16) + factor(MADEG) + factor(XNORCSIZ) + 
  factor(WRKSTAT) + factor(PADEG) + YEAR + factor(WRKSLF) + factor(PAWRKSLF) + factor(SATFIN) + 
  factor(HAPPY) + factor(BORN) + factor(PARBORN), 
  exact = ~ factor(RACE) + factor(DEGREE) + factor(SEX) + factor(YEAR_bin) +              # matching exact 
    factor(AGE_bin) + factor(REALINC_bin),                                                # on the most important ones
  data = polviews,
  method = "nearest",
  link = "logit",
  distance = "mahalanobis",
  replace = T, 
  reuse.max = 10
)

# plot(m.out, type = "density", which.xs = ~.)
# love.plot(m.out, stats = "variance.ratios")

matched_polviews <- match.data(m.out)
matched_polviews <- as.data.frame(matched_polviews)

length(unique(matched_polviews$ID_, matched_polviews$YEAR))

sum <- summary(m.out)
sum[["nn"]]
sum <- sum[["sum.matched"]]
sum <- as.data.frame(sum)
sum$`Std. Mean Diff.` <- abs(sum$`Std. Mean Diff.`)
head(arrange(sum, desc(`Std. Mean Diff.`)), 20)


##### THE MODEL

## on the matched data

# cols <- c(cols_d, cols_y)
# cols <- unique(cols)

# selected_main <- c(selected_main_d, selected_main_y)
# selected_main <- unique(selected_main)

# formula <- as.formula(paste(c(c("POLVIEWS ~ cop"), selected_main, cols), collapse = " + "))

# adding controls where we didn't achieve good balance. 

mod <- lm(POLVIEWS ~ cop + factor(WRKSLF) + factor(WRKSTAT) + PRESTG10 + 
            factor(SATFIN) + factor(PADEG) + factor(MADEG) + factor(RELIG) + 
            factor(ATTEND) + factor(MARITAL), data = matched_polviews, weights = weights)

sum <- summary(mod)

coef <- as.data.frame(coef(sum))

coef <- coef %>% mutate(signif = case_when(`Pr(>|t|)` < 0.001 ~ "***", 
                                           `Pr(>|t|)` < 0.01 ~ "**",
                                           `Pr(>|t|)` < 0.05 ~ "*",
                                           `Pr(>|t|)` < 0.1 ~ ".",
                                           T ~ " "))

print(coef[1:20, ])

#                   Estimate     Std. Error   t value   Pr(>|t|)     signif
# (Intercept)       3.974629547 0.53165600  7.47594220 7.457352e-13    ***
# cop               0.392215566 0.16809257  2.33333080 2.025188e-02      *
# factor(WRKSLF)2  -0.172887160 0.32053552 -0.53936973 5.900080e-01       
# factor(WRKSTAT)2  0.040672079 0.36636383  0.11101554 9.116738e-01       

## Sample size of approx. 180


##### On the entire data, only with the LASSO selected interactions 
# (and all the main effects that are involved in interactions)


#### COP

polviews <- GSS_out %>% select(c(ID_, YEAR, POLVIEWS)) %>% right_join(simple, by = join_by(ID_, YEAR))
polviews <- na.omit(polviews) 
polviews <- polviews %>% select(ID_, YEAR, POLVIEWS, AGE, REALINC, YEAR, PRESTG10) %>% 
  left_join(simple_clean, by = join_by(ID_, YEAR)) %>% 
  filter(privsec != 1)

main_effects <- c(selected_main_d1, selected_main_y)
main_effects <- unique(main_effects)

int_effects <- c(cols_y[str_detect(cols_y, ".+:.+")], cols_d1[str_detect(cols_d1, ".+:.+")])
int_effects <- unique(int_effects[!is.na(int_effects)])

cols <- c(main_effects, int_effects)

formula <- paste("POLVIEWS ~ cop + AGE + REALINC + YEAR + PRESTG10 +", paste(collapse = " + ", cols))

mod1 <- lm(formula, data = polviews)

sum <- summary(mod1)

coef <- as.data.frame(coef(sum))

coef <- coef %>% mutate(signif = case_when(`Pr(>|t|)` < 0.001 ~ "***", 
                                           `Pr(>|t|)` < 0.01 ~ "**",
                                           `Pr(>|t|)` < 0.05 ~ "*",
                                           `Pr(>|t|)` < 0.1 ~ ".",
                                           T ~ " "))

print(coef[1:20, ])

# (Intercept)      -1.670400e+01 1.525455e+01 -1.09501789 2.735168e-01       
# cop               3.639222e-01 7.925769e-02  4.59163260 4.414215e-06    ***
# AGE               1.295697e-02 6.279498e-03  2.06337626 3.908474e-02      *
# REALINC          -1.741968e-06 2.509522e-06 -0.69414349 4.875971e-01       
# YEAR              1.015353e-02 7.714001e-03  1.31624635 1.881006e-01       
# PRESTG10          1.312055e-02 4.474533e-03  2.93227166 3.367245e-03     **
# ...


vglm_mod1 <- vglm(formula, data = polviews, family=cumulative(parallel=T))

coef <- coef(vglm_mod1)

se <- sqrt(diag(vcov(vglm_mod1)))
z_values <- coef / se
p_values <- 2 * pnorm(-abs(z_values))

sum <- data.frame(
  Coefficient = coef,
  StdError = se,
  zValue = z_values,
  pValue = p_values
)

sum <- sum %>% mutate(signif = case_when(pValue < 0.001 ~ "***", 
                                           pValue < 0.01 ~ "**",
                                           pValue < 0.05 ~ "*",
                                           pValue < 0.1 ~ ".",
                                           T ~ " "))

print(sum[1:20, ])


# (Intercept):1     2.156567e+01 2.095995e+01  1.02889881 3.035272e-01       
# (Intercept):2     2.343348e+01 2.095996e+01  1.11801153 2.635621e-01       
# (Intercept):3     2.434265e+01 2.095998e+01  1.16138673 2.454847e-01       
# (Intercept):4     2.612095e+01 2.096005e+01  1.24622519 2.126817e-01       
# (Intercept):5     2.708922e+01 2.096008e+01  1.29241972 1.962118e-01       
# (Intercept):6     2.915370e+01 2.096013e+01  1.39091233 1.642520e-01       
# cop              -5.343163e-01 1.101748e-01 -4.84971513 1.236389e-06    ***
# AGE              -1.791246e-02 8.719315e-03 -2.05434264 3.994254e-02      *
# REALINC          -1.528850e-06 1.468718e-06 -1.04094181 2.979026e-01       
# YEAR             -1.216021e-02 1.059840e-02 -1.14736241 2.512319e-01       
# ...



############ PRIVATE SECURITY

polviews <- GSS_out %>% select(c(ID_, YEAR, POLVIEWS)) %>% right_join(simple, by = join_by(ID_, YEAR))
polviews <- na.omit(polviews) 
polviews <- polviews %>% select(ID_, YEAR, POLVIEWS, AGE, REALINC, YEAR, PRESTG10) %>% 
  left_join(simple_clean, by = join_by(ID_, YEAR)) %>% 
  filter(cop != 1)

main_effects <- c(selected_main_d2, selected_main_y)
main_effects <- unique(main_effects)

int_effects <- c(cols_y[str_detect(cols_y, ".+:.+")], cols_d2[str_detect(cols_d2, ".+:.+")])
int_effects <- unique(int_effects[!is.na(int_effects)])

cols <- c(main_effects, int_effects)

formula <- paste("POLVIEWS ~ privsec + AGE + REALINC + YEAR + PRESTG10 + ", paste(collapse = " + ", cols))

mod2 <- lm(formula, data = polviews)

sum <- summary(mod2)

coef <- as.data.frame(coef(sum))

coef <- coef %>% mutate(signif = case_when(`Pr(>|t|)` < 0.001 ~ "***", 
                                           `Pr(>|t|)` < 0.01 ~ "**",
                                           `Pr(>|t|)` < 0.05 ~ "*",
                                           `Pr(>|t|)` < 0.1 ~ ".",
                                           T ~ " "))

print(coef[1:20, ])

# (Intercept)       -1.853419e+01 1.528002e+01 -1.2129689 0.225150480       
# privsec            9.961465e-02 1.239601e-01  0.8036024 0.421632488       
# AGE                1.298431e-02 6.292930e-03  2.0633172 0.039090370      *
# REALINC           -2.241029e-06 2.511186e-06 -0.8924182 0.372175356       
# YEAR               1.103798e-02 7.726949e-03  1.4285042 0.153156337       
# PRESTG10           1.317514e-02 4.476504e-03  2.9431764 0.003250912     **
# ...




##########################################################################################################
##########################################################################################################


######## PARTYID

partyid <- GSS_out %>% select(c(ID_, YEAR, PARTYID)) %>% right_join(simple_clean, by = join_by(ID_, YEAR))
partyid <- na.omit(partyid)
partyid <- partyid %>% relocate(cop, privsec, boundary, .after = PARTYID)

sum(partyid$cop == 1)

## redefine control matrix (bc we lost some obs by omitting those with NA in POLVIEW)
X <- partyid[ ,(7:ncol(partyid))]
X <- model.matrix( ~ .^2 - 1, data = X) # all 2-way interactions (with dummy that's fully sat)
X <- X[ , keep] # keep all main effects and non-NZV interaction terms (this should be the same as above)

X <- Matrix(X, sparse = TRUE) # turn into sparse matrix for faster computation

Y <- partyid$PARTYID # outcome

## lasso the controls on the outcome
registerDoParallel(cores = parallel::detectCores()) # parallel cores for faster computation
cv_y <- cv.glmnet(X, Y, alpha = 1, parallel = T)
beta_y <- coef(cv_y, s = "lambda.min")[-1]
selected_y <- which(beta_y != 0)

cols_y <- colnames(X)[selected_y] # selected variables (interactions and main effects)

selected_main_y <- unlist(strsplit(cols_y, ":"))
selected_main_y <- unique(selected_main_y) # all variables involved whether main or interacted

non_dummy_y <- unlist(strsplit(selected_main_y, "_"))
non_dummy_y <- non_dummy_y[!str_detect(non_dummy_y, "X[0-9]+")]
non_dummy_y <- non_dummy_y[!str_detect(non_dummy_y, "bin")]
non_dummy_y <- unique(non_dummy_y) 

setdiff(colnames(controls), non_dummy_y) # All of them.


#### MODEL controlling for the LASSO selected variables

### COP

partyid <- GSS_out %>% select(c(ID_, YEAR, PARTYID)) %>% right_join(simple, by = join_by(ID_, YEAR))
partyid <- na.omit(partyid) 
partyid <- partyid %>% select(ID_, YEAR, PARTYID, AGE, REALINC, YEAR, PRESTG10) %>% 
  left_join(simple_clean, by = join_by(ID_, YEAR)) %>% 
  filter(privsec != 1)


main_effects <- c(selected_main_d1, selected_main_y)
main_effects <- unique(main_effects)

int_effects <- c(cols_y[str_detect(cols_y, ".+:.+")], cols_d1[str_detect(cols_d1, ".+:.+")])
int_effects <- unique(int_effects[!is.na(int_effects)])

cols <- c(main_effects, int_effects)

formula <- paste("PARTYID ~ cop + AGE + REALINC + YEAR + PRESTG10 +", paste(collapse = " + ", cols))

mod3 <- lm(formula, data = partyid)

sum <- summary(mod3)

coef <- as.data.frame(coef(sum))

coef <- coef %>% mutate(signif = case_when(`Pr(>|t|)` < 0.001 ~ "***", 
                                           `Pr(>|t|)` < 0.01 ~ "**",
                                           `Pr(>|t|)` < 0.05 ~ "*",
                                           `Pr(>|t|)` < 0.1 ~ ".",
                                           T ~ " "))

print(coef[1:20, ])

# (Intercept)       6.181363e+00 2.126735e+01  0.29065034 7.713205e-01       
# cop               3.878415e-01 1.120584e-01  3.46106699 5.386975e-04    ***
# AGE              -1.209796e-03 8.771454e-03 -0.13792423 8.903011e-01       
# REALINC           3.023647e-07 3.560635e-06  0.08491878 9.323265e-01       
# YEAR             -1.928587e-03 1.075443e-02 -0.17932945 8.576801e-01       
# PRESTG10          1.691431e-02 6.297425e-03  2.68590960 7.236768e-03     **
# ...

mem.maxVSize(9999999999)
vglm_mod3 <- vglm(formula, data = partyid, family=cumulative(parallel=T))

coef <- coef(vglm_mod3)

se <- sqrt(diag(vcov(vglm_mod3)))
z_values <- coef / se
p_values <- 2 * pnorm(-abs(z_values))

sum <- data.frame(
  Coefficient = coef,
  StdError = se,
  zValue = z_values,
  pValue = p_values
)

sum <- sum %>% mutate(signif = case_when(pValue < 0.001 ~ "***", 
                                         pValue < 0.01 ~ "**",
                                         pValue < 0.05 ~ "*",
                                         pValue < 0.1 ~ ".",
                                         T ~ " "))

print(sum[1:20, ])

# (Intercept):1     2.994489e+00 2.050700e+01  0.1460227 8.839034e-01       
# (Intercept):2     4.264408e+00 2.050700e+01  0.2079489 8.352689e-01       
# (Intercept):3     4.889289e+00 2.050701e+01  0.2384204 8.115550e-01       
# (Intercept):4     5.577280e+00 2.050701e+01  0.2719694 7.856455e-01       
# (Intercept):5     6.112867e+00 2.050702e+01  0.2980866 7.656371e-01       
# (Intercept):6     7.301586e+00 2.050703e+01  0.3560528 7.218010e-01       
# (Intercept):7     9.533257e+00 2.050706e+01  0.4648767 6.420198e-01       
# cop              -3.760613e-01 1.078570e-01 -3.4866650 4.890835e-04    ***
# AGE               1.472762e-03 8.472558e-03  0.1738273 8.620012e-01       
# REALINC          -4.933945e-07 3.427430e-06 -0.1439546 8.855363e-01       
# YEAR             -2.282518e-03 1.036992e-02 -0.2201094 8.257860e-01       
# PRESTG10         -1.764168e-02 6.086998e-03 -2.8982557 3.752445e-03     **



### PRIVATE SECURITY

partyid <- GSS_out %>% select(c(ID_, YEAR, PARTYID)) %>% right_join(simple, by = join_by(ID_, YEAR))
partyid <- na.omit(partyid) 
partyid <- partyid %>% select(ID_, YEAR, PARTYID, AGE, REALINC, YEAR, PRESTG10) %>% 
  left_join(simple_clean, by = join_by(ID_, YEAR)) %>% 
  filter(cop != 1)


main_effects <- c(selected_main_d2, selected_main_y)
main_effects <- unique(main_effects)

int_effects <- c(cols_y[str_detect(cols_y, ".+:.+")], cols_d2[str_detect(cols_d2, ".+:.+")])
int_effects <- unique(int_effects[!is.na(int_effects)])

cols <- c(main_effects, int_effects)

formula <- paste("PARTYID ~ privsec + AGE + REALINC + YEAR + PRESTG10 +", paste(collapse = " + ", cols))

mod4 <- lm(formula, data = partyid)

sum <- summary(mod4)

coef <- as.data.frame(coef(sum))

coef <- coef %>% mutate(signif = case_when(`Pr(>|t|)` < 0.001 ~ "***", 
                                           `Pr(>|t|)` < 0.01 ~ "**",
                                           `Pr(>|t|)` < 0.05 ~ "*",
                                           `Pr(>|t|)` < 0.1 ~ ".",
                                           T ~ " "))

print(coef[1:20, ])

# (Intercept)        6.954950e+00 2.128473e+01  0.32675779 7.438531e-01       
# privsec            1.832638e-01 1.737596e-01  1.05469692 2.915715e-01       
# AGE               -1.609909e-03 8.783214e-03 -0.18329386 8.545685e-01       
# REALINC           -1.331665e-07 3.559762e-06 -0.03740883 9.701592e-01       
# YEAR              -2.291483e-03 1.076332e-02 -0.21289736 8.314083e-01       
# PRESTG10           1.671632e-02 6.294633e-03  2.65564727 7.919328e-03     **
# ...



##########################################################################################################



tab_model(
  mod1, 
  mod3,
  terms = "cop",
  show.se = T, 
  show.p = T, 
  file = "~/Desktop/uni/thesis/GSS matching : stats final proj/DPS_cop.html"
)


tab_model(
  vglm_mod1, 
  vglm_mod3,
  terms = "cop",
  show.se = T, 
  show.p = T, 
  file = "~/Desktop/uni/thesis/GSS matching : stats final proj/DPS_cop_vglm.html"
)

tab_model(
  mod2, 
  mod4,
  terms = "privsec",
  pred.labels = "private security",
  show.se = T, 
  show.p = T, 
  file = "~/Desktop/uni/thesis/GSS matching : stats final proj/DPS_privsec.html"
)



nullmod1 <- vglm(POLVIEWS ~ cop, GSS_out, family=cumulative(parallel=T))
nullmod3 <- vglm(PARTYID ~ cop, GSS_out, family=cumulative(parallel=T))

tab_model(
  nullmod1, 
  nullmod3,
  terms = "cop",
  show.se = T, 
  show.p = T, 
  file = "~/Desktop/uni/thesis/GSS matching : stats final proj/cop_nullmod.html"
)

######## HELPPOOR

helppoor <- GSS_out %>% select(c(ID_, YEAR, HELPPOOR)) %>% right_join(simple_clean, by = join_by(ID_, YEAR))
helppoor <- na.omit(helppoor)

sum(helppoor$cop == 1)

## redefine control matrix (bc we lost some obs by omitting those with NA in HELPPOOR)
X <- helppoor[ ,(5:ncol(helppoor))]
X <- model.matrix( ~ .^2 - 1, data = X) # all 2-way interactions (with dummy that's fully sat)
X <- X[ , keep] # keep all main effects and non-NZV interaction terms (this should be the same as above, i.e. ~5500 variables)

X <- Matrix(X, sparse = TRUE) # turn into sparse matrix for faster computation

Y <- helppoor$HELPPOOR # outcome

## lasso the controls on the outcome
registerDoParallel(cores = parallel::detectCores()) # parallel cores for faster computation
cv_y <- cv.glmnet(X, Y, alpha = 1, parallel = T)
beta_y <- coef(cv_d, s = "lambda.min")[-1]
selected_y <- which(beta_y != 0)

cols_y <- colnames(X)[selected_y] # selected variables (interactions and main effects)

selected_main_y <- unlist(strsplit(cols_y, ":"))
selected_main_y <- unique(selected_main_y) # all variables involved whether main or interacted

non_dummy_y <- unlist(strsplit(selected_main_y, "_"))
non_dummy_y <- non_dummy_y[!str_detect(non_dummy_y, "X[0-9]+")]
non_dummy_y <- non_dummy_y[!str_detect(non_dummy_y, "bin")]
non_dummy_y <- unique(non_dummy_y) 

setdiff(colnames(controls), non_dummy_y) # again, all of them...


## On the entire data, only with the LASSO selected variables

helppoor <- GSS_out %>% select(c(ID_, YEAR, HELPPOOR)) %>% right_join(simple, by = join_by(ID_, YEAR))
helppoor <- na.omit(helppoor) 
helppoor <- helppoor %>% select(ID_, YEAR, HELPPOOR) %>% left_join(simple_clean, by = join_by(ID_, YEAR))

cols <- c(cols_d, cols_y)
cols <- unique(cols)

selected_main <- c(selected_main_d, selected_main_y)
selected_main <- unique(selected_main)

formula <- as.formula(paste(c(c("HELPPOOR ~ cop"), selected_main, cols), collapse = " + "))

mod <- lm(formula, data = helppoor)

sum <- summary(mod)

coef <- as.data.frame(coef(sum))

coef <- coef %>% mutate(signif = case_when(`Pr(>|t|)` < 0.001 ~ "***", 
                                           `Pr(>|t|)` < 0.01 ~ "**",
                                           `Pr(>|t|)` < 0.05 ~ "*",
                                           `Pr(>|t|)` < 0.1 ~ ".",
                                           T ~ " "))

print(coef[1:20, ])

# (Intercept)       3.0799536373 0.07903542  38.96928315 6.205465e-321    ***
# cop               0.1101359774 0.07849452   1.40310411  1.606004e-01       
# RACE_X2          -0.6012426514 0.03436499 -17.49578937  4.619228e-68    ***
# DEGREE_X3         0.2798765020 0.03588572   7.79910604  6.520201e-15    ***
# MARITAL_X4        0.0779689295 0.05225146   1.49218663  1.356650e-01       
# ...



mod2 <- vglm(formula, data = helppoor, family=cumulative(parallel=T))

coef2 <- coef(mod2)

se <- sqrt(diag(vcov(mod2)))
z_values <- coef2 / se
p_values <- 2 * pnorm(-abs(z_values))

sum2 <- data.frame(
  Coefficient = coef2,
  StdError = se,
  zValue = z_values,
  pValue = p_values
)

sum2 <- sum2 %>% mutate(signif = case_when(pValue < 0.001 ~ "***", 
                                           pValue < 0.01 ~ "**",
                                           pValue < 0.05 ~ "*",
                                           pValue < 0.1 ~ ".",
                                           T ~ " "))

print(sum2[1:20, ])

# Coefficient   StdError      zValue       pValue signif
# (Intercept):1    -1.89038492 0.12146034 -15.5638040 1.282448e-54    ***
# (Intercept):2    -1.05545618 0.12089175  -8.7305891 2.533496e-18    ***
# (Intercept):3     1.01501562 0.12085877   8.3983612 4.527526e-17    ***
# (Intercept):4     2.14158558 0.12184082  17.5769137 3.701859e-69    ***
# cop              -0.15690181 0.12977101  -1.2090667 2.266372e-01       
# RACE_X2           1.05053943 0.05704659  18.4154641 9.873930e-76    ***
# DEGREE_X3        -0.58772487 0.05867134 -10.0172404 1.280278e-23    ***
# MARITAL_X4       -0.08268727 0.08671255  -0.9535791 3.402967e-01       
# ...




###### with all variables & 2 way interactions 

helppoor <- helppoor %>% select(ID_, YEAR, HELPPOOR) %>% left_join(simple, by = join_by(ID_, YEAR))

non_dummy <- c(non_dummy_d, non_dummy_y)
non_dummy <- unique(non_dummy)

formula <- paste(non_dummy, collapse = " + ")

twoway <- as.formula(paste("HELPPOOR ~ cop + (", formula, ")^", 2, sep = ""))

factors <- colnames(helppoor[ , -c(2, 6, 8, 26, 27)])

helppoor <- helppoor %>% mutate(across(all_of(factors), as.character))

mem.maxVSize(9999999)
mod <- lm(twoway, data = helppoor)

sum <- summary(mod)

coef <- as.data.frame(coef(sum))

coef <- coef %>% mutate(signif = case_when(`Pr(>|t|)` < 0.001 ~ "***", 
                                           `Pr(>|t|)` < 0.01 ~ "**",
                                           `Pr(>|t|)` < 0.05 ~ "*",
                                           `Pr(>|t|)` < 0.1 ~ ".",
                                           T ~ " "))

print(coef[1:20, ])

#                Estimate   Std. Error    t value    Pr(>|t|)  signif
# (Intercept)  -7.70992686 28.31577389 -0.2722838 0.7854069515       
# cop1          0.07692224  0.08066985  0.9535438 0.3403275127       
# RACE2         1.48489232  6.01267516  0.2469603 0.8049417557       
# RACE3        -2.71776557  8.02891809 -0.3384971 0.7349925856       
# DEGREE1       5.82361197  5.71767190  1.0185285 0.3084406521       
# ...






##########################################################################################################




######## EQWLTH

eqwlth <- GSS_out %>% select(c(ID_, YEAR, EQWLTH)) %>% right_join(simple_clean, by = join_by(ID_, YEAR))
eqwlth <- na.omit(eqwlth)

sum(eqwlth$cop == 1)

## redefine control matrix (bc we lost some obs by omitting those with NA in EQWLTH)
X <- eqwlth[ ,(5:ncol(eqwlth))]
X <- model.matrix( ~ .^2 - 1, data = X) # all 2-way interactions (with dummy that's fully sat)
X <- X[ , keep] # keep all main effects and non-NZV interaction terms (this should be the same as above, i.e. ~5500 variables)

X <- Matrix(X, sparse = TRUE) # turn into sparse matrix for faster computation

Y <- eqwlth$EQWLTH # outcome

## lasso the controls on the outcome
registerDoParallel(cores = parallel::detectCores()) # parallel cores for faster computation
cv_y <- cv.glmnet(X, Y, alpha = 1, parallel = T)
beta_y <- coef(cv_d, s = "lambda.min")[-1]
selected_y <- which(beta_y != 0)

cols_y <- colnames(X)[selected_y] # selected variables (interactions and main effects)

selected_main_y <- unlist(strsplit(cols_y, ":"))
selected_main_y <- unique(selected_main_y) # all variables involved whether main or interacted

non_dummy_y <- unlist(strsplit(selected_main_y, "_"))
non_dummy_y <- non_dummy_y[!str_detect(non_dummy_y, "X[0-9]+")]
non_dummy_y <- non_dummy_y[!str_detect(non_dummy_y, "bin")]
non_dummy_y <- unique(non_dummy_y) 

setdiff(colnames(controls), non_dummy_y) # again, all of them...


## On the entire data, only with the LASSO selected variables

eqwlth <- GSS_out %>% select(c(ID_, YEAR, EQWLTH)) %>% right_join(simple, by = join_by(ID_, YEAR))
eqwlth <- na.omit(eqwlth) 
eqwlth <- eqwlth %>% select(ID_, YEAR, EQWLTH) %>% left_join(simple_clean, by = join_by(ID_, YEAR))

cols <- c(cols_d, cols_y)
cols <- unique(cols)

selected_main <- c(selected_main_d, selected_main_y)
selected_main <- unique(selected_main)

formula <- as.formula(paste(c(c("EQWLTH ~ cop"), selected_main, cols), collapse = " + "))

mod <- lm(formula, data = eqwlth)

sum <- summary(mod)

coef <- as.data.frame(coef(sum))

coef <- coef %>% mutate(signif = case_when(`Pr(>|t|)` < 0.001 ~ "***", 
                                           `Pr(>|t|)` < 0.01 ~ "**",
                                           `Pr(>|t|)` < 0.05 ~ "*",
                                           `Pr(>|t|)` < 0.1 ~ ".",
                                           T ~ " "))

print(coef[1:20, ])

# (Intercept)         3.650757311 0.11808387  30.91664650 1.175407e-205    ***
# cop                 0.017975007 0.13114231   0.13706489  8.909807e-01       
# RACE_X2            -0.735121608 0.05673613 -12.95685228  2.927769e-38    ***
# DEGREE_X3           0.795056537 0.05799557  13.70891778  1.322042e-42    ***
# MARITAL_X4         -0.004383799 0.08683533  -0.05048405  9.597371e-01       
# ...


######## GOVEQINC

goveqinc <- GSS_out %>% select(c(ID_, YEAR, GOVEQINC)) %>% right_join(simple_clean, by = join_by(ID_, YEAR))
goveqinc <- na.omit(goveqinc)

sum(goveqinc$cop == 1)

## redefine control matrix (bc we lost some obs by omitting those with NA in GOVEQINC)
X <- goveqinc[ ,(5:ncol(goveqinc))]
X <- model.matrix( ~ .^2 - 1, data = X) # all 2-way interactions (with dummy that's fully sat)
X <- X[ , keep] # keep all main effects and non-NZV interaction terms (this should be the same as above, i.e. ~5500 variables)

X <- Matrix(X, sparse = TRUE) # turn into sparse matrix for faster computation

Y <- goveqinc$GOVEQINC # outcome

## lasso the controls on the outcome
registerDoParallel(cores = parallel::detectCores()) # parallel cores for faster computation
cv_y <- cv.glmnet(X, Y, alpha = 1, parallel = T)
beta_y <- coef(cv_d, s = "lambda.min")[-1]
selected_y <- which(beta_y != 0)

cols_y <- colnames(X)[selected_y] # selected variables (interactions and main effects)

selected_main_y <- unlist(strsplit(cols_y, ":"))
selected_main_y <- unique(selected_main_y) # all variables involved whether main or interacted

non_dummy_y <- unlist(strsplit(selected_main_y, "_"))
non_dummy_y <- non_dummy_y[!str_detect(non_dummy_y, "X[0-9]+")]
non_dummy_y <- non_dummy_y[!str_detect(non_dummy_y, "bin")]
non_dummy_y <- unique(non_dummy_y) 

setdiff(colnames(controls), non_dummy_y) # again, all of them...


## On the entire data, only with the LASSO selected variables

goveqinc <- GSS_out %>% select(c(ID_, YEAR, GOVEQINC)) %>% right_join(simple, by = join_by(ID_, YEAR))
goveqinc <- na.omit(goveqinc) 
goveqinc <- goveqinc %>% select(ID_, YEAR, GOVEQINC) %>% left_join(simple_clean, by = join_by(ID_, YEAR))

cols <- c(cols_d, cols_y)
cols <- unique(cols)

selected_main <- c(selected_main_d, selected_main_y)
selected_main <- unique(selected_main)

formula <- as.formula(paste(c(c("GOVEQINC ~ cop"), selected_main, cols), collapse = " + "))

mod <- lm(formula, data = goveqinc)

sum <- summary(mod)

coef <- as.data.frame(coef(sum))

coef <- coef %>% mutate(signif = case_when(`Pr(>|t|)` < 0.001 ~ "***", 
                                           `Pr(>|t|)` < 0.01 ~ "**",
                                           `Pr(>|t|)` < 0.05 ~ "*",
                                           `Pr(>|t|)` < 0.1 ~ ".",
                                           T ~ " "))

print(coef[1:20, ])

# (Intercept)         3.26469224 0.12696019 25.7142990 5.546538e-140    ***
# cop                 0.04371554 0.13190473  0.3314176  7.403381e-01       
# RACE_X2            -0.53855993 0.05728370 -9.4016259  6.945135e-21    ***
# DEGREE_X3           0.48188944 0.06147778  7.8384329  5.172165e-15    ***
# MARITAL_X4         -0.09467684 0.09541124 -0.9923028  3.210809e-01       
# ...



###### NATARMS



# ....