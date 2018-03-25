# This R script recreates the dataset and the plots from the Sense and
# Sensibility study on the diagnostic value of control chart rules for detection
# of shifts in time series data.
#
# Beware that when running the script for the first time, creating the simulated
# dataset may take a long time. On my Intel i5 2.5 Ghz processor with 8 Gb of
# ram, the simulations took a couple of hours.
# 
# The scripts was created with R v. 3.4.4 and depends on the tidyverse package 
# v. 1.2.1
#
# Author: Jacob Anhøj 
# Email: jacob@anhoej.net 
# Date: 2018-03-24 
# Licence: GNU General Public Licence v. 3

# Initialise and setup ----

# Load tidyverse package
library(tidyverse)

# Names of files to hold data
fileprefix <- 'data/spc_diagnostics'
datafile   <- paste0(fileprefix, '_data', '.rds')
plotfile   <- paste0(fileprefix, '_plots', '.RData')

# Set recalc to TRUE if you want to rerun the simulations.
recalc <- F

# If the data file doesn't exist, run calculations anyway.
if(!file.exists(datafile))
  recalc <- TRUE

# Helper functions ----

# Diagnostic tests
diag.test <- function(x, y) {
  x      <- as.logical(x)
  y      <- as.logical(y)
  tbl    <- table(x, y)
  sens   <- tbl[2, 2] / (tbl[1, 2] + tbl[2, 2])
  spec   <- tbl[1, 1] / (tbl[1, 1] + tbl[2, 1])
  npv    <- tbl[1, 1] / (tbl[1, 1] + tbl[1, 2])
  ppv    <- tbl[2, 2] / (tbl[2, 1] + tbl[2, 2])
  lr.pos <- sens / (1 - spec)                     # TP / FP
  lr.neg <- (1 - sens) / spec                     # FN / TN
  
  return(list(
    tbl    = tbl,
    sens   = sens,
    spec   = spec,
    ppv    = ppv,
    npv    = npv,
    lr.pos = lr.pos,
    lr.neg = lr.neg))
}

# SPC analysis
zone.test <- function(x, z, n, m) {
  x1 <- x > z
  x2 <- x < -z
  v1 <- v2 <- vector(length = length(x) - m)
  for(i in 1:(length(x) - m + 1)) {
    v1[i] <- sum(x1[i:(i + m - 1)])
    v2[i] <- sum(x2[i:(i + m - 1)])
  }
  return(max(v1, v2) > n - 1)
}
# zone.test <- function(x, z, n, m) {
#   x1 <- x > z
#   x2 <- x < -z
#   v1 <- v2 <- vector(length = length(x) - m)
#   for(i in 1:(length(x) - m)) {
#     v1[i] <- sum(x1[i:(i + m - 1)])
#     v2[i] <- sum(x2[i:(i + m - 1)])
#   }
#   return(max(v1, v2) > n - 1)
# }

spc <- function(x) {
  n                 <- length(x)
  run               <- sign(x)
  run               <- run[run != 0]
  n.useful          <- length(run)
  run.lengths       <- rle(run)$lengths
  n.runs            <- length(run.lengths)
  n.crossings       <- n.runs - 1
  longest.run       <- max(run.lengths)
  we1.signal        <- zone.test(x, 3, 1, 1)
  we2.signal        <- zone.test(x, 2, 2, 3)
  we3.signal        <- zone.test(x, 1, 4, 5)
  we4.signal        <- zone.test(x, 0, 8, 8)
  crossings.signal  <- n.crossings < qbinom(0.05, n.useful - 1, 0.5)
  runs.signal.floor <- longest.run > log2(n.useful) + 3
  runs.signal.round <- longest.run > round(log2(n.useful) + 3)
  Anhoej            <- runs.signal.round | crossings.signal
  we1               <- we1.signal
  we12              <- we1.signal | we2.signal
  we123             <- we1.signal | we2.signal | we3.signal
  we1234            <- we1.signal | we2.signal | we3.signal | we4.signal
  we1Anhoej         <- we1.signal | Anhoej
  
  result <- data.frame(
    n                 = n,
    n.useful          = n.useful,
    n.runs            = n.runs,
    n.crossings       = n.crossings,
    longest.run       = longest.run,
    we1.signal        = we1.signal,
    we2.signal        = we2.signal,
    we3.signal        = we3.signal,
    we4.signal        = we4.signal,
    crossings.signal  = crossings.signal,
    runs.signal.floor = runs.signal.floor,
    runs.signal.round = runs.signal.round,
    Anhoej            = Anhoej,
    we1               = we1,
    we12              = we12,
    we123             = we123,
    we1234            = we1234,
    we1Anhoej         = we1Anhoej
  )
  
  return(result)
}

# Simulate data ----

# Function to simulate a single time series and do spc analysis
spc.sim <- function(k = 10, shift = 0){
  y          <- rnorm(k, shift)
  y          <- spc(y)
  y['shift'] <- shift
  return(y)
}

# Function to repeat spc.sim()
rep.sim <- function(r = 10, k = c(10, 20, 40), shifts = seq(0, 3, 0.2)) {
  d <- data.frame()
  for(i in shifts) {
    for(n in k) {
      x <- t(replicate(r, spc.sim(k = n, shift = i), simplify = TRUE))
      d <- rbind(d, x)
    }
  }
  d <- as.data.frame(sapply(d, unlist))
  return(d)
}

# Create dataset if recalc is TRUE or read dataset from from file
system.time({
  if (recalc) {
    d <- rep.sim(r = 10000, k = seq(10, 40, by = 2), shifts = seq(0, 3, by = 0.2))
    saveRDS(d, datafile)
  } else {
    d <- readRDS(datafile)
  }
})

# Create plots ----

# Power functions
pw <- d %>%
  filter(n %in% c(10, 20, 40)) %>% 
  select(n, Anhoej:shift) %>%
  group_by(n, shift) %>%
  summarise_all(funs(mean)) %>%
  gather(Rule, p, -n, -shift) %>%
  ungroup() %>%
  mutate(n = paste('k =', n),
         Rule = reorder(Rule, -p, median))

p1 <- ggplot(pw, aes(shift, p, colour = Rule)) +
  geom_point(colour = 'transparent') +
  geom_smooth(se = F, span = 0.5) +
  facet_wrap(~ n, ncol = 1) +
  theme_minimal() +
  labs(title = 'Power Function of Control Chart Rules',
       y = 'Probability of detecting a shift',
       x = 'Shift in standard deviation units') +
  scale_colour_brewer(palette = "Dark2")

p1b <- ggplot(filter(pw, n == 'k = 10'), aes(shift, p, colour = Rule)) +
  geom_point(colour = 'transparent') +
  geom_smooth(se = F, span = 0.5) +
  facet_wrap(~ n, ncol = 1) +
  theme_minimal() +
  labs(title = 'Power Function of Control Chart Rules',
       y = 'Probability of detecting a shift',
       x = 'Shift in standard deviation units') +
  scale_colour_brewer(palette = "Dark2")


# False positive rates
fp <- d %>%
  filter(shift == 0) %>% 
  select(n, Anhoej:we1Anhoej) %>%
  group_by(n) %>%
  summarise_all(funs(mean)) %>%
  gather(Rule, p, -n) %>%
  ungroup() %>%
  mutate(Rule = reorder(Rule, -p, median))

p2 <- ggplot(fp, aes(n, p, colour = Rule)) +
  geom_point(colour = 'transparent') +
  geom_step(size = 0.8) +
  theme_minimal() +
  labs(title = 'False Alarm Rates of Control Chart Rules',
       y = 'Probability of false alarm',
       x = 'Number of data points in chart') +
  scale_colour_brewer(palette = "Dark2")


# Likelihood ratios
lr <- d %>%
  filter(shift %in% c(0, 2), n %in% c(10, 20, 40)) %>%
  select(n, Anhoej:shift) %>% 
  gather(rule, signal, -n, -shift) %>%
  group_by(n, rule) %>%
  summarise('LR+' = diag.test(signal, shift)$lr.pos,
            'LR-' = diag.test(signal, shift)$lr.neg) %>%
  gather(test, val, -(1:2))%>%
  ungroup() %>%
  mutate(n = paste('k =', n))

p3 <- ggplot(lr,
             aes(rule, val, col = test)) +
  geom_linerange(aes(ymin = 1, ymax = val),
                 size = 1) +
  geom_errorbar(aes(ymin = val, ymax = val),
                size = 1,
                width = 0.2,
                show.legend = F) +
  geom_text(aes(label = signif(val, 2)),
            col = 1,
            size = 3,
            adj = 0,
            vjust = 'inward',
            nudge_x = 0.15) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format())) +
  facet_wrap(~ n, ncol = 1) +
  theme_minimal() +
  theme(legend.key = element_blank(), legend.title = element_blank()) +
  guides(col = guide_legend(reverse = TRUE)) +
  labs(title = 'Diagnostic Value of Control Chart Rules (shift = 2 SD)',
       x = 'Rule',
       y = 'Likelihood Ratio') +
  scale_colour_brewer(palette = "Dark2")


# save plots to file
plots <- ls()[map_lgl(ls(), function(x) inherits(get(x), 'ggplot'))]
save(list = plots, file = plotfile)
