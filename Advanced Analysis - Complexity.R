# Packages ----------------------------------------------------------------
library(tidyverse)
library(statcomp)
library(ggrepel)
library(future.apply)

# load ts2225

# Get all metrics (entropy, complexity, fischer)  -------------------------
get.all.metrics <- function(x, D, q=FALSE, seq.to=30, seq.step=0.005) {
  # NA's in time series?
  ret.list = list()
  ret.list$NAN = which(is.na(x))
  if ( any( is.na(x)) ) {
    x.nona = x[-ret.list$NAN]
  } else {
    x.nona = x
  }
  if (sum(range(x.nona))==0){
    ret.list$PE = 0 #H
    ret.list$MPR = 0 #C
    ret.list$FIS = 0 #Fischer
    ret.list$opd = 0
    ret.list$NAN= NULL
    return(ret.list)
  } else {
    x=x.nona
    # Standard complexity measures:
    opd = weighted_ordinal_pattern_distribution(x = x, ndemb = D); ret.list = list()
    ret.list$PE = permutation_entropy(opd = opd) #H
    ret.list$MPR = MPR_complexity(opd=opd) #C
    ret.list$FIS = fis(opd=opd) #Fischer
    ret.list$opd = opd
    if (q){
      # q-complexity:
      ret.list$q = seq(0, seq.to, seq.step) 
      ret.list$PEq = c(sapply(X = ret.list$q, FUN=function(q1) permutation_entropy_qlog(opd = opd, q = q1)))
      ret.list$MPRq = c(sapply(X = ret.list$q, FUN=function(q1) q_complexity(opd = opd, q = q1)))
      #Tarnopolski:
      ret.list$Abbe = Abbe(x.nona)
      ret.list$Turn_point = Turning_point(x)
    }
    ret.list$NAN= NULL
    return(ret.list)
  }}


# Embedding Dimensions ----------------------------------------------------
# N= 70142
# N> xD!,
# wenn x = 8
# N> ca 40000
# wenn x = 7 
# N> ca 5000 
# we choose 7 (more buffer)

glimpse(ts2225)

metrics7 <- lapply(
  ts2225[-1],
  get.all.metrics,
  D = 7
)

#remove flags
ts2225_noflag <- ts2225[!grepl("^flag_", names(ts2225))]
metrics7_names <- names(ts2225_noflag)[-1]
metrics7 <- metrics7[!grepl("^flag_", names(metrics7))]
#names(metrics7) <- names(metrics7_names)[-1]

glimpse(metrics7) # there are 5040 different opd patterns for D=7
#metrics7_names


# Plot opds (ordinal pattern distribution)  --------------------------------
# Split the 21 plots into groups of 9
groups <- split(
  seq_along(metrics7),
  ceiling(seq_along(metrics7) / 9)
)

for (g in seq_along(groups)) {
  
  inds <- groups[[g]]
  
  # 3x3 for first two pages, 1x3 for the last
  if (length(inds) == 9) {
    nrow <- 3
    ncol <- 3
  } else {
    nrow <- 1
    ncol <- length(inds)
  }
  
  png(
    filename = paste0("OPD_D7_", g, ".png"),
    width = 2400,
    height = ifelse(length(inds) == 9, 2400, 800),
    res = 300
  )
  
  par(mfrow = c(nrow, ncol))
  
  for (i in inds) {
    
    opd <- metrics7[[i]]$opd
    
    plot(
      opd,
      type = "h",
      lwd = 2,
      xlab = "Ordinal pattern (D = 7)",
      ylab = "OPD value",
      main = names(metrics7)[i],
      cex.main = 1.2
    )
  }
  
  dev.off()
}

library(magick)

img1 <- image_read("OPD_D7_1.png")
img2 <- image_read("OPD_D7_2.png")
img3 <- image_read("OPD_D7_3.png")

combined <- image_append(
  c(img1, img2, img3),
  stack = TRUE
)

image_write(
  combined,
  "OPD_D7_combined.png"
)

# H (Entropy), C (Complexity), F(Fischer) plot distribution ---------------
par(mfrow = c(1,1))

PE <- sapply(metrics7, `[[`, "PE")

hist(PE)

shapiro.test(PE)

getwd()

dev.copy(png, "Hist_PE.png", width = 2400, height = 1800, res = 300)
dev.off()

s# p-value = 0.2669 -> there is no evidence against normality


C <- sapply(metrics7, `[[`, "MPR")
hist(C)
shapiro.test(C)

dev.copy(png, "Hist_MPR.png", width = 2400, height = 1800, res = 300)
dev.off()
# p-value = p-value = 0.6592 -> there is no evidence against normality

FIS <- sapply(metrics7, `[[`, "FIS")
hist(FIS)
shapiro.test(FIS)

dev.copy(png, "Hist_FIS.png", width = 2400, height = 1800, res = 300)
dev.off()
# p-value = 0.0004279 -> there is evidence against normality

# Entropy-Complexity plane  ------------------------------------------------
# I use the limit curves of D=7 provided 



PEC_df <- data.frame(
  Variable_short = c(
    "Pre",
    "T@2m",
    "T@42m",
    "RH@2m",
    "RH@42m",
    "ST@5cm",
    "ST@50cm",
    "SWC@5cm",
    "SWC@50cm",
    "WS",
    "VPD",
    "SW_in",
    "SW_out",
    "LW_in",
    "LW_out",
    "LE",
    "H",
    "CO2",
    "GPP",
    "NEE",
    "RE"
  ),
  PE = unname(PE),
  C = unname(C)
)

PEC_df

Variable_Variableshort_df <- Variable_Variableshort_df %>%
  mutate(variable_short = PEC_df$Variable_short)

# You have to load in the min and max curves D=7

{
  ggplot(PEC_df, aes(x = PE, y = C, color = Variable_short)) +
    geom_point(size = 3) +
    geom_text_repel(
      aes(label = Variable_short),
      size = 4,
      direction = "both",
      box.padding = 0.5,
      point.padding = 0.3
    ) +
    geom_line(data = mind7, aes(x = x, y = y), inherit.aes = FALSE) +
    geom_line(data = maxd7, aes(x = x, y = y), inherit.aes = FALSE) +
    labs(x = "Normalized Permutation Entropy", y = "MPR Statistical Complexity") +
    theme_bw()+
    theme(legend.position = "none") 
}

ggsave(filename = "Complexity2/Entropy_Complexity.png", dpi = 300)


# Entropy-Fischer Plane  --------------------------------------------------

EF <- data.frame(
  var_short = PEC_df$Variable_short, 
  PE = unname(PE),
  FIS = unname(FIS)
)

EF

{
  ggplot(EF, aes(x = PE, y = FIS, color = var_short)) +
    geom_point(size = 3) +
    geom_text_repel(
      aes(label = var_short),
      size = 4,
      direction = "both",
      box.padding = 0.5,
      point.padding = 0.3
    ) +
    labs(x = "Normalized Permutation Entropy", y = "Fischer Information") +
    theme_bw() +
    theme(legend.position = "none")
}

ggsave(filename = "Complexity2/Entropy_Fischer.png", dpi = 300)


# Extention to q-Entropy and q-Complexity ---------------------------------

qmetrics7 <- lapply(
  ts2225[-1],
  function(x) get.all.metrics(x, D = 7, q = TRUE)
)

qmetrics7
get.all.metrics(ts2225$`Precipitation (mm)`, D = 5, q = TRUE)


# Faster calculations: warning (let two cores handle the system)
plan(
  multisession,
  workers = parallel::detectCores() - 2
)

qmetrics7 <- future_lapply(
  ts2225[-1],
  function(x) get.all.metrics(x, D = 7, q = TRUE)
)

qmetrics6 <- future_lapply(
  ts2225[-1],
  function(x) get.all.metrics(x, D = 6, q = TRUE)
)

qmetrics5 <- future_lapply(
  ts2225[-1],
  function(x) get.all.metrics(x, D = 5, q = TRUE)
)

# Selected variable 1: Precipitation

pre_q7 <- qmetrics7[[1]]
pre_q7 <- data.frame(
  PEq = pre_q7$PEq,
  MPRq = pre_q7$MPRq
)

pre_q6 <- qmetrics6[[1]]
pre_q6 <- data.frame(
  PEq = pre_q6$PEq,
  MPRq = pre_q6$MPRq
)

pre_q5 <- qmetrics5[[1]]
pre_q5 <- data.frame(
  PEq = pre_q5$PEq,
  MPRq = pre_q5$MPRq
)

pre_q567 <- data.frame(
  pre_q7_PEq = pre_q7$PEq,
  pre_q7_MPRq = pre_q7$MPRq,
  pre_q6_PEq = pre_q6$PEq, 
  pre_q6_MPRq = pre_q6$MPRq,
  pre_q5_PEq = pre_q5$PEq,
  pre_q5_MPRq = pre_q5$MPRq
)

pre_q567_long <- pre_q567 %>%
  pivot_longer(
    cols = everything(),
    names_to = c("q", ".value"),
    names_pattern = "pre_(q[0-9]+)_(.*)"
  )

{
  ggplot(pre_q567_long, aes(
    x = PEq,
    y = MPRq,
    color = q
  )) +
    geom_point(size = 1)+
    labs(x = "q Permutation Entropy", y = "q Complexity", title = "Precipitation q-Entropy q-Complexity Graph") +
    theme_bw() +
    theme()
}

# higher q values opens the loop. higher q values show a more chaotic process






# Based on simple exact results and numerical simulations
# of stochastic processes, we show that these curves can distinguish among different long-range, short-range,
# and oscillating correlated behaviors. Also, we verify that simulated chaotic and stochastic time series can be
# distinguished based on whether these curves are open or closed.


# Your ordinary entropy–complexity plane asks:
#   
#   Where does this time series sit in terms of randomness and structure?
#   
#   The \(q\)-entropy–complexity curve asks something closer to:
#   
#   How does my assessment of randomness and structure change when I change the sensitivity of the entropy measure to different probabilities?




