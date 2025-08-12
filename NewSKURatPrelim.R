library(tidyverse)

con <- DBI::dbConnect(drv = odbc::odbc(), 
                      DSN = "RockIt", 
                      uid="stuart.dykes@rockitapple.com", 
                      authenticator = "externalbrowser"
)

AnnualStats <- tbl(con, "STG_COMPAC_BATCH") |>
  mutate(Season = case_when(START_TIME >= as.POSIXct('2022-01-01 00:00:00.000') &
                              START_TIME < as.POSIXct('2023-01-01 00:00:00.000') ~ 2022,
                            START_TIME >= as.POSIXct('2023-01-01 00:00:00.000') &
                              START_TIME < as.POSIXct('2024-01-01 00:00:00.000') ~ 2023,
                            START_TIME >= as.POSIXct('2024-01-01 00:00:00.000') &
                              START_TIME < as.POSIXct('2025-01-01 00:00:00.000') ~ 2024,
                            START_TIME >= as.POSIXct('2025-01-01 00:00:00.000') &
                              START_TIME < as.POSIXct('2026-01-01 00:00:00.000') ~ 2025,
                            TRUE ~ 2021)) |>
  filter(START_TIME >= as.POSIXct('2022-01-01 00:00:00.000'),
         !SIZER_GRADE_NAME %in% c("Low Colour", "Leaf", "Doub", "Doubles", "Capt", "Capture", 
                                  "Class 1.5", "Reject/Spoilt", "Recycle", "Juice", "1.5"),
         SIZE_NAME != "Rejects") |>
  dplyr::select(c(Season, BATCH_ID, GROWER_NAME, GROWER_CODE, MINOR, MAJOR, WEIGHT, 
                  SIZE_NAME, SIZER_GRADE_NAME, START_TIME)) |>
  filter(MINOR > 30,
         MAJOR > 30) |>
  mutate(elong = MAJOR/MINOR) |>
  group_by(Season) |>
  summarise(meanEQ = mean(MINOR, na.rm=T),
            sdEQ = sd(MINOR, na.rm=T),
            meanElong = mean(elong, na.rm=T),
            sdElong = sd(elong, na.rm=T),
            covar = cov(MINOR, elong),
            meanMass = mean(WEIGHT, na.rm=T),
            sdMass = sd(WEIGHT, na.rm=T)) |>
  collect() |>
  arrange(Season)


AppleStats4YearMean <- tbl(con, "STG_COMPAC_BATCH") |>
  filter(START_TIME >= as.POSIXct('2022-01-01 00:00:00.000') &
           START_TIME < as.POSIXct('2026-01-01 00:00:00.000'),
         !SIZER_GRADE_NAME %in% c("Low", "Low ", "Low Colour", "Leaf", "Doub", "Doubles", 
                                  "Doubles ", "Capt", "Capture", "Capture ", "Class 1.5", "1.5", 
                                  "Reject/Spoilt", "Recycle", "Juice", "Juice ", "Juic"),
         !SIZE_NAME %in% c("test","Rejects","Reje")) |>
  dplyr::select(c(BATCH_ID, GROWER_NAME, GROWER_CODE, MINOR, MAJOR, WEIGHT, 
                  SIZE_NAME, SIZER_GRADE_NAME, START_TIME)) |>
  filter(MINOR > 30,
         MAJOR > 30) |>
  mutate(elong = MAJOR/MINOR) |>
  summarise(meanEQ = mean(MINOR, na.rm=T),
            sdEQ = sd(MINOR, na.rm=T),
            meanElong = mean(elong, na.rm=T),
            sdElong = sd(elong, na.rm=T),
            covar = cov(MINOR, elong),
            meanMass = mean(WEIGHT, na.rm=T)) |>
  collect() 

SGN_terms <- tbl(con, "STG_COMPAC_BATCH") |>
  filter(START_TIME >= as.POSIXct('2022-01-01 00:00:00.000') &
           START_TIME < as.POSIXct('2026-01-01 00:00:00.000'),
         !SIZER_GRADE_NAME %in% c("Low", "Low ", "Low Colour", "Leaf", "Doub", "Doubles", 
                                  "Doubles ", "Capt", "Capture", "Capture ", "Class 1.5", "1.5", 
                                  "Reject/Spoilt", "Recycle", "Juice", "Juice ", "Juic"),
         !SIZE_NAME %in% c("test","Rejects","Reje")) |>
  distinct(SIZE_NAME) |>
  collect()
    
DBI::dbDisconnect(con)

mean_vec_4yr <- c(AppleStats4YearMean[[1,1]], AppleStats4YearMean[[1,3]])

covMat_4yr <- matrix(c(AppleStats4YearMean[[1,2]]^2, AppleStats4YearMean[[1,5]], AppleStats4YearMean[[1,5]], AppleStats4YearMean[[1,4]]^2), 
                 nrow=2, byrow=T)

set.seed(123)

bivariate_data_4yr <- data.frame(MASS::mvrnorm(n=1e6,
                                           mu = mean_vec_4yr,
                                           Sigma=covMat_4yr)) |>
  rename(EQ = X1,
         Elong = X2) |>
  mutate(SL = EQ*Elong,
         Index = "4year")

#
# Determine the distribution for subset EQ and Elong
#
SKU <- "Snack Pack"
applesPerPack <- 2
ElongIncrement <- 0.01
EQAnchor <- 62
UpperLower <- "U"
EQIncrement <- 0.1


SKUHeight <- function(ElongIncrement,EQIncrement,EQAnchor,UpperLower,ApplesPerPack,AppleStats) {
  
  mu <- c(AppleStats[[1,1]], AppleStats[[1,3]])
  
  sigma <- matrix(c(AppleStats[[1,2]]^2, AppleStats[[1,5]], AppleStats[[1,5]], AppleStats[[1,4]]^2), 
                       nrow=2, byrow=T)
  
  if (UpperLower == "U") {
  
    lower_bounds <- c(EQAnchor-EQIncrement, AppleStats[[1,3]]-ElongIncrement)
    upper_bounds <- c(EQAnchor, AppleStats[[1,3]]+ElongIncrement)
    
  } else {
    
    lower_bounds <- c(EQAnchor, AppleStats[[1,3]]-ElongIncrement)
    upper_bounds <- c(EQAnchor+EQIncrement, AppleStats[[1,3]]+ElongIncrement)
    
  }
  
  n_samples <- ApplesPerPack
  
  temp_cw <- replicate(n = 1000, expr = {
    x_i <- as_tibble(tmvtnorm::rtmvnorm(n = n_samples, 
                                        mean = mu, 
                                        sigma = sigma, 
                                        lower = lower_bounds, 
                                        upper = upper_bounds),
                     .name_repair = "unique_quiet") |>
      rename(EQ = ...1,
             Elong = ...2) |>
      mutate(SL = Elong*EQ) |>
      pull(SL)
    sum(x_i)
  })
  
  # Calculate the probability of the truncated data set
  
  prob <- mvtnorm::pmvnorm(lower = lower_bounds, upper = upper_bounds, mean = mu, sigma = sigma)
  
  cw <- as_tibble_col(temp_cw, column_name = "TubeHeights") |>
    summarise(meanHeight = mean(TubeHeights),
              sdHeight = sd(TubeHeights)) |>
    mutate(EQIncrement = EQIncrement,
           ElongIncrement = ElongIncrement,
           maxHeight1000 = qnorm(0.001, mean=meanHeight, sd=sdHeight, lower.tail = F),
           maxHeight10000 = qnorm(0.0001, mean=meanHeight, sd=sdHeight, lower.tail = F),
           Width1000 = maxHeight1000-meanHeight,
           Width10000 = maxHeight10000-meanHeight,
           PropDist = prob[[1]])
  
  return(cw)
}
  
SKUHeight(ElongIncrement,EQIncrement,EQAnchor,UpperLower,applesPerPack,AppleStats4YearMean)

input_grid <- expand_grid(EQ = seq(0.1,4.0,0.1), Elong = seq(0.01,0.11,0.01))

DistTest <- map2(input_grid$Elong,input_grid$EQ, ~SKUHeight(.x,.y,EQAnchor,UpperLower,applesPerPack,AppleStats4YearMean)) |>
  bind_rows()

DTWide <- DistTest |>
  pivot_wider(id_cols = EQIncrement,
              names_from = ElongIncrement,
              values_from = PropDist) |>
  column_to_rownames(var = "EQIncrement") 

DTWide_Mat <- DTWide |>
  as.matrix()

library(plotly)

Y <- seq(0.1,4.0,0.1)
X <- seq(0.01,0.11,0.01)

plot_ly() %>%
  add_surface(x = ~X, y = ~Y, z = ~DTWide_Mat)

#1D model for fill height variation

model1D <- lm(ElongIncrement~Width1000, data = DistTest |> filter(EQIncrement == 2.0))
  
summary(model1D)

ELIMax <- predict(model1D, newdata = tibble(Width1000 = 10))

#2D Model for relative distribution

model2D <- rsm(PropDist ~ SO(EQIncrement,ElongIncrement), data = DistTest)

summary(model2D)

#Specify the proportion required
K <- 0.36



IntersecFunction <- function(x, K, model) {
  
  A <- summary(model)$coef[[6]]
  B <- summary(model)$coef[[4]]
  C <- summary(model)$coef[[5]]
  D <- summary(model)$coef[[3]]
  E <- summary(model)$coef[[2]]
  F <- summary(model)$coef[[1]]-K
  
  alpha <- (B^2)-(4*A*C)
  beta <- (2*B*E)-(4*C*D)
  gamma <- (E^2)-(4*C*F)
  
  y <- (-(B*x+E)+sqrt(alpha*x^2+beta*x+gamma))/(2*C)
  #y <- (-(B*x+E)-sqrt(alpha*x^2+beta*x+gamma))/(2*C)
  
  return(y)
}  

x <- seq(.01,0.11,0.01)

EQIMax <- IntersecFunction(ELIMax, 0.36, model2D)

DistCurve <- x |>
  map_dbl(~IntersecFunction(.,0.07,model2D)) |>
  as_tibble() |>
  bind_cols(tibble(ElongIncrement = x)) |>
  rename(EQIncrement = value)

DistCurve |>
  ggplot(aes(x = ElongIncrement, y = EQIncrement)) +
  geom_point() +
  geom_line()

bivariate_data_4yr |>
  ggplot(aes(x=Elong, y=EQ, groups = Index)) +
  geom_bin2d(binwidth = c(0.01,1)) +
  scale_fill_continuous(type = "viridis") +
  geom_hline(yintercept = AppleStats4YearMean[[1,1]], linewidth = 1) +
  geom_vline(xintercept = AppleStats4YearMean[[1,3]], linewidth = 1) +
  annotate("rect", 
           xmin = AppleStats4YearMean[[1,3]]-ELIMax,
           xmax = AppleStats4YearMean[[1,3]]+ELIMax,
           ymin = 62-EQIMax,
           ymax = 62,
           colour = "#a9342c",
           fill = "#a9342c",
           alpha = 0.5) +
  theme_bw()


SKUHeight(ELIMax,EQIMax,EQAnchor,UpperLower,applesPerPack,AppleStats4YearMean)

SKU <- "Hero Small"
applesPerPack <- 5
ElongIncrement <- 0.01
EQAnchor <- 62-EQIMax
UpperLower <- "U"
EQIncrement <- 0.1









