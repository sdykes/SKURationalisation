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


bivariate_data_4yr |>
  ggplot(aes(x=Elong, y=EQ, groups = Index)) +
  geom_bin2d(binwidth = c(0.01,1)) +
  scale_fill_continuous(type = "viridis") +
  geom_hline(yintercept = AppleStats4YearMean[[1,1]], linewidth = 1) +
  geom_vline(xintercept = AppleStats4YearMean[[1,3]], linewidth = 1) +
  theme_bw()

# Initial subset

ElongMin <- AppleStats4YearMean[[1,3]]-0.05
ElongMax <- AppleStats4YearMean[[1,3]]+0.05
EQMin <- AppleStats4YearMean[[1,1]]-2.5
EQMax <- AppleStats4YearMean[[1,1]]+2.5

SP_BV_Data <- bivariate_data_4yr |>
  filter(Elong > ElongMin & Elong < ElongMax,
         EQ > EQMin & EQ < EQMax) |>
  mutate(SL = Elong*EQ)

SP_BV_Data |>
  ggplot(aes(SL)) +
  geom_histogram()
 
#
# Determine the distribution for subset EQ and Elong
#
SKU <- "Snack Pack"
applesPerPack <- 2
width <- seq(0.01,0.11,0.01)

SKUHeight <- function(ElongIncrement, AppleStats4YearMean, bivariate_data_4yr) {
  
  SP_BV_Data <- bivariate_data_4yr |>
# Subset the distribution into the SKU range 
    filter(Elong > AppleStats4YearMean[[1,3]]-ElongIncrement & Elong < AppleStats4YearMean[[1,3]]+ElongIncrement,
           EQ > AppleStats4YearMean[[1,1]]-2.5 & EQ < AppleStats4YearMean[[1,1]]+2.5) |>
# convert Elongation to apple height
    mutate(SL = Elong*EQ)
  
  SL_data <- SP_BV_Data |>
    pull(SL)
  
  distr <- propagate::fitDistr(SL_data, verbose = F)
  
  params <- data.table::transpose(enframe(distr$par[[1]])) |>
    janitor::row_to_names(1) |>
    mutate(distribution = distr$stat$Distribution[[1]])
  
  if(params$distribution == "4P Beta") {
    
    temp_cw <- replicate(n = 100000, expr = {
      x_i = ExtDist::rBeta_ab(applesPerPack, 
                              shape1 = as.numeric(params[[1,1]]),
                              shape2 = as.numeric(params[[1,2]]),
                              a = as.numeric(params[[1,3]]),
                              b = as.numeric(params[[1,4]]))
      sum(x_i)
    })
    
  } else if (params$distribution == "Johnson SB") {
    
    temp_cw <- replicate(n = 100000, expr = {
      x_i = ExtDist::rJohnsonSB(applesPerPack, 
                                gamma = params[[1,5]],
                                delta = params[[1,6]],
                                xi = params[[1,3]],
                                lambda = params[[1,4]])
      sum(x_i)
    })
    
  }
  
}

SKUHeight(0.02,AppleStats4YearMean,bivariate_data_4yr)

distTest <- width |>
  map(~SKUHeight(.,AppleStats4YearMean,bivariate_data_4yr))
  




SL_data <- SP_BV_Data |>
  pull(SL)

distr <- propagate::fitDistr(SL_data, verbose = F)

params <- data.table::transpose(enframe(distr$par[[1]])) |>
  janitor::row_to_names(1)

output1 <- tibble(distribution = distr$stat$Distribution[[1]]) |>
  bind_cols(params)

output2 <- MCSimulation(applesPerPack, output1)

temp_cw <- replicate(n = 100000, expr = {
  x_i = ExtDist::rBeta_ab(applesPerPack, 
                          shape1 = as.numeric(params[[1,1]]),
                          shape2 = as.numeric(params[[1,2]]),
                          a = as.numeric(params[[1,3]]),
                          b = as.numeric(params[[1,4]]))
  sum(x_i)
})

cw <- as_tibble_col(temp_cw, column_name = "TubeHeights") |>
  summarise(meanHeight = mean(TubeHeights),
            sdHeight = sd(TubeHeights)) |>
  mutate(maxHeight1000 = qnorm(0.001, mean=meanHeight, sd=sdHeight, lower.tail = F),
         maxHeight10000 = qnorm(0.0001, mean=meanHeight, sd=sdHeight, lower.tail = F),
         maxHeight100000 = qnorm(0.00001, mean=meanHeight, sd=sdHeight, lower.tail = F),
         Width1000 = maxHeight1000-meanHeight,
         Width10000 = maxHeight10000-meanHeight,
         Width100000 = maxHeight100000-meanHeight)

minPackHeight <- function(applesPerPack, mean, sd) {
  # replicate the filling of 1,000,000 packs
  temp_cw <- replicate(n = 1000000, expr = {
    x_i = rnorm(applesPerPack, mean, sd)
    sum(x_i)
  })
  # calculate the mean and standard dev for each set of 1,000,000 packs
  # then calculate P[X<x] = 0.001 and P[X<x] = 0.0001 
  cw <- as_tibble_col(temp_cw, column_name = "AppleHeights") |>
    summarise(meanHeight = mean(AppleHeights),
              sdHeight = sd(AppleHeights)) |>
    mutate(maxHeight1000 = qnorm(0.001, mean=meanHeight, sd=sdHeight, lower.tail = F),
           maxHeight10000 = qnorm(0.0001, mean=meanHeight, sd=sdHeight, lower.tail = F),
           maxHeight100000 = qnorm(0.00001, mean=meanHeight, sd=sdHeight, lower.tail = F),
           applesPerPack = {{applesPerPack}})
  
  return(cw)
}

minPackHeight(5,SKUPop6753yr_stats[[1,1]],SKUPop6753yr_stats[[1,2]])

minPackHeight(5,SKUPop6752025_stats[[1,1]],SKUPop6752025_stats[[1,2]])



