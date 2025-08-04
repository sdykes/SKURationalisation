
DistributionAndParameters <- function(SKU, applesPerPack, bivariateAndMass_data) {
  
  if(SKU == "53/2" | SKU == "53/5") {
    
    SKU_distribution <- bivariateAndMass_data |>
      filter(EQ >= 47.0 & EQ <= 51.2)
    
  } else if (SKU == "58/5") {
    
    SKU_distribution <- bivariateAndMass_data |>
      filter(EQ >= 51.2 & EQ <= 54.4)
    
  } else if (SKU == "63/5" | SKU == "63/3N") {
    
    SKU_distribution <- bivariateAndMass_data |>
      filter(EQ >= 54.4 & EQ <= 61.7) |>
      filter(!(EQ >= 56.1 & EQ <= 61.7 & Elong >= 0.942))
    
  } else if (SKU == "63/5T" | SKU == "63/1T") {
    
    SKU_distribution <- bivariateAndMass_data |>
      filter(EQ >= 54.4 & EQ <= 61.7) |>
      filter(!(EQ >= 54.4 & EQ < 56.1 & Elong >= 0 & Elong <= 0.942)) |>
      filter(!(EQ >= 56.1 & EQ <= 59.0 & Elong >= 0 & Elong <= 0.865))
    
  } else if (SKU == "63/3") {
    
    SKU_distribution <- bivariateAndMass_data |>
      filter(EQ >= 56.1 & EQ <= 61.7,
             Elong >= 0.942 & Elong <= 1.2)
    
  } else if (SKU == "63/4") {
    
    SKU_distribution <- bivariateAndMass_data |>
      filter(EQ >= 59.0 & EQ <= 61.7)
    
  } else if (SKU == "67/4" | SKU == "67/5") {
    
    SKU_distribution <- bivariateAndMass_data |>
      filter(EQ >= 61.7 & EQ <= 66.2)
    
  } else if (SKU == "72/4" | SKU == "72/5") {
    
    SKU_distribution <- bivariateAndMass_data |>
      filter(EQ >= 66.2 & EQ <= 71.8)
    
  } else if (SKU == "FPS" | SKU == "FPL") {
    
    SKU_distribution <- bivariateAndMass_data |>
      filter(EQ >= 47.0 & EQ <= 56.1)
    
  } else if (SKU == HeroLarge) {
    
    SKU_distribution <- bivariateAndMass_data |>
      filter(EQ >= 59.2 & EQ <= 65.7,
             Elong >= 0.845 & Elong <= 0.945)
    
  } else if (SKU == HeroSmall) {
    
    SKU_distribution <- bivariateAndMass_data |>
      filter(EQ >= 53.6 & EQ <= 57.6,
             Elong >= 0.822 & Elong <= 0.968)
    
  } else if (SKU == LargeFormat) {
    
    SKU_distribution <- bivariateAndMass_data |>
      filter(EQ >= 65.7 & EQ <= 70.7,
             Elong >= 0.829 & Elong <= 0.961)
    
  } else if (SKU == Impulse) {
    
    SKU_distribution <- bivariateAndMass_data |>
      filter(EQ >= 57.6 & EQ <= 59.2,
             Elong >= 0.769 & Elong <= 1.020)
    
  }
  
  mass_data <- SKU_distribution |>
    pull(Mass)
  
  distr <- propagate::fitDistr(mass_data, verbose = F)
  
  params <- data.table::transpose(enframe(distr$par[[1]])) |>
    janitor::row_to_names(1)
  
  output1 <- tibble(SKU = SKU,
                   distribution = distr$stat$Distribution[[1]]) |>
    bind_cols(Params)
  
  output2 <- MCSimulation(applesPerPack, output1)
  
  return(output2)
  
}

# function to perform the Monte Carlo simulation

MCSimulation <- function(applesPerPack, params) {
  # replicate the filling of 1,000,000 packs
  
  if(params[[1,2]] == "Normal") {
    
    # Normal distribution
    
    temp_cw <- replicate(n = 100000, expr = {
      x_i = rnorm(applesPerPack, 
                  mean = params[[1,3]], 
                  sd = params[[1,4]])
      sum(x_i)
    })
    
  } else if(params[[1,2]] == "4P Beta") {
    
    temp_cw <- replicate(n = 100000, expr = {
      x_i = ExtDist::rBeta_ab(applesPerPack, 
                              shape1 = as.numeric(params[[1,3]]),
                              shape2 = as.numeric(params[[1,4]]),
                              a = as.numeric(params[[1,5]]),
                              b = as.numeric(params[[1,6]]))
      sum(x_i)
    })
    
    # default is the uniform distribution which needs the upper and lower bounds 
    
  } else if (params[[1,2]] == "Johnson SB") {
    
    temp_cw <- replicate(n = 100000, expr = {
      x_i = ExtDist::rJohnsonSB(applesPerPack, 
                                gamma = params[[1,5]],
                                delta = params[[1,6]],
                                xi = params[[1,3]],
                                lambda = params[[1,4]])
      sum(x_i)
    })
    
  } else {
    
    temp_cw <- replicate(n = 100000, expr = {
      x_i = runif(applesPerPack, 
                  min = params[[1,3]],
                  max = params[[1,4]])
      sum(x_i)
    })
    
  }
  # calculate the mean and standard dev for each set of 1,000,000 packs
  # then calculate P[X<x] = 0.001 and P[X<x] = 0.0001 
  cw <- as_tibble_col(temp_cw, column_name = "AppleWeights") |>
    summarise(meanWeight = mean(AppleWeights),
              sdWeight = sd(AppleWeights)) |>
    mutate(minWeight1000 = qnorm(0.001, mean=meanWeight, sd=sdWeight),
           minWeight10000 = qnorm(0.0001, mean=meanWeight, sd=sdWeight),
           minWeight100000 = qnorm(0.00001, mean=meanWeight, sd=sdWeight),
           applesPerPack = {{applesPerPack}})
  
  return(cw)

}

SKUSetNew2 <- tibble(SKUName = c("63/1T","63/3N"),
                     applesPerPack = c(1,3))

SKUSetNewMinMass <- SKUSetNew2 |>
  pmap(~DistributionAndParameters(..1,..2,bivariateAndMass_data)) |>
  bind_rows()
         





