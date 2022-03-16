function_excess_mortaliy <- function(Year_Pan, Year_max, Year_min) {
load(paste0("data/data_mortality_rate.RData"))

  
  # year_max <- 1895
  year_max <- Year_max
  year_min <- Year_min
  # pandemic_year <- 1890
  pandemic_year <- Year_Pan
  
  dat.excess <- data_mortality_rate %>%
    # filter(Year <1896) %>%
    filter(Year >=Year_min & Year <=Year_max ) %>%
    mutate(Bezirk= as.numeric(Bezirk),
           death = ifelse(death < 0, 0, death),
           death = ifelse(is.na(death), 0, death)) %>%
    arrange(Bezirk, Year) 
  
  year_smooth <- 4
  year_from <- min(dat.excess$Year)
  year_reg <- year_from + year_smooth
  
# YEAR <- 1891

Bezirk_vec <- dat.excess %>%
  distinct(Bezirk) 

Bezirk_vec <- Bezirk_vec$Bezirk


# function bootstrapping
set.seed(20220316)
boot_pi <- function(model, pdata, n, p) {
  odata <- model$data
  lp <- (1 - p) / 2
  up <- 1 - lp
  boot_y <- foreach(i = 1:n, .combine = rbind) %dopar% {
    bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
    bpred <- predict(update(model, data = bdata), type = "response", newdata = pdata)
    rpois(length(bpred), lambda = bpred)
  }
  boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up)))
  return(data.frame(pred = predict(model, newdata = pdata, type = "response"), 
                    lower = boot_ci[, 1], upper = boot_ci[, 2]))
}




# 
expected_deaths <- list()

for (BEZIRK in  Bezirk_vec) {
  
  dat.excess.bezirk <- dat.excess %>%
    filter(Bezirk ==BEZIRK)
  print(BEZIRK)
  
  
  for (YEAR in year_reg:year_max){
  
  print(YEAR)
    
  reg_data <-  dat.excess.bezirk %>% 
    filter(Year >= YEAR - year_smooth & Year < YEAR) %>%
    filter(!Year == pandemic_year)

  
  pred_data <- dat.excess.bezirk %>% 
    filter(Year == YEAR) %>%
    mutate(
      fit = NA_real_,
      lpi = NA_real_,
      upi = NA_real_
    )
  

  
  #  Regression
  timespan <- glm(death ~ Year,
                          offset = log(population),
                          data = reg_data, 
                          family = "poisson")

  # Prediction
  predict <- boot_pi(timespan, pred_data, 1000, 0.95)
  
  pred_data <- pred_data %>%
    mutate(
      fit = predict$pred,
      lpi = predict$lower,
      upi = predict$upper
    )
  
  # Expected death
  expected_deaths[[YEAR]] <- pred_data
  expected_deaths <- expected_deaths[-which(sapply(expected_deaths, is.null))] 

  }
}

expected_deaths <- expected_deaths %>%
  bind_rows(., .id = "column_label")

write.xlsx(expected_deaths,paste0("data/expected_death_",Year_Pan,".xlsx"), row.names=FALSE, overwrite = TRUE)
save(expected_deaths,file=paste0("data/expected_death_",Year_Pan,".RData"))

}


function_excess_mortaliy(Year_Pan=1890, Year_max=1895, Year_min=1882)
function_excess_mortaliy(Year_Pan=1918, Year_max=1920, Year_min=1908)
function_excess_mortaliy(Year_Pan=2020, Year_max=2020, Year_min=2013)
