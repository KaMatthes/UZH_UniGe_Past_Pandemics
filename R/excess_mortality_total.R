
load(paste0("data/data_mortality_rate.RData"))


year_smooth <- 5
year_from <- min(data_mortality_rate$Year)
year_reg <- year_from + year_smooth
year_max <- 1895

pandemic_year <- 1890
# YEAR <- 1891

dat.excess <- data_mortality_rate %>%
  filter(Year <1896) %>%
  mutate(Bezirk= as.numeric(Bezirk)) %>%
  arrange(Bezirk, Year) 


Bezirk_vec <- dat.excess %>%
  distinct(Bezirk) 

Bezirk_vec <- Bezirk_vec$Bezirk



# function bootstrapping
boot_pi <- function(model, pdata, n, p) {
  odata <- model$data
  lp <- (1 - p) / 2
  up <- 1 - lp
  seeds <- round(runif(n, 1, 1000), 0)
  boot_y <- foreach(i = 1:n, .combine = rbind) %dopar% {
    set.seed(seeds[i])
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
  
  # summary(reg_data$Year)
  # summary(pred_data$Year)
  
  # all data
  timespan <- glm(death ~ Year,
                          offset = log(population),
                          data = reg_data, 
                          family = "poisson")
  
  # dp = sum(residuals(timespan,type ="pearson")^2)/timespan$df.residual
  # 
  # summary(timespan <- glm(death ~ Year,
  #                         offset = log(population),
  #                         data = reg_data, 
  #                         family = "poisson"))
  
  predict <- boot_pi(timespan, pred_data, 1000, 0.99)
  
  pred_data <- pred_data %>%
    mutate(
      fit = predict$pred,
      lpi = predict$lower,
      upi = predict$upper
    )
  
  # flu hc excluded
  expected_deaths[[YEAR]] <- pred_data
  expected_deaths <- expected_deaths[-which(sapply(expected_deaths, is.null))] 
  # 
  # write.xlsx(expected_deaths,paste0("data/",today(), "_expected_deathtest.xlsx"), overwrite =TRUE)
  # write_rds(expected_deaths,paste0("data/",today(), "expected_death_",data.timespan,"_stmf.Rds"))
  }
}

expected_deaths <- expected_deaths %>%
  bind_rows(., .id = "column_label")

write.xlsx(expected_deaths,paste0("data/",today(), "_expected_deathtest.xlsx"), row.names=FALSE, overwrite = TRUE)
save(expected_deaths,file=paste0("data/",today(), "_expected_deathtest.RData"))