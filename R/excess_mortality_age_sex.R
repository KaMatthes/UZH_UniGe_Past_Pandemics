function_excess_mortaliy_age_sex <- function(Year_Pan, Year_max, Year_min, Age, Name, Sex) {
# load(paste0("data/data_mortality_rate_age.RData"))
  
  load(paste0("data/data_mortality_rate_age2groups_sex.RData"))
  
  
  year_smooth <- 4

  
  # year_max <- 1895
  year_max <- Year_max
  year_min <- Year_min
  # pandemic_year <- 1890
  pandemic_year <- Year_Pan
  
  dat.excess <- data_mortality_rate_age_sex %>%
    # filter(Year <1896) %>%
    mutate(Year = as.character(Year),
           Year= as.numeric(Year)) %>%
    filter(Year >=Year_min & Year <=Year_max ) %>%
    filter(age_group==Age) %>%
    filter(sex==Sex) %>%
    mutate(Bezirk= as.numeric(Bezirk),
           death = ifelse(death < 0, 0, death),
           death = ifelse(is.na(death), 0, death)) %>%
    arrange(Bezirk, Year) 

  year_from <- min(dat.excess$Year)
  year_reg <- year_from + year_smooth
  
  
# YEAR <- 1891

Bezirk_vec <- dat.excess %>%
  dplyr::distinct(Bezirk)

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

write.xlsx(expected_deaths,paste0("data/expected_death_",Name,"_",Sex,"_",Year_Pan,".xlsx"), row.names=FALSE, overwrite = TRUE)
save(expected_deaths,file=paste0("data/expected_death_",Name,"_",Sex, "_",Year_Pan,".RData"))

}


function_excess_mortaliy_age_sex(Year_Pan=1890, Year_max=1895, Year_min=1886, Age="0_69", Name="0_69", Sex="f")
function_excess_mortaliy_age_sex(Year_Pan=1918, Year_max=1920, Year_min=1908, Age="0_69", Name="0_69", Sex="f")
function_excess_mortaliy_age_sex(Year_Pan=2020, Year_max=2020, Year_min=2014, Age="0_69", Name="0_69", Sex="f")

function_excess_mortaliy_age_sex(Year_Pan=1890, Year_max=1895, Year_min=1886, Age="0_69", Name="0_69", Sex="m")
function_excess_mortaliy_age_sex(Year_Pan=1918, Year_max=1920, Year_min=1908, Age="0_69", Name="0_69", Sex="m")
function_excess_mortaliy_age_sex(Year_Pan=2020, Year_max=2020, Year_min=2014, Age="0_69", Name="0_69", Sex="m")

function_excess_mortaliy_age_sex(Year_Pan=1890, Year_max=1895, Year_min=1886, Age=">70", Name="70", Sex="f")
function_excess_mortaliy_age_sex(Year_Pan=1918, Year_max=1920, Year_min=1908, Age=">70", Name="70", Sex="f")
function_excess_mortaliy_age_sex(Year_Pan=2020, Year_max=2020, Year_min=2014, Age=">70", Name="70", Sex="f")

function_excess_mortaliy_age_sex(Year_Pan=1890, Year_max=1895, Year_min=1886, Age=">70", Name="70", Sex="m")
function_excess_mortaliy_age_sex(Year_Pan=1918, Year_max=1920, Year_min=1908, Age=">70", Name="70", Sex="m")
function_excess_mortaliy_age_sex(Year_Pan=2020, Year_max=2020, Year_min=2014, Age=">70", Name="70", Sex="m")

# 
# function_excess_mortaliy_age(Year_Pan=1890, Year_max=1895, Year_min=1886, Age="0_19", Name="0_19")
# function_excess_mortaliy_age(Year_Pan=1890, Year_max=1895, Year_min=1886, Age="20_49",Name="20_49")
# function_excess_mortaliy_age(Year_Pan=1890, Year_max=1895, Year_min=1886, Age="50_69", Name="50_69")
# function_excess_mortaliy_age(Year_Pan=1890, Year_max=1895, Year_min=1886, Age=">70", Name="70")
# 
# function_excess_mortaliy_age(Year_Pan=1918, Year_max=1920, Year_min=1908, Age="0_19",Name="0_19")
# function_excess_mortaliy_age(Year_Pan=1918, Year_max=1920, Year_min=1908, Age="20_49",Name="20_49")
# function_excess_mortaliy_age(Year_Pan=1918, Year_max=1920, Year_min=1908, Age="50_69",Name="50_69")
# function_excess_mortaliy_age(Year_Pan=1918, Year_max=1920, Year_min=1908, Age=">70",Name="70")
# 
# # function_excess_mortaliy_age(Year_Pan=2020, Year_max=2020, Year_min=2014, Age="0_19", Name="0_19")
# # function_excess_mortaliy_age(Year_Pan=2020, Year_max=2020, Year_min=2014, Age="20_49", Name="20_49")
# 
# # function_excess_mortaliy_age(Year_Pan=2020, Year_max=2020, Year_min=2014, Age="50_69",Name="50_69")
# # function_excess_mortaliy_age(Year_Pan=2020, Year_max=2020, Year_min=2014, Age=">70",Name="70")
# 
# function_excess_mortaliy_sex(Year_Pan=1890, Year_max=1895, Year_min=1886, Sex="f")
# function_excess_mortaliy_sex(Year_Pan=1890, Year_max=1895, Year_min=1886, Sex="m")
# 
# function_excess_mortaliy_sex(Year_Pan=1918, Year_max=1920, Year_min=1908, Sex="f")
# function_excess_mortaliy_sex(Year_Pan=1918, Year_max=1920, Year_min=1908, Sex="m")
