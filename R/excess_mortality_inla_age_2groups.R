function_inla_age2groups <- function(Year_Pan,Year_max, Year_min, Age, Name) {
  load(paste0("data/data_mortality_rate_age2groups.RData"))

nc.sids <- sf::st_read("data_raw/Map_2020/Maps_dissolved/Maps_dissolved_2020.shp") %>%
  filter(!(  BEZIRKSNUM=="1110" |BEZIRKSNUM=="1101" | BEZIRKSNUM=="1102"  | BEZIRKSNUM=="1103" | BEZIRKSNUM=="1104" | BEZIRKSNUM=="1105"
             | BEZIRKSNUM=="1107"  | BEZIRKSNUM=="1106"| BEZIRKSNUM=="1108"| BEZIRKSNUM=="1109"| BEZIRKSNUM=="2225" | BEZIRKSNUM=="2229"
             | BEZIRKSNUM=="1401"  | BEZIRKSNUM=="1402"| BEZIRKSNUM=="1403"| BEZIRKSNUM=="1404"| BEZIRKSNUM=="1405" | BEZIRKSNUM=="1406"
             | BEZIRKSNUM=="311"  | BEZIRKSNUM=="312" | BEZIRKSNUM=="112" | BEZIRKSNUM=="111")) %>%
  mutate(Bezirk=BEZIRKSNUM)

row.names(nc.sids) <- nc.sids$Bezirk

nc.nb <- poly2nb(nc.sids, nc.sids$Bezirk) 
nb2INLA("Bezirk_Inla", nc.nb)

region.names <- poly2nb(nc.sids, nc.sids$Bezirk) %>%
  attr("region.id") %>%
  as.data.frame() %>%
  rename(Bezirk = ".") %>%
  mutate(Region = 1:130) 


dat.excess <- data_mortality_rate_age2groups %>%
  mutate(Bezirk=as.character(Bezirk)) %>%
  full_join(region.names) %>%
  filter(Year >=Year_min & Year <=Year_max ) %>%
  filter(age_group==Age) %>%
  arrange(Region) %>%
  mutate(Bezirk= as.numeric(Bezirk),
         death = ifelse(death < 0, 0, death),
         death = ifelse(is.na(death), 0, death),
         Region.struct= Region,
         Region.beta = Region,
         death = as.integer(death)) %>%
  select(Year,death, population,Region, Region.struct,age_group)

year_smooth <- 4
year_from <- min(dat.excess$Year)
year_reg <- year_from + year_smooth

control.family <- inla.set.control.family.default()

formula <- death ~ 1 + offset(log(population)) +
  f(Year, model='iid',constr = TRUE) +
  f(Region, model='bym', graph="Bezirk_Inla", scale.model = TRUE)
 
  expected_deaths <- list()
  
  for (YEAR in year_reg:Year_max){
    
    print(YEAR)
    
    if (YEAR==Year_Pan) {
    reg_data <-  dat.excess %>% 
      filter(Year >= YEAR+1 - year_smooth & Year < YEAR+1)%>%
      mutate(death=ifelse (Year ==YEAR, NA, death),
             death= as.integer(death))
    }
    
    else {
      reg_data <-  dat.excess %>% 
        filter(Year >= YEAR+1 - year_smooth & Year < YEAR+1)%>%
        mutate(death=ifelse (Year ==YEAR, NA, death),
               death=as.integer(death)) %>%
        filter(!Year == Year_Pan) 
    }
      # 
      # 
    
    # zeroinflatednbinomial0, because other values than zero might be observe
    set.seed(202200421)
    
    inla.mod <- inla(formula,
                     data=reg_data,
                     family = "zeroinflatednbinomial0", 
                     # verbose = TRUE,
                     control.family = control.family,
                     control.compute = list(config = TRUE),
                     control.mode = list(restart = TRUE),
                     # num.threads = round(parallel::detectCores() * .8),
                     control.predictor = list(compute = TRUE, link = 1))
  
  
  
  post.samples <- inla.posterior.sample(n = 1000, result = inla.mod,seed=20220421)
  predlist <- do.call(cbind, lapply(post.samples, function(X)
    exp(X$latent[startsWith(rownames(X$latent), "Pred")])))
  
  rate.drawsMed<-array(unlist( predlist), dim=c(dim(reg_data)[1], 1000)); dim(rate.drawsMed) 
  dM = as.data.frame(rate.drawsMed)
  # Add to the data and save
  Data= cbind(reg_data,dM)
  
  mean.samples <- Data %>%
    select(starts_with("V"), "Region", "Year", "age_group") %>%
    rowwise(Region) %>%
    mutate(fit = median(c_across(V1:V1000)),
           LL = quantile(c_across(V1:V1000), probs= 0.025),
           UL = quantile(c_across(V1:V1000), probs= 0.975)) %>%
    select(Region, fit, LL, UL, Year, age_group) %>%
    filter(Year==YEAR) %>%
    arrange(Region,Year) %>%
    left_join(dat.excess, by=c("Year", "Region")) %>%
    left_join(region.names) 
  
  
  expected_deaths[[YEAR]] <-  mean.samples
  expected_deaths <- expected_deaths[-which(sapply(expected_deaths, is.null))] 
  
  }
  
  expected_deaths <- expected_deaths %>%
    bind_rows(., .id = "column_label")
  
  write.xlsx(expected_deaths,paste0("data/expected_death_inla",Name,"_",Year_Pan,".xlsx"), row.names=FALSE, overwrite = TRUE)
  save(expected_deaths,file=paste0("data/expected_death_inla",Name,"_",Year_Pan,".RData"))
  }

function_inla_age2groups(Year_Pan=1890, Year_max=1891, Year_min=1886, Age="0_69", Name="0_69")
function_inla_age2groups(Year_Pan=1918, Year_max=1919, Year_min=1914, Age="0_69", Name="0_69")
function_inla_age2groups(Year_Pan=2020, Year_max=2020, Year_min=2016, Age="0_69", Name="0_69")

function_inla_age(Year_Pan=1890, Year_max=1891, Year_min=1886, Age=">70", Name="70")
function_inla_age(Year_Pan=1918, Year_max=1919, Year_min=1914, Age=">70", Name="70")
function_inla_age(Year_Pan=2020, Year_max=2020, Year_min=2016, Age=">70", Name="70")


  
  