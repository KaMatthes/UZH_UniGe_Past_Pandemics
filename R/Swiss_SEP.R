Swiss_SEP <- readxl::read_excel(paste0("data_raw/SwissSEP_FSO_2.xlsx")) %>%
  rename(Bezirk=District) %>%
  mutate( Bezirk = as.character(Bezirk),
          Bezirk = recode(Bezirk, 
                        "111" = "113",
                        "112" = "113",
                        "311" = "303",
                        "312" = "303",
                        "1103" = "1112",
                        "1106" = "1112",
                        "1104" = "1113",
                        "1110" = "1113",
                        "1105" = "1114",
                        "1108" = "1114",
                        "1107" = "1115",
                        "1109" = "1115",
                        "1101" = "1116",
                        "1102" = "1116",
                        "1401" = "1400",
                        "1402" = "1400",
                        "1403" = "1400",
                        "1404" = "1400",
                        "1405" = "1400",
                        "1406" = "1400",
                        "1821" = "1841",
                        "1822" = "1842",
                        "1825" = "1843",
                        "1824" = "1844",
                        "1826" = "1845",
                        "1827" = "1846",
                        "1828" = "1847",     
                        "1829" = "1848",
                        "1830" = "1849",
                        "1823" = "1851",           
                        "1831" = "1850",           
                        "2225" = "2207",
                        "2229" = "2207",
                        "2401" = "2400",
                        "2402" = "2400",
                        "2403" = "2400",
                        "2404" = "2400",
                        "2405" = "2400",
                        "2406" = "2400"),
          Language = recode(Language,
                            "1" = "German",
                            "2" = "French",
                            "3" = "Italien",
                            "4" = "German")) %>%
  group_by(Bezirk) %>%
  mutate(SEP_Bezirk = mean(median_ssep )) %>%
  ungroup() %>%
  distinct(Bezirk, .keep_all = TRUE)


write.xlsx(Swiss_SEP ,file=paste0("data/Swiss_SEP.xlsx"),row.names=FALSE, overwrite = TRUE)
# save
save(Swiss_SEP ,file=paste0("data/Swiss_SEP.RData"))