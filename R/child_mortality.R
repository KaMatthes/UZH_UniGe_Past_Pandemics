load(paste0("data/pop_total.RData"))
  
normalit<-function(m){
  (m - min(m))/(max(m)-min(m))
}


pop_total <- pop_total %>%
  mutate(Year = as.factor(Year))

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# load data 1879 - 1895
alldata.1879 <- read_excel_allsheets(paste0("data_raw/Data1890/",data1879_1895)) %>%
  do.call(rbind.fill, .) %>%
  dplyr::mutate(`0_4` = `<1mo_m` + `<1mo_f`+  `1_11mo_m` + `1_11mo_f` +  `1m` + `1f`  +  `2_4m` + `2_4f`) %>%
  dplyr::select(Year,MapName, death= `0_4`) 

# load data 1908 - 1918
alldata.1908 <- read_excel_allsheets(paste0("data_raw/Data1918/",data1908_1918))%>%
  do.call(rbind.fill, .)  %>%
  dplyr::mutate(`0_4` = `<1m` +`1_4m` +`<1f` +`1_4f`) %>%
  dplyr::select(Year,MapName, death=`0_4`) 

# merge both hist data
alldata.hist <- alldata.1879 %>%
  rbind.fill(alldata.1908) %>%
  mutate(Year= as.factor(Year))

# load data 2010 - 2020
# Bezirke
data.bezirk.2010 <- readxl::read_excel(paste0("data_raw/Data2020/",bezirk2010 ), sheet="BZN") %>%
  dplyr::mutate(MapName=word(GDEBZNA ,-1),
         Bezirk=GDEBZNR,
         Bezirk=as.factor(Bezirk)) %>%
  dplyr::select(-GDEBZNR) %>%
  add_row(GDEKT = "SH",
          GDEBZNA = "Schaffhausen",
          MapName = "Kanton Schaffhausen",
          Bezirk = "1400") %>%
  add_row(GDEKT = "SO",
          GDEBZNA = "Solothurn",
          MapName = "Bucheggberg-Wasseramt",
          Bezirk = "1112") %>%
  add_row(GDEKT = "SO",
          GDEBZNA = "Solothurn",
          MapName = "Dorneck-Thierstein",
          Bezirk = "1113") %>%
  add_row(GDEKT = "SO",
          GDEBZNA = "Solothurn",
          MapName = "Olten-Gösgen",
          Bezirk = "1114") %>%
  add_row(GDEKT = "SO",
          GDEBZNA = "Solothurn",
          MapName = "Solothurn-Lebern",
          Bezirk = "1115") %>%
  add_row(GDEKT = "SO",
          GDEBZNA = "Solothurn",
          MapName = "Thal-Gäu",
          Bezirk = "1116") %>%
  add_row(GDEKT = "LU",
          GDEBZNA = "Luzern",
          MapName = "Luzern",
          Bezirk = "303") %>%
  add_row(GDEKT = "VD",
          GDEBZNA = "Lausanne",
          MapName = "Lausanne",
          Bezirk = "2207") %>%
  add_row(GDEKT = "ZH",
          GDEBZNA = "Zürich",
          MapName = "Bezirk Zürich",
          Bezirk = "113") %>%
  dplyr::mutate(MapName=recode(MapName, "d'Aigle"  = "Aigle",
                        "d'Entremont" = "Entremont",
                        "d'Hérens" = "Hérens",
                        "bernois" = "Jura bernois",
                        "Biel/Bienne" = "Biel",
                        "lausannois" = "Lausanne",
                        "Innerrhoden" = "Appenzell Innerrhoden",
                        "Müstair" = "Engiadina Bassa / Val Müstair",
                        "vaudois" = "Jura-Nord vaudois", 
                        "Gallen" = "St. Gallen",
                        "Davos" = "Prättigau / Davos"))%>%
  filter(!(Bezirk=="2225" | Bezirk=="2229"))


data.bezirk.fill <- data.bezirk.2010 %>%
  select(Bezirk)
# data
alldata.2010 <- read.csv(paste0("data_raw/Data2020/",data2010_2020), sep=";") %>%
  dplyr::mutate(Bezirk=as.factor(Bezirk))%>%
  filter(EREIGNISJAHR_N==2020) %>%
  filter(alt7==1) %>%
  dplyr::mutate(Bezirk =recode(Bezirk, 
                         "2407" = "2400",
                         "2408" = "2400",
                         "2409" = "2400",
                         "2410" = "2400",
                         "2501" = "2500",
                         "2502" = "2500",
                         "2503" = "2500",
                         "2504" = "2500",
                         "1403" = "1400",
                         "2225" = "2207",
                         "2229" = "2207",
                         "112" = "113",
                         "111" = "113")) %>%
  full_join(data.bezirk.2010) %>%
  dplyr::mutate(Year = EREIGNISJAHR_N,
         Canton = GDEKT,
         Age = as.character(alt7)) %>%
  dplyr::select(-EREIGNISJAHR_N,-Bezirk,-GDEKT, -GDEBZNA,-alt7) %>%
  dplyr::mutate(age_group = Age, 
         age_group = replace(age_group, age_group=="1","0_4"),
         sex = tolower(GESCHLECHT_CD_T),
         death=anzTF) %>%
  dplyr::select(-GESCHLECHT_CD_T, -anzTF, -Age)%>%
  dplyr:: mutate( Year= as.factor(Year)) %>%
  group_by(MapName, Year, Canton, age_group, death) %>%
  summarise(death=sum(death)) %>%
  ungroup() %>%
  select(-Canton, -age_group) %>%
  mutate(death= as.numeric(death),
         Year = 2020,
         death = ifelse(is.na(death), 0, death))


child_mortality <- rbind(alldata.2010,alldata.hist) %>%
  dplyr::mutate(MapName=recode(MapName,"Aarberg"="Seeland",
                        "Aarwangen" = "Oberaargau",
                        "Aelen" ="Aigle",
                        "Alttoggenburg" = "Toggenburg",
                        "Appenzell Inner-Rhoden" = "Appenzell Innerrhoden",
                        "Aubonne" = "Morges" ,
                        "Avenches" = "Broye-Vully" ,
                        "Balsthal"= "Thal-Gäu",
                        "Bellenz" = "Bellinzona",
                        "Bern" =  "Bern-Mittelland",
                        "Bischofszell" = "Weinfelden",
                        "Bollenz" =  "Blenio",
                        "Boudry" =  "Neuchâtel",
                        "Burgdorf" = "Emmental",
                        "Bürgen" = "Seeland",
                        "Chaux-de-fonds" = "Neuchâtel",
                        "Cossonay" ="Morges",
                        "Courtelary"= "Jura bernois",
                        "Delsberg" = "Delémont",
                        "Diessenhofen"= "Frauenfeld",
                        "Echallens" = "Gros-de-Vaud",
                        "Entremont" = "Entremont",
                        "Ering" = "Hérens",
                        "Erlach" = "Seeland",
                        "Fraubrunnen"	= "Bern-Mittelland",
                        "Freibergen" = "Franches-Montagnes",
                        "Frutigen" = "Frutigen-Niedersimmental",
                        "Gaster" = "See-Gaster",
                        "Glenner"	= "Surselva",
                        "Gossau" = "St. Gallen",
                        "Grandson" = "Jura-Nord vaudois",
                        "Greyerz"	= "Gruyère",
                        "Gundis" = "Conthey",
                        "Heinzenberg" = "Viamala",
                        "Hinterrhein" = "Viamala",
                        "Iferten" = "Jura-Nord vaudois",
                        "Im Boden" = "Imboden",
                        "Inn" = "Engiadina Bassa / Val Müstair",
                        "Interlaken" = "Interlaken-Oberhasli",
                        "Jouxthal"= "Jura-Nord vaudois",
                        "Konolfingen" = "Bern-Mittelland",
                        "Landbezirk"= "Basel-Stadt",
                        "Lauis"	= "Lugano",
                        "Laupen"= "Bern-Mittelland",
                        "Linkes Ufer" = "Genève",
                        "Livinen"= "Leventina",
                        "Locle"= "Neuchâtel",
                        "Luggarus" = "Locarno",
                        "Luzern-Stadt" = "Luzern",
                        "Luzern-Land" = "Luzern",
                        "Mainthal"= "Vallemaggia",
                        "Martinach"	= "Martigny",
                        "Mendris"	= "Mendrisio",
                        "Milden"= "Broye-Vully",
                        "Morsee"	="Morges",
                        "Münchweilen" = "Münchwilen",
                        "Münster"	= "Jura bernois",
                        "Münsterthal"	= "Engiadina Bassa / Val Müstair",
                        "Neuenburg" = "Neuchâtel",
                        "Neuenstadt" = "Jura bernois",
                        "Neuss"	= "Nyon",
                        "Neutoggenburg" = "Toggenburg",
                        "Niedau" = "Biel",
                        "Nidau" = "Biel",
                        "Niedersimmentahl" = "Frutigen-Niedersimmental",
                        "Oberhasle"	=  "Interlaken-Oberhasli",
                        "Oberklettgau"= "Kanton Schaffhausen",
                        "Oberlanquart"= "Prättigau / Davos",
                        "Oberrheinthal"	= "Rheintal",
                        "Obersimmentahl" = "Obersimmental-Saanen",
                        "Obertoggenburg"	= "Toggenburg",
                        "Orbe" 	= "Jura-Nord vaudois",
                        "Oron" = "Lavaux-Oron",
                        "Pays-d'Enhaut"	= "Riviera-Pays-d'Enhaut",
                        "Peterlingen" = "Broye-Vully",
                        "Pruntrut" = "Porrentruy",
                        "Rechtes Ufer"	= "Genève",
                        "Revierthal"	= "Riviera",
                        "Reyath"= "Kanton Schaffhausen",
                        "Reiat"= "Kanton Schaffhausen",
                        "Reiath"= "Kanton Schaffhausen",
                        "Rolle"	= "Nyon",
                        "Rudolfsthal"= "Neuchâtel",
                        "Ryfthal"	= "Lavaux-Oron",
                        "Saane"	= "Sarine",
                        "Saanen"	= "Obersimmental-Saanen",
                        "Sankt Gallen" = "St. Gallen",
                        "Sankt Moritz"	= "Saint-Maurice",
                        "Sargans" = "Sarganserland",
                        "Schwarzenburg" = "Bern-Mittelland",
                        "Schwiz" = "Schwyz",
                        "See" = "See-Gaster",
                        "Seftigen"	= "Thun",
                        "Siders"	= "Sierre",
                        "Signau"	= "Emmental",
                        "Singine"	= "Sense",
                        "Sitten"	= "Sion",
                        "Stadt Basel"	= "Basel-Stadt",
                        "Stadtbezirk"	= "Genève",
                        "Steckborn" = "Frauenfeld",
                        "Tablat"	= "St. Gallen",
                        "Trachselwald" = "Emmental",
                        "Traversthal"	= "Neuchâtel",
                        "Tscherlitz" =	"Gros-de-Vaud",
                        "Unterklettgau" = "Kanton Schaffhausen",
                        "Unterlanquart" = "Landquart",
                        "Unterrheinthal" = "Rheintal",
                        "Untertoggenburg"	= "Toggenburg",
                        "Unterwalden nid dem Wald" = "Nidwalden",
                        "Unterwalden ob dem Wald"	= "Obwalden",
                        "Ursern"	= "Uri",
                        "Vivis"	= "Riviera-Pays-d'Enhaut",
                        "Vivisbach"	= "Veveyse",
                        "Vorderrhein"	= "Surselva",
                        "Wangen"	= "Oberaargau",
                        "Wifflisburg" = "Broye-Vully",
                        "Wyl" = "Wil",
                        "Schleitheim"	= "Kanton Schaffhausen",
                        "Schaffhausen"	= "Kanton Schaffhausen",
                        "Stein"	= "Kanton Schaffhausen",
                        "Bucheggberg-Kriegstetten" = "Bucheggberg-Wasseramt",
                        "Dorneck-Thierstein" = "Dorneck-Thierstein",
                        "Olten-Gösgen"	= "Olten-Gösgen",
                        "Solothurn-Lebern"	= "Solothurn-Lebern",
                        "Solothurn"	= "Solothurn-Lebern",
                        "Gäu"	= "Thal-Gäu",
                        "Thal"	= "Thal-Gäu",
                        "Bucheggberg"	= "Bucheggberg-Wasseramt",
                        "Dorneck"	= "Dorneck-Thierstein",
                        "Gösgen"	= "Olten-Gösgen",
                        "Wasseramt"	= "Bucheggberg-Wasseramt",
                        "Lebern" = 	"Solothurn-Lebern",
                        "Olten" = "Olten-Gösgen",
                        "Thierstein" = 	"Dorneck-Thierstein",
                        "Büren" = "Seeland",
                        "Davos" = "Prättigau / Davos",
                        "Zürich" = "Bezirk Zürich",
                        "Dietikon" = "Bezirk Zürich"))%>%
  left_join(data.bezirk.2010[,c(3,4)]) %>%
  dplyr::group_by(MapName, Year, Bezirk) %>%
  dplyr::summarise(death = sum(death)) %>%
  dplyr::ungroup() %>%
  left_join(pop_total) %>%
  filter(!is.na(MapName)) %>%
  mutate(prop_child_death = (death/population)*10000) %>%
  group_by(Year) %>%
  mutate(prop_norm = normalit(prop_child_death)) %>%
  ungroup() %>%
  filter(Year==1889 | Year==1917  | Year ==2020)
  
write.xlsx(child_mortality ,file=paste0("data/child_mortality.xlsx"),row.names=FALSE, overwrite = TRUE)
save(child_mortality  ,file=paste0("data/child_mortality.RData"))

