suppressWarnings(function_prepare_data <- function() {
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
  mutate(`15_19f` =as.numeric(`15_19f`),
         `0_4` = `<1` + `1_4`,
         `0_4m` = `<1mo_m` +`1_11mo_m`+ `1m` + `2_4m`,
         `0_4f` = `<1mo_f` +`1_11mo_f`+ `1f` + `2_4f`,
         `0_19m` = `0_4m` + `5_14m` + `15_19m`,
         `0_19f` = `0_4f` + `5_14f` + `15_19f`,
         `>70m` = `70_79m` + `>80m`,
         `>70f` = `70_79f` + `>80f`) %>%
  select(-`<1`,-`1_4`, -`<1mo_m`, -`1_11mo_m`,- `1m`, -`2_4m`,-`<1mo_f`, -`1_11mo_f`,- `1f`, -`2_4f`,
         -`70_79m`,-`70_79f`,-`>80m`,-`>80f`, -`70_79m`, -`70_79f`, -`5_14m`, -`5_14f`,
         -`0_4m`, -`0_4f` , -`15_19m`, -`15_19f`)

# load data 1908 - 1918
alldata.1908 <- read_excel_allsheets(paste0("data_raw/Data1918/",data1908_1918))%>%
  do.call(rbind.fill, .)  %>%
  mutate(`60_69m` = as.numeric(`60_69m`),
         `0_4m` = `<1m` +`1_4m`,
         `0_4f` = `<1f` +`1_4f`,
         `0_19m` = `0_4m` + `5_14m` + `15_19m`,
         `0_19f` = `0_4f` + `5_14f` + `15_19f`) %>%
  select(-`<1m`,- `1_4m`, -`<1f`,- `1_4f`, -`5_14m`, -`5_14f`, -`0_4m`, -`0_4f`, -`15_19m`, -`15_19f`) 

# merge both hist data
alldata.hist <- alldata.1879 %>%
  rbind.fill(alldata.1908)%>%
  select(-DistrictNo,-LineID,-Scale,-CantonID,-Canton,-MapID,-District) %>%
  mutate(Canton =CantonShort) %>%
  select(-CantonShort,-UniqueID) %>%
  gather(., age_group, death, `Male`:`>70f`, factor_key=TRUE) %>%
  mutate(sex = str_sub(age_group, -1),
         sex=ifelse(sex=="0" | sex=="4" | sex=="9" | sex=="n","both", sex),
         sex=replace(sex,age_group=="Male", "m"),
         sex=replace(sex,age_group=="Female", "f"),
         sex=replace(sex,age_group=="Overall", "both"),
         age_group = recode(age_group,"Male"="total","Female"="total","Overall"="total"),
         Canton = recode(Canton, "IR"="AI")) %>%
  filter(!is.na(MapName))

# load data 2010 - 2020
# Bezirke
data.bezirk.2010 <- readxl::read_excel(paste0("data_raw/Data2020/",bezirk2010 ), sheet="BZN") %>%
  mutate(MapName=word(GDEBZNA ,-1)) %>%
  mutate(Bezirk=GDEBZNR) %>%
  mutate(Bezirk=as.factor(Bezirk)) %>%
  select(-GDEBZNR) %>%
  add_row(GDEKT = "SH",
          GDEBZNA = "Schaffhausen",
          MapName = "Schaffhausen",
          Bezirk = "1400") %>%
  mutate(MapName=recode(MapName, "d'Aigle"  = "Aigle",
                        "d'Entremont" = "Entremont",
                        "d'Hérens" = "Hérens",
                        "bernois" = "Jura bernois",
                        "Biel/Bienne" = "Biel",
                        "lausannois" = "Ouest lausannois",
                        "Innerrhoden" = "Appenzell Innerrhoden",
                        "Müstair" = "Engiadina Bassa/Val Müstair",
                        "vaudois" = "Jura-Nord vaudois", 
                        "Gallen" = "St. Gallen"))
# data
alldata.2010 <- read.csv(paste0("data_raw/Data2020/",data2010_2020), sep=";") %>%
  mutate(Bezirk=as.factor(Bezirk),
         Bezirk = recode(Bezirk, 
                         "2407" = "2400",
                         "2408" = "2400",
                         "2409" = "2400",
                         "2410" = "2400",
                         "2501" = "2500",
                         "2502" = "2500",
                         "2503" = "2500",
                         "2504" = "2500")) %>%
  left_join(data.bezirk.2010) %>%
  mutate(Year = EREIGNISJAHR_N,
         Canton = GDEKT,
         Age = as.character(alt7)) %>%
  select(-EREIGNISJAHR_N,-Bezirk,-GDEKT, -GDEBZNA,-alt7) %>%
  mutate(age_group = Age, 
         age_group = replace(age_group, age_group=="1","0_19"),
         age_group = replace(age_group, age_group=="2","0_19"),
         age_group = replace(age_group, age_group=="3","0_19"),
         age_group = replace(age_group, age_group=="4","0_19"),
         age_group = replace(age_group, age_group=="5","20_29"),
         age_group = replace(age_group, age_group=="6","20_29"),
         age_group = replace(age_group, age_group=="7","30_39"),
         age_group = replace(age_group, age_group=="8","30_39"),
         age_group = replace(age_group, age_group=="9","40_49"),
         age_group = replace(age_group, age_group=="10","40_49"),
         age_group = replace(age_group, age_group=="11","50_59"),
         age_group = replace(age_group, age_group=="12","50_59"),
         age_group = replace(age_group, age_group=="13","60_69"),
         age_group = replace(age_group, age_group=="14","60_69"),
         age_group = replace(age_group, age_group=="15",">70"),
         age_group = replace(age_group, age_group=="16",">70"),
         age_group = replace(age_group, age_group=="17",">70"),
         age_group = replace(age_group, age_group=="18",">70"),
         age_group = replace(age_group, age_group=="19",">70"),
         age_group = replace(age_group, age_group=="20",">70"),
         sex = tolower(GESCHLECHT_CD_T),
         death=anzTF) %>%
  select(-GESCHLECHT_CD_T, -anzTF, -Age) 


alldata.2010.total <- alldata.2010 %>%
  group_by(Year, MapName, Canton) %>%
  summarise(death=sum(death)) %>%
  ungroup() %>%
  mutate(age_group="total",
         sex="both")

alldata.2010.sex <- alldata.2010 %>%
  group_by(Year, MapName, Canton, sex) %>%
  summarise(death=sum(death)) %>%
  ungroup() %>%
  mutate(age_group="total")

alldata.2010 <- rbind(alldata.2010,alldata.2010.total,alldata.2010.sex)

# merge all data
all.data <- rbind(alldata.2010,alldata.hist) %>%
  mutate(MapName=recode(MapName,"Aarberg"="Seeland",
                        "Aarwangen" = "Bern-Mittelland",
                        "Aelen" ="Aigle",
                        "Alttoggenburg" = "Toggenburg",
                        "Appenzell Inner-Rhoden" = "Appenzell Innerrhoden",
                        "Aubonne" = "Morges" ,
                        "Balsthal"= "Solothurn",
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
                        "Inn" = "Engiadina Bassa/Val Müstair",
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
                        "Luzern" = "Luzern-Stadt",
                        "Mainthal"= "Vallemaggia",
                        "Martinach"	= "Martigny",
                        "Mendris"	= "Mendrisio",
                        "Milden"= "Broye-Vully",
                        "Morsee"	="Morges",
                        "Münchweilen" = "Münchwilen",
                        "Münster"	= "Jura bernois",
                        "Münsterthal"	= "Engiadina Bassa/Val Müstair",
                        "Neuenburg" = "Neuchâtel",
                        "Neuenstadt" = "Jura bernois",
                        "Neuss"	= "Nyon",
                        "Neutoggenburg" = "Toggenburg",
                        "Niedau" = "Biel",
                        "Niedersimmentahl" = "Frutigen-Niedersimmental",
                        "Oberhasle"	=  "Interlaken-Oberhasli",
                        "Oberklettgau"= "Schaffhausen",
                        "Oberlanquart"= "Landquart",
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
                        "Reyath"= "Schaffhausen",
                        "Reiat"= "Schaffhausen",
                        "Rolle"	= "Nyon",
                        "Rudolfsthal"= "Neuchâtel",
                        "Ryfthal"	= "Lavaux-Oron",
                        "Saane"	= "Sarine",
                        "Saanen"	= "Obersimmental-Saanen",
                        "Sankt Gallen" = "St. Gallen",
                        "Sankt Moritz"	= "Maloja",
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
                        "Unterklettgau" = "Schaffhausen",
                        "Unterlanquart" = "Landquart",
                        "Unterrheinthal" = "Rheintal",
                        "Untertoggenburg"	= "Toggenburg",
                        "Unterwalden nid dem Wald" = "Nidwalden",
                        "Unterwalden ob dem Wald"	= "Obwalden",
                        "Ursern"	= "Uri",
                        "Vivis"	= "Riviera-Pays-d'Enhaut",
                        "Vivisbach"	= "Veveyse",
                        "Vorderrhein"	= "Surselva",
                        "Wangen"	= "March",
                        "Wifflisburg" = "Broye-Vully",
                        "Wyl" = "Wil",
                        "Schleitheim"	= "Schaffhausen",
                        "Stein"	= "Schaffhausen",
                        "Bucheggberg-Kriegstetten" = "Solothurn",
                        "Dorneck-Thierstein" = "Solothurn",
                        "Olten-Gösgen"	= "Solothurn",
                        "Solothurn-Lebern"	= "Solothurn",
                        "Gäu"	= "Solothurn",
                        "Thal"	= "Solothurn",
                        "Bucheggberg"	= "Solothurn",
                        "Dorneck"	= "Solothurn",
                        "Gösgen"	= "Solothurn",
                        "Wasseramt"	= "Solothurn",
                        "Lebern" = 	"Solothurn",
                        "Olten" = "Solothurn",
                        "Thierstein" = 	"Solothurn",
                        "Büren" = "Seeland"),
         age_group= recode(age_group, 
                     ">70f" = ">70",
                     ">70m" = ">70",
                     ">80f" = ">80",
                     ">80m" = ">80",
                     "0_19f" = "0_19",
                     "0_19m" = "0_19",
                     "20_29m" = "20_29",
                     "20_29f" = "20_29",
                     "30_39f" = "30_39",
                     "30_39m" = "30_39",
                     "40_49f" = "40_49",
                     "40_49m" = "40_49",
                     "50_59f" = "50_59",
                     "50_59m" = "50_59",
                     "60_69f" = "60_69",
                     "60_69m" = "60_69",
                     "70_79f" = "70_79",
                     "70_79m" = "70_79"
                     )) %>%
  left_join(data.bezirk.2010[,c(3,4)]) %>%
  group_by(MapName, Year,Canton, age_group, sex, Bezirk) %>%
  summarise(death = sum(death)) %>%
  ungroup() %>%
  filter(!is.na(death))

# save
save(all.data,file=paste0("data/data_total.RData"))
write.xlsx(all.data,file=paste0("data/data_total.xlsx"),row.names=FALSE, overwrite = TRUE)
})

