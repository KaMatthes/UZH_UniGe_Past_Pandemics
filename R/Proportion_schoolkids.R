# load population total

normalit<-function(m){
  (m - min(m))/(max(m)-min(m))
}

load(paste0("data/data_total.RData"))
Bezirke_nr <- data_total %>%
  dplyr::select(MapName, Bezirk) %>%
  distinct(Bezirk, .keep_all=TRUE)

load(paste0("data/pop_total.RData"))
pop_hist <- pop_total %>%
  filter(Year < 1921)

pop_1888_age <- readxl::read_excel(paste0("data_raw/Data1890/Population_Age1888.xlsx")) %>%
  mutate(Year= "1888")

pop_1910_age <- readxl::read_excel(paste0("data_raw/Data1918/Population_Age1910.xlsx")) %>%
  mutate(Year= "1910")

pop_age <- rbind(pop_1888_age,pop_1910_age) %>%
 filter(!Agegroups=="Total") %>%
  rename(age_group=Agegroups,
         MapName = Name) %>%
  gather(., sex, population, `Total`:`f_pop`, factor_key=TRUE) %>%
  dplyr::mutate(sex = str_sub(sex, 1,1)) %>%
  mutate(sex=replace(sex,sex=="T", "both")) %>%
  mutate(
         age_group = replace(age_group, age_group=="5-9","5_14"),
         age_group = replace(age_group, age_group=="10-14","5_14")) %>%
  mutate(MapName=dplyr::recode(MapName,"Aarberg"="Seeland",
                               "Aarwangen" = "Oberaargau",
                               "Aelen" ="Aigle",
                               "Alttoggenburg" = "Toggenburg",
                               "Appenzell Inner-Rhoden" = "Appenzell Innerrhoden",
                               "Aubonne" = "Morges" ,
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
                               "Glane" = "Glâne",
                               "Glenner"	= "Surselva",
                               "Greyerz" = "Gruyère",
                               "Gossau" = "St. Gallen",
                               "Grandson" = "Jura-Nord vaudois",
                               "Greyerz"	= "Gruyère",
                               "Greierz"	= "Gruyère",
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
                               "La Chaux-de-Fonds"= "Neuchâtel",
                               "LaVallée" = "Jura-Nord vaudois",
                               "La Vallée" = "Jura-Nord vaudois",
                               "Lavaux" = "Lavaux-Oron",
                               "Lauis"	= "Lugano",
                               "Laupen"= "Bern-Mittelland",
                               "Le Locle" = "Neuchâtel",
                               "Val-de-Ruz" = "Neuchâtel",
                               "Val-de-Travers"= "Neuchâtel",
                               "Linkes Ufer" = "Genève",
                               "Livinen"= "Leventina",
                               "Locle"= "Neuchâtel",
                               "Luggarus" = "Locarno",
                               "Luzern" = "Luzern",
                               "Luzern-Stadt" = "Luzern",
                               "Luzern-Land" = "Luzern",
                               "Mainthal"= "Vallemaggia",
                               "Martinach"	= "Martigny",
                               "Mendris"	= "Mendrisio",
                               "Milden"= "Broye-Vully",
                               "Morsee"	="Morges",
                               "Moësa"	="Moesa",
                               "Münchweilen" = "Münchwilen",
                               "Moutier"	= "Jura bernois",
                               "Moudon"	= "Broye-Vully",
                               "Payerne"	= "Broye-Vully",
                               "Münsterthal"	= "Engiadina Bassa / Val Müstair",
                               "Neuenburg" = "Neuchâtel",
                               "Neuveville" = "Jura bernois",
                               "Neuss"	= "Nyon",
                               "Neutoggenburg" = "Toggenburg",
                               "Niedau" = "Biel",
                               "Nidau" = "Biel",
                               "Simmenthal, Nieder-" = "Frutigen-Niedersimmental",
                               "Oberhasle"	=  "Interlaken-Oberhasli",
                               "Oberklettgau"= "Kanton Schaffhausen",
                               "Klettgau, Ober-"= "Kanton Schaffhausen",
                               "Oberlanquart"= "Prättigau / Davos",
                               "Landquart, Ober-"= "Prättigau / Davos",
                               "Oberrheinthal"	= "Rheintal",
                               "Rheinthal, Ober-"	= "Rheintal",
                               "Simmenthal, Ober-" = "Obersimmental-Saanen",
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
                               "Unterklettgau" = "Kanton Schaffhausen",
                               "Unterklettgau" = "Kanton Schaffhausen",
                               "Klettgau, Unter-" = "Landquart",
                               "Landquart, Unter-" = "Landquart",
                               "Unterrheinthal" = "Rheintal",
                               "Rheinthal, Unter-" = "Rheintal",
                               "Untertoggenburg"	= "Toggenburg",
                               "Toggenburg, Alt-"	= "Toggenburg",
                               "Toggenburg, Neu-"	= "Toggenburg",
                               "Toggenburg, Ober-"	= "Toggenburg",
                               "Toggenburg, Unter-"	= "Toggenburg",
                               "Avenches"	= "Broye-Vully",
                               "Unterwalden nid dem Wald" = "Nidwalden",
                               "Unterwalden ob dem Wald"	= "Obwalden",
                               "Ursern"	= "Uri",
                               "Valle-Maggia"= "Vallemaggia",
                               "Vevey"= "Riviera-Pays-d'Enhaut",
                               "Vivis"	= "Riviera-Pays-d'Enhaut",
                               "Vivisbach"	= "Veveyse",
                               "Vorderrhein"	= "Surselva",
                               "Wangen"	= "Oberaargau",
                               "Wifflisburg" = "Broye-Vully",
                               "Schleitheim"	= "Kanton Schaffhausen",
                               "Schaffhausen"	= "Kanton Schaffhausen",
                               "Seebezirk"	= "Lac",
                               "Stein"	= "Kanton Schaffhausen",
                               "St-Maurice"	= "Saint-Maurice",
                               "Yverdon"	= "Jura-Nord vaudois",
                               "Rive droite"	= "Genève",
                               "Rive gauche"	= "Genève",
                               "Ville de Genève"	= "Genève",
                               "Raron, östlich"	= "Raron",
                               "Raron, westlich"	= "Raron",
                               "Bucheggberg-Kriegstetten" = "Bucheggberg-Wasseramt",
                               "Kriegstetten" = "Bucheggberg-Wasseramt",
                               "Dorneck-Thierstein" = "Dorneck-Thierstein",
                               "Dornegg-Thierstein" = "Dorneck-Thierstein",
                               "Olten-Gösgen"	= "Olten-Gösgen",
                               "Solothurn-Lebern"	= "Solothurn-Lebern",
                               "Balsthal" = "Solothurn-Lebern",
                               "Solothurn"	= "Solothurn-Lebern",
                               "Balsthal-Gäu" =  "Thal-Gäu",
                               "Balthal-Thal" =  "Thal-Gäu",
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
  left_join(Bezirke_nr) %>%
  mutate(population = as.numeric(population)) %>%
  dplyr::group_by(Canton,Year,MapName, age_group, sex, Bezirk) %>%
  dplyr::summarise(population = sum(population)) %>%
  dplyr::ungroup() %>%
  filter(!sex=="both") %>%
  dplyr::group_by(MapName, Year) %>%
  mutate(prop= population / sum(population)) %>%
  dplyr::ungroup()

pop_age_tmp1 <- pop_age %>%
  filter(Year==1888) %>%
  select(Bezirk, prop, sex,age_group, MapName)

pop_age_extrapolate_tmp1 <- pop_hist %>%
  filter(Year < 1901) %>%
  left_join(pop_age_tmp1) %>%
  mutate(pop_age=round(population*prop))

pop_age_tmp2 <- pop_age %>%
  filter(Year==1910) %>%
  select(Bezirk, prop, sex,age_group,MapName)

pop_age_extrapolate_tmp2 <- pop_hist %>%
  filter(Year > 1900) %>%
  left_join(pop_age_tmp2) %>%
  mutate(pop_age=round(population*prop))


pop_extrapolate_age <- rbind(pop_age_extrapolate_tmp1,pop_age_extrapolate_tmp2) %>%
  select(-population) %>%
  rename(population=pop_age) %>%
  filter(Year == 1890 | Year == 1918) %>%
  filter(age_group=="5-14")

pop_2014_2020_age <- readxl::read_excel(paste0("data_raw/Data2020/Population_Age2014_2020.xlsx")) %>%
  gather(.,age_group, population, `0-4 Jahre`:`100 Jahre`, factor_key=TRUE) %>%
  rename(Year= Jahr,
         MapName = Bezirk,
         sex =  Geschlecht) %>%
  mutate(MapName=word(MapName ,-1),
         age_group=word(age_group ,1),
         sex=replace(sex,sex=="Mann", "m"),
         sex=replace(sex,sex=="Frau", "f")) %>%
  mutate(
         age_group = replace(age_group, age_group=="5-9","5_14"),
         age_group = replace(age_group, age_group=="10-14","5_14")) %>%
  mutate(MapName=recode(MapName, "d'Aigle"  = "Aigle",
                        "d'Entremont" = "Entremont",
                        "d'Hérens" = "Hérens",
                        "bernois" = "Jura bernois",
                        "Biel/Bienne" = "Biel",
                        "lausannois" = "Lausanne",
                        "Innerrhoden" = "Appenzell Innerrhoden",
                        "Müstair" = "Engiadina Bassa / Val Müstair",
                        "vaudois" = "Jura-Nord vaudois", 
                        "Gallen" = "St. Gallen",
                        "Davos" = "Prättigau / Davos",
                        "Aarberg"="Seeland",
                 "Aarwangen" = "Oberaargau",
                 "Aelen" ="Aigle",
                 "Alttoggenburg" = "Toggenburg",
                 "Appenzell Inner-Rhoden" = "Appenzell Innerrhoden",
                 "Aubonne" = "Morges",
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
                 "Glane" = "Glâne",
                 "Glenner"	= "Surselva",
                 "Greyerz" = "Gruyère",
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
                 "La Chaux-de-Fonds"= "Neuchâtel",
                 "LaVallée" = "Jura-Nord vaudois",
                 "La Vallée" = "Jura-Nord vaudois",
                 "Lavaux" = "Lavaux-Oron",
                 "Lauis"	= "Lugano",
                 "Laupen"= "Bern-Mittelland",
                 "Le Locle" = "Neuchâtel",
                 "Val-de-Ruz" = "Neuchâtel",
                 "Val-de-Travers"= "Neuchâtel",
                 "Linkes Ufer" = "Genève",
                 "Livinen"= "Leventina",
                 "Locle"= "Neuchâtel",
                 "Luggarus" = "Locarno",
                 "Luzern" = "Luzern",
                 "Luzern-Stadt" = "Luzern",
                 "Luzern-Land" = "Luzern",
                 "Mainthal"= "Vallemaggia",
                 "Martinach"	= "Martigny",
                 "Mendris"	= "Mendrisio",
                 "Milden"= "Broye-Vully",
                 "Morsee"	="Morges",
                 "Moësa"	="Moesa",
                 "Münchweilen" = "Münchwilen",
                 "Moutier"	= "Jura bernois",
                 "Moudon"	= "Broye-Vully",
                 "Payerne"	= "Broye-Vully",
                 "Münsterthal"	= "Engiadina Bassa / Val Müstair",
                 "Neuenburg" = "Neuchâtel",
                 "Neuveville" = "Jura bernois",
                 "Neuss"	= "Nyon",
                 "Neutoggenburg" = "Toggenburg",
                 "Niedau" = "Biel",
                 "Nidau" = "Biel",
                 "Simmenthal, Nieder-" = "Frutigen-Niedersimmental",
                 "Oberhasle"	=  "Interlaken-Oberhasli",
                 "Oberklettgau"= "Kanton Schaffhausen",
                 "Oberlanquart"= "Prättigau / Davos",
                 "Landquart, Ober-"= "Prättigau / Davos",
                 "Oberrheinthal"	= "Rheintal",
                 "Rheinthal, Ober-"	= "Rheintal",
                 "Simmenthal, Ober-" = "Obersimmental-Saanen",
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
                 "Unterklettgau" = "Kanton Schaffhausen",
                 "Unterlanquart" = "Landquart",
                 "Landquart, Unter-" = "Landquart",
                 "Unterrheinthal" = "Rheintal",
                 "Rheinthal, Unter-" = "Rheintal",
                 "Untertoggenburg"	= "Toggenburg",
                 "Unterwalden nid dem Wald" = "Nidwalden",
                 "Unterwalden ob dem Wald"	= "Obwalden",
                 "Ursern"	= "Uri",
                 "Valle-Maggia"= "Vallemaggia",
                 "Vevey"= "Riviera-Pays-d'Enhaut",
                 "Vivis"	= "Riviera-Pays-d'Enhaut",
                 "Vivisbach"	= "Veveyse",
                 "Vorderrhein"	= "Surselva",
                 "Wangen"	= "Oberaargau",
                 "Wifflisburg" = "Broye-Vully",
                 "Schleitheim"	= "Kanton Schaffhausen",
                 "Schaffhausen"	= "Kanton Schaffhausen",
                 "Seebezirk"	= "Lac",
                 "Stein"	= "Kanton Schaffhausen",
                 "St-Maurice"	= "Saint-Maurice",
                 "Yverdon"	= "Jura-Nord vaudois",
                 "Bucheggberg-Kriegstetten" = "Bucheggberg-Wasseramt",
                 "Dorneck-Thierstein" = "Dorneck-Thierstein",
                 "Olten-Gösgen"	= "Olten-Gösgen",
                 "Solothurn-Lebern"	= "Solothurn-Lebern",
                 "Solothurn"	= "Solothurn-Lebern",
                 "Gäu"	= "Thal-Gäu",
                 "Thal"	= "Thal-Gäu",
                 "Bucheggberg"	= "Bucheggberg-Wasseramt",
                 "Dornegg-Thierstein"  = "Dorneck-Thierstein",
                 "Dorneck"	= "Dorneck-Thierstein",
                 "Gösgen"	= "Olten-Gösgen",
                 "Wasseramt"	= "Bucheggberg-Wasseramt",
                 "Lebern" = 	"Solothurn-Lebern",
                 "Olten" = "Olten-Gösgen",
                 "Thierstein" = 	"Dorneck-Thierstein",
                 "Büren" = "Seeland",
                 "Davos" = "Prättigau / Davos",
                 "Zürich" = "Bezirk Zürich",
                 "Dietikon" = "Bezirk Zürich")) %>%
  left_join(Bezirke_nr) %>%
  dplyr::group_by(Bezirk,Year,sex, MapName,age_group) %>%
  dplyr::summarise(population = sum(population)) %>%
  dplyr::ungroup() %>%
  filter(Year==2020) 
 

prop_school_kids <- rbind(pop_extrapolate_age, pop_2014_2020_age) %>%
  select(-prop) %>%
  filter(!is.na(population)) %>%
  dplyr::group_by(Year,Bezirk,age_group,MapName) %>%
  dplyr::summarise(population = sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(MapName, Year) %>%
  mutate(prop= (population / sum(population))*100) %>%
  dplyr::ungroup() %>%
  filter(age_group=="5_14") %>%
  select(Year, Bezirk, prop_school=prop) %>%
  group_by(Year) %>%
  mutate(prop_norm = normalit(prop_school)) %>%
  ungroup()

write.xlsx(prop_school_kids ,file=paste0("data/prop_school_kids.xlsx"),row.names=FALSE, overwrite = TRUE)
save(prop_school_kids,file=paste0("data/prop_school_kids.RData"))
