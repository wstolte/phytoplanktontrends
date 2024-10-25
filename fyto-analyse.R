

require(tidyverse)
require(lubridate)
require(readxl)

wdir <- "p:/emodnet_biology/DeltaResBiology2OBIS/mwtl_phytoplankton2/"
datadir <- "p:/emodnet_biology/DeltaResBiology2OBIS/mwtl_phytoplankton2/data/Levering_2023_CIV"


#-----------------------------------------------------------------------#
####                       Inport data                               ####
#-----------------------------------------------------------------------#

# list.files(datadir)
# xlsx_phyto <- "Laadbestand_FPzout_ 2000-2022_20230526_geharmoniseerd_master.xlsx"
# phyto <- read_xlsx(file.path(datadir, xlsx_phyto), sheet = "Analysedata FPzout 2000-2022")
# names(phyto) <- tolower(names(phyto))
# 
# commontax_url = 'p:/emodnet_biology/DeltaResBiology2OBIS/mwtl_phytoplankton2/data/data/data/phyto_species_corrected_2000-2018_matched.csv'
# newtax_url = 'p:/emodnet_biology/DeltaResBiology2OBIS/mwtl_phytoplankton2/data/Levering_2023_CIV/new_species_list_20182022_matched.txt'
# 
# commontax <- read_delim(commontax_url, delim = ';')
# newtax <-  read_delim(newtax_url, delim = ';')
# 
# save(list = c("phyto", "commontax", "newtax"), file = "data.Rdata")

load("data.Rdata")

phytoorg <- phyto  # keep original
# phyto <- phytoorg

# Check for duplicates
colnames(phytoorg)

# This df contains duplicates from different datasets that have the same location and time, which seems unlikely
dup_phy <- phytoorg %>% 
  select(-amt_meas, -amt_calc, -ext_ref) %>%
  arrange(loc_code,date_smp,prod_code,par_name, sub_par,rmk_meas, unit_calc) %>%
  duplicated() %>% which(arr.ind = TRUE)

# Create the df with duplicates
dbs_zero1 <- phytoorg %>% 
  select(-amt_meas, -amt_calc, -ext_ref) %>%
  arrange(loc_code,date_smp,prod_code,par_name, sub_par,rmk_meas, unit_calc) %>%
  ungroup() %>%
  slice(sort(c(dup_phy, dup_phy-1))) 

# Create the df without duplicates
dbs_zero2 <- phytoorg %>% 
  select(-amt_meas, -amt_calc, -ext_ref) %>%
  arrange(loc_code,date_smp,prod_code,par_name, sub_par,rmk_meas, unit_calc) %>%
  ungroup() %>%
  slice(sort(c(dup_phy)))

dup_test <- left_join(dbs_zero2, phytoorg, by = c("loc_code","date_smp","prod_code","par_name", "sub_par", "rmk_meas", "unit_calc"))

phyto[as.character(phyto) =='NA' | as.character(phyto) =='' | as.character(phyto) ==' '] <- NA
phyto %>% duplicated() %>% which() %>% length()
phyto[duplicated(phyto),]  # zero real duplicates, rest is empty lines
phyto <- (phyto[!duplicated(phyto),]) 
#### remove lines with date == NA
phyto <- phyto[!is.na(phyto$date_smp),] # 1 line

# Here we remove the duplicates that are explained above. MTWL is looking into this.
#phyto <- phytoorg %>%
#  distinct(loc_code,date_smp,prod_code,par_name, sub_par,rmk_meas, unit_calc, .keep_all = TRUE)

phyto <- phyto %>%
  dplyr::mutate(
    verticalreference = case_when(
      prod_code == "OW" ~ "surface",
      prod_code == "LN.2" ~ "pycnocline",
      prod_code == "LN.3" ~ "bottom"),
    bio_Aquo = case_when(
      sub_par == "AD" ~ "adult",
      sub_par == "C" ~ "cell",
      sub_par == "FL" ~ "flagellate",
      sub_par == "JU" ~ "juvenile",
      sub_par == "K" ~ "colony",
      sub_par == "N" ~ "naked form (without silica skelet)",
      sub_par == "SP" ~ "spores"),
    date = as.Date(date_smp, "%d-%m-%y", tz = "")) %>%
  dplyr::select(
    locationID = loc_code,
    date,
    verticalreference,
    species = par_name,
    identificationQualifier = rmk_meas,
    lifeStage = bio_Aquo,
    cells.l = amt_calc,
    cellsObsSample = amt_meas,
    ext_ref
  )

###Time zones. Indien er geen timezone ingevuld is, gaat het om MET. Dit wil zeggen UTC +1. Willem checkt of deze offset mee kan opgenomen worden in de WFS output.

range(phyto$date)
# no times available

# add taxonomic hiearchy



tax <- commontax |> bind_rows(newtax) |>
  select(
    speciesname_original,
    AphiaID,
    `Match type`,
    LSID,
    ScientificName,
    Authority,
    AphiaID_accepted,
    ScientificName_accepted,
    Authority_accepted,
    Phylum,
    Class,
    Order,
    Family,
    Genus,
    Subgenus,
    Species,
    Subspecies,
    isMarine,
    isFresh,
    isTerrestrial
  )

phyto2 <- phyto %>% left_join(tax, by = c(species = 'speciesname_original')) %>%
  mutate(
    group = case_when(
      Class == 'Bacillariophyceae' ~ 'diatoms',
      Class == 'Cyanophyceae' ~ 'cyanobacteria',
      Class %in% c('Chlorophyceae', 'Pyramimonadophyceae', 'Prasinophyceae') ~ 'green algae',
      Class == 'Dinophyceae' ~ 'dinoflagellates',
      Class == 'Prymnesiophyceae' ~ 'prymnesiofyten'
      
      # Conjugatophyceae
      # Xanthophyceae
      # 
      # Chlorodendrophyceae
      # Bicoecea
      # 
      # Trebouxiophyceae
      # Dictyochophyceae
      # Raphidophyceae
      # Choanoflagellatea
      # Cryptophyceae
      # Chrysophyceae
      # Coccolithophyceae
      # Coscinodiscophyceae
      # 
      # Mamiellophyceae
      # Ebriophyceae
      # Euglenoidea
      # Euglenophyceae
      # Chlorophyta incertae sedis
      # Cryptophyta incertae sedis
      # Synurophyceae
      # Khakista incertae sedis
      # Litostomatea
      # Nephroselmidophyceae
      # Imbricatea
      # Thecofilosea
      # 
      # Spirotrichea
      # Ulvophyceae
      # Pedinophyceae
      # Zygnematophyceae
    )
  )

phyto2 %>%
  filter(locationID %in% c("VLISSGBISSVH", "WALCRN2", "NOORDWK2", "NOORDWK10", "TERSLG10", "WALCRN20", "NOORDWK20")) %>%
  # filter(grepl('Thalassiosira|Skeletonema|Chaetoceros|Phaeocystis', Genus)) |>
  group_by(group) |>
  count(Genus) |>
  View()

# monthly summer means
phyto2 %>%
  filter(
    !is.na(group),
    verticalreference == "surface"
  ) %>%
  filter(locationID %in% c("VLISSGBISSVH", "WALCRN2", "NOORDWK2", "NOORDWK10", "TERSLG10", "WALCRN20", "NOORDWK20")) %>%
  # take mean of duplicates
  group_by(locationID, date, month = month(date), Genus, species, group, verticalreference) |>
  summarize(mean = mean(`cells.l`), .groups = "drop") |>
  filter(
    # grepl('Thalassiosira|Skeletonema|Chaetoceros|Phaeocystis', Genus),
    grepl('Protperidinium|Gyrodinium|Heterocapsa|Prorocentrum', Genus),
    month %in% c(3:10)
  ) |>
  # take sum per genus
  group_by(locationID, date, Genus, group, verticalreference) |>
  summarise(sumClass = sum(mean), .groups = "drop") |>
  # calculate monthly means
  group_by(year=year(date), month=month(date), locationID, Genus, group, verticalreference) |>
  summarise(monthlymeanClass = mean(sumClass), .groups = "drop") |>
  mutate(date = ymd(paste(year, month, "15"))) |>
  ggplot(aes(date, monthlymeanClass)) +
  geom_point(aes(color = Genus), alpha = 0.4) +
  geom_smooth(aes(color = Genus)) +
  scale_y_log10() +
  coord_cartesian(ylim = c(1000, 1000000)) +
  facet_wrap(facets = c("locationID"), ncol = 2)

# yearly summer means
phyto2 %>%
  filter(
    !is.na(group),
    verticalreference == "surface"
  ) %>%
  filter(locationID %in% c("VLISSGBISSVH", "WALCRN2", "NOORDWK2", "NOORDWK10", "TERSLG10", "WALCRN20", "NOORDWK20")) %>%
  # take mean of duplicates
  group_by(locationID, date, month = month(date), Genus, species, group, verticalreference) |>
  summarize(mean = mean(`cells.l`), .groups = "drop") |>
  filter(
    grepl('Thalassiosira|Skeletonema|Chaetoceros|Phaeocystis', Genus),
    # grepl('Protperidinium|Gyrodinium|Heterocapsa|Prorocentrum', Genus),
    month %in%c(3:10)
  ) |>
  # take sum per genus
  group_by(locationID, date, Genus, group, verticalreference) |>
  summarise(sumClass = sum(mean), .groups = "drop") |>
  # calculate monthly means
  group_by(year=year(date), month=month(date), locationID, Genus, group, verticalreference) |>
  summarise(monthlymeanClass = mean(sumClass), .groups = "drop") |>
  # calculate annual means
  group_by(year=year, locationID, Genus, group, verticalreference) |>
  summarise(yearlymeanClass = mean(monthlymeanClass)) |>
  ggplot(aes(year, yearlymeanClass)) +
  geom_point(aes(color = Genus), alpha = 0.4) +
  geom_smooth(aes(color = Genus), method = "lm") +
  # geom_boxplot(aes(group = year, fill = Genus), position=position_dodge(1)) +
  scale_y_log10() +
  # coord_cartesian(ylim = c(100, 1000000)) +
  facet_wrap(facets = c("locationID"), ncol = 2)


