# SHERPA 1.1 – Soil O-LIVE analysis
# Packages used in this script
library(ggplot2)
library(readxl)
library(tidyr)
library(foreign)
library(dplyr)
library(raster)
library(terra)

Location_soil_live<- read_excel("C:/Users/surya/Downloads/Soil_o_live_data/SOC_DATA(ALL).xlsx", sheet = 3)

Soil_Parameters<- read_excel("C:/Users/surya/Downloads/Soil_o_live_data/SOC_DATA(ALL).xlsx", sheet = 2)

data_soil_live<- merge(Location_soil_live, Soil_Parameters, by= "ID", all = TRUE)

data_soil_live$BD_empirical[is.na(data_soil_live$BD_empirical)] <- data_soil_live$BD_pedotransfer[is.na(data_soil_live$BD_empirical)]

data_soil_live<- data_soil_live[!data_soil_live$COUNTRY=="MOROCCO",]

data_soil_live<- data_soil_live[!is.na(data_soil_live$Clay),]

data_soil_live$class<- paste(data_soil_live$ID,"_",data_soil_live$Rep)

data_soil_live$class1<- paste(data_soil_live$class,"_",data_soil_live$Depth)

data_soil_live <- data_soil_live %>%
  mutate(Input_N = case_when(
    COUNTRY == "GREECE"   & MANAGEMENT == "CONVENTIONAL" ~ 33,
    COUNTRY == "GREECE"   & MANAGEMENT == "HIGH DENSITY" ~ 200,
    COUNTRY == "GREECE"   & MANAGEMENT == "ORGANIC"      ~ 38,

    COUNTRY == "ITALY"    & MANAGEMENT == "CONVENTIONAL" ~ 80,
    COUNTRY == "ITALY"    & MANAGEMENT == "HIGH DENSITY" ~ 120,
    COUNTRY == "ITALY"    & MANAGEMENT == "ORGANIC"      ~ 40,

    COUNTRY == "PORTUGAL" & MANAGEMENT == "CONVENTIONAL" ~ 33,
    COUNTRY == "PORTUGAL" & MANAGEMENT == "HIGH DENSITY" ~ 91,
    COUNTRY == "PORTUGAL" & MANAGEMENT == "ORGANIC"      ~ 32,

    COUNTRY == "SPAIN"    & MANAGEMENT == "CONVENTIONAL" ~ 30,
    COUNTRY == "SPAIN"    & MANAGEMENT == "HIGH DENSITY" ~ 120,
    COUNTRY == "SPAIN"    & MANAGEMENT == "ORGANIC"      ~ 35,

    TRUE ~ NA_real_  # default if nothing matches
  ))

data_soil_live <- data_soil_live %>%
  mutate(Input_P = case_when(
    COUNTRY == "GREECE"   & MANAGEMENT == "CONVENTIONAL" ~ 8,
    COUNTRY == "GREECE"   & MANAGEMENT == "HIGH DENSITY" ~ 30,
    COUNTRY == "GREECE"   & MANAGEMENT == "ORGANIC"      ~ 12,

    COUNTRY == "ITALY"    & MANAGEMENT == "CONVENTIONAL" ~ 21,
    COUNTRY == "ITALY"    & MANAGEMENT == "HIGH DENSITY" ~ 22,
    COUNTRY == "ITALY"    & MANAGEMENT == "ORGANIC"      ~ 14,

    COUNTRY == "PORTUGAL" & MANAGEMENT == "CONVENTIONAL" ~ 8,
    COUNTRY == "PORTUGAL" & MANAGEMENT == "HIGH DENSITY" ~ 12,
    COUNTRY == "PORTUGAL" & MANAGEMENT == "ORGANIC"      ~ 8,

    COUNTRY == "SPAIN"    & MANAGEMENT == "CONVENTIONAL" ~ 8,
    COUNTRY == "SPAIN"    & MANAGEMENT == "HIGH DENSITY" ~ 22,
    COUNTRY == "SPAIN"    & MANAGEMENT == "ORGANIC"      ~ 12,

    TRUE ~ NA_real_  # default if nothing matches
  ))

data_soil_live <- data_soil_live %>%
  mutate(Harvest_N = case_when(
    COUNTRY == "GREECE"   & MANAGEMENT == "CONVENTIONAL" ~ 16.57,
    COUNTRY == "GREECE"   & MANAGEMENT == "HIGH DENSITY" ~ 53.01,
    COUNTRY == "GREECE"   & MANAGEMENT == "ORGANIC"      ~ 26.5,

    COUNTRY == "ITALY"    & MANAGEMENT == "CONVENTIONAL" ~ 16.57,
    COUNTRY == "ITALY"    & MANAGEMENT == "HIGH DENSITY" ~ 53.01,
    COUNTRY == "ITALY"    & MANAGEMENT == "ORGANIC"      ~ 26.5,

    COUNTRY == "PORTUGAL" & MANAGEMENT == "CONVENTIONAL" ~ 16.57,
    COUNTRY == "PORTUGAL" & MANAGEMENT == "HIGH DENSITY" ~ 53.01,
    COUNTRY == "PORTUGAL" & MANAGEMENT == "ORGANIC"      ~26.5,

    COUNTRY == "SPAIN"    & MANAGEMENT == "CONVENTIONAL" ~ 16.57,
    COUNTRY == "SPAIN"    & MANAGEMENT == "HIGH DENSITY" ~ 53.01,
    COUNTRY == "SPAIN"    & MANAGEMENT == "ORGANIC"      ~ 26.5,

    TRUE ~ NA_real_  # default if nothing matches
  ))

data_soil_live <- data_soil_live %>%
  mutate(Harvest_P = case_when(
    COUNTRY == "GREECE"   & MANAGEMENT == "CONVENTIONAL" ~ 2.38,
    COUNTRY == "GREECE"   & MANAGEMENT == "HIGH DENSITY" ~ 7.63,
    COUNTRY == "GREECE"   & MANAGEMENT == "ORGANIC"      ~ 3.81,

    COUNTRY == "ITALY"    & MANAGEMENT == "CONVENTIONAL" ~ 2.38,
    COUNTRY == "ITALY"    & MANAGEMENT == "HIGH DENSITY" ~ 7.63,
    COUNTRY == "ITALY"    & MANAGEMENT == "ORGANIC"      ~ 3.81,

    COUNTRY == "PORTUGAL" & MANAGEMENT == "CONVENTIONAL" ~ 2.38,
    COUNTRY == "PORTUGAL" & MANAGEMENT == "HIGH DENSITY" ~ 7.63,
    COUNTRY == "PORTUGAL" & MANAGEMENT == "ORGANIC"      ~3.81,

    COUNTRY == "SPAIN"    & MANAGEMENT == "CONVENTIONAL" ~ 2.38,
    COUNTRY == "SPAIN"    & MANAGEMENT == "HIGH DENSITY" ~ 7.63,
    COUNTRY == "SPAIN"    & MANAGEMENT == "ORGANIC"      ~ 3.81,

    TRUE ~ NA_real_  # default if nothing matches
  ))

data_soil_live$Total_N<- data_soil_live$Input_N- data_soil_live$Harvest_N

data_soil_live$Total_P<- data_soil_live$Input_P- data_soil_live$Harvest_P

data_soil_live <- data_soil_live %>%
  mutate(Organic_F_per = case_when(
    COUNTRY == "GREECE"   & MANAGEMENT == "CONVENTIONAL" ~ 45,
    COUNTRY == "GREECE"   & MANAGEMENT == "HIGH DENSITY" ~ 15,
    COUNTRY == "GREECE"   & MANAGEMENT == "ORGANIC"      ~ 95,

    COUNTRY == "ITALY"    & MANAGEMENT == "CONVENTIONAL" ~ 45,
    COUNTRY == "ITALY"    & MANAGEMENT == "HIGH DENSITY" ~ 15,
    COUNTRY == "ITALY"    & MANAGEMENT == "ORGANIC"      ~ 95,

    COUNTRY == "PORTUGAL" & MANAGEMENT == "CONVENTIONAL" ~ 45,
    COUNTRY == "PORTUGAL" & MANAGEMENT == "HIGH DENSITY" ~ 15,
    COUNTRY == "PORTUGAL" & MANAGEMENT == "ORGANIC"      ~95,

    COUNTRY == "SPAIN"    & MANAGEMENT == "CONVENTIONAL" ~ 50,
    COUNTRY == "SPAIN"    & MANAGEMENT == "HIGH DENSITY" ~ 15,
    COUNTRY == "SPAIN"    & MANAGEMENT == "ORGANIC"      ~ 95,

    TRUE ~ NA_real_  # default if nothing matches
  ))

data_soil_live <- data_soil_live %>%
  mutate(Mineral_F_per = case_when(
    COUNTRY == "GREECE"   & MANAGEMENT == "CONVENTIONAL" ~ 55,
    COUNTRY == "GREECE"   & MANAGEMENT == "HIGH DENSITY" ~ 85,
    COUNTRY == "GREECE"   & MANAGEMENT == "ORGANIC"      ~ 5,

    COUNTRY == "ITALY"    & MANAGEMENT == "CONVENTIONAL" ~ 55,
    COUNTRY == "ITALY"    & MANAGEMENT == "HIGH DENSITY" ~ 85,
    COUNTRY == "ITALY"    & MANAGEMENT == "ORGANIC"      ~ 5,

    COUNTRY == "PORTUGAL" & MANAGEMENT == "CONVENTIONAL" ~ 55,
    COUNTRY == "PORTUGAL" & MANAGEMENT == "HIGH DENSITY" ~ 85,
    COUNTRY == "PORTUGAL" & MANAGEMENT == "ORGANIC"      ~5,

    COUNTRY == "SPAIN"    & MANAGEMENT == "CONVENTIONAL" ~ 50,
    COUNTRY == "SPAIN"    & MANAGEMENT == "HIGH DENSITY" ~ 85,
    COUNTRY == "SPAIN"    & MANAGEMENT == "ORGANIC"      ~ 5,

    TRUE ~ NA_real_  # default if nothing matches
  ))

BD_data_depth1<- data_soil_live[data_soil_live$Depth==10,]

BD_data_depth2<- data_soil_live[data_soil_live$Depth==20,]

BD_data_depth1[] <- lapply(BD_data_depth1, function(col) {
  if (is.character(col)) {
    col_clean <- gsub(",", ".", col)
    if (all(is.na(suppressWarnings(as.numeric(col_clean))) == is.na(col))) {
      return(as.numeric(col_clean))
    } else {
      return(col)
    }
  } else {
    return(col)
  }
})

df_avg_depth1 <- BD_data_depth1 %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    dplyr::across(where(is.character), ~ first(.x)),
    .groups = "drop"
  )
df_avg_depth2 <- BD_data_depth2 %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    dplyr::across(where(is.numeric), mean, na.rm = TRUE),
    dplyr::across(where(is.character), ~ first(.x))
  )

Todat_data<- rbind(df_avg_depth1,df_avg_depth2)

Final_BD_data <- Todat_data %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    dplyr::across(where(is.numeric), mean, na.rm = TRUE),
    dplyr::across(where(is.character), ~ first(.x))
  )

Copper_Parameters<- read_excel("C:/Users/surya/Downloads/Soil_o_live_data/Copper_Soil_O_Live.xlsx", sheet = 1)

Final_Cu_data <- Copper_Parameters %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    dplyr::across(where(is.numeric), mean, na.rm = TRUE),
    dplyr::across(where(is.character), ~ first(.x))
  )
Final_Cu_data<- Final_Cu_data[,c(1,3)]

Final_BD_data<- merge(Final_BD_data,Final_Cu_data, by = "ID")

##############Soil_o_live_data

Final_BD_data1<- Final_BD_data[,c(1,4,5,20, 22,23,26,31,32,33,34,36,42:59)]

Soil_o_live_data<- read.dbf("C:/Users/surya/Downloads/Soil_o_live_data/Final_soil_o_live_data.dbf")

Soil_o_live_data1<- Soil_o_live_data[,c(2,11,12,14,16:58)]

Soil_o_live_data1$Hg[Soil_o_live_data1$ID == "ES-TD9"] <- 8.1
Soil_o_live_data1$Hg[Soil_o_live_data1$ID == "IT-ORG18"] <- 0.73
Soil_o_live_data1$Hg[Soil_o_live_data1$ID == "IT-ORG19"] <- 0.43

Soil_o_live_data1$BD_Panos[Soil_o_live_data1$ID == "IT-ORG48"] <- 0.95

Soil_o_live_data<- merge(Final_BD_data1,Soil_o_live_data1, by = "ID")

Soil_o_live_data$BD_empirical[Soil_o_live_data$ID == "GR-ORG47"] <- 0.99

Soil_o_live_data$SOC_clay_ratio<- Soil_o_live_data$Co_dc1/Soil_o_live_data$Clay

#####read_new_soil erosion_data

Soil_erosion_data<- read_excel("C:/Users/surya/Downloads/Soil_o_live_data/Soil_loss_OLIVE Pilot Sites/Soil_loss_OLIVE Pilot Sites/Excel/STATs_SOIL_OLIVE_Parcels.xlsx", sheet = 1)

colnames(Soil_erosion_data)[which(colnames(Soil_erosion_data)%in% c("ID_PLOT"))]<- "ID"

Soil_o_live_data<- merge(Soil_o_live_data,Soil_erosion_data, by = "ID", all = TRUE)

colnames(Soil_o_live_data)[which(colnames(Soil_o_live_data)%in% c("Soil_loss"))]<- "SE"

Soil_o_live_data$SE[Soil_o_live_data$ID == "PT-ORG42"] <- 1.91

uy<- Soil_o_live_data[,c(1,51)]

Soil_o_live_data<- Soil_o_live_data[!is.na(Soil_o_live_data$SE),]

########Part2 calculations
############Soil erosion

Soil_o_live_data<-Soil_o_live_data %>% mutate(Soil_erosion_number =
                                                case_when(SE == -9999 ~ NA_real_,
                                                          SE>= 0 &  SE<=0.5 ~ 0,
                                                          SE> 0.5 &  SE<=1~ 1,
                                                          SE> 1 &  SE<=2 ~ 2,
                                                          SE> 2 &  SE<=3 ~ 3,
                                                          SE> 3 &  SE<=4 ~ 4,
                                                          SE> 4 &  SE<=5 ~ 5,
                                                          SE> 5 &  SE<=6 ~ 6,
                                                          SE> 6 &  SE<=8 ~ 7,
                                                          SE> 8 &  SE<=10 ~ 8,
                                                          SE> 10 ~ 9
                                                )
)

###########################land slide

colnames(Soil_o_live_data)[which(colnames(Soil_o_live_data)%in% c("LD"))]<- "LS"

Soil_o_live_data<-Soil_o_live_data %>% mutate(LS_number =
                                                case_when(LS==1 ~ 6,
                                                          LS==2 ~ 1,
                                                          LS==3 ~ 4,
                                                          LS== -9999 ~ 0
                                                )
)

#######################Heavy metals

colnames(Soil_o_live_data)[which(colnames(Soil_o_live_data)%in% c("Zn"))]<- "Zinc"

Soil_o_live_data<-Soil_o_live_data %>% mutate(Zinc_number =
                                                case_when(Zinc == -9999 ~ NA_real_,
                                                          Zinc>= 0 &  Zinc<=33 ~ 0,
                                                          Zinc> 33 &  Zinc<=57~ 1,
                                                          Zinc> 57 &  Zinc<=81 ~ 2,
                                                          Zinc> 81 &  Zinc<=105 ~ 3,
                                                          Zinc> 105 &  Zinc<=129 ~ 4,
                                                          Zinc> 129 &  Zinc<=153 ~ 5,
                                                          Zinc> 153 &  Zinc<=177 ~ 6,
                                                          Zinc> 177 &  Zinc<=201 ~ 7,
                                                          Zinc> 201 &  Zinc<=225 ~ 8,
                                                          Zinc> 225 ~ 9
                                                )
)

Soil_o_live_data<-Soil_o_live_data %>% mutate(Antimony_number =
                                                case_when(Antimony == -9999 ~ NA_real_,
                                                          Antimony>= 0 &  Antimony<=0.80 ~ 0,
                                                          Antimony> 0.80 &  Antimony<=1.30 ~ 1,
                                                          Antimony> 1.30 &  Antimony<=1.80~ 2,
                                                          Antimony> 1.80 &  Antimony<=2.30 ~ 3,
                                                          Antimony> 2.30 &  Antimony<=2.80 ~ 4,
                                                          Antimony> 2.80 &  Antimony<=3.20 ~ 5,
                                                          Antimony> 3.20 &  Antimony<=3.80 ~ 6,
                                                          Antimony> 3.80 &  Antimony<=4.30 ~ 7,
                                                          Antimony> 4.30 &  Antimony<=5.00 ~ 8,
                                                          Antimony> 5 ~ 9
                                                )
)

Soil_o_live_data<-Soil_o_live_data %>% mutate(Lead_number =
                                                case_when(Lead == -9999 ~ NA_real_,
                                                          Lead>= 0 &  Lead<=23 ~ 0,
                                                          Lead> 23 &  Lead<=42 ~ 1,
                                                          Lead> 42 &  Lead<=61~ 2,
                                                          Lead> 61 &  Lead<=80 ~ 3,
                                                          Lead> 80 &  Lead<=99 ~ 4,
                                                          Lead> 99 &  Lead<=118 ~ 5,
                                                          Lead> 118 &  Lead<=137 ~ 6,
                                                          Lead> 137 &  Lead<=156 ~ 7,
                                                          Lead> 156 &  Lead<=175 ~ 8,
                                                          Lead> 175 ~ 9
                                                )
)

colnames(Soil_o_live_data)[which(colnames(Soil_o_live_data)%in% c("Nicle"))]<- "Nickel"

Soil_o_live_data<-Soil_o_live_data %>% mutate(Nickel_number =
                                                case_when(Nickel == -9999 ~ NA_real_,
                                                          Nickel>= 0 &  Nickel<=9 ~ 0,
                                                          Nickel> 9 &  Nickel<=14 ~ 1,
                                                          Nickel> 14 & Nickel<=20~ 2,
                                                          Nickel> 20 &  Nickel<=25 ~ 3,
                                                          Nickel> 25 &  Nickel<=31 ~ 4,
                                                          Nickel> 31 &  Nickel<=36 ~ 5,
                                                          Nickel> 36 &  Nickel<=42 ~ 6,
                                                          Nickel> 42 &  Nickel<=47 ~ 7,
                                                          Nickel> 47 &  Nickel<=53 ~ 8,
                                                          Nickel> 53 ~ 9
                                                )
)

colnames(Soil_o_live_data)[which(colnames(Soil_o_live_data)%in% c("Hg"))]<- "Mercury"

Soil_o_live_data_hg<- Soil_o_live_data[,c("ID","Mercury")]

Soil_o_live_data<-Soil_o_live_data %>% mutate(Mercury_number =
                                                case_when(Mercury == -9999 ~ NA_real_,
                                                          Mercury>= 0 &  Mercury<=23 ~ 0,
                                                          Mercury> 23 &  Mercury<=179 ~ 1,
                                                          Mercury> 179 & Mercury<=332~ 2,
                                                          Mercury> 332 &  Mercury<=485 ~ 3,
                                                          Mercury> 485 &  Mercury<=638 ~ 4,
                                                          Mercury> 638 &  Mercury<=791 ~ 5,
                                                          Mercury> 791 &  Mercury<=944 ~ 6,
                                                          Mercury> 944 &  Mercury<=1097 ~ 7,
                                                          Mercury> 1097 &  Mercury<=1250 ~ 8,
                                                          Mercury> 1250 ~ 9
                                                )
)

Soil_o_live_data<-Soil_o_live_data %>% mutate(Cobalt_number =
                                                case_when(Cobalt == -9999 ~ NA_real_,
                                                          Cobalt>= 0 &  Cobalt<=20 ~ 0,
                                                          Cobalt> 20 &  Cobalt<=25 ~ 1,
                                                          Cobalt> 25 & Cobalt<=30~ 2,
                                                          Cobalt> 30 &  Cobalt<=35 ~ 3,
                                                          Cobalt> 35 &  Cobalt<=40 ~ 4,
                                                          Cobalt> 40 &  Cobalt<=45 ~ 5,
                                                          Cobalt> 45 &  Cobalt<=50 ~ 6,
                                                          Cobalt> 50 &  Cobalt<=55 ~ 7,
                                                          Cobalt> 55 &  Cobalt<=60 ~ 8,
                                                          Cobalt> 60 ~ 9
                                                )
)

colnames(Soil_o_live_data)[which(colnames(Soil_o_live_data)%in% c("Cromium"))]<- "Chromium"

Soil_o_live_data<-Soil_o_live_data %>% mutate(Chromium_number =
                                                case_when(Chromium == -9999 ~ NA_real_,
                                                          Chromium>= 0 &  Chromium<=70 ~ 0,
                                                          Chromium> 70 &  Chromium<=90 ~ 1,
                                                          Chromium> 90 & Chromium<=110~ 2,
                                                          Chromium> 110 &  Chromium<=130 ~ 3,
                                                          Chromium> 130 &  Chromium<=155 ~ 4,
                                                          Chromium> 155 &  Chromium<=180 ~ 5,
                                                          Chromium> 180 &  Chromium<=205 ~ 6,
                                                          Chromium> 205 &  Chromium<=230 ~ 7,
                                                          Chromium> 230 &  Chromium<=250 ~ 8,
                                                          Chromium> 250 ~ 9
                                                )
)

colnames(Soil_o_live_data)[which(colnames(Soil_o_live_data)%in% c("CuT"))]<- "Copper"

Soil_o_live_data<-Soil_o_live_data %>% mutate(Copper_number =
                                                case_when(Copper == -9999 ~ NA_real_,
                                                          Copper>= 0 &  Copper<=22 ~ 0,
                                                          Copper> 22 &  Copper<=32 ~ 1,
                                                          Copper> 32 & Copper<=41~ 2,
                                                          Copper> 41 &  Copper<=50 ~ 3,
                                                          Copper> 50 &  Copper<=59 ~ 4,
                                                          Copper> 59 &  Copper<=68 ~ 5,
                                                          Copper> 68 &  Copper<=77 ~ 6,
                                                          Copper> 77 &  Copper<=86 ~ 7,
                                                          Copper> 86 &  Copper<=95 ~ 8,
                                                          Copper> 95 ~ 9
                                                )
)

Soil_o_live_data<-Soil_o_live_data %>% mutate(Cadmium_number =
                                                case_when(Cadmium == -9999 ~ NA_real_,
                                                          Cadmium>= 0 &  Cadmium<=0.5 ~ 0,
                                                          Cadmium> 0.5 &  Cadmium<=0.6 ~ 1,
                                                          Cadmium> 0.6 & Cadmium<=0.8~ 2,
                                                          Cadmium> 0.8 &  Cadmium<=1.0 ~ 3,
                                                          Cadmium> 1.0 &  Cadmium<=1.2 ~ 4,
                                                          Cadmium> 1.2 &  Cadmium<=1.4 ~ 5,
                                                          Cadmium> 1.4 &  Cadmium<=1.6 ~ 6,
                                                          Cadmium> 1.6 &  Cadmium<=1.8 ~ 7,
                                                          Cadmium> 1.8 &  Cadmium<=2.0 ~ 8,
                                                          Cadmium> 2.0 ~ 9
                                                )
)

Soil_o_live_data<-Soil_o_live_data %>% mutate(Arsenic_number =
                                                case_when(Arsenic == -9999 ~ NA_real_,
                                                          Arsenic>= 0 &  Arsenic<=10 ~ 0,
                                                          Arsenic> 10 &  Arsenic<=30 ~ 1,
                                                          Arsenic> 30 & Arsenic<=50~ 2,
                                                          Arsenic> 50 &  Arsenic<=70 ~ 3,
                                                          Arsenic> 70 &  Arsenic<=95 ~ 4,
                                                          Arsenic> 95 &  Arsenic<=120 ~ 5,
                                                          Arsenic> 120 &  Arsenic<=145 ~ 6,
                                                          Arsenic> 145 &  Arsenic<=175 ~ 7,
                                                          Arsenic> 175 &  Arsenic<=200 ~ 8,
                                                          Arsenic> 200 ~ 9
                                                )
)

########################EC

Soil_o_live_data$EC<- Soil_o_live_data$ElectrC

Soil_o_live_data<-Soil_o_live_data %>% mutate(EC_number =
                                                case_when(EC == -9999 ~ NA_real_,
                                                          EC>= 0 &  EC<=2 ~ 0,
                                                          EC> 2 &  EC<=2.2 ~ 1,
                                                          EC> 2.2 & EC<=2.4~ 2,
                                                          EC> 2.4 &  EC<=2.6 ~ 3,
                                                          EC> 2.6 &  EC<=2.9 ~ 4,
                                                          EC> 2.9 &  EC<=3.1 ~ 5,
                                                          EC> 3.1 &  EC<=3.4 ~ 6,
                                                          EC> 3.4 &  EC<=3.7 ~ 7,
                                                          EC> 3.7 &  EC<=4 ~ 8,
                                                          EC> 4 ~ 9
                                                )
)

##################Nitrogen

Soil_o_live_data<-Soil_o_live_data %>% mutate(Nitrogen_number =
                                                case_when(Total_N == -9999 ~ NA_real_,
                                                          Total_N<2 ~ 0,
                                                          Total_N> 2 &  Total_N<=3 ~ 1,
                                                          Total_N> 3 & Total_N<=4~ 2,
                                                          Total_N>4 &  Total_N<=5 ~ 3,
                                                          Total_N> 5 &  Total_N<=10 ~ 4,
                                                          Total_N> 10 & Total_N<=15 ~ 5,
                                                          Total_N> 15 &  Total_N<=20 ~ 6,
                                                          Total_N> 20 &  Total_N<=25 ~ 7,
                                                          Total_N> 25 & Total_N<=30 ~ 8,
                                                          Total_N> 30 ~ 9
                                                )
)

Soil_o_live_data <- Soil_o_live_data %>%
  mutate(Phophorus_number_excess = case_when(
    Total_P == -9999 ~ NA_real_,  # Missing value
    Total_P <= 0 ~ 0,  # Assign 0 for all negative values and zero
    Total_P > 0 & Total_P <= 1 ~ 0,
    Total_P > 1 & Total_P <= 2 ~ 1,
    Total_P > 2 & Total_P <= 3 ~ 2,
    Total_P> 3 & Total_P <= 4 ~ 3,
    Total_P > 4 & Total_P <= 5 ~ 4,
    Total_P > 5 & Total_P <= 6 ~ 5,
    Total_P > 6 & Total_P <= 7 ~ 6,
    Total_P > 7 & Total_P <= 8 ~ 7,
    Total_P > 8 & Total_P <= 9 ~ 8,
    Total_P > 9 ~ 9  # Assign 9 for values greater than 9
  ))

Soil_pesticide_data<- read_excel("C:/Users/surya/Downloads/Soil_o_live_data/Olive_riskscore_V2.xlsx", sheet = 1)

Soil_pesticide_data <- Soil_pesticide_data %>%
  mutate(
    Depth = sub("^([^_]+_[^_]+)_.*", "\\1", ID),
    Site  = sub("^[^_]+_[^_]+_(.*_[^_]+)$", "\\1", ID)
  )

colnames(Soil_pesticide_data)[which(colnames(Soil_pesticide_data)%in% c("Risk Score (RS)"))]<- "RS_M"

colnames(Soil_pesticide_data)[which(colnames(Soil_pesticide_data)%in% c("Number of AI detected"))]<- "AI_M"

Soil_pesticide_data <- Soil_pesticide_data %>%
  mutate(Pesticide_Risk_Score1 = case_when(
    RS_M <= 0 & AI_M == 0 ~ 0,  # Negligible risk, no active ingredient
    RS_M <= 0 & AI_M >= 0 ~ 1,
    RS_M > 0 & RS_M <= 1 & AI_M > 0 & AI_M <= 10 ~ 2,# Negligible risk, some active ingredie  # Low risk, AI between 1-10
    RS_M > 1 & RS_M <= 2 & AI_M >= 1 & AI_M <= 10 ~ 3,  # Medium-low risk, AI between 1-10
    RS_M > 1 & RS_M <= 2 & AI_M > 10 & AI_M <= 20 ~ 4, # Medium-low risk, AI between 10-20
    RS_M > 2 & RS_M <= 3 & AI_M > 1 & AI_M <= 10 ~ 5,  # Medium risk, AI between 1-10
    RS_M > 2 & RS_M <= 3 & AI_M > 10 & AI_M <= 20 ~ 6, # Medium risk, AI between 10-20
    RS_M > 3 &RS_M <= 4 & AI_M > 1 & AI_M <= 10 ~ 7,  # High risk, AI between 1-10
    RS_M > 3 & RS_M <= 4 & AI_M > 10 & AI_M <= 20 ~ 8, # High risk, AI between 10-20
    RS_M > 4 & AI_M > 10 ~ 9,  # Very high risk, AI greater than 10
    TRUE ~ NA_real_  # Assign NA for unclassified cases
  ))

Soil_pesticide_data1<- Soil_pesticide_data[,c("Site","Pesticide_Risk_Score1")]

Site_avg <- Soil_pesticide_data1 %>%
  dplyr::group_by(Site) %>%
  dplyr::summarise(
    Avg_Pesticide_Risk_Score1 = mean(Pesticide_Risk_Score1, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

Soil_pesticide_data1<- Site_avg[,c("Site","Avg_Pesticide_Risk_Score1")]

colnames(Soil_pesticide_data1)[which(colnames(Soil_pesticide_data1)%in% c("Site"))]<- "ID"

colnames(Soil_pesticide_data1)[which(colnames(Soil_pesticide_data1)%in% c("Avg_Pesticide_Risk_Score1"))]<- "Pesticide_Risk_Score1"

Soil_pesticide_data1$ID <- trimws(gsub("_","-", Soil_pesticide_data1$ID))

Soil_o_live_data1<- merge(Soil_o_live_data,Soil_pesticide_data1, by = "ID")

Soil_data2<- Soil_o_live_data1[,c("ID","Pesticide_Risk_Score1")]

###########################Old_pesticide_data

Location_soil_live_pesticide<- read_excel("C:/Users/surya/Downloads/Soil_o_live_data/Olive_riskscore_pesticides.xlsx", sheet = 1)

Location_soil_live_pesticide<-Location_soil_live_pesticide %>% mutate(Depth_number =
                                                                        case_when(Depth == 1 ~ 10,
                                                                                  Depth == 2 ~ 20
                                                                        )
)

Location_soil_live_pesticide$class<- paste(Location_soil_live_pesticide$ID,"_",Location_soil_live_pesticide$Replicate)

Location_soil_live_pesticide$class1<- paste(Location_soil_live_pesticide$class,"_",Location_soil_live_pesticide$Depth_number)

Location_soil_live_pesticide<- Location_soil_live_pesticide[!Location_soil_live_pesticide$Country=="MOROCCO",]

colnames(Location_soil_live_pesticide)[which(colnames(Location_soil_live_pesticide)%in% c("Risk Score (RS)"))]<- "RS_meas"

colnames(Location_soil_live_pesticide)[which(colnames(Location_soil_live_pesticide)%in% c("Number of AI detected"))]<- "AI_meas"

Location_soil_live_pesticide <- Location_soil_live_pesticide %>%
  mutate(Pesticide_Risk_Score1 = case_when(
    RS_meas <= 0 & AI_meas == 0 ~ 0,  # Negligible risk, no active ingredient
    RS_meas <= 0 & AI_meas >= 0 ~ 1,
    RS_meas > 0 & RS_meas <= 1 & AI_meas> 0 & AI_meas <= 10 ~ 2,# Negligible risk, some active ingredie  # Low risk, AI between 1-10
    RS_meas > 1 & RS_meas <= 2 & AI_meas >= 1 & AI_meas <= 10 ~ 3,  # Medium-low risk, AI between 1-10
    RS_meas > 1 & RS_meas <= 2 & AI_meas > 10 & AI_meas <= 20 ~ 4, # Medium-low risk, AI between 10-20
    RS_meas > 2 & RS_meas <= 3 & AI_meas > 1 & AI_meas <= 10 ~ 5,  # Medium risk, AI between 1-10
    RS_meas > 2 & RS_meas <= 3 & AI_meas > 10 & AI_meas <= 20 ~ 6, # Medium risk, AI between 10-20
    RS_meas > 3 & RS_meas <= 4 & AI_meas > 1 & AI_meas <= 10 ~ 7,  # High risk, AI between 1-10
    RS_meas > 3 & RS_meas <= 4 & AI_meas > 10 & AI_meas <= 20 ~ 8, # High risk, AI between 10-20
    RS_meas > 4 & AI_meas > 10 ~ 9,  # Very high risk, AI greater than 10
    TRUE ~ NA_real_  # Assign NA for unclassified cases
  ))

Pesticide_data_depth1<- Location_soil_live_pesticide[Location_soil_live_pesticide$Depth==1,]

Pesticide_data_depth2<- Location_soil_live_pesticide[Location_soil_live_pesticide$Depth==2,]

Pesticide_data_depth1[] <- lapply(Pesticide_data_depth1, function(col) {
  if (is.character(col)) {
    col_clean <- gsub(",", ".", col)
    if (all(is.na(suppressWarnings(as.numeric(col_clean))) == is.na(col))) {
      return(as.numeric(col_clean))
    } else {
      return(col)
    }
  } else {
    return(col)
  }
})

pest_avg_depth1 <- Pesticide_data_depth1 %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    dplyr::across(where(is.character), ~ first(.x)),
    .groups = "drop"
  )
pest_avg_depth2 <- Pesticide_data_depth2 %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    dplyr::across(where(is.numeric), mean, na.rm = TRUE),
    dplyr::across(where(is.character), ~ first(.x))
  )

Todat_data_pest<- rbind(pest_avg_depth1,pest_avg_depth2)

Final_pest_data <- Todat_data_pest %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    dplyr::across(where(is.numeric), mean, na.rm = TRUE),
    dplyr::across(where(is.character), ~ first(.x))
  )

Final_pest_data1<- Final_pest_data[,c("ID","Pesticide_Risk_Score1")]

Soil_o_live_data2<- merge(Soil_o_live_data,Final_pest_data1, by = "ID")

missing_in_df2 <- anti_join(Soil_o_live_data2, Soil_o_live_data1, by = "ID")

Soil_o_live_data3<- rbind(Soil_o_live_data1, missing_in_df2)

###################################SOC loss

Soil_o_live_data<- Soil_o_live_data3

Soil_o_live_data$SOC_clay_ratio<- Soil_o_live_data$Co_dc1/Soil_o_live_data$Clay

Soil_o_live_data <- Soil_o_live_data %>%
  mutate(SOC_clay_raio_number = case_when(
    SOC_clay_ratio == -9999 ~ NA_real_,
    SOC_clay_ratio >= 0.125 ~ 0,
    SOC_clay_ratio < 0.125 & SOC_clay_ratio >= 0.111 ~ 1,
    SOC_clay_ratio < 0.111 & SOC_clay_ratio >= 0.1   ~ 3,
    SOC_clay_ratio < 0.1   & SOC_clay_ratio >= 0.090 ~ 5,
    SOC_clay_ratio < 0.090 & SOC_clay_ratio >= 0.0833 ~ 7,
    SOC_clay_ratio < 0.0833 & SOC_clay_ratio >= 0.077 ~ 8,
    SOC_clay_ratio < 0.077 ~ 9
  ))

Soil_o_live_data <- Soil_o_live_data %>%
  mutate(SOC_score2 = case_when(
    Co_dc1 > 5               ~ 0,
    Co_dc1 > 4.5 & Co_dc1 <= 5  ~ 1,
    Co_dc1 > 4   & Co_dc1 <= 4.5  ~ 2,
    Co_dc1 > 3.5 & Co_dc1 <= 4    ~ 3,
    Co_dc1 > 3   & Co_dc1 <= 3.5  ~ 4,
    Co_dc1 > 2.5 & Co_dc1 <= 3    ~ 5,
    Co_dc1 > 2   & Co_dc1 <= 2.5  ~ 6,
    Co_dc1 > 1.5 & Co_dc1 <= 2    ~ 7,
    Co_dc1 > 0.5 & Co_dc1 <= 1.5  ~ 8,
    Co_dc1 <= 0.5                     ~ 9,
    TRUE                                ~ NA_real_
  ))

Soil_o_live_data$soc_final_number<- (Soil_o_live_data$SOC_clay_raio_number + Soil_o_live_data$SOC_score2)/2

#########################################compaction

colnames(Soil_o_live_data)[which(colnames(Soil_o_live_data)%in% c("BD_empirical"))]<- "BD"

colnames(Soil_o_live_data)[which(colnames(Soil_o_live_data)%in% c("Soil_textu"))]<- "Soil_tex_class"

Soil_o_live_data <- Soil_o_live_data %>%
  mutate(BD = ifelse(BD == -9999, NA, BD)) %>%  # Convert BD -9999 to NA
  mutate(Soil_bulk_density_score = case_when(
    Soil_tex_class == -9999 ~ NA_real_,
    Soil_tex_class == "CLAY" ~ ifelse(BD < 1.10, 0, ifelse(BD >= 1.10 & BD <= 1.47, 3, 9)),
    Soil_tex_class == "CLAY LOAM" ~ ifelse(BD < 1.40, 0, ifelse(BD >= 1.40 & BD <= 1.75, 3, 9)),
    Soil_tex_class == "LOAM" ~ ifelse(BD < 1.40, 0, ifelse(BD >= 1.40 & BD <= 1.80, 3, 9)),
    Soil_tex_class == "Loamy Sand" ~ ifelse(BD < 1.60, 0, ifelse(BD >= 1.60 & BD <= 1.80, 3, 9)),
    Soil_tex_class == "Sand" ~ ifelse(BD < 1.60, 0, ifelse(BD >= 1.60 & BD <= 1.80, 3, 9)),
    Soil_tex_class == "SANDY CLAY LOAM" ~ ifelse(BD < 1.40, 0, ifelse(BD >= 1.40 & BD <= 1.75, 3, 9)),
    Soil_tex_class == "SANDY LOAM" ~ ifelse(BD < 1.40, 0, ifelse(BD >= 1.40 & BD <= 1.80, 3, 9)),
    Soil_tex_class == "Silt Loam" ~ ifelse(BD < 1.40, 0, ifelse(BD >= 1.40 & BD <= 1.65, 3, 9)),
    Soil_tex_class == "SANDY CLAY" ~ ifelse(BD < 1.10, 0, ifelse(BD >= 1.10 & BD <= 1.58, 3, 9)),
    Soil_tex_class == "Silty Clay Loam" ~ ifelse(BD < 1.40, 0, ifelse(BD >= 1.40 & BD <= 1.65, 3, 9)),
    TRUE ~ NA_real_  # Assign NA for unrecognized texture names
  ))

#############PanosBD#########
colnames(Soil_o_live_data)[which(colnames(Soil_o_live_data)%in% c("BD_Panos"))]<- "BD1"

Soil_o_live_data <- Soil_o_live_data %>%
  mutate(BD1 = ifelse(BD1 == -9999, NA, BD1)) %>%  # Convert BD -9999 to NA
  mutate(Soil_bulk_density_score_Panos = case_when(
    Soil_tex_class == -9999 ~ NA_real_,
    Soil_tex_class == "CLAY" ~ ifelse(BD1 < 1.10, 0, ifelse(BD1 >= 1.10 & BD1 <= 1.47, 3, 9)),
    Soil_tex_class == "CLAY LOAM" ~ ifelse(BD1 < 1.40, 0, ifelse(BD1 >= 1.40 & BD1 <= 1.75, 3, 9)),
    Soil_tex_class == "LOAM" ~ ifelse(BD1 < 1.40, 0, ifelse(BD1 >= 1.40 & BD1 <= 1.80, 3, 9)),
    Soil_tex_class == "Loamy Sand" ~ ifelse(BD1 < 1.60, 0, ifelse(BD1 >= 1.60 & BD1 <= 1.80, 3, 9)),
    Soil_tex_class == "Sand" ~ ifelse(BD1 < 1.60, 0, ifelse(BD1 >= 1.60 & BD1 <= 1.80, 3, 9)),
    Soil_tex_class == "SANDY CLAY LOAM" ~ ifelse(BD1 < 1.40, 0, ifelse(BD1 >= 1.40 & BD1 <= 1.75, 3, 9)),
    Soil_tex_class == "SANDY LOAM" ~ ifelse(BD1 < 1.40, 0, ifelse(BD1 >= 1.40 & BD1 <= 1.80, 3, 9)),
    Soil_tex_class == "Silt Loam" ~ ifelse(BD1 < 1.40, 0, ifelse(BD1 >= 1.40 & BD1 <= 1.65, 3, 9)),
    Soil_tex_class == "SANDY CLAY" ~ ifelse(BD1 < 1.10, 0, ifelse(BD1 >= 1.10 & BD1 <= 1.58, 3, 9)),
    Soil_tex_class == "Silty Clay Loam" ~ ifelse(BD1 < 1.40, 0, ifelse(BD1 >= 1.40 & BD1 <= 1.65, 3, 9)),
    TRUE ~ NA_real_  # Assign NA for unrecognized texture names
  ))

#####################Connecting the gully data
#Gully_data<- read.csv("C:/Users/surya/Documents/Copy of Gully_Sherpa_score.csv")

Gully_data<- read.csv("C:/Users/surya/Documents/Gully_data_values.csv")

Gully_data$ID <- trimws(gsub("_", "-", Gully_data$ID))

Gully_data <- Gully_data %>%
  mutate(
    max_score = case_when(
      Gullies_per_ha == 0                         ~  0,
      Gullies_per_ha > 0    & Gullies_per_ha <= 0.06 ~ 1,
      Gullies_per_ha > 0.06 & Gullies_per_ha <= 0.07 ~ 2,
      Gullies_per_ha > 0.07 & Gullies_per_ha <= 0.08 ~ 3,
      Gullies_per_ha > 0.08 & Gullies_per_ha <= 0.09 ~ 4,
      Gullies_per_ha > 0.09 & Gullies_per_ha <= 0.10 ~ 5,
      Gullies_per_ha > 0.10 & Gullies_per_ha <= 0.15 ~ 6,
      Gullies_per_ha > 0.15 & Gullies_per_ha <= 0.20 ~ 7,
      Gullies_per_ha > 0.20 & Gullies_per_ha <= 0.25 ~ 8,
      Gullies_per_ha > 0.25                       ~ 9,
      TRUE                                        ~ NA_real_
    )
  )

table(Gully_data$max_score, useNA = "ifany")

Soil_o_live_data6<- merge(Soil_o_live_data, Gully_data, by = "ID")

Soil_o_live_data4<- Soil_o_live_data6[,c(1,2,3,13:20,25,7,31,32,78:100,104)]

Soil_o_live_data4$Part1_number_old<- Soil_o_live_data4$Nitrogen_number+Soil_o_live_data4$EC_number+
  Soil_o_live_data4$Arsenic_number+Soil_o_live_data4$Cadmium_number+
  Soil_o_live_data4$Copper_number+Soil_o_live_data4$Chromium_number+
  Soil_o_live_data4$Cobalt_number+Soil_o_live_data4$Mercury_number+
  Soil_o_live_data4$Nickel_number+Soil_o_live_data4$Lead_number+Soil_o_live_data4$Antimony_number+
  Soil_o_live_data4$Zinc_number+Soil_o_live_data4$Soil_erosion_number+
  Soil_o_live_data4$Soil_bulk_density_score+Soil_o_live_data4$Pesticide_Risk_Score+
  Soil_o_live_data4$LS_number+
  Soil_o_live_data4$Phophorus_number_excess+Soil_o_live_data4$soc_final_number+Soil_o_live_data4$max_score

Soil_o_live_data4<- Soil_o_live_data4[!is.na(Soil_o_live_data4$Part1_number_old),]

Soil_o_live_data4$Part1_number_old<- (Soil_o_live_data4$Part1_number_old)/19

hm_cols <- c("Arsenic_number","Cadmium_number","Copper_number","Chromium_number",
             "Cobalt_number","Mercury_number","Nickel_number","Lead_number",
             "Antimony_number","Zinc_number")

Soil_o_live_data4[hm_cols] <- lapply(Soil_o_live_data4[hm_cols], function(x) ifelse(x == 0, NA, x))

Soil_o_live_data4$HM_average12 <- rowMeans(Soil_o_live_data4[hm_cols], na.rm = TRUE)

other_cols <- c("Nitrogen_number","EC_number","max_score",
                "Soil_erosion_number","Soil_bulk_density_score",
                "Pesticide_Risk_Score1","LS_number",
                "Phophorus_number_excess","soc_final_number")
Soil_o_live_data4$Part1_averaged_Score <- rowMeans(
  cbind(Soil_o_live_data4$HM_average12, Soil_o_live_data4[other_cols]),
  na.rm = TRUE
)

########################part1

grid <- list.files("C:/Users/surya/Downloads/Fcover_data_2018_NC/First_four_months/" , pattern = "*.tif$")
All_cov <- raster::stack(paste0("C:/Users/surya/Downloads/Fcover_data_2018_NC/First_four_months/", grid))

sp.pnts = Soil_o_live_data4[,c("LONGITUDE", "LATITUDE")]
data<- terra::extract(All_cov,sp.pnts)
hghgh15<- cbind(Soil_o_live_data4,data)

hghgh15$Fcover<- hghgh15$Mean_2018_Fcover*100

hghgh15<- hghgh15 %>%
  mutate(Fcover_number = case_when(
    Fcover >= 80 & Fcover <= 100 ~ 10,
    Fcover < 80 & Fcover >= 75 ~ 9,
    Fcover < 75 & Fcover >= 70 ~ 8,
    Fcover < 70 & Fcover >= 65 ~ 7,
    Fcover < 65 & Fcover >= 60 ~ 6,
    Fcover < 60 & Fcover >= 55 ~ 5,
    Fcover < 55 & Fcover >= 50 ~ 4,
    Fcover < 50 & Fcover >= 45 ~ 3,
    Fcover < 45 & Fcover >= 40 ~ 2,
    Fcover < 40 ~ 1
  ))

hghgh15 <-hghgh15 %>%
  mutate(Fertilizer_type_number = case_when(
    Organic_F_per == 100 ~ 10,  # Organic fertilizer only
    Organic_F_per == 0 ~ 1,  # Mineral fertilizer only
    Organic_F_per >= 95 & Organic_F_per <= 100 ~ 9,
    Organic_F_per < 95 & Organic_F_per >= 80 ~ 8,
    Organic_F_per < 80 & Organic_F_per >= 65 ~ 7,
    Organic_F_per< 65 & Organic_F_per >= 50 ~ 6,
    Organic_F_per < 50 & Organic_F_per>= 35 ~ 5,
    Organic_F_per < 35 & Organic_F_per >= 20 ~ 4,
    Organic_F_per < 20 & Organic_F_per >= 10 ~ 3,
    Organic_F_per < 10 & Organic_F_per > 0 ~ 2
  ))

hghgh15$Part2<- (hghgh15$Fcover_number+hghgh15$Fertilizer_type_number)/2

hghgh15$Final_number<- hghgh15$Part2-hghgh15$Part1_averaged_Score

test<- hghgh15[,c(1:48)]

test[hm_cols] <- lapply(test[hm_cols], function(x) {
  x[is.na(x)] <- 0
  x
})

old_names <- c(
  "Soil_erosion_number", "LS_number", "Zinc_number", "Antimony_number", "Lead_number",
  "Nickel_number", "Mercury_number", "Cobalt_number", "Chromium_number", "Copper_number",
  "Cadmium_number", "Arsenic_number", "EC_number","Pesticide_Risk_Score1","Nitrogen_number","Phophorus_number_excess","soc_final_number",
  "Soil_bulk_density_score","max_score", "Part1_averaged_Score","Part2", "Final_number"
)

new_names <- c(
  "Soil erosion", "Landslide", "Zinc", "Antimony", "Lead",
  "Nickel", "Mercury", "Cobalt", "Chromium", "Copper",
  "Cadmium", "Arsenic", "Salinity","Pesticide","Nitrogen","Phophorus excess","Carbon assessment", "Compaction","Gully", "Part2", "Part1", "Final_number"
)

test <- test %>% rename(!!!setNames(old_names, new_names))

Part1_part2_grassland <- test %>%
  mutate(Country = as.character(Country))  # Convert Class to character if needed
Part1_part2_grassland_avg <- Part1_part2_grassland %>%
  group_by(Country) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))

variable_order <- c(
  "Part1","Soil erosion","Landslide","Gully", "Compaction",
  "Salinity","Pesticide","Nitrogen","Phophorus excess","Carbon assessment", "Zinc",
  "Antimony", "Lead", "Nickel", "Mercury",
  "Cobalt", "Chromium", "Copper", "Cadmium",
  "Arsenic"
)

data_long <- Part1_part2_grassland_avg %>%
  dplyr::select(all_of(c("Country", variable_order))) %>%  # Select required variables
  pivot_longer(cols = -Country,
               names_to = "Variable",
               values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = variable_order)) %>%  # Order variables
  group_by(Country) %>%
  arrange(Country, Variable) %>%  # Ensure correct order for cumulative calculation
  mutate(Cumulative = if_else(Variable == "Part1", Value, NA_real_)) %>%  # Initialize Cumulative
  mutate(Cumulative = replace(Cumulative, is.na(Cumulative),
                              first(Cumulative, order_by = Variable) - cumsum(Value[Variable != "Part1"]))) %>%
  ungroup()

data_long$Country <- as.factor(data_long$Country)

class_colors <- c("#7fbf4d","#d864a6","#ffbf33","#4a98c9")

# Histogram Plot
ggplot(test, aes(x = Final_number, fill = Country)) +
  geom_histogram(binwidth = 2, alpha = 0.6, position = "stack") +
  labs(title = "Histogram of Final Score (Olive_trees)",
       x = "Final SHERPA Score",
       y = "Number of Samples") +
  scale_fill_manual(values = class_colors) +  # Apply color mapping
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 2),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16)
  )

# Line Plot
ggplot(data_long, aes(x = Variable, y = Cumulative, color = Country, group = Country)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(-60, 10)) +
  scale_color_manual(values = class_colors) +  # Apply color mapping
  labs(
    title = "Cumulative Score (Olive_trees)",
    x = "Variable",
    y = "Cumulative Reduction",
    color = "Country"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 2),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16)
  )

percentage_lt <- test %>%
  summarise(
    percent = mean(Final_number <= 0, na.rm = TRUE) * 100
  )

test %>%
  group_by(COUNTRY) %>%
  summarise(mean_value = median(max_score, na.rm = TRUE))
