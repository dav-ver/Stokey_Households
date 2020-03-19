library(dplyr)
library(tidyverse)

Postcode_to_Output_Area_to_Lower_Layer_Super_Output_Area_to_Middle_Layer_Super_Output_Area_to_Local_Authority_District_February_2018_Lookup_in_the_UK <- read.csv("C:/Users/David/Downloads/Postcode_to_Output_Area_to_Lower_Layer_Super_Output_Area_to_Middle_Layer_Super_Output_Area_to_Local_Authority_District_February_2018_Lookup_in_the_UK.csv")

Area_Codes = Postcode_to_Output_Area_to_Lower_Layer_Super_Output_Area_to_Middle_Layer_Super_Output_Area_to_Local_Authority_District_February_2018_Lookup_in_the_UK

Area_Codes = Area_Codes %>%
  select(3, 7)

Stokey_Area_Codes = Area_Codes %>%
  filter(pcds %in% Stoke.Newington.Ward.Postcodes$V1)

Stokey_Area_Codes2 = Area_Codes %>%
  filter(oa11 %in% Stokey_Area_Codes$oa11)

names(MYPE_2018_London_OA)[1] = "oa11"

Stokey_Area_Codes3 = Stokey_Area_Codes2%>%
  left_join(MYPE_2018_London_OA, by = "oa11")

Stokey_Area_Codes4 = Stokey_Area_Codes3 %>%
  mutate("Over_70" = rowSums(select(.,X70:X90.)),
         "Population" = rowSums(select(.,X0:X90.)))

Stokey_Area_Codes5 = Stokey_Area_Codes4 %>%
  group_by(oa11) %>%
  summarise(Over_70_Pop = min(Over_70), 
            Pop = min(Population),
            Total_pcds = n(), 
            Pop_per_pcd = Pop/Total_pcds,
            Over_70s_per_pcd = Over_70_Pop/Total_pcds,
            Over_70s_perc = 100*(Over_70_Pop/Pop))

Stokey_Area_Codes6 = Stokey_Area_Codes %>%
  left_join(Stokey_Area_Codes5, by = "oa11") %>%
  arrange(desc(Over_70s_per_pcd), oa11, pcds) %>%
  mutate_if(is.numeric, round)

write.csv(Stokey_Area_Codes6, file = "~/Over_70s_by_postcode.csv", row.names = FALSE) 
