library(dplyr)
library(dbplyr)

con <- nhsbsaR::con_nhsbsa(database = "DALP")

DB <- tbl(con, in_schema("ADNSH", "INT646_BASE_20210401_20220331"))

FACT <- tbl(con, in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT"))

DISP <- FACT |> group_by(YEAR_MONTH, PF_ID, DISP_ID, DISP_OUPDT_TYPE) |> summarise(n = n()) |> select(-n)

ORG <- tbl(con, in_schema("DIM", "CUR_EP_LEVEL_5_FLAT_DIM"))



# Table with forms matched to CH UPRN level, with dispenser info brought in ad-hoc

DB_UPRN <- DB |> left_join(DISP, by = c("YEAR_MONTH", "PF_ID")) |> 
                 left_join(ORG, by = c("DISP_ID" = "LVL_5_OU",
                                       "DISP_OUPDT_TYPE" = "LVL_5_OUPDT")) |> 
  filter(UPRN_FLAG == 1)
  
# PRESC to CH
PRESC_TO_CH <- DB_UPRN |>
  group_by(LVL_5_LTST_ALT_CDE, UPRN) |>
  summarise(NIC_GBP = sum(ITEM_PAY_DR_NIC)/100,
            .groups = "drop")

# CH to DISP




# prep for sankey: from, to, weight, id
# Prefix PRESC AND DISP to help keep Sankey clear in case of dispensing doctors

# 3 level Sankey diagram showing flow of NIC


# Distributions of % of items/nic prescribed/dispensed by top org

# 3D graph: X = % prescribed by top presc org; Y = % dispensed by top disp org; Z (colour) = total items/nic



