-- To run from DALL_REF

-- 1. Drop table if exists
--drop table int646_base_20200401_20250331 purge;

-- 2. Create new 5 year base table
create table int646_base_20200401_20250331 nologging compress for query low as

select
    -- 4 years previous base table with FY 24/25 postcode information joined
    FY,
    YEAR_MONTH,
    PART_DATE,
    EPM_ID,
    PF_ID,
    EPS_FLAG,
    NHS_NO,
    GENDER,
    AGE,
    AGE_BAND,
    BSA_POSTCODE,
    BSA_SLA,
    MATCH_SLA,
    MATCH_SLA_STD,
    MATCH_SLA_PARENT,
    MATCH_TYPE,
    SCORE,
    MAX_MONTHLY_PATIENTS,
    AB_FLAG,
    UPRN_FLAG,
    CH_FLAG,
    UPRN,
    PARENT_UPRN,
    LOCATION_ID,
    NURSING_HOME_FLAG,
    RESIDENTIAL_HOME_FLAG,
    AB_DATE,
    CQC_DATE,
    ITEM_COUNT,
    ITEM_PAY_DR_NIC,
    ITEM_CALC_PAY_QTY,
    PAY_DRUG_RECORD_ID,
    CHAPTER_DESCR,
    SECTION_DESCR,
    PARAGRAPH_DESCR,
    CHEMICAL_SUBSTANCE_BNF_DESCR,
    BNF_CHEMICAL_SUBSTANCE,
    BASE_NAME,
    CHAPTER_1_4_6_10_CAT,
    ACB_CAT,
    DAMN_CAT,
    FALLS_CAT,
    PRESC_SLA,
    PRESC_POSTCODE,
    PRESC_ORG_TYPE,
    PRESC_ORG_SUB_TYPE,
    PRESC_ORG_NM,
    PRESC_ORG_CODE,
    PRESC_ORG_LIST_SIZE,
    PRESCRIBER_TYPE,
    PRESCRIBER_SUB_TYPE,
    PRESCRIBER_NM,
    PRESCRIBER_CODE,
    DISP_CODE,
    DISP_TYPE,
    DISP_NM,
    DISP_TRADING_NM,
    DISP_SLA,
    DISP_POSTCODE,
    pcd.PCD_REGION_CODE,
    pcd.PCD_REGION_NAME,
    pcd.PCD_ICB_CODE,
    pcd.PCD_ICB_NAME,
    pcd.PCD_LAD_CODE,
    pcd.PCD_LAD_NAME,
    pcd.IMD_DECILE
from
    dall_ref.int646_base_20200401_20240331  base
inner join
    int646_postcode_lookup  pcd
    on base.bsa_postcode  =  pcd.postcode

union all

select
    -- Most recent data with FY field manually added 
    '2024/25'  as  FY,
    y5.*
from
    adnsh.int646_base_20240401_20250331 y5;
    
-- 3. Indexes

grant select on int646_base_20200401_20250331 to migar, adnsh, mamcp;

create index int646_i01 on int646_base_20200401_20250331 (fy);
create index int646_i02 on int646_base_20200401_20250331 (fy, year_month);

create index int646_i03 on int646_base_20200401_20250331 (fy, age_band, gender);
create index int646_i04 on int646_base_20200401_20250331 (fy, age_band, gender, pcd_region_code, pcd_region_name);
create index int646_i05 on int646_base_20200401_20250331 (fy, age_band, gender, pcd_icb_code, pcd_icb_name);
create index int646_i06 on int646_base_20200401_20250331 (fy, age_band, gender, pcd_lad_code, pcd_lad_name);
create index int646_i07 on int646_base_20200401_20250331 (fy, imd_decile);
create index int646_i08 on int646_base_20200401_20250331 (ch_flag);
create index int646_i09 on int646_base_20200401_20250331 (fy, ch_flag);
create index int646_i10 on int646_base_20200401_20250331 (fy, nursing_home_flag);
create index int646_i11 on int646_base_20200401_20250331 (fy, residential_home_flag);

create index int646_i12 on int646_base_20200401_20250331 (fy, ch_flag, pcd_region_code, pcd_region_name);
create index int646_i13 on int646_base_20200401_20250331 (fy, ch_flag, pcd_icb_code, pcd_icb_name);
create index int646_i14 on int646_base_20200401_20250331 (fy, ch_flag, pcd_lad_code, pcd_lad_name);

create index int646_i15 on int646_base_20200401_20250331 (fy, chapter_descr, pcd_region_code, pcd_region_name);
create index int646_i16 on int646_base_20200401_20250331 (fy, chapter_descr, pcd_icb_code, pcd_icb_name);
create index int646_i17 on int646_base_20200401_20250331 (fy, chapter_descr, pcd_lad_code, pcd_lad_name);

create index int646_i18 on int646_base_20200401_20250331 (fy, section_descr, pcd_region_code, pcd_region_name);
create index int646_i19 on int646_base_20200401_20250331 (fy, section_descr, pcd_icb_code, pcd_icb_name);
create index int646_i20 on int646_base_20200401_20250331 (fy, section_descr, pcd_lad_code, pcd_lad_name);

create index int646_i21 on int646_base_20200401_20250331 (fy, paragraph_descr, pcd_region_code, pcd_region_name);
create index int646_i22 on int646_base_20200401_20250331 (fy, paragraph_descr, pcd_icb_code, pcd_icb_name);
create index int646_i23 on int646_base_20200401_20250331 (fy, paragraph_descr, pcd_lad_code, pcd_lad_name);

create index int646_i24 on int646_base_20200401_20250331 (fy, chemical_substance_bnf_descr, pcd_region_code, pcd_region_name);
create index int646_i25 on int646_base_20200401_20250331 (fy, chemical_substance_bnf_descr, pcd_icb_code, pcd_icb_name);
create index int646_i26 on int646_base_20200401_20250331 (fy, chemical_substance_bnf_descr, pcd_lad_code, pcd_lad_name);

create index int646_i27 on int646_base_20200401_20250331 (uprn_flag);

create index int646_i28 on int646_base_20200401_20250331 (fy, year_month, nhs_no);
create index int646_i29 on int646_base_20200401_20250331 (fy, year_month, pcd_region_code, pcd_region_name, nhs_no);
create index int646_i30 on int646_base_20200401_20250331 (fy, year_month, pcd_icb_code, pcd_icb_name, nhs_no);
create index int646_i31 on int646_base_20200401_20250331 (fy, year_month, pcd_lad_code, pcd_lad_name, nhs_no);

create index int646_i32 on int646_base_20200401_20250331 (fy, age_band, gender, ch_flag, year_month, nhs_no);