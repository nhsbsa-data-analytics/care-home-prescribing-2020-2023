with base as (
    select
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
        DISP_POSTCODE
    from dall_ref.int646_base_20200401_20240331
    fetch first 10 rows only
)
-- check for postcodes not in new mapping
--,
--check_for_missing_postcodes as (
--    select *
--    from base
--    where bsa_postcode not in (select postcode from int646_postcode_lookup)
--)
--select * from check_for_missing_postcodes
---- 0 rows
select *
from base
left join (
    select
        POSTCODE,
        PCD_REGION_CODE,
        PCD_REGION_NAME,
        PCD_ICB_CODE,
        PCD_ICB_NAME,
        PCD_LAD_CODE,
        PCD_LAD_NAME,
        IMD_DECILE
    from int646_postcode_lookup
) pl
on pl.postcode = base.bsa_postcode
;


/*
Recreate base table using newer mappings

Version 1.0

AMENDMENTS:
	2025-30-04  : Mark McPherson    : Script creation
    date        : name              : details


DESCRIPTION:
    Recreates base table using postcode lookup table for geographic mappings and IMD.

DEPENDENCIES:
    DALL_REF.INT646_BASE_20200401_20240331
    {USER_SCHEMA}.INT646_POSTCODE_LOOKUP

NOTES:
    The postcode lookup table is not in DALL_REF, so the actual schema needs to be added before running.
        - CTRL-F "USER_SCHEMA"
    Dropping and creating of table commented out to prevent accidental running.
*/

------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------SCRIPT START----------------------------------------------------------------------------------------------------------------------

--drop table int646_base_20200401_20240331;
--create table int646_base_20200401_20240331 compress for query high as
----- SECTION START: Base table
with
base as (
    select
        year_month,
        part_date,
        epm_id,
        pf_id,
        eps_flag,
        nhs_no, 
        gender,
        age,
        age_band,
        bsa_postcode,
        bsa_sla,
        match_sla,
        match_sla_std,
        match_sla_parent,
        match_type,
        score,
        max_monthly_patients,
        ab_flag,
        uprn_flag,
        ch_flag,
        uprn,
        parent_uprn,
        location_id,
        nursing_home_flag,
        residential_home_flag,
        ab_date,
        cqc_date,
        item_count,
        item_pay_dr_nic,
        item_calc_pay_qty,
        pay_drug_record_id,
        chapter_descr,
        section_descr,
        paragraph_descr,
        chemical_substance_bnf_descr,
        bnf_chemical_substance,
        base_name,
        chapter_1_4_6_10_cat,
        acb_cat,
        damn_cat,
        falls_cat,
        presc_sla,
        presc_postcode,
        presc_org_type,
        presc_org_sub_type,
        presc_org_nm,
        presc_org_code,
        presc_org_list_size,
        prescriber_type,
        prescriber_sub_type,
        prescriber_nm,
        prescriber_code,
        disp_code,
        disp_type,
        disp_nm,
        disp_trading_nm,
        disp_sla,
        disp_postcode
    from dall_ref.int646_base_20200401_20240331
)
--select * from base fetch first 10 rows only;
----- SECTION END: Base table

----- SECTION START: Check all postcodes exist in mapping
, missing_postcodes as (
    select *
    from base
    where bsa_postcode not in (select postcode from int646_postcode_lookup)
)
select * from missing_postcodes fetch first 10 rows only;
-- Should be 0 rows. If so, can comment out this check section.
----- SECTION END: Check all postcodes exist in mapping

----- SECTION START: Output
select *
from base
left join (
    select
        postcode,
        pcd_region_code,
        pcd_region_name,
        pcd_icb_code,
        pcd_icb_name,
        pcd_lad_code,
        pcd_lad_name,
        imd_decile
    from USER_SCHEMA.int646_postcode_lookup
) pl
on pl.postcode = base.bsa_postcode
;
-----SECTION END: Output

--------------------SCRIPT END------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
