/*
Recreate base table using newer mappings

Version 1.0

AMENDMENTS:
    2025-30-04  : Mark McPherson    : Script creation
    2025-05-13  : Steven Buckley    : Amendments to script to improve efficiency
                                        "missing_postcodes" check section replaced with seperate query
                                            Switched to use join rather than "not in" filter which improves run time from over 1hr to 2.5min
                                            Removes need to comment out code during execution
                                        Adjusted table creation code to single query (removing unneccessary sub-queries)
                                            Also set this to initially created a new table rather than replace the existing table
                                                This allows output to be checked
                                        Added new sections to compare the new table to the existing table and then handling removing and renaming tables
    2025-16-10  : Mark McPherson    : Replace last year in table names with END_YEAR + add note to check year and update before running                                     
    date        : name              : details


DESCRIPTION:
    Recreates base table using postcode lookup table for geographic mappings and IMD.

DEPENDENCIES:
    DALL_REF.INT646_BASE_20200401_{END_YEAR}0331
    {USER_SCHEMA}.INT646_POSTCODE_LOOKUP

NOTES:
    The postcode lookup table is not in DALL_REF, so the actual schema needs to be added before running.
        - CTRL-F "USER_SCHEMA"

    The last year in the table names will need to be set correctly throughout
        - CTRL-F "END_YEAR"

    Dropping and creating of table (see REPLACE DATASET section) commented out to prevent accidental running.
    
    Permissions on the table will need to be added back once finished.
*/
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------SCRIPT START----------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------------------------------------
----------REVIEW POSTCODE MAPPING---------------------------------------------------------------------------------------------------------------------
--All postcodes should be covered by the INT646_POSTCODE_LOOKUP table
--Therefore query should return NULL for records that cannot be joined
select      'Original Table : New postcode check'   as DATA_SOURCE,
            sum(1)                                  as ROW_COUNT,
            sum(base.PF_ID)                         as PF_ID_SUM,
            sum(base.ITEM_COUNT)                    as ITEM_SUM
from        DALL_REF.INT646_BASE_20200401_END_YEAR0331  base
left join   USER_SCHEMA.INT646_POSTCODE_LOOKUP      pcd    on  base.BSA_POSTCODE    =   pcd.POSTCODE
where       1=1
    and     pcd.POSTCODE is null
;
----------REVIEW POSTCODE MAPPING---------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------------------------------------
----------CREATE REVISED DATA TABLE-------------------------------------------------------------------------------------------------------------------
--drop table INT646_BASE_20200401_END_YEAR0331_V2;
create table INT646_BASE_20200401_END_YEAR0331_V2 compress for query high as
select      base.FY,
            base.YEAR_MONTH,
            base.PART_DATE,
            base.EPM_ID,
            base.PF_ID,
            base.EPS_FLAG,
            base.NHS_NO, 
            base.GENDER,
            base.AGE,
            base.AGE_BAND,
            base.BSA_POSTCODE,
            base.BSA_SLA,
            base.MATCH_SLA,
            base.MATCH_SLA_STD,
            base.MATCH_SLA_PARENT,
            base.MATCH_TYPE,
            base.SCORE,
            base.MAX_MONTHLY_PATIENTS,
            base.AB_FLAG,
            base.UPRN_FLAG,
            base.CH_FLAG,
            base.UPRN,
            base.PARENT_UPRN,
            base.LOCATION_ID,
            base.NURSING_HOME_FLAG,
            base.RESIDENTIAL_HOME_FLAG,
            base.AB_DATE,
            base.CQC_DATE,
            base.ITEM_COUNT,
            base.ITEM_PAY_DR_NIC,
            base.ITEM_CALC_PAY_QTY,
            base.PAY_DRUG_RECORD_ID,
            base.CHAPTER_DESCR,
            base.SECTION_DESCR,
            base.PARAGRAPH_DESCR,
            base.CHEMICAL_SUBSTANCE_BNF_DESCR,
            base.BNF_CHEMICAL_SUBSTANCE,
            base.BASE_NAME,
            base.CHAPTER_1_4_6_10_CAT,
            base.ACB_CAT,
            base.DAMN_CAT,
            base.FALLS_CAT,
            base.PRESC_SLA,
            base.PRESC_POSTCODE,
            base.PRESC_ORG_TYPE,
            base.PRESC_ORG_SUB_TYPE,
            base.PRESC_ORG_NM,
            base.PRESC_ORG_CODE,
            base.PRESC_ORG_LIST_SIZE,
            base.PRESCRIBER_TYPE,
            base.PRESCRIBER_SUB_TYPE,
            base.PRESCRIBER_NM,
            base.PRESCRIBER_CODE,
            base.DISP_CODE,
            base.DISP_TYPE,
            base.DISP_NM,
            base.DISP_TRADING_NM,
            base.DISP_SLA,
            base.DISP_POSTCODE,
            pl.PCD_REGION_CODE,
            pl.PCD_REGION_NAME,
            pl.PCD_ICB_CODE,
            pl.PCD_ICB_NAME,
            pl.PCD_LAD_CODE,
            pl.PCD_LAD_NAME,
            pl.IMD_DECILE
from        DALL_REF.INT646_BASE_20200401_END_YEAR0331  base
left join   (
            select      POSTCODE,
                        PCD_REGION_CODE,
                        PCD_REGION_NAME,
                        PCD_ICB_CODE,
                        PCD_ICB_NAME,
                        PCD_LAD_CODE,
                        PCD_LAD_NAME,
                        IMD_DECILE
            from        USER_SCHEMA.INT646_POSTCODE_LOOKUP
            )                                       pl    on pl.POSTCODE = base.BSA_POSTCODE
;
----------CREATE REVISED DATA TABLE-------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------------------------------------
----------COMPARE DATASETS----------------------------------------------------------------------------------------------------------------------------
--Process is to recreate table replacing geographic mapping data
--Therefore the underlying data should not change
--Check figures to allow comparison with new created table
--Compare column details (via USER_TAB_COLS system view) to confirm both tables have the same structure
--
--If row count figures match and table structures match, it is safe to replace the original table with the revised table

with
column_count as 
(
select      TABLE_NAME, 
            count(distinct(COLUMN_NAME)) as COLUMN_COUNT
from        USER_TAB_COLS
where       1=1
    and     TABLE_NAME in ('INT646_BASE_20200401_END_YEAR0331','INT646_BASE_20200401_END_YEAR0331_V2')
group by    TABLE_NAME
)
--select * from column_count;
,
unique_column_list as
(
select      FLAG_ORIGINAL_TABLE,
            FLAG_NEW_TABLE,
            listagg(COLUMN_NAME,',') within group (order by COLUMN_NAME) as UNIQUE_COLUMN_LIST            
from        (            
            select      COLUMN_NAME,
                        max(case when TABLE_NAME = 'INT646_BASE_20200401_END_YEAR0331' then 1 else 0 end)       as FLAG_ORIGINAL_TABLE,
                        max(case when TABLE_NAME = 'INT646_BASE_20200401_END_YEAR0331_V2' then 1 else 0 end)    as FLAG_NEW_TABLE
            from        USER_TAB_COLS
            where       1=1
                and     TABLE_NAME in ('INT646_BASE_20200401_END_YEAR0331','INT646_BASE_20200401_END_YEAR0331_V2')
            group by    COLUMN_NAME
            )
where       1=1
    and     (   FLAG_ORIGINAL_TABLE = 0
            or  FLAG_NEW_TABLE      = 0
            )
group by    FLAG_ORIGINAL_TABLE,
            FLAG_NEW_TABLE
)
--select * from unique_column_list;

select      'Original Table' as DATA_SOURCE,
            cc.TABLE_NAME,
            cc.COLUMN_COUNT,
            ucl.UNIQUE_COLUMN_LIST,
            dat.ROW_COUNT,
            dat.PF_ID_SUM,
            dat.ITEM_SUM
from        (
            select      sum(1)          as ROW_COUNT,
                        sum(PF_ID)      as PF_ID_SUM,
                        sum(ITEM_COUNT) as ITEM_SUM
            from        INT646_BASE_20200401_END_YEAR0331
            )                   dat
left join   column_count        cc  on  cc.TABLE_NAME = 'INT646_BASE_20200401_END_YEAR0331'
left join   unique_column_list  ucl on  ucl.FLAG_ORIGINAL_TABLE = 1

union all   

select      'New Table' as DATA_SOURCE,
            cc.TABLE_NAME,
            cc.COLUMN_COUNT,
            ucl.UNIQUE_COLUMN_LIST,
            dat.ROW_COUNT,
            dat.PF_ID_SUM,
            dat.ITEM_SUM
from        (
            select      sum(1)          as ROW_COUNT,
                        sum(PF_ID)      as PF_ID_SUM,
                        sum(ITEM_COUNT) as ITEM_SUM
            from        INT646_BASE_20200401_END_YEAR0331_V2
            )                   dat
left join   column_count        cc  on  cc.TABLE_NAME = 'INT646_BASE_20200401_END_YEAR0331_V2'
left join   unique_column_list  ucl on  ucl.FLAG_NEW_TABLE = 1
;
----------COMPARE DATASETS----------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------------------------------------
----------REPLACE DATASET-----------------------------------------------------------------------------------------------------------------------------
--drop the existing table
--drop table INT646_BASE_20200401_END_YEAR0331;

--rename the new table
--alter table INT646_BASE_20200401_END_YEAR0331_V2 rename to INT646_BASE_20200401_END_YEAR0331;

----------REPLACE DATASET-----------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------------------------------------
----------FOLLOW-UP ACTIONS---------------------------------------------------------------------------------------------------------------------------
--Once the table has been recreated and used to replace the previous version there are two follow-up actions that need to be handled

--INDEX CREATION
-- run the 'create index' commands in the table creation script from the Git repo
-- the latest version of the script is:
-- https://github.com/nhsbsa-data-analytics/care-home-prescribing-2020-2023/blob/main/data-raw/workflow/workflow_union_5_years_in_one_table.sql

--TABLE ACCESS
-- if the table is used to create a new table with the same name, the table access process will reassign access the next time it is run the following morning
-- if access is required sooner than this raise a request can be made to trigger the data access manager process
----------FOLLOW-UP ACTIONS---------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------



--------------------SCRIPT END------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------