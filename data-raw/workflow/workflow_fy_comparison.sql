
-- STEP 1: AB Check ------------------------------------------------------------

-- 20/21: 29.1m
-- 21/22: 29.4m
-- 22/23: 29.7m
-- 23/24: 38.0m (no filters, such as country, applied to DALL_REF ABP)

select /* +parallel(16) */ count(*) from mamcp.INT646_ABP_20210324
union all
select /* +parallel(16) */ count(*) from mamcp.INT646_ABP_20220324
union all
select /* +parallel(16) */ count(*) from mamcp.INT646_ABP_20230331
union all
select /* +parallel(16) */ count(*) from adnsh.ADDRESSBASE_PLUS_20240516;

-- STEP 2: CQC Check (single epoch used for initial 3 FY -----------------------

-- MAMCP: 29.3k
-- ADNSH: 29.9k

select /* +parallel(16) */ count(*) from mamcp.INT646_CQC_20230602
union all
select /* +parallel(16) */ count(*) from adnsh.CQC_BASE_20240621;

-- STEP 3: AB-CQC Union Check --------------------------------------------------

-- 20/21: 846k
-- 21/22: 834k
-- 22/23: 837k
-- 23/24: 743k

select /* +parallel(16) */ count(*) from mamcp.int646_abp_cqc_20200401_20210331
union all
select /* +parallel(16) */ count(*) from mamcp.int646_abp_cqc_20210401_20220331
union all
select /* +parallel(16) */ count(*) from mamcp.int646_abp_cqc_20220401_20230331
union all
select /* +parallel(16) */ count(*) from adnsh.int646_abp_cqc_20230401_20240331;

-- STEP 4: Form Check ----------------------------------------------------------

-- 20/21: 257m
-- 21/22: 264m
-- 22/23: 274m
-- 23/24: 285m

select /* +parallel(16) */ count(*) from mamcp.int646_forms_20200401_20210331
union all
select /* +parallel(16) */ count(*) from mamcp.int646_forms_20210401_20220331
union all
select /* +parallel(16) */ count(*) from mamcp.int646_forms_20220401_20230331
union all
select /* +parallel(16) */ count(*) from adnsh.int646_forms_20230401_20240331;

-- STEP 5: Match Output Check --------------------------------------------------

-- 20/21: 20.8m
-- 21/22: 20.9m
-- 22/23: 21.9m
-- 23/24: 22.9m

select /* +parallel(16) */ count(*) from mamcp.int646_match_20200401_20210331
union all
select /* +parallel(16) */ count(*) from mamcp.int646_match_20210401_20220331
union all
select /* +parallel(16) */ count(*) from mamcp.int646_match_20220401_20230331
union all
select /* +parallel(16) */ count(*) from adnsh.int646_match_20230401_20240331;

-- STEP 5.1: Match Output Match Type Check -------------------------------------

-- Order: NON-EXACT / EXACT  

-- 20/21: 15m / 4.6m 
-- 21/22: 15m / 4.7m 
-- 22/23: 16m / 5.1m 
-- 23/24: 17m / 5.3m

select /* +parallel(16) */ match_type, count(*) from mamcp.int646_match_20200401_20210331 group by match_type 
union all
select /* +parallel(16) */ match_type, count(*) from mamcp.int646_match_20210401_20220331 group by match_type
union all
select /* +parallel(16) */ match_type, count(*) from mamcp.int646_match_20220401_20230331 group by match_type
union all
select /* +parallel(16) */ match_type, count(*) from adnsh.int646_match_20230401_20240331 group by match_type;

-- STEP 5.2: Match Output CH Flag Check ----------------------------------------

-- Order: Y / N

-- 20/21: 15.9m / 4.9m
-- 21/22: 16.0m / 4.9m
-- 22/23: 16.9m / 4.9m
-- 23/24: 18.6m / 4.3m

select /* +parallel(16) */ ch_flag, count(*) from mamcp.int646_match_20200401_20210331 group by ch_flag
union all
select /* +parallel(16) */ ch_flag, count(*) from mamcp.int646_match_20210401_20220331 group by ch_flag
union all
select /* +parallel(16) */ ch_flag, count(*) from mamcp.int646_match_20220401_20230331 group by ch_flag
union all
select /* +parallel(16) */ ch_flag, count(*) from adnsh.int646_match_20230401_20240331 group by ch_flag;

-- STEP 6: Postcode Lookup Check -----------------------------------------------

-- MAMCP: 224k
-- ADNSH: 224k

select /* +parallel(16) */ count(*) from mamcp.int646_postcode_lookup
union all
select /* +parallel(16) */ count(*) from adnsh.int646_postcode_lookup;

-- STEP 7: Final Base Table Check ----------------------------------------------

-- 20/21: 595m
-- 21/22: 602m
-- 22/23: 598m
-- 23/24: 639m

select /* +parallel(16) */ count(*) from dall_ref.int646_base_20200401_20230331 where fy = '2020/21'
union all
select /* +parallel(16) */ count(*) from dall_ref.int646_base_20200401_20230331 where fy = '2021/22'
union all
select /* +parallel(16) */ count(*) from dall_ref.int646_base_20200401_20230331 where fy = '2022/23'
union all
select /* +parallel(16) */ count(*) from adnsh.int646_base_20230401_20240331;

-- STEP 7.1: Final Base Table Match Type Check ---------------------------------

-- Order: NON-EXACT / EXACT / NO MATCH / DOUBLE KW / SINGLE KW / PATIENT COUNT

-- 20/21: 34.5m / 10.3m / 548m
-- 21/22: 34.3m / 10.6m / 555m
-- 22/23: 35.3m / 11.1m / 550m
-- 23/24: 37.7m / 11.6m / 589m

select /* +parallel(16) */ fy, match_type, count(*) from dall_ref.int646_base_20200401_20230331 where fy = '2020/21' group by fy, match_type 
union all
select /* +parallel(16) */ fy, match_type, count(*) from dall_ref.int646_base_20200401_20230331 where fy = '2021/22' group by fy, match_type
union all
select /* +parallel(16) */ fy, match_type, count(*) from dall_ref.int646_base_20200401_20230331 where fy = '2022/23' group by fy, match_type
union all
select /* +parallel(16) */ '2023/24'  as  fy, match_type, count(*) from adnsh.int646_base_20230401_20240331 group by '2023/24', match_type order by fy, match_type;

-- STEP 7.2: Final Base Table CH Flag Check ------------------------------------

-- Order: Y / N

-- 20/21: 35m / 560m
-- 21/22: 35m / 566m
-- 22/23: 37m / 561m
-- 23/24: 40m / 599m

select /* +parallel(16) */ fy, ch_flag, count(*) from dall_ref.int646_base_20200401_20230331 where fy = '2020/21' group by fy, ch_flag
union all
select /* +parallel(16) */ fy, ch_flag, count(*) from dall_ref.int646_base_20200401_20230331 where fy = '2021/22' group by fy, ch_flag
union all
select /* +parallel(16) */ fy, ch_flag, count(*) from dall_ref.int646_base_20200401_20230331 where fy = '2022/23' group by fy, ch_flag
union all
select /* +parallel(16) */ '2023/24'  as  fy, ch_flag, count(*) from adnsh.int646_base_20230401_20240331 group by '2023/24', ch_flag order by fy, ch_flag;

--------------------------------------------------------------------------------