

-- STEP 3: AB-CQC Union Check --------------------------------------------------


-- STEP 4: Form Check ----------------------------------------------------------

-- 20/21: 878m
-- 21/22: 885m
-- 22/23: 880m
-- 23/24: 639m

select /* +parallel(16) */ count(*) from mamcp.int646_forms_20200401_20210331
union all
select /* +parallel(16) */ count(*) from mamcp.int646_forms_20210401_20220331
union all
select /* +parallel(16) */ count(*) from mamcp.int646_forms_20220401_20230331
union all
select /* +parallel(16) */ count(*) from adnsh.int646_forms_20230401_20240331;

-- STEP 5: Match Output Check --------------------------------------------------

-- STEP 6: Postcode Lookup Check -----------------------------------------------

-- STEP 7: Final Base Table Check ----------------------------------------------

-- 20/21: 878m
-- 21/22: 885m
-- 22/23: 880m
-- 23/24: 639m

select count(*) from mamcp.int646_forms_20200401_20210331
union all
select count(*) from mamcp.int646_forms_20210401_20220331
union all
select count(*) from mamcp.int646_forms_20220401_20230331
union all
select count(*) from adnsh.int646_forms_20230401_20240331;

--------------------------------------------------------------------------------