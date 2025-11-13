with
annual_counts as (
    select
        case
            when substr(year_month, 5, 2) >= '04'
            then substr(year_month, 1, 4) || '/' || (to_number(substr(year_month, 1, 4)) + 1)
            else (to_number(substr(year_month, 1, 4)) - 1) || '/' || substr(year_month, 1, 4)
        end as financial_year,
        sum(item_count) as total_item_count,

        -- Columns named according to present values, e.g. age_nhsno means both age and nhsno are present, but not postcode
        sum(case when calc_age != -1 and identified_patient_nhs_no is not null and patient_addr_postcode is not null then item_count else 0 end) as age_nhsno_postcode,
        sum(case when calc_age != -1 and identified_patient_nhs_no is not null and patient_addr_postcode is null     then item_count else 0 end) as age_nhsno,
        sum(case when calc_age != -1 and identified_patient_nhs_no is null     and patient_addr_postcode is not null then item_count else 0 end) as age_postcode,
        sum(case when calc_age = -1  and identified_patient_nhs_no is not null and patient_addr_postcode is not null then item_count else 0 end) as nhsno_postcode,
        sum(case when calc_age != -1 and identified_patient_nhs_no is null     and patient_addr_postcode is null     then item_count else 0 end) as age,
        sum(case when calc_age = -1  and identified_patient_nhs_no is not null and patient_addr_postcode is null     then item_count else 0 end) as nhsno,
        sum(case when calc_age = -1  and identified_patient_nhs_no is null     and patient_addr_postcode is not null then item_count else 0 end) as postcode,
        sum(case when calc_age = -1  and identified_patient_nhs_no is null     and patient_addr_postcode is null     then item_count else 0 end) as none
    from
        aml.px_form_item_elem_comb_fact
    where
        year_month >= '202004' and year_month <= '202503' and
        calc_age >= 65
--        calc_age < 65 or calc_age is null
    group by
        case
            when substr(year_month, 5, 2) >= '04' -- April or later
            then substr(year_month, 1, 4) || '/' || (to_number(substr(year_month, 1, 4)) + 1)
            else (to_number(substr(year_month, 1, 4)) - 1) || '/' || substr(year_month, 1, 4)
        end
    order by
        case
            when substr(year_month, 5, 2) >= '04' -- April or later
            then substr(year_month, 1, 4) || '/' || (to_number(substr(year_month, 1, 4)) + 1)
            else (to_number(substr(year_month, 1, 4)) - 1) || '/' || substr(year_month, 1, 4)
        end
),

overall_counts as (
    select
        sum(item_count) as total_item_count,

        -- Columns named according to present values, e.g. age_nhsno means both age and nhsno are present, but not postcode
        sum(case when calc_age != -1 and identified_patient_nhs_no is not null and patient_addr_postcode is not null then item_count else 0 end) as age_nhsno_postcode,
        sum(case when calc_age != -1 and identified_patient_nhs_no is not null and patient_addr_postcode is null     then item_count else 0 end) as age_nhsno,
        sum(case when calc_age != -1 and identified_patient_nhs_no is null     and patient_addr_postcode is not null then item_count else 0 end) as age_postcode,
        sum(case when calc_age = -1  and identified_patient_nhs_no is not null and patient_addr_postcode is not null then item_count else 0 end) as nhsno_postcode,
        sum(case when calc_age != -1 and identified_patient_nhs_no is null     and patient_addr_postcode is null     then item_count else 0 end) as age,
        sum(case when calc_age = -1  and identified_patient_nhs_no is not null and patient_addr_postcode is null     then item_count else 0 end) as nhsno,
        sum(case when calc_age = -1  and identified_patient_nhs_no is null     and patient_addr_postcode is not null then item_count else 0 end) as postcode,
        sum(case when calc_age = -1  and identified_patient_nhs_no is null     and patient_addr_postcode is null     then item_count else 0 end) as none
    from
        aml.px_form_item_elem_comb_fact
    where
        year_month >= '202004' and year_month <= '202503' and
        calc_age >= 65
--        calc_age < 65 or calc_age is null
)

select
    ac.financial_year,
    -- The percentage of total items for each category
    round((ac.age_nhsno_postcode / nullif(ac.total_item_count, 0)) * 100, 2) as has_all,
    round((ac.age_nhsno          / nullif(ac.total_item_count, 0)) * 100, 2) as missing_postcode,
    round((ac.age_postcode       / nullif(ac.total_item_count, 0)) * 100, 2) as missing_nhsno,
    round((ac.nhsno_postcode     / nullif(ac.total_item_count, 0)) * 100, 2) as missing_age,
    round((ac.age                / nullif(ac.total_item_count, 0)) * 100, 2) as missing_nhsno_and_postcode,
    round((ac.nhsno              / nullif(ac.total_item_count, 0)) * 100, 2) as missing_age_and_postcode,
    round((ac.postcode           / nullif(ac.total_item_count, 0)) * 100, 2) as missing_age_and_nhsno,
    round((ac.none               / nullif(ac.total_item_count, 0)) * 100, 2) as missing_all
from
    annual_counts ac
union all
select
    'Overall' as financial_year,
    -- The percentage of total items for each category
    round((oc.age_nhsno_postcode / nullif(oc.total_item_count, 0)) * 100, 2) as has_all,
    round((oc.age_nhsno          / nullif(oc.total_item_count, 0)) * 100, 2) as missing_postcode,
    round((oc.age_postcode       / nullif(oc.total_item_count, 0)) * 100, 2) as missing_nhsno,
    round((oc.nhsno_postcode     / nullif(oc.total_item_count, 0)) * 100, 2) as missing_age,
    round((oc.age                / nullif(oc.total_item_count, 0)) * 100, 2) as missing_nhsno_and_postcode,
    round((oc.nhsno              / nullif(oc.total_item_count, 0)) * 100, 2) as missing_age_and_postcode,
    round((oc.postcode           / nullif(oc.total_item_count, 0)) * 100, 2) as missing_age_and_nhsno,
    round((oc.none               / nullif(oc.total_item_count, 0)) * 100, 2) as missing_all
from
    overall_counts oc    
;