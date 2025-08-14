with
counts as (
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
    group by
        case
            when substr(year_month, 5, 2) >= '04' -- April or later
            then substr(year_month, 1, 4) || '/' || (to_number(substr(year_month, 1, 4)) + 1)
            else (to_number(substr(year_month, 1, 4)) - 1) || '/' || substr(year_month, 1, 4)
        end
)

select
    c.financial_year,
    -- The percentage of total items for each category
    round((c.age_nhsno_postcode / nullif(c.total_item_count, 0)) * 100, 2) as has_all,
    round((c.age_nhsno          / nullif(c.total_item_count, 0)) * 100, 2) as missing_postcode,
    round((c.age_postcode       / nullif(c.total_item_count, 0)) * 100, 2) as missing_nhsno,
    round((c.nhsno_postcode     / nullif(c.total_item_count, 0)) * 100, 2) as missing_age,
    round((c.age                / nullif(c.total_item_count, 0)) * 100, 2) as missing_nhsno_and_postcode,
    round((c.nhsno              / nullif(c.total_item_count, 0)) * 100, 2) as missing_age_and_postcode,
    round((c.postcode           / nullif(c.total_item_count, 0)) * 100, 2) as missing_age_and_nhsno,
    round((c.none               / nullif(c.total_item_count, 0)) * 100, 2) as missing_all
from
    counts c;
