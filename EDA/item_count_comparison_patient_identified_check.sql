with

counts_fact_65plus as (
    select
        case
            when substr(year_month, 5, 2) >= '04'
            then substr(year_month, 1, 4) || '/' || (to_number(substr(year_month, 3, 2)) + 1)
            else (to_number(substr(year_month, 1, 4)) - 1) || '/' || substr(year_month, 3, 2)
        end as fy,
        patient_identified,
        sum(item_count) as total_item_count_65plus
    from
        aml.px_form_item_elem_comb_fact
    where 1=1
        and calc_age >= 65
        and year_month >= 202004 and year_month <= 202503
--        and patient_identified = 'Y'
        and pay_da_end = 'N'
        and pay_nd_end = 'N'
        and pay_rb_end = 'N'
        and cd_req = 'N'
        and oohc_ind = 0
        and private_ind = 0
        and ignore_flag = 'N'
        and item_count >= 1
        AND DISPENSER_COUNTRY_OU = 1 AND PRESC_COUNTRY_OU = 1 -- we indirectly filter to England only on the postcode lookup
    group by
        case
            when substr(year_month, 5, 2) >= '04' -- April or later
            then substr(year_month, 1, 4) || '/' || (to_number(substr(year_month, 3, 2)) + 1)
            else (to_number(substr(year_month, 1, 4)) - 1) || '/' || substr(year_month, 3, 2)
        end,
        patient_identified
),

counts_fact_under65 as (
    select
        case
            when substr(year_month, 5, 2) >= '04'
            then substr(year_month, 1, 4) || '/' || (to_number(substr(year_month, 3, 2)) + 1)
            else (to_number(substr(year_month, 1, 4)) - 1) || '/' || substr(year_month, 3, 2)
        end as fy,
        patient_identified,
        sum(item_count) as total_item_count_under65
    from
        aml.px_form_item_elem_comb_fact
    where 1=1
        and calc_age < 65
        and year_month >= 202004 and year_month <= 202503
--        and patient_identified = 'Y'
        and pay_da_end = 'N'
        and pay_nd_end = 'N'
        and pay_rb_end = 'N'
        and cd_req = 'N'
        and oohc_ind = 0
        and private_ind = 0
        and ignore_flag = 'N'
        and item_count >= 1
        AND DISPENSER_COUNTRY_OU = 1 AND PRESC_COUNTRY_OU = 1 -- we indirectly filter to England only on the postcode lookup
    group by
        case
            when substr(year_month, 5, 2) >= '04' -- April or later
            then substr(year_month, 1, 4) || '/' || (to_number(substr(year_month, 3, 2)) + 1)
            else (to_number(substr(year_month, 1, 4)) - 1) || '/' || substr(year_month, 3, 2)
        end,
        patient_identified
)

select
    cf65plus.*,
    round((cf65plus.total_item_count_65plus * 100.0) / sum(cf65plus.total_item_count_65plus) over (partition by cf65plus.fy), 4) PERC_65PLUS,
    cfunder65.total_item_count_under65,
    round((cfunder65.total_item_count_under65 * 100.0) / sum(cfunder65.total_item_count_under65) over (partition by cfunder65.fy), 4) PERC_UNDER65
from counts_fact_65plus cf65plus
left join counts_fact_under65 cfunder65 on cf65plus.fy = cfunder65.fy and cf65plus.patient_identified = cfunder65.patient_identified

order by cf65plus.fy, cf65plus.patient_identified;
