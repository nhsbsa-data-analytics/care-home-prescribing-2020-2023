with

counts_fact as (
    select
        case
            when substr(year_month, 5, 2) >= '04'
            then substr(year_month, 1, 4) || '/' || (to_number(substr(year_month, 3, 2)) + 1)
            else (to_number(substr(year_month, 1, 4)) - 1) || '/' || substr(year_month, 3, 2)
        end as fy,
        sum(item_count) as total_item_count
    from
        aml.px_form_item_elem_comb_fact
    where 1=1
        and calc_age >= 65
        and year_month >= 202004 and year_month <= 202503
        and patient_identified = 'Y'
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
        end
),

counts_int646 as (
    select
        fy,
        sum(item_count) as total_item_count
    from dall_ref.int646_base_20200401_20250331
    group by fy
)

select
    cf.fy,
    cf.total_item_count item_count_fact,
    c646.total_item_count item_count_int646,
    round(c646.total_item_count / cf.total_item_count, 4) ratio_int646_to_fact
from counts_fact cf
left join counts_int646 c646 on c646.fy = cf.fy
 
order by cf.fy;
