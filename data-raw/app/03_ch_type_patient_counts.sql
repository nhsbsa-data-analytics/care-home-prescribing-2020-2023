alter session force parallel query parallel 32;

--drop table INT646_CH_TYPE_PROPS;
create table INT646_CH_TYPE_PROPS nologging as

with ch as (
    select /* + PARALLEL(32) */
        year_month,
        nhs_no,
        case
            when residential_home_flag is null then '1 - Unknown'
            when residential_home_flag = 1 and nursing_home_flag = 0 then '2 - Residential'
            when residential_home_flag = 0 and nursing_home_flag = 1 then '3 - Nursing'
            when residential_home_flag = 1 and nursing_home_flag = 1 then '4 - Both'
            when residential_home_flag = 0 and nursing_home_flag = 0 then '5 - Neither'
            when 1=1 then '6 - ERROR'
        end ch_type
    from dall_ref.int646_base_20200401_20240331
    where 1=1
        and ch_flag = 1
        and fy = '2023/24'
    group by
        year_month,
        nhs_no,
        case
            when residential_home_flag is null then '1 - Unknown'
            when residential_home_flag = 1 and nursing_home_flag = 0 then '2 - Residential'
            when residential_home_flag = 0 and nursing_home_flag = 1 then '3 - Nursing'
            when residential_home_flag = 1 and nursing_home_flag = 1 then '4 - Both'
            when residential_home_flag = 0 and nursing_home_flag = 0 then '5 - Neither'
            when 1=1 then '6 - ERROR'
        end
)
select * from ch;

with chtype as (
    select
        year_month ym,
        ch_type cht,
        count(*) num
    from INT646_CH_TYPE_PROPS
    group by
        year_month,
        ch_type
)
select
    avg(case when cht = '1 - Unknown' then num else null end) as unknown,
    avg(case when cht = '2 - Residential' then num else null end) as residential,
    avg(case when cht = '3 - Nursing' then num else null end) as nursing,
    avg(case when cht = '4 - Both' then num else null end) as both,
    avg(case when cht = '5 - Neither' then num else null end) as neither,
    avg(case when cht = '6 - ERROR' then num else null end) as error
from chtype
;
