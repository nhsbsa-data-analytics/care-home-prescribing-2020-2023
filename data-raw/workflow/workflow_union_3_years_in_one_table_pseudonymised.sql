-- To run from DALL_REF

drop table int646_base_20200401_20230331_pseudo purge;
create table int646_base_20200401_20230331_pseudo nologging compress for query low as

select 
    rawtohex(standard_hash(to_char(nhs_no), 'SHA256')) nhs_no_hash,
    base.*,
    opdd.lsoa_code
from dall_ref.int646_base_20200401_20230331 base
left join (
    select
      postcode,
      census_lower as lsoa_code
    from (
      select
        q01.*,
        rank() over (partition by postcode order by year_month desc) as rank
      from (
        select
          year_month,
          date_of_termination,
          county_code,
          local_auth_district_code,
          ward_code,
          country_code,
          sha_code,
          region,
          local_office,
          ccg_code,
          pcon_code,
          oseast1m,
          osnrth1m,
          census_lower,
          census_middle,
          postcode
        from (
          select
            year_month,
            date_of_termination,
            county_code,
            local_auth_district_code,
            ward_code,
            country_code,
            sha_code,
            region,
            local_office,
            ccg_code,
            pcon_code,
            oseast1m,
            osnrth1m,
            census_lower,
            census_middle,
            postcode as postcode_old
          from dim.ons_postcode_data_dim
          where (country_code = 'E92000001')
        ) lhs
        left join (
          select
            postcode_old,
            case
              when (len = 7.0 and substr(pcd_temp, 1, 1) = '5') then ('s' || substr(pcd_temp, 2, 6))
              when (len = 7.0 and substr(pcd_temp, 1, 1) = '0') then ('o' || substr(pcd_temp, 2, 6))
              when (len = 7.0 and substr(pcd_temp, 2, 1) = '5') then (substr(pcd_temp, 1, 1) || 's' || substr(pcd_temp, 3, 5))
              when (len = 7.0 and substr(pcd_temp, 2, 1) = '0') then (substr(pcd_temp, 1, 1) || 'o' || substr(pcd_temp, 3, 5))
              when (len = 7.0 and substr(pcd_temp, 3, 1) = 's') then (substr(pcd_temp, 1, 2) || '5' || substr(pcd_temp, 4, 4))
              when (len = 7.0 and substr(pcd_temp, 3, 1) = 'o') then (substr(pcd_temp, 1, 2) || '0' || substr(pcd_temp, 4, 4))
              when (len = 7.0 and substr(pcd_temp, 3, 1) = 'i') then (substr(pcd_temp, 1, 2) || '1' || substr(pcd_temp, 4, 4))
              when (len = 7.0 and substr(pcd_temp, 3, 1) = 'l') then (substr(pcd_temp, 1, 2) || '1' || substr(pcd_temp, 4, 4))
              when (len = 7.0 and substr(pcd_temp, 5, 1) = 's') then (substr(pcd_temp, 1, 4) || '5' || substr(pcd_temp, 6, 2))
              when (len = 7.0 and substr(pcd_temp, 5, 1) = 'o') then (substr(pcd_temp, 1, 4) || '0' || substr(pcd_temp, 6, 2))
              when (len = 7.0 and substr(pcd_temp, 5, 1) = 'i') then (substr(pcd_temp, 1, 4) || '1' || substr(pcd_temp, 6, 2))
              when (len = 7.0 and substr(pcd_temp, 5, 1) = 'l') then (substr(pcd_temp, 1, 4) || '1' || substr(pcd_temp, 6, 2))
              when (len = 7.0 and substr(pcd_temp, 6, 1) = '5') then (substr(pcd_temp, 1, 5) || 's' || substr(pcd_temp, 7, 1))
              when (len = 7.0 and substr(pcd_temp, 6, 1) = '0') then (substr(pcd_temp, 1, 5) || 'o' || substr(pcd_temp, 7, 1))
              when (len = 7.0 and substr(pcd_temp, 7, 1) = '5') then (substr(pcd_temp, 1, 6) || 's')
              when (len = 7.0 and substr(pcd_temp, 7, 1) = '0') then (substr(pcd_temp, 1, 6) || 'o')
              when (len = 6.0 and substr(pcd_temp, 1, 1) = '5') then ('s' || substr(pcd_temp, 2, 5))
              when (len = 6.0 and substr(pcd_temp, 1, 1) = '0') then ('o' || substr(pcd_temp, 2, 5))
              when (len = 6.0 and substr(pcd_temp, 4, 1) = 's') then (substr(pcd_temp, 1, 3) || '5' || substr(pcd_temp, 5, 2))
              when (len = 6.0 and substr(pcd_temp, 4, 1) = 'o') then (substr(pcd_temp, 1, 3) || '0' || substr(pcd_temp, 5, 2))
              when (len = 6.0 and substr(pcd_temp, 4, 1) = 'i') then (substr(pcd_temp, 1, 3) || '1' || substr(pcd_temp, 5, 2))
              when (len = 6.0 and substr(pcd_temp, 4, 1) = 'l') then (substr(pcd_temp, 1, 3) || '1' || substr(pcd_temp, 5, 2))
              when (len = 6.0 and substr(pcd_temp, 5, 1) = '5') then (substr(pcd_temp, 1, 4) || 's' || substr(pcd_temp, 6, 1))
              when (len = 6.0 and substr(pcd_temp, 5, 1) = '0') then (substr(pcd_temp, 1, 4) || 'o' || substr(pcd_temp, 6, 1))
              when (len = 6.0 and substr(pcd_temp, 6, 1) = '5') then (substr(pcd_temp, 1, 5) || 's')
              when (len = 6.0 and substr(pcd_temp, 6, 1) = '0') then (substr(pcd_temp, 1, 5) || 'o')
              when (len = 5.0 and substr(pcd_temp, 1, 1) = '5') then ('s' || substr(pcd_temp, 2, 4))
              when (len = 5.0 and substr(pcd_temp, 1, 1) = '0') then ('o' || substr(pcd_temp, 2, 4))
              when (len = 5.0 and substr(pcd_temp, 2, 1) = 's') then (substr(pcd_temp, 1, 1) || '5' || substr(pcd_temp, 3, 3))
              when (len = 5.0 and substr(pcd_temp, 2, 1) = 'o') then (substr(pcd_temp, 1, 1) || '0' || substr(pcd_temp, 3, 3))
              when (len = 5.0 and substr(pcd_temp, 2, 1) = 'i') then (substr(pcd_temp, 1, 1) || '1' || substr(pcd_temp, 3, 3))
              when (len = 5.0 and substr(pcd_temp, 2, 1) = 'l') then (substr(pcd_temp, 1, 1) || '1' || substr(pcd_temp, 3, 3))
              when (len = 5.0 and substr(pcd_temp, 3, 1) = 's') then (substr(pcd_temp, 1, 2) || '5' || substr(pcd_temp, 4, 2))
              when (len = 5.0 and substr(pcd_temp, 3, 1) = 'o') then (substr(pcd_temp, 1, 2) || '0' || substr(pcd_temp, 4, 2))
              when (len = 5.0 and substr(pcd_temp, 3, 1) = 'i') then (substr(pcd_temp, 1, 2) || '1' || substr(pcd_temp, 4, 2))
              when (len = 5.0 and substr(pcd_temp, 3, 1) = 'l') then (substr(pcd_temp, 1, 2) || '1' || substr(pcd_temp, 4, 2))
              when (len = 5.0 and substr(pcd_temp, 4, 1) = '5') then (substr(pcd_temp, 1, 3) || 's' || substr(pcd_temp, 5, 1))
              when (len = 5.0 and substr(pcd_temp, 4, 1) = '0') then (substr(pcd_temp, 1, 3) || 'o' || substr(pcd_temp, 5, 1))
              when (len = 5.0 and substr(pcd_temp, 5, 1) = '5') then (substr(pcd_temp, 1, 4) || 's')
              when (len = 5.0 and substr(pcd_temp, 5, 1) = '0') then (substr(pcd_temp, 1, 4) || 'o')
              else pcd_temp
            end as postcode,
            len,
            case
              when (len = 7.0 and substr(pcd_temp, 1, 1) = '5') then ('s' || substr(pcd_temp, 2, 6))
              when (len = 7.0 and substr(pcd_temp, 1, 1) = '0') then ('o' || substr(pcd_temp, 2, 6))
              when (len = 7.0 and substr(pcd_temp, 2, 1) = '5') then (substr(pcd_temp, 1, 1) || 's' || substr(pcd_temp, 3, 5))
              when (len = 7.0 and substr(pcd_temp, 2, 1) = '0') then (substr(pcd_temp, 1, 1) || 'o' || substr(pcd_temp, 3, 5))
              when (len = 7.0 and substr(pcd_temp, 3, 1) = 's') then (substr(pcd_temp, 1, 2) || '5' || substr(pcd_temp, 4, 4))
              when (len = 7.0 and substr(pcd_temp, 3, 1) = 'o') then (substr(pcd_temp, 1, 2) || '0' || substr(pcd_temp, 4, 4))
              when (len = 7.0 and substr(pcd_temp, 3, 1) = 'i') then (substr(pcd_temp, 1, 2) || '1' || substr(pcd_temp, 4, 4))
              when (len = 7.0 and substr(pcd_temp, 3, 1) = 'l') then (substr(pcd_temp, 1, 2) || '1' || substr(pcd_temp, 4, 4))
              when (len = 7.0 and substr(pcd_temp, 5, 1) = 's') then (substr(pcd_temp, 1, 4) || '5' || substr(pcd_temp, 6, 2))
              when (len = 7.0 and substr(pcd_temp, 5, 1) = 'o') then (substr(pcd_temp, 1, 4) || '0' || substr(pcd_temp, 6, 2))
              when (len = 7.0 and substr(pcd_temp, 5, 1) = 'i') then (substr(pcd_temp, 1, 4) || '1' || substr(pcd_temp, 6, 2))
              when (len = 7.0 and substr(pcd_temp, 5, 1) = 'l') then (substr(pcd_temp, 1, 4) || '1' || substr(pcd_temp, 6, 2))
              when (len = 7.0 and substr(pcd_temp, 6, 1) = '5') then (substr(pcd_temp, 1, 5) || 's' || substr(pcd_temp, 7, 1))
              when (len = 7.0 and substr(pcd_temp, 6, 1) = '0') then (substr(pcd_temp, 1, 5) || 'o' || substr(pcd_temp, 7, 1))
              when (len = 7.0 and substr(pcd_temp, 7, 1) = '5') then (substr(pcd_temp, 1, 6) || 's')
              when (len = 7.0 and substr(pcd_temp, 7, 1) = '0') then (substr(pcd_temp, 1, 6) || 'o')
              when (len = 6.0 and substr(pcd_temp, 1, 1) = '5') then ('s' || substr(pcd_temp, 2, 5))
              when (len = 6.0 and substr(pcd_temp, 1, 1) = '0') then ('o' || substr(pcd_temp, 2, 5))
              when (len = 6.0 and substr(pcd_temp, 4, 1) = 's') then (substr(pcd_temp, 1, 3) || '5' || substr(pcd_temp, 5, 2))
              when (len = 6.0 and substr(pcd_temp, 4, 1) = 'o') then (substr(pcd_temp, 1, 3) || '0' || substr(pcd_temp, 5, 2))
              when (len = 6.0 and substr(pcd_temp, 4, 1) = 'i') then (substr(pcd_temp, 1, 3) || '1' || substr(pcd_temp, 5, 2))
              when (len = 6.0 and substr(pcd_temp, 4, 1) = 'l') then (substr(pcd_temp, 1, 3) || '1' || substr(pcd_temp, 5, 2))
              when (len = 6.0 and substr(pcd_temp, 5, 1) = '5') then (substr(pcd_temp, 1, 4) || 's' || substr(pcd_temp, 6, 1))
              when (len = 6.0 and substr(pcd_temp, 5, 1) = '0') then (substr(pcd_temp, 1, 4) || 'o' || substr(pcd_temp, 6, 1))
              when (len = 6.0 and substr(pcd_temp, 6, 1) = '5') then (substr(pcd_temp, 1, 5) || 's')
              when (len = 6.0 and substr(pcd_temp, 6, 1) = '0') then (substr(pcd_temp, 1, 5) || 'o')
              when (len = 5.0 and substr(pcd_temp, 1, 1) = '5') then ('s' || substr(pcd_temp, 2, 4))
              when (len = 5.0 and substr(pcd_temp, 1, 1) = '0') then ('o' || substr(pcd_temp, 2, 4))
              when (len = 5.0 and substr(pcd_temp, 2, 1) = 's') then (substr(pcd_temp, 1, 1) || '5' || substr(pcd_temp, 3, 3))
              when (len = 5.0 and substr(pcd_temp, 2, 1) = 'o') then (substr(pcd_temp, 1, 1) || '0' || substr(pcd_temp, 3, 3))
              when (len = 5.0 and substr(pcd_temp, 2, 1) = 'i') then (substr(pcd_temp, 1, 1) || '1' || substr(pcd_temp, 3, 3))
              when (len = 5.0 and substr(pcd_temp, 2, 1) = 'l') then (substr(pcd_temp, 1, 1) || '1' || substr(pcd_temp, 3, 3))
              when (len = 5.0 and substr(pcd_temp, 3, 1) = 's') then (substr(pcd_temp, 1, 2) || '5' || substr(pcd_temp, 4, 2))
              when (len = 5.0 and substr(pcd_temp, 3, 1) = 'o') then (substr(pcd_temp, 1, 2) || '0' || substr(pcd_temp, 4, 2))
              when (len = 5.0 and substr(pcd_temp, 3, 1) = 'i') then (substr(pcd_temp, 1, 2) || '1' || substr(pcd_temp, 4, 2))
              when (len = 5.0 and substr(pcd_temp, 3, 1) = 'l') then (substr(pcd_temp, 1, 2) || '1' || substr(pcd_temp, 4, 2))
              when (len = 5.0 and substr(pcd_temp, 4, 1) = '5') then (substr(pcd_temp, 1, 3) || 's' || substr(pcd_temp, 5, 1))
              when (len = 5.0 and substr(pcd_temp, 4, 1) = '0') then (substr(pcd_temp, 1, 3) || 'o' || substr(pcd_temp, 5, 1))
              when (len = 5.0 and substr(pcd_temp, 5, 1) = '5') then (substr(pcd_temp, 1, 4) || 's')
              when (len = 5.0 and substr(pcd_temp, 5, 1) = '0') then (substr(pcd_temp, 1, 4) || 'o')
              else pcd_temp
            end as pcd_temp
          from (
            select q01.*, length(postcode) as len, postcode as pcd_temp
            from (
              select
                postcode_old,
                upper(regexp_replace(postcode, '[^[:alnum:]]', '')) as postcode
              from (
                select
                  postcode_old,
                  case when (length(postcode) = 0.0) then null when not (length(postcode) = 0.0) then postcode end as postcode
                from (
                  select distinct postcode as postcode_old, postcode
                  from dim.ons_postcode_data_dim
                  where
                    (country_code = 'E92000001') and
                    (not((postcode is null)))
                ) q01
              ) q01
            ) q01
          ) q01
        ) rhs
          on (lhs.postcode_old = rhs.postcode_old)
      ) q01
    ) q01
    where (rank = 1.0)
) opdd
on base.bsa_postcode = opdd.postcode;

alter table int646_base_20200401_20210331_pseudo drop column bsa_sla;
alter table int646_base_20200401_20210331_pseudo drop column match_sla;
alter table int646_base_20200401_20210331_pseudo drop column match_sla_std;
alter table int646_base_20200401_20210331_pseudo drop column nhs_no;
alter table int646_base_20200401_20210331_pseudo rename column nhs_no_hash to nhs_no;
commit;

grant select on int646_base_20200401_20230331_pseudo to migar, adnsh, mamcp;

create index int646_i01 on int646_base_20200401_20230331_pseudo (fy);
create index int646_i02 on int646_base_20200401_20230331_pseudo (fy, year_month);

create index int646_i03 on int646_base_20200401_20230331_pseudo (fy, age_band, gender);
create index int646_i04 on int646_base_20200401_20230331_pseudo (fy, age_band, gender, pcd_region_code, pcd_region_name);
create index int646_i05 on int646_base_20200401_20230331_pseudo (fy, age_band, gender, pcd_icb_code, pcd_icb_name);
create index int646_i06 on int646_base_20200401_20230331_pseudo (fy, age_band, gender, pcd_lad_code, pcd_lad_name);
create index int646_lsoa_i01 on int646_base_20200401_20230331_pseudo (fy, age_band, gender, lsoa_code);
create index int646_i07 on int646_base_20200401_20230331_pseudo (fy, imd_decile);
create index int646_i08 on int646_base_20200401_20230331_pseudo (ch_flag);
create index int646_i09 on int646_base_20200401_20230331_pseudo (fy, ch_flag);
create index int646_i10 on int646_base_20200401_20230331_pseudo (fy, nursing_home_flag);
create index int646_i11 on int646_base_20200401_20230331_pseudo (fy, residential_home_flag);

create index int646_i12 on int646_base_20200401_20230331_pseudo (fy, ch_flag, pcd_region_code, pcd_region_name);
create index int646_i13 on int646_base_20200401_20230331_pseudo (fy, ch_flag, pcd_icb_code, pcd_icb_name);
create index int646_i14 on int646_base_20200401_20230331_pseudo (fy, ch_flag, pcd_lad_code, pcd_lad_name);
create index int646_lsoa_i02 on int646_base_20200401_20230331_pseudo (fy, ch_flag, lsoa_code);

create index int646_i15 on int646_base_20200401_20230331_pseudo (fy, chapter_descr, pcd_region_code, pcd_region_name);
create index int646_i16 on int646_base_20200401_20230331_pseudo (fy, chapter_descr, pcd_icb_code, pcd_icb_name);
create index int646_i17 on int646_base_20200401_20230331_pseudo (fy, chapter_descr, pcd_lad_code, pcd_lad_name);
create index int646_lsoa_i03 on int646_base_20200401_20230331_pseudo (fy, chapter_descr, lsoa_code);

create index int646_i18 on int646_base_20200401_20230331_pseudo (fy, section_descr, pcd_region_code, pcd_region_name);
create index int646_i19 on int646_base_20200401_20230331_pseudo (fy, section_descr, pcd_icb_code, pcd_icb_name);
create index int646_i20 on int646_base_20200401_20230331_pseudo (fy, section_descr, pcd_lad_code, pcd_lad_name);
create index int646_lsoa_i04 on int646_base_20200401_20230331_pseudo (fy, section_descr, lsoa_code);

create index int646_i21 on int646_base_20200401_20230331_pseudo (fy, paragraph_descr, pcd_region_code, pcd_region_name);
create index int646_i22 on int646_base_20200401_20230331_pseudo (fy, paragraph_descr, pcd_icb_code, pcd_icb_name);
create index int646_i23 on int646_base_20200401_20230331_pseudo (fy, paragraph_descr, pcd_lad_code, pcd_lad_name);
create index int646_lsoa_i05 on int646_base_20200401_20230331_pseudo (fy, paragraph_descr, lsoa_code);

create index int646_i24 on int646_base_20200401_20230331_pseudo (fy, chemical_substance_bnf_descr, pcd_region_code, pcd_region_name);
create index int646_i25 on int646_base_20200401_20230331_pseudo (fy, chemical_substance_bnf_descr, pcd_icb_code, pcd_icb_name);
create index int646_i26 on int646_base_20200401_20230331_pseudo (fy, chemical_substance_bnf_descr, pcd_lad_code, pcd_lad_name);
create index int646_lsoa_i06 on int646_base_20200401_20230331_pseudo (fy, chemical_substance_bnf_descr, lsoa_code);

create index int646_i27 on int646_base_20200401_20230331_pseudo (uprn_flag);

create index int646_i28 on int646_base_20200401_20230331_pseudo (fy, year_month, nhs_no);
create index int646_i29 on int646_base_20200401_20230331_pseudo (fy, year_month, pcd_region_code, pcd_region_name, nhs_no);
create index int646_i30 on int646_base_20200401_20230331_pseudo (fy, year_month, pcd_icb_code, pcd_icb_name, nhs_no);
create index int646_i31 on int646_base_20200401_20230331_pseudo (fy, year_month, pcd_lad_code, pcd_lad_name, nhs_no);
create index int646_lsoa_i07 on int646_base_20200401_20230331_pseudo (fy, year_month, lsoa_code, nhs_no);

create index int646_i32 on int646_base_20200401_20230331_pseudo (fy, age_band, gender, ch_flag, year_month, nhs_no);
