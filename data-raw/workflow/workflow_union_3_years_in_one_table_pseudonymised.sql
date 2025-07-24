-- To run from DALL_REF

drop table int646_base_20200401_20230331_pseudo purge;
create table int646_base_20200401_20230331_pseudo nologging compress for query low as

select
    fy, year_month, part_date, epm_id, pf_id, eps_flag, 
    rawtohex(standard_hash(to_char(nhs_no), 'SHA256')) nhs_no, -- REPLACES nhs_no
    gender, age, age_band, bsa_postcode, 
    -- REMOVES bsa_sla, match_sla, match_sla_std, 
    match_sla_parent, match_type, score, max_monthly_patients, ab_flag, 
    uprn_flag, ch_flag, uprn, parent_uprn, location_id, nursing_home_flag, 
    residential_home_flag, ab_date, cqc_date, item_count, item_pay_dr_nic, 
    item_calc_pay_qty, pay_drug_record_id, chapter_descr, section_descr, 
    paragraph_descr, chemical_substance_bnf_descr, bnf_chemical_substance, 
    base_name, chapter_1_4_6_10_cat, acb_cat, damn_cat, falls_cat, presc_sla, 
    presc_postcode, presc_org_type, presc_org_sub_type, presc_org_nm, 
    presc_org_code, presc_org_list_size, prescriber_type, prescriber_sub_type, 
    prescriber_nm, prescriber_code, disp_code, disp_type, disp_nm, 
    disp_trading_nm, disp_sla, disp_postcode, pcd_region_code, pcd_region_name, 
    pcd_icb_code, pcd_icb_name, pcd_lad_code, pcd_lad_name, 
    opdd.lsoa_code, -- ADDS
    imd_decile
from int646_base_20200401_20230331 base
left join (
    select postcode, lsoa_code from (
      select 
          rank() over (partition by postcode order by year_month desc) rank,
          -- All English postcodes are already valid,
          -- i.e. no need to replace homoglyphs, just remove spaces
          regexp_replace(postcode, '\s') postcode,
          census_lower lsoa_code
      from dim.ons_postcode_data_dim
    )
    where rank = 1.0
) opdd
on base.bsa_postcode = opdd.postcode;

-- Must ensure each index is uniquely named with schema, so append _pseudo
create index int646_i01_pseudo on int646_base_20200401_20230331_pseudo (fy);
create index int646_i02_pseudo on int646_base_20200401_20230331_pseudo (fy, year_month);

create index int646_i03_pseudo on int646_base_20200401_20230331_pseudo (fy, age_band, gender);
create index int646_i04_pseudo on int646_base_20200401_20230331_pseudo (fy, age_band, gender, pcd_region_code, pcd_region_name);
create index int646_i05_pseudo on int646_base_20200401_20230331_pseudo (fy, age_band, gender, pcd_icb_code, pcd_icb_name);
create index int646_i06_pseudo on int646_base_20200401_20230331_pseudo (fy, age_band, gender, pcd_lad_code, pcd_lad_name);
create index int646_lsoa_i01 on int646_base_20200401_20230331_pseudo (fy, age_band, gender, lsoa_code);
create index int646_i07_pseudo on int646_base_20200401_20230331_pseudo (fy, imd_decile);
create index int646_i08_pseudo on int646_base_20200401_20230331_pseudo (ch_flag);
create index int646_i09_pseudo on int646_base_20200401_20230331_pseudo (fy, ch_flag);
create index int646_i10_pseudo on int646_base_20200401_20230331_pseudo (fy, nursing_home_flag);
create index int646_i11_pseudo on int646_base_20200401_20230331_pseudo (fy, residential_home_flag);

create index int646_i12_pseudo on int646_base_20200401_20230331_pseudo (fy, ch_flag, pcd_region_code, pcd_region_name);
create index int646_i13_pseudo on int646_base_20200401_20230331_pseudo (fy, ch_flag, pcd_icb_code, pcd_icb_name);
create index int646_i14_pseudo on int646_base_20200401_20230331_pseudo (fy, ch_flag, pcd_lad_code, pcd_lad_name);
create index int646_lsoa_i02 on int646_base_20200401_20230331_pseudo (fy, ch_flag, lsoa_code);

create index int646_i15_pseudo on int646_base_20200401_20230331_pseudo (fy, chapter_descr, pcd_region_code, pcd_region_name);
create index int646_i16_pseudo on int646_base_20200401_20230331_pseudo (fy, chapter_descr, pcd_icb_code, pcd_icb_name);
create index int646_i17_pseudo on int646_base_20200401_20230331_pseudo (fy, chapter_descr, pcd_lad_code, pcd_lad_name);
create index int646_lsoa_i03 on int646_base_20200401_20230331_pseudo (fy, chapter_descr, lsoa_code);

create index int646_i18_pseudo on int646_base_20200401_20230331_pseudo (fy, section_descr, pcd_region_code, pcd_region_name);
create index int646_i19_pseudo on int646_base_20200401_20230331_pseudo (fy, section_descr, pcd_icb_code, pcd_icb_name);
create index int646_i20_pseudo on int646_base_20200401_20230331_pseudo (fy, section_descr, pcd_lad_code, pcd_lad_name);
create index int646_lsoa_i04 on int646_base_20200401_20230331_pseudo (fy, section_descr, lsoa_code);

create index int646_i21_pseudo on int646_base_20200401_20230331_pseudo (fy, paragraph_descr, pcd_region_code, pcd_region_name);
create index int646_i22_pseudo on int646_base_20200401_20230331_pseudo (fy, paragraph_descr, pcd_icb_code, pcd_icb_name);
create index int646_i23_pseudo on int646_base_20200401_20230331_pseudo (fy, paragraph_descr, pcd_lad_code, pcd_lad_name);
create index int646_lsoa_i05 on int646_base_20200401_20230331_pseudo (fy, paragraph_descr, lsoa_code);

create index int646_i24_pseudo on int646_base_20200401_20230331_pseudo (fy, chemical_substance_bnf_descr, pcd_region_code, pcd_region_name);
create index int646_i25_pseudo on int646_base_20200401_20230331_pseudo (fy, chemical_substance_bnf_descr, pcd_icb_code, pcd_icb_name);
create index int646_i26_pseudo on int646_base_20200401_20230331_pseudo (fy, chemical_substance_bnf_descr, pcd_lad_code, pcd_lad_name);
create index int646_lsoa_i06 on int646_base_20200401_20230331_pseudo (fy, chemical_substance_bnf_descr, lsoa_code);

create index int646_i27_pseudo on int646_base_20200401_20230331_pseudo (uprn_flag);

create index int646_i28_pseudo on int646_base_20200401_20230331_pseudo (fy, year_month, nhs_no);
create index int646_i29_pseudo on int646_base_20200401_20230331_pseudo (fy, year_month, pcd_region_code, pcd_region_name, nhs_no);
create index int646_i30_pseudo on int646_base_20200401_20230331_pseudo (fy, year_month, pcd_icb_code, pcd_icb_name, nhs_no);
create index int646_i31_pseudo on int646_base_20200401_20230331_pseudo (fy, year_month, pcd_lad_code, pcd_lad_name, nhs_no);
create index int646_lsoa_i07 on int646_base_20200401_20230331_pseudo (fy, year_month, lsoa_code, nhs_no);

create index int646_i32_pseudo on int646_base_20200401_20230331_pseudo (fy, age_band, gender, ch_flag, year_month, nhs_no);
