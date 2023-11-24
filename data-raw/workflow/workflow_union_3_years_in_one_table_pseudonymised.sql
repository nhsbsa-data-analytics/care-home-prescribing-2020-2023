-- To run from DALL_REF

create table int646_base_20200401_20230331_pseudo nologging compress for query low as

select * from
(select RawToHex(standard_hash(nhs_no)) nhs_no_hash, base.*, opdd.lsoa_code
from dall_ref.int646_base_20200401_20230331 base
left join (select census_lower lsoa_code
           from dim.ons_postcode_data_dim) opdd
on base.bsa_postcode = opdd.postcode)

pivot -- remove columns named in 'for': https://stackoverflow.com/a/33220953/8519200
( 
  max(1) -- fake  
  for (bsa_postcode, bsa_sla, nhs_no) -- put the undesired columns here
  IN () -- no values here...
);

alter table int646_base_20200401_20230331_pseudo rename column nhs_no_hash TO nhs_no;

grant select on int646_base_20200401_20230331_pseudo to migar, adnsh, mamcp;

create index int646_i01 on int646_base_20200401_20230331_pseudo (fy);
create index int646_i02 on int646_base_20200401_20230331_pseudo (fy, year_month);

create index int646_i03 on int646_base_20200401_20230331_pseudo (fy, age_band, gender);
create index int646_i04 on int646_base_20200401_20230331_pseudo (fy, age_band, gender, pcd_region_code, pcd_region_name);
create index int646_i05 on int646_base_20200401_20230331_pseudo (fy, age_band, gender, pcd_icb_code, pcd_icb_name);
create index int646_i06 on int646_base_20200401_20230331_pseudo (fy, age_band, gender, pcd_lad_code, pcd_lad_name);
create index int646_i07 on int646_base_20200401_20230331_pseudo (fy, imd_decile);
create index int646_i08 on int646_base_20200401_20230331_pseudo (ch_flag);
create index int646_i09 on int646_base_20200401_20230331_pseudo (fy, ch_flag);
create index int646_i10 on int646_base_20200401_20230331_pseudo (fy, nursing_home_flag);
create index int646_i11 on int646_base_20200401_20230331_pseudo (fy, residential_home_flag);

create index int646_i12 on int646_base_20200401_20230331_pseudo (fy, ch_flag, pcd_region_code, pcd_region_name);
create index int646_i13 on int646_base_20200401_20230331_pseudo (fy, ch_flag, pcd_icb_code, pcd_icb_name);
create index int646_i14 on int646_base_20200401_20230331_pseudo (fy, ch_flag, pcd_lad_code, pcd_lad_name);

create index int646_i15 on int646_base_20200401_20230331_pseudo (fy, chapter_descr, pcd_region_code, pcd_region_name);
create index int646_i16 on int646_base_20200401_20230331_pseudo (fy, chapter_descr, pcd_icb_code, pcd_icb_name);
create index int646_i17 on int646_base_20200401_20230331_pseudo (fy, chapter_descr, pcd_lad_code, pcd_lad_name);

create index int646_i18 on int646_base_20200401_20230331_pseudo (fy, section_descr, pcd_region_code, pcd_region_name);
create index int646_i19 on int646_base_20200401_20230331_pseudo (fy, section_descr, pcd_icb_code, pcd_icb_name);
create index int646_i20 on int646_base_20200401_20230331_pseudo (fy, section_descr, pcd_lad_code, pcd_lad_name);

create index int646_i21 on int646_base_20200401_20230331_pseudo (fy, paragraph_descr, pcd_region_code, pcd_region_name);
create index int646_i22 on int646_base_20200401_20230331_pseudo (fy, paragraph_descr, pcd_icb_code, pcd_icb_name);
create index int646_i23 on int646_base_20200401_20230331_pseudo (fy, paragraph_descr, pcd_lad_code, pcd_lad_name);

create index int646_i24 on int646_base_20200401_20230331_pseudo (fy, chemical_substance_bnf_descr, pcd_region_code, pcd_region_name);
create index int646_i25 on int646_base_20200401_20230331_pseudo (fy, chemical_substance_bnf_descr, pcd_icb_code, pcd_icb_name);
create index int646_i26 on int646_base_20200401_20230331_pseudo (fy, chemical_substance_bnf_descr, pcd_lad_code, pcd_lad_name);

create index int646_i27 on int646_base_20200401_20230331_pseudo (uprn_flag);

create index int646_i28 on int646_base_20200401_20230331_pseudo (fy, year_month, nhs_no);
create index int646_i29 on int646_base_20200401_20230331_pseudo (fy, year_month, pcd_region_code, pcd_region_name, nhs_no);
create index int646_i30 on int646_base_20200401_20230331_pseudo (fy, year_month, pcd_icb_code, pcd_icb_name, nhs_no);
create index int646_i31 on int646_base_20200401_20230331_pseudo (fy, year_month, pcd_lad_code, pcd_lad_name, nhs_no);

create index int646_i32 on int646_base_20200401_20230331_pseudo (fy, age_band, gender, ch_flag, year_month, nhs_no);
