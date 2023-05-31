-- To run from DALL_REF

--drop table int646_base_20200401_20230321 purge;
create table int646_base_20200401_20230321 nologging nocompress as

select '2020/21' fy, y1.* from migar.int646_base_20200401_20210321 y1
union all
select '2021/22' fy, y2.* from migar.int646_base_20210401_20220321 y2
union all
select '2022/23' fy, y3.* from migar.int646_base_20220401_20230321 y3
;

-- TODO: re-number indices
create index int646_i01 on int646_base_20200401_20230321 (fy);
create index int646_i01 on int646_base_20200401_20230321 (fy, year_month);

create index int646_i01 on int646_base_20200401_20230321 (fy, age_band, gender);
create index int646_i01 on int646_base_20200401_20230321 (fy, age_band, gender, pcd_region_code, pcd_region_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, age_band, gender, pcd_icb_code, pcd_icb_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, age_band, gender, pcd_lad_code, pcd_lad_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, age_band, gender, prescriber_pcn);
create index int646_i01 on int646_base_20200401_20230321 (fy, imd_decile);
create index int646_i01 on int646_base_20200401_20230321 (fy, ch_flag);
create index int646_i01 on int646_base_20200401_20230321 (fy, nursing_home_flag);
create index int646_i01 on int646_base_20200401_20230321 (fy, residential_home_flag);

create index int646_i01 on int646_base_20200401_20230321 (fy, ch_flag, pcd_region_code, pcd_region_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, ch_flag, pcd_icb_code, pcd_icb_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, ch_flag, pcd_lad_code, pcd_lad_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, ch_flag, prescriber_pcn);

create index int646_i01 on int646_base_20200401_20230321 (fy, chapter_descr, pcd_region_code, pcd_region_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, chapter_descr, pcd_icb_code, pcd_icb_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, chapter_descr, pcd_lad_code, pcd_lad_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, chapter_descr, prescriber_pcn);

create index int646_i01 on int646_base_20200401_20230321 (fy, section_descr, pcd_region_code, pcd_region_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, section_descr, pcd_icb_code, pcd_icb_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, section_descr, pcd_lad_code, pcd_lad_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, section_descr, prescriber_pcn);

create index int646_i01 on int646_base_20200401_20230321 (fy, paragraph_descr, pcd_region_code, pcd_region_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, paragraph_descr, pcd_icb_code, pcd_icb_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, paragraph_descr, pcd_lad_code, pcd_lad_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, paragraph_descr, prescriber_pcn);

create index int646_i01 on int646_base_20200401_20230321 (fy, bnf_chemical_substance, chemical_substance_bnf_descr, pcd_region_code, pcd_region_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, bnf_chemical_substance, chemical_substance_bnf_descr, pcd_icb_code, pcd_icb_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, bnf_chemical_substance, chemical_substance_bnf_descr, pcd_lad_code, pcd_lad_name);
create index int646_i01 on int646_base_20200401_20230321 (fy, bnf_chemical_substance, chemical_substance_bnf_descr, prescriber_pcn);
