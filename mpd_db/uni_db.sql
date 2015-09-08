-- Unified experiment database
-- drop schema public cascade; create schema public;
-- drop database if exists mpd_db;
create database mpd_db;
use mpd_db;
GRANT ALL ON SCHEMA public TO public;
-- MS SQL Server convertion
-- replace serial : int identity
-- replace references : foreign key references
-- replace timestamp : smalltimestamp
-- replace bytea : blob

-- MySQL convertion
-- replace timestamp : datetime
-- replace boolean : bit
-- replace true : 1
-- replace bytea : blob

-- EXPERIMENT SESSIONS
create table experiment_session
(
 session_number int primary key,
 start_date timestamp not null,
 end_date timestamp null
);

-- SHIFTS
-- drop table shift
create table shift
(
 shift_id serial primary key,
 session_number int not null references experiment_session(session_number) on update cascade,
 FIO varchar(40) not null,
 start_date timestamp not null,
 end_date timestamp not null,
 responsibility varchar(20) null
);

-- DETECTORS AND OTHER COMPONENTS
create table component
(
 component_name varchar(10) primary key,
 manufacturer_name varchar(30) null,
 responsible_person varchar(40) null,
 description varchar(30) null
);

-- DETECTORS' MAPPING
-- map_type: 1 - map_1dim, 2 - map_2dim
create table component_map
(
 map_id serial primary key,
 map_type int not null
);

create table map_1dim
(
 map_id int primary key references component_map(map_id) on update cascade on delete cascade,
 serial_hex varchar(20) not null,
 plane int not null,
 map_group int not null,
 slot int not null,
 channel_low int not null,
 channel_high int not null,
 check (channel_high > channel_low)
);

create table map_2dim
(
 map_id int primary key references component_map(map_id) on update cascade on delete cascade,
 serial_hex varchar(20) not null,
 channel int not null,
 f_channel int not null,
 channel_size int not null,
 x int not null,
 y int not null,
 is_connected boolean not null default true
);

create table session_component
(
 session_number int references experiment_session(session_number) on update cascade,
 component_name varchar(10) references component(component_name),
 map_id int null references component_map(map_id) on update cascade on delete cascade,
 primary key (session_number, component_name)
);

-- GEOMETRY PART
create table run_geometry
(
 geometry_id serial primary key,
 root_geometry bytea not null
);

-- RUN INFO
create table run
(
 run_number int primary key,
 seession_number int references experiment_session(session_number) on update cascade,
 file_path varchar(200) not null unique,
 beam_particle varchar(10) not null,
 target_particle varchar(10) null,
 energy float null check (energy > 0),
 start_date timestamp not null,
 end_date timestamp null,
 event_count int null check (event_count >= 0),
 field_current_a int null check (field_current_a >= 0),
 file_size_kb float null check (file_size_kb > 0),
 geometry_id int references component_geometry(geometry_id) on update cascade
);

-- FILES WiTH GENERATOR DATA
create table simulation_file
(
 file_id serial primary key,
 file_path varchar(200) not null unique,
 generator_name varchar(20) not null,
 beam_particle varchar(10) not null,
 target_particle varchar(10) null,
 energy float null check (energy > 0),
 centrality varchar(10) not null,
 event_count int null check (event_count >= 0),
 file_desc varchar(30) null,
 file_size_kb float null check (file_size_kb > 0)
);

-- COMPONENT PARAMETERS
-- parameter_type: 0 - int, 1 - double, 2 - string
create table component_parameter
(
 parameter_id serial primary key,
 parameter_name varchar(20) not null unique,
 parameter_type int not null
);

create table parameter_value
(
 value_id serial primary key
);

create table run_parameter
(
 run_number int references run(run_number),
 component_name varchar(10) references component(component_name),
 parameter_id int references component_parameter(parameter_id),
 value_id int not null references parameter_value(value_id),
 primary key (run_number, component_name, parameter_id)
);

create table int_value
(
 value_id int primary key references parameter_value(value_id),
 par_value int not null
);

create table double_value
(
 value_id int primary key references parameter_value(value_id),
 par_value float not null
);

create table string_value
(
 value_id int primary key references parameter_value(value_id),
 par_value varchar(200) not null
);

/*-- GEOMETRY PART (decomposition)
create table geometry_media
(
 media_name varchar(20) primary key,
 density float null,
 radiation_length float null,
 interaction_length float null,
 sensitivity_flag int null,
 field_flag int null,
 field_max float null,
 ang_deviation_max float null,
 step_max float null,
 energy_loss_max float,
 b_crossing_precision float null,
 step_min float null,
 media_desc varchar(50) null
);

create table media_element
(
 element_id serial primary key,
 media_name varchar(20) references geometry_media(media_name),
 atomic_weight float not null,
 atomic_number float not null,
 relative_weight float null
);

-- dictionary of possible geometric shapes
create table geometry_shape
(
 shape_index int primary key,
 shape_name varchar(20) not null,
 shape_desc varchar(40) null
);

-- filling all kinds of geometric shapes
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (0, "box", "box");
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (1, "pgon", "polygone");
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (2, "pcon", "polycone");
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (3, "tube", "cylindrical tube");
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (4, "tubeseg", "cylindrical tube segment");
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (5, "sphere", "sphere");
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (6, "torus", "torus");
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (7, "cone", "conical tube");
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (8, "coneseg", "conical tube segment");
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (9, "trd1", "TRD1 shape");
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (10, "trd2", "TRD2 shape");
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (11, "trap", "G3 TRAP");
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (12, "comb", "boolean composite shape");
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (13, "para", "parallelipeped");
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (14, "arb8", "arbitrary trapezoid with 8 vertices");
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (15, "eltu", "elliptical tube");
insert into geometry_shape(shape_index, shape_name, shape_desc)
values (16, "xtru", "extruded polygon");

create table geometry_node
(
 node_id serial primary key,
 node_name varchar(20) not null,
 parent_node_id int null references geometry_node(node_id),
 media_name varchar(20) references geometry_media(media_name),
 shape_index int references geometry_shape(shape_index)
);

create table component_geometry
(
 geometry_id serial primary key,
 file_id int references raw_data(file_id),
 component_name varchar(10) references component(component_name),
 node_id int references geometry_volume(volume_id)
);

create table geometry_parameter
(
 parameter_type int not null,
 node_id int not null references geometry_node(node_id),
 parameter_value float not null,
 primary key (parameter_type, node_id)
);
*/

-- !!! GET INFORMATION ABOUT TABLES ---
--
-- MYSQL --
-- SHOW KEYS FROM detector_geometry WHERE Key_name = 'PRIMARY';
-- SHOW INDEX FROM detector_geometry WHERE key_name = 'primary';
-- show columns from detector_geometry where extra like '%auto_increment%';
-- SHOW TABLE STATUS FROM `mpd_db` LIKE 'detector_geometry';
-- SELECT ordinal_position, column_name, data_type, (is_nullable = 'YES') AS is_nullable, (extra = 'auto_increment') AS is_identity,
-- 		  (column_key = 'PRI') AS is_primary, (column_key = 'UNI') AS is_unique
-- FROM INFORMATION_SCHEMA.COLUMNS
-- WHERE table_name = 'run'
-- ORDER BY ordinal_position;
--
-- POSTGRES --
-- SELECT DISTINCT
--    a.attnum as ordinal_position,
--    a.attname as column_name,
--    format_type(a.atttypid, a.atttypmod) as data_type,
--    a.attnotnull as is_nullable,
--    def.adsrc as is_default,
--    coalesce(i.indisprimary, false) as is_primary,
--    coalesce(i.indisunique, false) as is_unique
-- FROM pg_attribute a JOIN pg_class pgc ON pgc.oid = a.attrelid
-- LEFT JOIN pg_index i ON (pgc.oid = i.indrelid AND i.indkey[0] = a.attnum)
-- LEFT JOIN pg_description com ON (pgc.oid = com.objoid AND a.attnum = com.objsubid)
-- LEFT JOIN pg_attrdef def ON (a.attrelid = def.adrelid AND a.attnum = def.adnum)
-- WHERE a.attnum > 0 AND pgc.oid = a.attrelid
-- AND pg_table_is_visible(pgc.oid)
-- AND NOT a.attisdropped
-- AND pgc.relname = 'run'  -- Your table name here
-- ORDER BY a.attnum;

create index component_name_lower_idx on component ((lower(component_name)));
create unique index parameter_name_lower_idx on component_parameter ((lower(parameter_name)));

insert into experiment_session(session_number, start_date, end_date)
values (1, '2015-02-22 15:55:12', '2015-02-25 01:11:57');
insert into experiment_session(session_number, start_date, end_date)
values (2, '2015-03-05 03:34:22', '2015-03-08 14:27:21');
insert into experiment_session(session_number, start_date, end_date)
values (3, '2015-03-11 20:59:55', '2015-03-15 08:24:29');