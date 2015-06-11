-- GPS data

DROP TABLE IF EXISTS gps;
create table gps(
	datetime timestamp,
	longitude numeric(9,5),
	latitude numeric(8,5),
	elevation numeric(5,1),
	subjectID character(8),
	instrumentID character(6),
	sessionID character(5),
  filename varchar(100),
  projectID varchar(30),
	uniquekey serial not null PRIMARY KEY,
  dateadded timestamp default current_timestamp

);

-- ambulatory blood pressure

DROP TABLE IF EXISTS abp;
create table abp(
	datetime timestamp,
	systolic_BP numeric(5,0),
	mean_arterial_P numeric(5,0),
	diastolic_BP numeric(5,0),
	heart_rate numeric(5,0),
	event_code varchar(10),
	studyid varchar(10),
	code numeric(2,0),
	subjectID character(8),
	instrumentID character(6),
	sessionID character(5),
  filename varchar(100),
  projectID varchar(30),
	uniquekey serial not null PRIMARY KEY,
  dateadded timestamp default current_timestamp

);

-- black carbon

DROP TABLE IF EXISTS mae;
create table mae(
	datetime timestamp,
	ref numeric(8,0),
	sen numeric(8,0),
	atn numeric(10,5),
	flow numeric(4,0),
	temp numeric(4,0),
	status numeric(2,0),
	battery numeric(3,0),
	bc numeric(10,0),
	subjectID character(8),
	instrumentID character(6),
	sessionID character(5),
 filename varchar(100),
projectID varchar(30),
	uniquekey serial not null PRIMARY KEY,
    dateadded timestamp default current_timestamp
);

-- activity levels, particulate matter

DROP TABLE IF EXISTS mpm;
create table mpm(
	datetime timestamp,
	neph_rhcorrect numeric(12,2),
	temperature numeric(5,2),
	RH numeric(5,2),
	battery  numeric(10,4),
	inlpressure  numeric(10,4),
	oripressure  numeric(10,4),
	flow  numeric(10,4),
	xaxis  numeric(10,4),
	yaxis   numeric(10,4),
	zaxis   numeric(10,4),
	vectorsumcomp   numeric(10,4),
	action varchar(100),
	hdr_filename varchar(100),
	hdr_downloaddate varchar(10),
	hdr_downloadtime varchar(10),
	hdr_deviceserial varchar(20),
	hdr_datetimehard varchar(10),
	hdr_datetimesoft varchar(30),
	hdr_participantID varchar(100),
	hdr_filterID varchar(20),
	hdr_participantWt varchar(3),
	hdr_inletsize varchar(10),
	hdr_delay_samp_off varchar(20),
	hdr_systimes varchar(30),
	hdr_nephelometer varchar(40),
	hdr_temperature varchar(40),
	hdr_humidity varchar(40),
	hdr_inlpressure varchar(40),
	hdr_oripressure varchar(40),
	hdr_flow varchar(40),
	hdr_accelerometer varchar(40),
	hdr_battery varchar(40),
	hdr_ventilation varchar(40),
	subjectID character(8),
	instrumentID character(6),
	sessionID character(5),
	filterID character(8),
 filename varchar(100),
projectID varchar(30),
	uniquekey serial not null PRIMARY KEY,
    dateadded timestamp default current_timestamp
	
);

-- heart rate variability, breathing rate/volume
DROP TABLE IF EXISTS hxi;
create table hxi(
  datetime timestamp,
  breathing_rate numeric(6,2),
  heart_rate numeric(3,0),
  minute_ventilation numeric(12, 2),
  cadence numeric(10,4),
  activity numeric(14,10),
  subjectID character(8),
  instrumentID character(6),
  sessionID character(5),
  filename varchar(100),
  projectID varchar(30),
  uniquekey serial not null PRIMARY KEY,
  dateadded timestamp default current_timestamp
		
) ;


-- PDR
DROP TABLE IF EXISTS pdr;
  create table pdr(
  datetime timestamp,
  point integer,
  avg_mg3 numeric(8,4),
  hdr_serial varchar(40),
  hdr_userid varchar(40),
  hdr_tagnum varchar(40),
  hdr_numlogged varchar(40),
  hdr_start varchar(40),
  hdr_elapsed varchar(40),
  hdr_logperiod varchar(40),
  hdr_calibration varchar(40),
  hdr_maxdispconc varchar(40),
  hdr_timemax varchar(40),
  hdr_maxstelconc varchar(40),
  hdr_timemaxstel varchar(40),
  hdr_avgconc varchar(40),
  subjectID character(8),
  instrumentID character(6),
  sessionID character(5),
  filename varchar(100),
  projectID varchar(30),
  uniquekey serial not null PRIMARY KEY,
  dateadded timestamp default current_timestamp
		
) ;


