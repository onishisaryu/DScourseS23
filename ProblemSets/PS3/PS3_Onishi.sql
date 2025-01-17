--Create table
CREATE TABLE "insurance" (
  policyID INTEGER,
  statecode TEXT,
  county TEXT,
  eq_site_limit REAL,
  hu_site_limit REAL,
  fl_site_limit REAL,
  fr_site_limit REAL,
  tiv_2011 REAL,
  tiv_2012 REAL,
  eq_site_deductible REAL,
  hu_site_deductible REAL,
  fl_site_deductible REAL,
  fr_site_deductible REAL,
  point_latitude REAL,
  point_longitude REAL,
  line TEXT,
  construction TEXT,
  point_granularity INTEGER
);

--importing the csv file
.mode csv
.separator ,
.import FL_insurance_sample.csv "insurance"

--printing the first 10 lines of the csv file
DELETE FROM insurance WHERE policyID = 'policyID';
SELECT * FROM insurance LIMIT 10;

--list of unique counties in the sample
SELECT DISTINCT county FROM insurance;

--change in average property appretiation
SELECT AVG(tiv_2012 - tiv_2011) AS difference FROM insurance;

--construction material count
SELECT construction, COUNT(*) AS frequency FROM insurance GROUP BY construction;

--save output
.output insurance.sqlite3
.dump
