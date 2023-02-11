--importing the csv file
.mode csv
.separator ,
.import FL_insurance_sample.csv insurance

--printing the first 10 lines of the csv file
SELECT * FROM insurance LIMIT 10;

--list of unique counties in the sample
SELECT DISTINCT county FROM insurance;

--change in average property appretiation
SELECT AVG(tiv_2012 - tiv_2011) AS difference
FROM insurance;

--construction material count
SELECT construction, COUNT(*) AS frequency
FROM insurance
GROUP BY construction;



