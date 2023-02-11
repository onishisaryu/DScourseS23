#!/bin/sh

wget http://spatialkeydocs.s3.amazonaws.com/FL_insurance_sample.csv.zip
unzip FL_insurance_sample.csv.zip
rm -rf __MACOSX
rm -f FL_insurance_sample.csv.zip
dos2unix -c mac FL_insurance_sample.csv

