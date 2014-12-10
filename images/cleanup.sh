#!/bin/sh

INPUT=$1
OUTPUT=$2
TMP=/tmp/snfsoudnf

if [ "$2" == "" ]; then
  echo "Usage: $0 input output"
  exit
fi

echo Blurring...
convert $INPUT -threshold '85%' -gaussian-blur 20 -quality 20 ${TMP}1.jpg

echo Tracing...
autotrace -despeckle-level 9 -despeckle-tightness 0.6 \
  -error-threshold 1 -output-format pdf -filter-iterations 8 \
  -report-progress ${TMP}1.jpg >${TMP}2.pdf

echo Downsampling...
convert ${TMP}2.pdf -quality 20 $OUTPUT
