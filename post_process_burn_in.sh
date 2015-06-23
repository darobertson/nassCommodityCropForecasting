for y in `echo "2015 2020 2025" | tr " " "\n"`; do
  echo -n "-- burning "$y"_burn_in_src.tif: "
  gdal_rasterize -burn -38 -tr 0.000317039605830 0.000317039605830 -ot Byte -sql "SELECT * FROM gplcc_pilot_region_tillage_clu_with_worldbank_price_yield_rojections WHERE mid_$y = 1" gplcc_pilot_region_tillage_clu_with_worldbank_price_yield_rojections.shp $y"_burn_in_src.tif"
done
echo " -- launching R for reclassification and merge operations"
R --no-save --vanilla --slave < reclassify_and_merge.R
