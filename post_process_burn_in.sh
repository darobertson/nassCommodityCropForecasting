for y in `echo "2015 2020 2025" | tr " " "\n"`; do
  echo -n "-- burning "$y"_burn_in_src.tif"
  gdal_rasterize -burn -38 -tr 0.000317039605830 0.000317039605830 -ot Byte -sql "SELECT * FROM gplcc_pilot_region_tillage_clu_with_worldbank_price_yield_rojections WHERE mid_$y = 1" gplcc_pilot_region_tillage_clu_with_worldbank_price_yield_rojections.shp $y"_burn_in_src.tif"
  gdal_calc.py -A $y"_burn_in_src.tif" 
  echo -n " -- merging $y"_landcover_projected_ag.tif""
  gdal_merge -n 0 -o $y"_landcover_projected_ag.tif" ~/PLJV/products/landcover/gplcc_LD/pljv_landcover_gplcc_pilot_region.tif $y"_burn_in_src.tif"
done
