# Leaf shape plasticity in response to temperature in _C. bursa-pastoris_ genotypes from NYC
Data repository including leaf images and data frames from leaves grown at 16C, 20C, and 30C in a growth chambers at Michigan State University. This data is currently being analyzed for publication.  

Leaf images are included in the "Temperature leaf image" folder. The images are .jpgs and are in a compressed zip files. 

The count of individuals included in this study by temperature condition (condition) and genotype are included in the comma separated file (.csv) labeled count_data_pub.csv. 

Growth data collected from Janurary 2022 to December 2024 includes the leaf number (LN), rosette width (RW), and the length of the longest leaf (LLL) from the start of planting (Days After Planting (DAP) 0) to the time each individual bolted. The relative bolting date is the DAP/max(DAP) for each genotype in each condition. This data can be found in the csv labeled growth_data_pub_042226.csv. 

The leaf shape data includes the file, condition, genotype, pixels per centimeter (px_cm), coordinates for the base (base_x and base_y) and tip (tip_x and tip_y) of each leaf. Each rosette is assigned a unique two letter plant identifier (plant_id). This data also includes the measured length, width, area, circularity, aspect ratio, solidity, asymmetry, and relative node data. The relative node represents the leaf position in the rosette normalized by temperature condition and genotype as node/max(node). Leaf shape data can be found in the csv labeled shape_data_pub_042226.csv. 

### Jupyter notebooks

Steps for leaf shape analysis can be found in the jupyter notebook GPA_leaf_shape_v1.ipynb. 

Steps for measuring the Euclidean distance between mean leaf shapes can be found in the jupyter notebook capsella_temp_common_garden_adden.ipynb.

### Soil moisture data

Soil moisuture data was not included in the growth or leaf shape analysis. The data is available here for future analysis. 

Count of individuals - soil_mositure_count_pub_042226.csv

Growth data - soil_moisture_growth_data_pub_042226.csv

Leaf shape data - soil_mositure_leaf_shape_pub_042226.csv

Any inquires on this data should be sent to Asia Hightower at asiac313@gmail.com


