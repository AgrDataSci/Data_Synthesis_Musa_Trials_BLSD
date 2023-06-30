#Downloads data from:
#Essential climate variables for assessment of climate variability from 1979 to present

import cdsapi

c = cdsapi.Client()

c.retrieve(
    'ecv-for-climate-change',
    {
        'format': 'zip',
        'variable': 'precipitation',
        'product_type': 'climatology',
        'climate_reference_period': '1991_2020',
        'month': [
            '01', '02', '03',
            '04', '05', '06',
            '07', '08', '09',
            '10', '11', '12',
        ],
        'origin': 'era5',
    },
    'data/climate/ecv_ERA5/climatology_1990_2020_ERA5/download.zip')
    
c.retrieve(
    'ecv-for-climate-change',
    {
        'format': 'zip',
        'variable': 'surface_air_temperature',
        'product_type': 'climatology',
        'climate_reference_period': '1991_2020',
        'month': [
            '01', '02', '03',
            '04', '05', '06',
            '07', '08', '09',
            '10', '11', '12',
        ],
        'origin': 'era5',
    },
    'data/climate/ecv_ERA5/climatology_1990_2020_ERA5/temp/download.zip')
    
    
c.retrieve(
    'ecv-for-climate-change',
    {
        'format': 'zip',
        'variable': 'surface_air_relative_humidity',
        'product_type': 'climatology',
        'climate_reference_period': '1991_2020',
        'month': [
            '01', '02', '03',
            '04', '05', '06',
            '07', '08', '09',
            '10', '11', '12',
        ],
        'origin': 'era5',
    },
    'data/climate/ecv_ERA5/climatology_1990_2020_ERA5/rhum/download.zip')  

    
