# Barcelona city data


Piecing together some AQ and cycle network data for a Barcelona study
trip.

# Air Quality

There are 8 air quality sites in Barcelona monitoring NO2 and PM, the
main vehicle pollutants of interest.

Two useful ways to use this data to gain insights into the effect of
sources is to combine the data with wind speed and direction and do a
“Polar Plot” from the openair R package. This essentially bins
concentrations onto a compass and shows what combination of conditions
had which concentrations.

This has been done for all sites and species as an average for the last
5 years on [an interactive
map](https://blaisekelly.github.io/barcelona_city_dat/maps/L5Y_polar_map.html)

The second is to meteorologically normalise the data and remove the
effect of the weather, which is in many cases a stronger influence on
local concentrations than sources themselves. This allows patterns from
sources to be more visible. This was done using the [mweather R package] (https://github.com/skgrange/rmweather).

## Sant Adria de Beso (Olimpic) es1148a 

### NO<sub>2</sub>

#### concentrations by wind direction and speed for last 5 years

![](plots/no2_es1148a_polar.png)

#### Normalised concentrations since monitor installed

![](plots/no2_es1148a_norm.png)

### PM<sub>10</sub>

#### concentrations by wind direction and speed for last 5 years

![](plots/pm10_es1148a_polar.png)

#### Normalised concentrations since monitor installed

![](plots/pm10_es1148a_norm.png)


## Barcelon (I’Eixample) es1438a  


### NO<sub>2</sub>

#### concentrations by wind direction and speed for last 5 years

![](plots/no2_es1438a_polar.png)

#### Normalised concentrations since monitor installed

![](plots/no2_es1438a_norm.png)

### PM<sub>10</sub>

![](plots/pm10_es1438a_polar.png)

#### Normalised concentrations since monitor installed

![](plots/pm10_es1438a_norm.png)

## Barcelona (Gracia - Sant Gervasi) es1480a

### NO<sub>2</sub>

![](plots/no2_es1480a_polar.png)

#### Normalised concentrations since monitor installed

![](plots/no2_es1480a_norm.png)

## PM<sub>10</sub>

![](plots/pm10_es1480a_polar.png)

#### Normalised concentrations since monitor installed

![](plots/pm10_es1480a_norm.png) \## Barcelona (pl. de la Universitat)
es0559a

### PM<sub>10</sub>

![](plots/pm10_es0559a_polar.png)

#### Normalised concentrations since monitor installed

![](plots/pm10_es0559a_norm.png)

## I5 Badalona es0693a

### NO<sub>2</sub>

#### Normalised concentrations since monitor installed

![](plots/no2_es0693a_norm.png)

## IA Cornella es0695a

#### NO<sub>2</sub>

## Normalised concentrations since monitor installed

![](plots/no2_es0695a_norm.png)

# Cycle Network

The evolution of the Barcelona cycle network,since 2009 according to OSM
data, is shown below. Historic data was obtained using the ohsome R
package and processed with osmactive. Also shown on [an interactive
map](https://blaisekelly.github.io/barcelona_city_dat/maps/osm_cycle_paths.html)

![](plots/osm_evolution.gif)
