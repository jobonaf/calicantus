collecting data:

- [x] `get_ftp`
- [x] `get_http`
- [x] `get_ssh`
- [x] `get_local`
- [x] `get_dbqaemr`
- [x] `get_metadata_dbqaemr`
- [x] `get_data`

preparing data:

- [x] `read_ArpaV`
- [x] `read_ArpaT`
- [x] `read_data`
- [x] `import_data`
- [x] `data2db`
- [x] `collect_data.R` under crontab

web interface:

- [x] page:login
- [x] tab:data
- [x] tab:map
- [x] tab:timeseries
- [x] tab:about

improvements, details, etc:
- [x] avoid zoom resetting
- [x] import older data
- [x] add some helps
- [x] set daterange min-max for the map
- [x] add ARPAT contact
- [x] add colorbar to map
- [ ] map delta
- [x] statistics over a long period (table & map)
- [x] data table: selection of single day
- [x] timeseries: x axis with days, weeks, months, wday...
- [x] cluster analysis
- [x] users with different access authorizations
- [x] add Campania http://88.45.133.130/meteoambientecampania/prodotti/aria/arpac_dati_centraline_YYYYMMDD.csv
- [x] add Puglia http://www.arpa.puglia.it/pentaho/ViewAction?&DATAINIZIO=YYYYMMDD&DATAFINE=YYYYMMDD&INQUINANTE=PM10&type=csv&solution=ARPAPUGLIA&action=meta-aria.xaction&path=metacatalogo
- [x] add Lazio http://www.arpalazio.net/main/aria/sci/annoincorso/chimici.php
- [x] Puglia: fix metadata
- [ ] Campania: load older data, managing split at 23:00
- [x] time series of maps
- [x] add Croatia
    + data: http://iszz.azo.hr/iskzl/rs/podatak/export/json?postaja=265&polutant=5&tipPodatka=4&vrijemeOd=22.01.2017&vrijemeDo=22.01.2017
    + coordinates: http://iszz.azo.hr/iskzl/koordinate.htm
    + codes: http://iszz.azo.hr/iskzl/doc/servis_uputa.docx
- [ ] publish metadata tables
- [ ] daily map: automatic plot without "plot" button
- [ ] daily map: forward-backward buttons
- [ ] popup with more info