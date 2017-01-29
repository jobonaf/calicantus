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
- [ ] `read_info`
- [x] `data2db`
- [ ] `info2db`
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
- [ ] add Campania http://88.45.133.130/meteoambientecampania/prodotti/aria/arpac_dati_centraline_YYYYMMDD.csv
- [x] add Puglia http://www.arpa.puglia.it/pentaho/ViewAction?&DATAINIZIO=YYYYMMDD&DATAFINE=YYYYMMDD&INQUINANTE=PM10&type=csv&solution=ARPAPUGLIA&action=meta-aria.xaction&path=metacatalogo
- [x] add Lazio http://www.arpalazio.net/main/aria/sci/annoincorso/chimici.php
