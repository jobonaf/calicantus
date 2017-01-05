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
- [x] avoid zoom resetting (http://stackoverflow.com/questions/28393310/how-to-prevent-leaflet-map-from-resetting-zoom-in-shiny-app)
- [ ] import older data
- [ ] add some helps
- [x] set daterange min-max for the map
- [x] duplicate PoI selection in 'map' tab
- [x] add ARPAT contact
- [ ] add colorbar to map
- [ ] map delta
- [ ] statistics over a long period (table & map)
- [ ] table: selection of single day