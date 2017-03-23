- [x] collecting data
- [x] preparing data
- [x] web interface

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
- [x] time series of maps
- [x] add Croatia
    + data: http://iszz.azo.hr/iskzl/rs/podatak/export/json?postaja=265&polutant=5&tipPodatka=4&vrijemeOd=22.01.2017&vrijemeDo=22.01.2017
    + coordinates: http://iszz.azo.hr/iskzl/koordinate.htm
    + codes: http://iszz.azo.hr/iskzl/doc/servis_uputa.docx
- [ ] publish metadata tables
- [x] daily map: automatic plot without "plot" button
- [ ] daily map: forward-backward buttons
- [ ] popup with more info
- [x] add Ticino: http://www.oasi.ti.ch/web/rest/measure/csv?domain=air&resolution=d&parameter=PM10&from=2017-02-03&to=2017-02-10&location=auto_275
- [x] add CAMS services 
    + docs: http://www.regional.atmosphere.copernicus.eu/doc/Guide_Numerical_Data_CAMS_new.pdf
    + example: http://download.regional.atmosphere.copernicus.eu/services/CAMS50?&token=__M0bChV6QsoOFqHz31VRqnpr4GhWPtcpaRy3oeZjBNSg__&grid=0.1&model=CHIMERE&package=FORECAST_PM10_SURFACE&time=0H24H&referencetime=2017-02-14T00:00:00Z&format=NETCDF&licence=yes
- [ ] add Carinthia http://www.umwelt.ktn.gv.at/luft/tagesbericht/lt_170201.htm
- [ ] add Serbia paste0("http://www.amskv.sepa.gov.rs/konektori/pregled_tabela_uporedni.php?",
              paste(paste0("stanice[]=",c(2,9,16,38,41,50,51,53,57,58,59)),collapse="&"),
              "&komponente[]=5&tipgrafikona[]=column&periodi[]=dana30&agregacija[]=24")
- [ ] CAMS: EPSgrams
- [ ] add ozone
- [ ] tools for models evaluation
