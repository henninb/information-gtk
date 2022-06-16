https://hackage.haskell.org/package/aeson-schemas

curl -s 'https://api.weather.com/v2/pws/observations/current?apiKey=e1f10a1e78da46f5b10a1e78da96f525&units=e&stationId=KMNCOONR65&format=json' | jq


https://github.com/evuez/ttyme/blob/321fba3807a0a635421128ebf4055705dc46301b/src/API/Harvest.hs


https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html

https://mmhaskell.com/blog/2017/6/5/flexible-data-with-aeson

curl -s 'https://api.weather.com/v3/aggcommon/v3alertsHeadlines;v3-wx-observations-current;v3-location-point?apiKey=e1f10a1e78da46f5b10a1e78da96f525&geocodes=45.18,-93.32&language=en-US&units=e&format=json' | jq
curl -s 'https://api.weather.com/v3/aggcommon/v3-wx-observations-current?apiKey=e1f10a1e78da46f5b10a1e78da96f525&geocodes=45.18,-93.32&language=en-US&units=e&format=json' | jq

curl -s 'https://api.weather.com/v3/wx/forecast/daily/10day?apiKey=e1f10a1e78da46f5b10a1e78da96f525&geocode=44.977,-93.265&units=e&language=en-US&format=json' | jq

curl -s 'https://api.weather.com/v2/astro?apiKey=e1f10a1e78da46f5b10a1e78da96f525&geocode=44.977,-93.265&days=15&date=20220613&format=json' | jq

curl -s 'https://api.weather.com/v3/dateTime?apiKey=e1f10a1e78da46f5b10a1e78da96f525&geocode=44.977,-93.265&format=json' | jq


https://gist.github.com/nbogie/985645/10bcafecb208552bf23781bda6fbd4ee3de9dac8

https://github.com/jaejin/haskell-workbook/blob/042e36b886a31d3bbce3b9defb963857eeb6e9cf/hello-gigtk/app/Main.hs
