https://hackage.haskell.org/package/aeson-schemas

curl -s 'https://api.weather.com/v2/pws/observations/current?apiKey=e1f10a1e78da46f5b10a1e78da96f525&units=e&stationId=KMNCOONR65&format=json' | jq


https://github.com/evuez/ttyme/blob/321fba3807a0a635421128ebf4055705dc46301b/src/API/Harvest.hs


https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html

https://mmhaskell.com/blog/2017/6/5/flexible-data-with-aeson

curl -s 'https://api.weather.com/v3/aggcommon/v3alertsHeadlines;v3-wx-observations-current;v3-location-point?apiKey=e1f10a1e78da46f5b10a1e78da96f525&geocodes=45.18,-93.32&language=en-US&units=e&format=json' | jq
