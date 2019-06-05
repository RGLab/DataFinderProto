# DataFinderProto

This is a Shiny app which will serve as a prototype for the new ImmuneSpace DataFinder. The shiny app resides in the `app/` directory. 

To run it from an R session:   
1. Make `app/` your working directory  
2. `shiny::runApp()`

To run it in a local docker instance:  
1. Navigate to the top-level directory of this repo (`DataFinderProto/`)  
2. `docker build -t datafinderproto:latest .`  
3. `docker run -d --name datafinderproto -p 8888:8888 datafinderproto:latest`  
4. Go to http://localhost:8888/ in your browser  
5. To stop it: `docker stop datafinderproto`  

```
app
├── d3
│   └── heatmap.js
├── data
│   └── cube.RData
├── global.R
├── helpers
│   ├── d3heatmap.R
│   ├── interactiveHeatmap.R
│   ├── manipulateData.R
│   ├── plots.R
│   ├── studyCard.R
│   └── uiFilterHelpers.R
├── server.R
├── start.R
├── ui.R
└── www
    ├── data_explorer.png
    ├── data.png
    ├── dimension_reduction.png
    ├── modules.png
    ├── participant_data.png
    ├── participants_overview.png
    ├── reports.png
    └── styles.css
```