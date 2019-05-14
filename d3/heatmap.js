// inputs from r2d3:
// * data 
// * svg
// * height 
// * width
// * options (breaks, colors, xaxis, yaxis)

// get position
coord = svg.node().getBoundingClientRect(); 
// x, y, width, top, right, left, height, bottom

// Create margins 
var margin = { top: 20, right: 0, bottom: 30, left: 100 },
    width = width - margin.left - margin.right,
    height = height - margin.top - margin.bottom;


if (svg.selectAll("g").empty()) {
  var heatmap = svg.append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")")
    .attr("id", "heatmap");
} else {
  var heatmap = svg.select("#heatmap");
}

// Define the div for the tooltip
var div = d3.select("body").append("div") 
    .attr("class", "tooltip")       
    .style("opacity", 0);

// Set scales using options
// color scale
var colorScale = d3.scaleThreshold()
  .domain(options.breaks)
  .range(options.colors);
  
var xaxisScale = d3.scaleBand()
  .domain(options.xaxis)
  .range([0,width]);
  
var yaxisScale = d3.scaleBand()
  .domain(options.yaxis)
  .range([0,height]);

// Axis labels
// x
svg.append("g")
  .attr("transform", "translate(" + margin.left + ", " + (height + margin.top) + ")")
  .call(d3.axisBottom(xaxisScale));

svg.append("g")
  .attr("transform", "translate(" + margin.left + "," + margin.top + ")")
  .call(d3.axisLeft(yaxisScale));

// add data
boxes = heatmap.selectAll("rect").data(data);
boxes
  .enter().append("rect")
  .attr("x", function(d) { return xaxisScale(d.timepoint); })
  .attr("width", xaxisScale.bandwidth())
  .attr("y", function(d) { return yaxisScale(d.assay); })
  .attr("height", yaxisScale.bandwidth())
  .attr("id", function(d, i) { return "d" + i } ) 
  .style("fill", function(d) { return colorScale(d.participantCount); })
  .on("mouseover", function(d, i) {
    var r = coord.right - margin.left - coord.x - xaxisScale(d.timepoint);
    var t = coord.bottom - coord.x + margin.top + yaxisScale(d.assay);
    d3.select(this)
      .attr("stroke-width", "3px")
      .attr("stroke", "#faff84");
    div.transition()    
                .duration(100)    
                .style("opacity", .9);    
    div.html(d.participantCount + " participants <br>" + d.assay + " at day " + d.timepoint)  
                .style("right", r + xaxisScale.bandwidth()/2 + "px")   
                .style("top", (t - 35 - yaxisScale.bandwidth() * 1.5) + "px");  
    

      
  })
  .on("mouseout", function(d) {
    d3.select(this)
      .attr("stroke-width", "0px");
      div.transition()    
                .duration(100)    
                .style("opacity", 0); 
  })
  
  .on("click", function(d){
    Shiny.setInputValue(
      "heatmap_value",
      JSON.stringify(d),
      {priority: "event"}
    );
  });
  
boxes.transition()
  .attr("x", function(d) { return xaxisScale(d.timepoint); })
  .attr("width", xaxisScale.bandwidth())
  .attr("y", function(d) { return height - yaxisScale(d.assay); })
  .attr("height", yaxisScale.bandwidth())
  .style("fill", function(d) { return colorScale(d.participantCount); });

boxes.exit().remove();
