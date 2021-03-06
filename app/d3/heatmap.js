// inputs from r2d3:
// * data 
// * svg
// * height 
// * width
// * options (breaks, colors, xaxis, yaxis, selected)

// Get selected
if (options.selected === null) {
  var selected = []
} else {
  var selected = options.selected
}

// get position
coord = svg.node().getBoundingClientRect(); 
// x, y, width, top, right, left, height, bottom

// Create margins 
var margin = { top: 20, right: 0, bottom: 30, left: 100 },
    width = width - margin.left - margin.right,
    height = height - margin.top - margin.bottom;



// // Define the div for the tooltip
var tooltip = d3.select("#heatmap-label")
var arrow = d3.select(".arrow-down")

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


// Create body and axes

if (svg.selectAll("g").empty()) {
  var heatmap = svg.append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")")
    .attr("id", "heatmap");
  svg.append("g")
    .attr("id", "xaxis-labels")
    .attr("transform", "translate(" + margin.left + ", " + (height + margin.top) + ")")
    .call(d3.axisBottom(xaxisScale));

  svg.append("g")
    .attr("id", "yaxis-labels")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")")
    .call(d3.axisLeft(yaxisScale));
} else {
  var heatmap = svg.select("#heatmap");
}

// Axis labels
// x


// add data
boxes = heatmap.selectAll("rect").data(data);
boxes
  .enter().append("rect")
  .attr("x", function(d) { return xaxisScale(d.timepoint); })
  .attr("width", xaxisScale.bandwidth())
  .attr("y", function(d) { return yaxisScale(d.assay); })
  .attr("height", yaxisScale.bandwidth())
  .attr("id", function(d) { return d.id } ) 
  .style("fill", function(d) { 
    if (selected.includes(this.id)) {
     return "#fff766"
    } else {
     return colorScale(d.participantCount); }
   })
  .on("mouseover", function(d, i) {
    // Tooltip coordinates
    var r = coord.right - margin.left - coord.x - xaxisScale(d.timepoint);
    var t = coord.top + margin.top + yaxisScale(d.assay) + yaxisScale.bandwidth();
    // Change style
    d3.select(this)
      .attr("stroke-width", "3px")
      .attr("stroke", "#faff84");
    // Tooltip
    tooltip.transition()
      .duration(100)
      .style("opacity", .9);
    arrow.transition()
      .duration(100)
      .style("opacity", .9);
    arrow
      .style("right", r - (xaxisScale.bandwidth()/2 - 5) + "px")
      .style("top", (t - yaxisScale.bandwidth() - 12) + "px");
    tooltip.html(d.participantCount + " participants <br>" + d.assay + " at day " + d.timepoint)  
                .style("right", r - xaxisScale.bandwidth()/2 + "px")   
                .style("top", (t - yaxisScale.bandwidth() - 35 - 12) + "px");  
  })
  .on("mouseout", function(d) {
    // Reset to original 
    d3.select(this)
      .attr("stroke-width", "0px");
    tooltip.transition()    
              .duration(100)    
              .style("opacity", 0); 
    arrow.transition()
      .duration(100)
      .style("opacity", 0);
  })
  
  .on("click", function(d, i){  
    var id = d3.select(this).attr("id");
    Shiny.setInputValue(
      "heatmap_value",
      JSON.stringify({id : d3.select(this).attr("id"),
        value : d}),
      {priority: "event"}
    )
    d3.select(this)
      .style("fill", 
      function(d) { 
        if (selected.includes(this.id)) {

         return colorScale(d.participantCount);
       } else {
        return "#fff766"
 }
       });
  });
  
boxes.transition()
  .duration(100)
  .attr("x", function(d) { return xaxisScale(d.timepoint); })
  .attr("width", xaxisScale.bandwidth())
  .attr("y", function(d) { return yaxisScale(d.assay); })
  .attr("height", yaxisScale.bandwidth())
  .attr("id", function(d, i) { return d.id } ) 
  .style("fill", function(d) { 
    if (selected.includes(this.id)) {
     return "#fff766"
    } else {
     return colorScale(d.participantCount); }
   })

boxes.exit().remove();