// inputs from r2d3:
// * data 
// * svg
// * height 
// * width
// * options (breaks, colors, xaxis, yaxis)


// Create margins 
var margin = { top: 0, right: 0, bottom: 50, left: 50 },
    width = width - margin.left - margin.right,
    height = height - margin.top - margin.bottom;

var body = svg.append("g").attr("transform", "translate(" + margin.left + "," + margin.top + ")");

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


// add data
boxes = body.selectAll(".box")
  .data(data)
  .enter().append("rect")
  .attr("x", function(d) { return xaxisScale(d.timepoint); })
  .attr("width", xaxisScale.bandwidth())
  .attr("y", function(d) { return height - yaxisScale(d.assay); })
  .attr("height", yaxisScale.bandwidth())
  .style("fill", function(d) { return colorScale(d.participantCount); });
  
boxes.exit().remove();

// mouseover action

boxes.on("mouseover", function(d, i) {
    d3.selectAll("text").remove();
    svg.append("text")
      .attr("x", xaxisScale(d.timepoint) + xaxisScale.bandwidth() + margin.left )
      .attr("y", height - yaxisScale(d.assay) + yaxisScale.bandwidth() - 5 )
      .text(function(){return d.assay + " at day " + d.timepoint});
    svg.append("text")
      .attr("x",  xaxisScale(d.timepoint) + xaxisScale.bandwidth() + margin.left )
      .attr("y",  height - yaxisScale(d.assay) + yaxisScale.bandwidth() - 20 )
      .text(function(){ return d.participantCount + " participants"; });
    d3.select(this)
      //.attr("fill", "#005377")
      .attr("stroke-width", "3px")
      .attr("stroke", "#FFFFFF");
      
  })
  .on("mouseout", function(d) {
    svg.selectAll("text").remove();
    d3.select(this)
      //.attr("fill", function(d) { return d.color })
      .attr("stroke-width", "0px");
  })
  
  .on("click", function(d){
    Shiny.setInputValue(
      "heatmap_value",
      JSON.stringify(d),
      {priority: "event"}
    );
  });

/*
rows = svg.selectAll('.row')
  .data(r2d3.data)
  .enter().append("g")
  .attr("class", "row")
  .attr("transform", function(d, i) { return "translate(0," + i * barHeight + ")"; });

rows.exit().remove();
    
rows.selectAll("rect")
  .data(function(d) { return d; })
  .enter().append("rect")
  .attr("class", "heatmap-box")
  .attr("x", function(d, i) { return i * barWidth })
  .attr("width", barWidth)
  .attr("height", barHeight)
  .attr("fill", function(d) { return d.color });
  */


    