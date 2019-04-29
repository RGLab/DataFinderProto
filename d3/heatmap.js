var barHeight = Math.floor(height / 10);
var barWidth = Math.floor(width / 21);


rows = r2d3.svg.selectAll('.row')
  .data(r2d3.data)
  .enter().append("g")
    .attr("class", "row")
    .attr("transform", function(d, i) {return "translate(0," + i * barHeight + ")";});

rows.exit().remove();
    
rows.selectAll("rect")
  .data(function(d) { return d; })
  .enter().append("rect")
  .attr("class", "heatmap-box")
  .attr("x", function(d, i) { return i * barWidth })
  .attr("width", barWidth)
  .attr("height", barHeight)
  .attr("fill", function(d) { return d.color })
  .attr("d", function(d) { return JSON.stringify(d) }); // Need to add data as an attribute to be accessed by shiny
  
// Add mouseover action  
svg.selectAll("rect")
  .on("mouseover", function(d, i) {
    d3.selectAll("text").remove();
    var pos = d3.mouse(this);
    svg.append("text")
      .attr("x", (i % 21) * barWidth + barWidth)
      .attr("y", Math.floor(i/21) * barHeight + barHeight - 5)
      .text(function(){return d.assay + " at day " + d.timepoint});
    svg.append("text")
      .attr("x", (i % 21) * barWidth + barWidth)
      .attr("y", Math.floor(i/21) * barHeight + barHeight - 20)
      .text(function(){return d.participantCount + " participants"});
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
  
  // add click action
  .on("click", function(){
    Shiny.setInputValue(
      "heatmap_value",
      d3.select(this).attr("d"),
      {priority: "event"}
    );
  });


    