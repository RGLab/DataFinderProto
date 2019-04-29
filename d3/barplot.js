// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)

var barHeight = Math.floor(height / data.length);

svg.selectAll('rect')
  .data(data)
  .enter().append('rect')
    .attr('width', function(d) { return d * width; })
    .attr('height', barHeight)
    .attr('y', function(d, i) { return i * barHeight; })
    .attr('fill', 'steelblue');

svg.selectAll("rect")
  .on("mousemove", function(d) {
    d3.selectAll("text").remove();
    var pos = d3.mouse(this);
    svg.append("text")
      .attr("x", pos[0])
      .attr("y", pos[1])
      .text(function(){return d;})
  .on("mouseout", function(d) {
    d3.selectAll("text").remove();
  });

  });