// Accessing properties via string literal prevents renaming by javascript minifiers which can cause FFI errors
window['saline'] = (function () {
  const d3 = window['d3'];

  function linkArc(d) {
    const r = Math.hypot(d.target.x - d.source.x, d.target.y - d.source.y);
    return `
      M${d.source.x},${d.source.y}
      A${r},${r} 0 0,1 ${d.target.x},${d.target.y}
    `;
  }

  drag = simulation => {

    function dragstarted(event, d) {
      if (!event.active) simulation.alphaTarget(0.3).restart();
      d.fx = d.x;
      d.fy = d.y;
    }

    function dragged(event, d) {
      d.fx = event.x;
      d.fy = event.y;
    }

    function dragended(event, d) {
      if (!event.active) simulation.alphaTarget(0);
      d.fx = null;
      d.fy = null;
    }

    return d3.drag()
        .on("start", dragstarted)
        .on("drag", dragged)
        .on("end", dragended);
  }

  const svg = (data) => {
    const width = 500;
    const height = 500;
    const nodes = data.nodes.map(Object.create);
    const edges = data.edges.map(Object.create);
    const types = data.types;

    const color = () => "#009FC0";

    const simulation = d3.forceSimulation(nodes)
      .force("link", d3.forceLink(edges).id(d => d.id))
      .force("charge", d3.forceManyBody().strength(-3000))
      .force("center", d3.forceCenter(0,0))
      ;

    const svg = d3.create("svg")
      .attr("viewBox", [-width / 2, -height / 2, width, height])
      .attr("width", width)
      .attr("height", height)
      .attr("style", "max-width: 100%; height: auto; font: 12px sans-serif;");

    svg.append("defs").selectAll("marker")
      .data(types)
      .join("marker")
        .attr("id", d => `arrow-${d}`)
        .attr("viewBox", "0 -5 10 10")
        .attr("refX", 15)
        .attr("refY", -0.5)
        .attr("markerWidth", 6)
        .attr("markerHeight", 6)
        .attr("orient", "auto")
      .append("path")
        .attr("fill", color)
        .attr("d", "M0,-5L10,0L0,5");

    const link = svg.append("g")
        .attr("fill", "none")
        .attr("stroke-width", 1.5)
      .selectAll("path")
      .data(edges)
      .join("path")
        .attr("stroke", d => color(d.type))
        .attr("stroke-dasharray", d => d.required ? null : "4")
        .attr("marker-end", d => `url(${new URL(`#arrow-${d.type}`, location)})`);

    const node = svg.append("g")
        .attr("fill", "currentColor")
        .attr("stroke-linecap", "round")
        .attr("stroke-linejoin", "round")
      .selectAll("g")
      .data(nodes)
      .join("g")
        .call(drag(simulation));

    node.append("circle")
      .attr("stroke", "black")
      .attr("stroke-width", 1.5)
      .attr("r", 4);

    node.append("text")
        .attr("text-anchor", d => d.x > 0 ? "start" : "end")
        .attr("x", (d => Math.sign (d.x) * 5))
        .attr("y", (d => Math.sign (d.y) * 15))
        .text(d => d.id)
      .clone(true).lower()
        .attr("fill", "none")
        .attr("stroke", "black")
        .attr("stroke-width", 3);

    simulation.on("tick", () => {
      link.attr("d", linkArc);
      node.attr("transform", d => `translate(${d.x},${d.y})`);
      });

    data.element.replaceChildren(Object.assign(svg.node(), {scales: {color}}));
  };

  // Used to make pasting the 12-word mnemonic easier: with the 12 words copied (and separated by space)
  // pasting them in any of the grid will smartyl fill them with the words
  async function pasteMnemonic(elem) {
    // Get the string that is currently copied
    const clipboardContents = await navigator.clipboard.readText()

    // Reliably split them in words (supports language, quotes, etc.)
    const segmenter = new Intl.Segmenter([], { granularity: 'word' })
    const segmentedContents = segmenter.segment(clipboardContents)
    const words = [...segmentedContents].filter(s => s.isWordLike).map(s => s.segment)

    // Only do our smart filling if exactly 12 words are found...
    if (words.length === 12) {
      const fields = document.getElementsByClassName("mnemonicsField")
      let i = 0
      for (const field of fields) {
        field.value = words[i++]
        let chgEvt = new Event('input', { bubbles: true }) // Need to trigger 'input' and not 'change' here
        field.dispatchEvent(chgEvt)
      }
    }
    // ... otherwise, let the normal paste happen
    else {
      elem.value = clipboardContents
    }
  };

  // Copy text in clipboard, optionally taking a string to display the user
  copyToClipboard = (addr, msg = null) => {
    navigator.clipboard.writeText(addr)
    if (msg !== null)
      // TODO: display pop-up saying address was copied (nice UI hint)
      console.log (msg)
  };

  pollUntil = (intervalMilliseconds, condition) => new Promise((resolve) => {
    const loop = () => {
      if (condition())
        resolve();
      else
        setTimeout(loop, intervalMilliseconds);
    };
    loop();
  });

  initTokenChart = (container, onInit) => {
    const chartConfig = {
      labels: [],
      series: [],
      colors: [],
      noData: {text: 'Waiting for Wallet...'},
      chart: {
        type: "pie",
        width: "130%",
        toolbar: {show: false,},
      },
      responsive: [
        {
          breakpoint: 770,
          options: {
            chart: {
              width: "70%",
            },
            dataLabels: {enabled: true,},
        }
        }
      ],
      title: {
        text: 'Token Division ($ value)',
        align: 'top',
        style: {
          fontWeight: 'bold',
          color: '#A6A5A5',
        }
      },
      dataLabels: {enabled: false,}, // No need: number in the table
      legend: {show: false,} // no need: table right next to it
    };

    // We have to create the variable here so it's in scope for returning
    let chart

    // Perfom actual init, but we need to have the 'container' in the DOM present
    initChart = () => {
      chart = new ApexCharts(container, chartConfig)
      chart.render()
      onInit([])
    }

      // newData format: [['BTC', amount, '#RRGGBB'], ['ETH', amount, '#RRGGBB'], ...]
    const updateChart = (newData) => {
      if (newData.some(e => e.length !== 3)) {
        console.error('Wrongly formatted entry to \'updateChart()\':')
        console.error(e)
        console.error('Should be a triple [\'TOKEN\', amount, \'#RRGGBB\']')
        return
      }

      chart.updateOptions({
        labels: newData.map(e => e[0]),
        series: newData.map(e => e[1]),
        colors: newData.map(e => e[2]),
      })
    }

    pollUntil(50, () => document.contains(container)).then(initChart);
    return {updateChart}
  };

  const roundtripBinary = f => a => bytestring2hex(f(hex2bytestring(a)));

  const hex2bytestring = hex =>
    Uint8Array.from(hex.match(/.{1,2}/g).map((byte) => parseInt(byte, 16)));

  const hex2bytestrings = hex =>
    hex.split(' ').map(hex2bytestring);

  const bytestring2hex = arrayBuffer =>
    Array
      .from(new Uint8Array(arrayBuffer))
      .map(n => n.toString(16).padStart(2, "0"))
      .join("");


  // https://stackoverflow.com/a/11562550
  // TODO: can stack overflow for large buffers - do we care?
  const bytestring2base64 = arrayBuffer => btoa(stringFromBytes(arrayBuffer));
  const stringFromBytes = arrayBuffer => String.fromCharCode(...new Uint8Array(arrayBuffer))

  const quoted = s => '"' + s + '"';

  const rpc = {
    block: (uri,height) => tendermint.RpcClient(uri).block({height}),
    transaction: (uri, hash) => tendermint.RpcClient(uri).tx({hash: "0x" + hash}),
    query: (uri, path, data) => tendermint.RpcClient(uri).abciQuery({path: quoted(path), data: quoted(data)}),
    broadcastTxSync: (uri, data) => tendermint.RpcClient(uri).broadcastTxSync({tx: quoted(data)}),
  };

  return {
    log: txt => console.log('Received "' + txt + '" from FFI'),
    svg,
    diagram: (element, nodes, edges, groups) => svg (
      { element
      , nodes: nodes.map((id) => ({id}))
      , edges: edges.map(([s, t, assets, step, required]) => ({ source: s, target: t, assets, type: step, required}))
      , types: groups
      },
    ),
    pasteMnemonic,
    pollUntil,
    initTokenChart,
    bytestring2base64,
    bytestring2hex,
    hex2bytestring, hex2bytestrings,
    roundtripBinary,
    rpc,
  };
})();
