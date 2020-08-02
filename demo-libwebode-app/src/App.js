import React from "react";
import "./App.css";
import {Controlled as CodeMirror} from 'react-codemirror2'
import "codemirror/lib/codemirror.css";
import "codemirror/theme/material.css";
import RawCodeMirror from "codemirror";
import createPlotlyComponent from "react-plotly.js/factory";

const SERVER_HOST = "http://localhost:50505";

// Importing plotly is a little bit of a nightmare.
// If we just try to directly `import Plot from "react-plotly.js"` then webpack runs out of memory trying to build.
const Plotly = require('plotly.js/lib/core');
Plotly.register([
    require('plotly.js/lib/heatmap'),
]);
const Plot = createPlotlyComponent(Plotly);

//require("./vim.js").addVimKeybindings(RawCodeMirror);
require("./simple.js").addDefineSimpleMode(RawCodeMirror);

RawCodeMirror.defineSimpleMode("odelang", {
    start: [
        // Match strings.
        {regex: /"(?:[^\\]|\\.)*?(?:"|$)/, token: "string"},
        // Match keywords.
        {regex: /(?:javascript|fn|as|for|in|if|else|return|native|var|dyn|const|int|float|plot|tolerance|simtime)\b/, token: "keyword"},
        // Match initialization and driving.
        {regex: /~|<-/, token: "drive"},
        // Match built-ins.
        {regex: /(?:Uniform|Slider|Gaussian|WienerProcess|WienerDerivative|exp|log|sin|cos|sqrt|abs|len|index_interpolating|print)\b/, token: "builtin"},
        {regex: /(?:globalTime|globalStepSize|e|pi)\b/, token: "atom"},
        // Match embedded javascript.
        //{regex: /javascript\s{/, token: "meta", mode: {spec: "javascript", end: /}/}},
        // Match numbers.
        {regex: /0x[a-f\d]+|[-+]?(?:\.\d+|\d+\.?\d*)(?:e[-+]?\d+)?/i, token: "number"},
        // Handle comments.
        {regex: /\/\/.*/, token: "comment"},
        {regex: /\/\*/, token: "comment", next: "comment"},
        // Match operators.
        {regex: /[-+\/*=<>!~]+/, token: "operator"},
        // Match compile-time variables.
        {regex: /\$[a-zA-Z_][a-zA-Z0-9_']*/, token: "compilevar"},
        // Match variables.
        {regex: /[a-zA-Z_][a-zA-Z0-9_']*/, token: "neutral"},
        // Indent and dedent on list/dict literals.
        {regex: /[\{\[\(]/, indent: true},
        {regex: /[\}\]\)]/, dedent: true},
    ],
    comment: [
        {regex: /.*?\*\//, token: "comment", next: "start"},
        {regex: /.*/, token: "comment"}
    ],
    meta: {
        dontIndentStates: ["comment"],
        lineComment: "//",
    },
});

const STARTING_CODE = `// Simple oscillator
// Hit ctrl+enter to recompile

/*
freq <- Slider(1, 2);
x ~ Uniform(-1, 1);
x'' <- -freq * x;
*/

fn ewma($x: dyn, $timeConstant: dyn) -> dyn {
    result ~ 0;
    result' <- ($x - result) / $timeConstant;
    return result;
}

x' <- WienerDerivative();
y <- ewma(x, 1.0);

plot x;
plot y;
simtime 10;
`;

/*
$x := 1;

print($x);

freq ~ Slider(1, 2);
x ~ Uniform(-1, 1);
x'' <- -freq * x;

plot x;
simtime 10;

fn index($x: [dyn], $i: int) -> dyn {
  if $i < 0 || $i >= len($x) {
    return 0.0;
  }
  return $x[$i];
}

fn second_order_upwind($x: [dyn], $i: int) -> dyn {
  return
    + 3*index($x, $i - 1)
    - 4*index($x, $i + 0)
    +   index($x, $i + 1);
}

fn ewma($x: dyn, $smoothing: float) -> dyn {
    result ~ 0;
    // Using (1 / $smoothing) here allows constant folding to help out.
    result' <- ($x - result) * (1 / $smoothing);
    return result;
}

freq ~ Slider(1, 2);
x ~ Uniform(-1, 1);
x'' <- -freq * x;

plot x;
simtime 10;
*/

class CodeEditor extends React.Component {
    constructor() {
        super()
        this.cmRef = React.createRef();
        this.state = {code: STARTING_CODE};
    }

    moveCursor(line, ch) {
        if (this.cmRef.current)
            this.cmRef.current.editor.setCursor({line, ch});
    }

    render() {
        return <div style ={{
            border: "2px solid black",
            width: "500px",
            height: "800px",
            borderRadius: "3px",
        }}>
            <CodeMirror
                value={this.state.code}
                options={{
                    mode: "odelang",
                    theme: "material",
                    lineNumbers: true,
                    indentUnit: 2,
                    extraKeys: {
                        "Ctrl-Enter": () => {
                            this.props.onCompile(this.state.code);
                        },
                        "Ctrl-S": () => {
                            
                        },
                        "Tab": (cm) => {
                            cm.replaceSelection("  ", "end");
                        },
                    },
                }}
                onBeforeChange={(editor, data, code) => {
                    this.setState({code});
                }}
                onChange={(editor, data, code) => {}}
                ref={this.cmRef}
            />
        </div>;
    }
}

/*
[
                        {
                            x: [1, 2, 3],
                            y: [2, 6, 3],
                            type: 'scatter',
                            mode: 'lines+markers',
                            marker: {color: 'red'},
                        },
                        {type: 'bar', x: [1, 2, 3], y: [2, 5, 3]},
                    ]

{width: 320, height: 240, title: 'A Fancy Plot'}
*/

class ResultsWindow extends React.Component {
    render() {
        return <div>
            {this.props.plotSpecs.map((plotSpec, i) =>
                <Plot data={plotSpec.data} layout={plotSpec.layout} key={i}/>
            )}
        </div>;
    }
}

const libwebode = require("./libwebode.js");

class App extends React.Component {
    constructor() {
        super();
        this.editorRef = React.createRef();
        this.state = {val: -1, plotSpecs: []};
    }

    componentDidMount() {
        //this.updateVal();
    }

    async updateVal() {
        await libwebode.initializationPromise;
        this.setState({val: libwebode.getValue()});
        this.forceUpdate();
    }

    async rerunSimulation(simData) {
        const ctx = simData.allocate();
        simData.initialize(ctx);
        let t = 0.0;
        const stepSize = Math.max(1e-3, simData.settings.tolerance);
        const {state, statePrime} = ctx;
        simData.extractPlotDatum(ctx, t, stepSize);
        while (t < simData.settings.simtime) {
            // Do a first-order Euler step.
            simData.getDerivative(ctx, t, stepSize);
            for (let i = 0; i < state.length; i++)
                state[i] += stepSize * statePrime[i];
            t += stepSize;
            simData.extractPlotDatum(ctx, t, stepSize);
        }
        console.log(ctx.plotData);
        const plotSpecs = [];
        for (const plotName of Object.keys(simData.plots)) {
            const plot = simData.plots[plotName];
            console.log("Plot:", plot);
            plotSpecs.push({
                data: [
                    {
                        ...ctx.plotData[plotName],
                        type: 'scatter',
                        mode: 'lines',
                    },
                ],
                layout: {width: 500, height: 300, title: plotName},
            });
        }
        this.setState({plotSpecs});
    }

    onCompile = async (code) => {
        const response = await fetch(SERVER_HOST + "/compile", {
            method: "POST",
            headers: {"Content-Type": "application/json"},
            body: JSON.stringify({code}),
        });
        const result = await response.json();
        if (result.error) {
            if (this.editorRef.current && result.line_number !== -1)
                this.editorRef.current.moveCursor(result.line_number - 1, result.column_number - 1);
        } else {
            this.rerunSimulation(eval(result.js));
        }
    }

    render() {
        return <div style={{
            display: "flex",
            justifyContent: "space-between",
        }}>
            {/*<div>Hello, world! {this.state.val}</div>*/}
            <div style={{
            }}>
                <CodeEditor
                    onCompile={this.onCompile}
                    ref={this.editorRef}
                />
            </div>
            <div style={{
            }}>
                <ResultsWindow plotSpecs={this.state.plotSpecs}/>
            </div>
        </div>;
    }
}

export default App;