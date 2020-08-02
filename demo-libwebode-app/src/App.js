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
        {regex: /(?:javascript|global|fn|as|for|in|if|else|return|native|var|dyn|const|int|float|plot|tolerance|simtime)\b/, token: "keyword"},
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

/*
freq <- Slider(1, 2);
x ~ Uniform(-1, 1);
x'' <- -freq * x;
*/

cycle ~ 1;
cycle'' <- -cycle * Slider(0, 2);
plot cycle;

fn logistic($x) {
  return exp($x) / (1 + exp($x));
}

fn ewma($x: dyn, $timeConstant: dyn) -> dyn {
  result ~ 0;
  //smoothing <- 1 / ((1.01 + global.cycle) * $timeConstant);
  smoothing <- (1 + global.cycle) / $timeConstant;
  result' <- ($x - result) * smoothing;
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
        this.state = {
            code: STARTING_CODE,
            lastCompiledCode: "",
            dialogMessage: null,
            showDialog: false,
        };
        this.dialogCounter = 0;
    }

    moveCursor(line, ch) {
        if (this.cmRef.current)
            this.cmRef.current.editor.setCursor({line, ch});
    }

    setDialog(message, timeout) {
        const counterValue = ++this.dialogCounter;
        if (message === null) {
            this.setState({showDialog: false});
        } else {
            this.setState({dialogMessage: message, showDialog: true});
        }
        if (timeout !== undefined) {
            setTimeout(
                () => {
                    // Check that this timeout is still current by checking the ticket number.
                    if (this.dialogCounter === counterValue) {
                        this.setState({showDialog: false});
                    }
                },
                timeout,
            );
        }
    }

    render() {
        return <div style ={{
            //border: this.state.code === this.state.lastCompiledCode ? "2px solid black" : "2px solid #e22",
            border: "2px solid black",
            width: "600px",
            height: "96vh",
            borderRadius: "3px",
            position: "relative",
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
                            this.props.parent.onCompile(this.state.code);
                        },
                        "Ctrl-Space": () => {
                            this.props.parent.rerunSimulation(true);
                        },
                        "Ctrl-S": () => {
                            this.props.parent.onSaveCode(this.state.code);
                        },
                        "Tab": (cm) => {
                            cm.replaceSelection("  ", "end");
                        },
                        "Esc": () => {
                            this.setDialog(null);
                        },
                    },
                }}
                onBeforeChange={(editor, data, code) => {
                    this.setState({code});
                }}
                onChange={(editor, data, code) => {}}
                ref={this.cmRef}
            />
            {
                this.state.code !== this.state.lastCompiledCode &&
                <span style={{
                    border: "2px solid black",
                    borderRadius: "10px",
                    backgroundColor: "#f00",
                    opacity: 0.5,
                    position: "absolute",
                    bottom: "10px",
                    right: "10px",
                    padding: "5px",
                    fontSize: "60%",
                    fontFamily: "monospace",
                    color: "white",
                    zIndex: 10,
                }}>
                    ctrl+enter to recompile
                </span>
            }
            {
                <div style={{
                    //width: "300px",
                    display: "block",
                    border: "2px solid black",
                    borderRadius: "10px",
                    backgroundColor: "#456",
                    position: "absolute",
                    bottom: "10px",
                    left: "10px",
                    padding: "20px",
                    fontSize: "120%",
                    fontFamily: "monospace",
                    color: "white",
                    zIndex: 20,
                    opacity: this.state.showDialog ? 1 : 0,
                    transition: "opacity 0.15s ease-in-out",
                }}>
                    <pre style={{margin: "0px"}}>{this.state.dialogMessage}</pre>
                </div>
            }
        </div>;
    }
}

class ResultsWindow extends React.Component {
    constructor() {
        super();
        this.state = {widgetStates: {}};
    }

    getWidgetValue(name, defaultValue) {
        if (this.state.widgetStates.hasOwnProperty(name)) {
            return this.state.widgetStates[name];
        }
        this.updateWidgetValueNoCheck(name, defaultValue);
        return defaultValue;
    }

    updateWidgetValueNoCheck(name, newValue) {
        this.setState({widgetStates: {...this.state.widgetStates, [name]: newValue}});
    }

    updateWidgetValue(name, newValue) {
        const oldValue = this.getWidgetValue(name, newValue);
        if (oldValue !== newValue) {
            this.props.parent.setSimParameter(name, newValue);
            this.props.parent.rerunSimulation();
        }
        this.updateWidgetValueNoCheck(name, newValue);
    }

    applyAllParameters() {
        for (const widgetSpec of this.props.widgetSpecs) {
            const name = widgetSpec.name;
            if (!this.state.widgetStates.hasOwnProperty(name))
                continue;
            this.props.parent.setSimParameter(name, this.state.widgetStates[name]);
        }
    }

    renderWidget(widgetSpec) {
        if (widgetSpec.kind === "slider") {
            const liftedTextStyle = {
                color: "white",
                marginLeft: "10px",
                marginRight: "10px",
                marginTop: "-5px",
                marginBottom: "-5px",
                // Lift the text up a little to line up with the range input.
                // The display: inline-block is required to make transform work.
                transform: "translateY(-25%)",
                display: "inline-block",
                fontFamily: "monospace",
                fontSize: "130%",
            };
            const value = this.getWidgetValue(widgetSpec.name, (widgetSpec.low + widgetSpec.high) / 2);
            const step = (widgetSpec.high - widgetSpec.low) / 1000;
            return <div style={{
                border: "2px solid black",
                borderRadius: "10px",
                backgroundColor: "#555",
                padding: "10px",
                verticalAlign: "middle",
            }}>
                <span style={liftedTextStyle}>{widgetSpec.name}:</span>
                <input
                    type="range"
                    min={widgetSpec.low}
                    max={widgetSpec.high}
                    value={value}
                    step={step}
                    onChange={(event) => { this.updateWidgetValue(widgetSpec.name, event.target.value); }}
                />
                <span style={liftedTextStyle}>({value}) [{widgetSpec.low} - {widgetSpec.high}]</span>
            </div>
        }
        return <div style={{backgroundColor: "#833"}}>Bad widget spec: <code>{JSON.stringify(widgetSpec)}</code></div>;
    }

    render() {
        const boxStyle = {
            flexGrow: 1,
            border: "2px solid black",
            borderRadius: "10px",
            backgroundColor: "#444",
            padding: "10px",
            display: "flex",
            flexWrap: "wrap",
        };
        return <div style={{display: "flex", flexDirection: "column"}}>
            <div style={boxStyle}>
                {this.props.plotSpecs.map((plotSpec, i) =>
                    <div key={i} style={{margin: "2px"}}>
                        <Plot data={plotSpec.data} layout={plotSpec.layout}/>
                    </div>
                )}
            </div>
            <div style={{...boxStyle, marginTop: "10px"}}>
                {this.props.widgetSpecs.map((widgetSpec, i) =>
                    <div key={i}>
                        {this.renderWidget(widgetSpec)}
                    </div>
                )}
            </div>
        </div>;
    }
}

//const libwebode = require("./libwebode.js");

class App extends React.Component {
    constructor() {
        super();
        this.editorRef = React.createRef();
        this.parametersRef = React.createRef();
        this.state = {val: -1, plotSpecs: [], widgetSpecs: []};
        this.simData = null;
        this.simCtx = null;
        this.simRNGStartingState = null;
    }

    componentDidMount() {
        //this.updateVal();
    }

    async updateVal() {
        //await libwebode.initializationPromise;
        //this.setState({val: libwebode.getValue()});
        //this.forceUpdate();
    }

    async setupSimulation(simData) {
        this.simData = simData;
        this.simCtx = this.simData.allocate();
        if (this.parametersRef.current)
            this.parametersRef.current.applyAllParameters();
        await this.rerunSimulation(true);
    }

    async rerunSimulation(reseed) {
        if (this.simData === null)
            return;
        if (reseed) {
            this.simData.reseedRNG();
            this.simRNGStartingState = this.simData.getRNGState();
        } else if (this.simRNGStartingState !== null) {
            this.simData.setRNGState(this.simRNGStartingState);
        }
        this.simData.initialize(this.simCtx);
        let t = 0.0;
        const stepSize = Math.max(1e-3, this.simData.settings.tolerance);
        const {state, statePrime} = this.simCtx;
        // This first getDerivative is to fill in zeroth order variables for plotting.
        this.simData.getDerivative(this.simCtx, t, stepSize);
        this.simData.extractPlotDatum(this.simCtx, t, stepSize);
        while (t < this.simData.settings.simtime) {
            // Do a first-order Euler step.
            this.simData.getDerivative(this.simCtx, t, stepSize);
            for (let i = 0; i < state.length; i++)
                state[i] += stepSize * statePrime[i];
            t += stepSize;
            this.simData.extractPlotDatum(this.simCtx, t, stepSize);
        }
        console.log(this.simCtx.plotData);
        const plotSpecs = [];
        for (const plotName of Object.keys(this.simData.plots)) {
            const plot = this.simData.plots[plotName];
            plotSpecs.push({
                data: [
                    {
                        ...this.simCtx.plotData[plotName],
                        type: 'scatter',
                        mode: 'lines',
                    },
                ],
                layout: {
                    width: 500,
                    height: 300,
                    title: plotName,
                    margin: {
                        l: 30,
                        r: 30,
                        b: 30,
                        t: 30,
                        pad: 4,
                    },
                    plot_bgcolor: "#eee",
                    paper_bgcolor: "#eee",
                },
            });
        }
        this.setState({plotSpecs, widgetSpecs: this.simData.widgets});
    }

    setDialog(message, timeout) {
        if (this.editorRef.current)
            this.editorRef.current.setDialog(message, timeout);
        else
            alert(message);
    }

    setSimParameter(name, value) {
        if (this.simData === null)
            return;
        if (!this.simData.parameterTable.hasOwnProperty(name)) {
            this.setDialog("BUG BUG BUG: Attempt to set invalid parameter: " + name);
            return;
        }
        this.simCtx.parameters[this.simData.parameterTable[name]] = value;
    }

    onSaveCode = async (code) => {
        this.setDialog("Saved!", 750);
    }

    onCompile = async (code) => {
        const response = await fetch(SERVER_HOST + "/compile", {
            method: "POST",
            headers: {"Content-Type": "application/json"},
            body: JSON.stringify({code}),
        });
        const result = await response.json();
        if (result.error) {
            if (this.editorRef.current) {
                let errorMessage = result.message;
                if (result.line_number !== -1) {
                    errorMessage += "\nLine: " + result.line_number + " Column: " + result.column_number;
                    this.editorRef.current.moveCursor(result.line_number - 1, result.column_number - 1);
                }
                errorMessage += "\n(press esc to clear)";
                this.setDialog(errorMessage);
            }
        } else {
            // Update the most recently successfully compiled code.
            if (this.editorRef.current)
                this.editorRef.current.setState({lastCompiledCode: code});
            this.setupSimulation(eval(result.js));
        }
    }

    render() {
        return <div style={{
            //display: "flex",
            /*justifyContent: "space-between",*/
        }}>
            {/*<div>Hello, world! {this.state.val}</div>*/}
            <div style={{
                float: "left",
                paddingRight: "20px",
            }}>
                <CodeEditor
                    parent={this}
                    ref={this.editorRef}
                />
            </div>
            <div style={{
                width: "100%",
                heigh: "100%",
            }}>
                <ResultsWindow
                    parent={this}
                    widgetSpecs={this.state.widgetSpecs}
                    plotSpecs={this.state.plotSpecs}
                    ref={this.parametersRef}
                />
            </div>
        </div>;
    }
}

export default App;