import React from "react";
import "./App.css";
import {Controlled as CodeMirror} from 'react-codemirror2'
import "codemirror/lib/codemirror.css";
import "codemirror/theme/material.css";
import RawCodeMirror from "codemirror";
import createPlotlyComponent from "react-plotly.js/factory";
import * as d3 from "d3";
import { update } from "plotly.js/lib/core";
//const ode45 = require('ode45-cash-karp');
const ode45 = require("./ode45cashkarpmodified.js");

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
        {regex: /(?:javascript|array|global|fn|as|with|for|in|if|else|return|native|var|dyn|const|int|float|unit|Function|plot|title|trace|trace2d|layout|options|optim|objective|tunable)\b/, token: "keyword"},
        // Match initialization and driving.
        {regex: /~|<-/, token: "drive"},
        // Match built-ins.
        {regex: /(?:Slider|Selector|Checkbox|Uniform|Gaussian|Gamma|Beta|Frechet|PoissonProcess|WienerProcess|WienerDerivative|WienerDerivativeUnstable|D|Integrate|exp|log|sin|cos|sqrt|abs|floor|ceil|round|min|max|len|str|addDeriv|subDeriv|index_interpolating|print)\b/, token: "builtin"},
        {regex: /(?:globalTime|globalStepSize|e|pi|true|false|backend|tolerance|stepsize|plotperiod|integrator|simtime|minstep|maxstep|mcsamples|mctraces|mcenvelope|randomseed|processscale|mcpercentile|prefix|unitname|crossoverprob|diffweight|populationsize|maxsteps|patience|patiencefactor|objectiveaggregation)\b/, token: "atom"},
        // Match embedded javascript.
        //{regex: /javascript\s{/, token: "meta", mode: {spec: "javascript", end: /}/}},
        // Match numbers.
        {regex: /0x[a-f\d]+|[-+]?(?:\.\d+|\d+\.?\d*)(?:e[-+]?\d+)?/i, token: "number"},
        // Match units.
        {regex: /`.*`/, token: "units"},
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

let BAD_STARTING_CODE = null;
let STARTING_CODE = "// System\n\n\n";

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

function tail(a) {
    return a[a.length - 1];
}

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
                        /*
                        "Ctrl-R": () => {
                            this.props.parent.onReload();
                        },
                        */
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
                    whiteSpace: "pre-wrap",
                }}>
                    {this.state.dialogMessage}
                </div>
            }
        </div>;
    }
}

const widgetTextStyle = {
    color: "white",
    marginLeft: "10px",
    marginRight: "10px",
    marginTop: "-5px",
    marginBottom: "-5px",
    // Lift the text up a little to line up with the range input.
    // The display: inline-block is required to make transform work.
    display: "inline-block",
    fontFamily: "monospace",
    fontSize: "130%",
};
const liftedTextStyle = {...widgetTextStyle, transform: "translateY(-25%)"};
const widgetBoxStyle = {
    display: "inline-block",
    whiteSpace: "nowrap",
    height: "22px",
    border: "2px solid black",
    borderRadius: "10px",
    backgroundColor: "#555",
    padding: "10px",
    verticalAlign: "middle",
};

const objectiveFormat = d3.format(".4r");

class OptimizerBox extends React.Component {
    constructor() {
        super();
        this.state = {objectivePlot: null, totalCalls: null};
    }

    updateOptimizerPlot(objectivePlot, totalCalls) {
        this.setState({objectivePlot, totalCalls});
    }

    render() {
        let plotX = null;
        let plotY = null;
        if (this.state.objectivePlot !== null) {
            plotX = this.state.objectivePlot.x.slice();
            plotY = this.state.objectivePlot.y.slice();
            plotX.push(this.state.totalCalls);
            plotY.push(tail(plotY))
        }
        return <div style={{...widgetBoxStyle, height: "auto"}}>
            <span style={widgetTextStyle}>Optimize</span>
            <button
                style={{marginRight: "10px", marginBottom: "5px"}}
                onClick={() => { this.props.parent.activateOptimizer(this.props.widgetSpec, this); }}
            >
                Start/Stop
            </button>
            {this.state.objectivePlot !== null && <>
                <span style={widgetTextStyle}>Calls: {this.state.totalCalls}</span>
                <span style={widgetTextStyle}>Best: {objectiveFormat(tail(plotY))}</span>
                <br/>
                <Plot
                    data={[{
                        x: plotX,
                        y: plotY,
                        type: "scatter", mode: "lines",
                    }]}
                    layout={{
                        title: "Objective",
                        width: 500, height: 300,
                        margin: {l: 30, r: 30, b: 30, t: 30, pad: 4},
                    }}
                />
            </>}
        </div>;
    }
}

class ResultsWindow extends React.Component {
    constructor() {
        super();
        this.state = {widgetStates: {}, editBoxName: null, editBoxContents: ""};
        this.optimizerRef = React.createRef();
    }

    getWidgetValue(name, defaultValue) {
        if (this.state.widgetStates.hasOwnProperty(name)) {
            return this.state.widgetStates[name];
        }
        this._updateWidgetValueNoCheck(name, defaultValue);
        return defaultValue;
    }

    _updateWidgetValueNoCheck(name, newValue) {
        this.setState({widgetStates: {...this.state.widgetStates, [name]: newValue}});
    }

    updateWidgetValue(widgetSpec, newValue) {
        const oldValue = this.getWidgetValue(widgetSpec.name, newValue);
        //console.log("updateWidgetValue:", widgetSpec.name, oldValue, "->", newValue);
        if (oldValue !== newValue) {
            this.props.parent.setSimParameter(widgetSpec.name, newValue, widgetSpec.recompile === true);
            this.props.parent.rerunSimulation();
        }
        this._updateWidgetValueNoCheck(widgetSpec.name, newValue);
    }

    applyAllParameters() {
        for (const widgetSpec of this.props.widgetSpecs) {
            const name = widgetSpec.name;
            if (!this.state.widgetStates.hasOwnProperty(name))
                continue;
            this.props.parent.setSimParameter(name, this.state.widgetStates[name], widgetSpec.recompile === true);
        }
    }

    onDoubleClick = (event, widgetSpec) => {
        this.setState({
            editBoxName: widgetSpec.name,
            editBoxContents: String(this.getWidgetValue(widgetSpec.name, widgetSpec.default_value)),
        });
    }

    renderWidget(widgetSpec) {
        if (widgetSpec.kind === "slider") {
            const value = this.getWidgetValue(widgetSpec.name, widgetSpec.default_value);
            let low = widgetSpec.low;
            let high = widgetSpec.high;
            let step = (high - low) / 1000;
            let sliderPos = value;
            if (widgetSpec.log) {
                sliderPos = Math.round(
                    1000 * (Math.log(value) - Math.log(low)) / (Math.log(high) - Math.log(low))
                );
                low = 0;
                high = 1000;
                step = 1;
            }
            // Try to guess a reasonable fixed width.
            let formattedRange;
            if (widgetSpec.format !== null) {
                const f = d3.format(widgetSpec.format);
                const fixedWidth = Math.max(
                    f(widgetSpec.high - step).length,
                    f(widgetSpec.high).length,
                );
                formattedRange = `(${f(value).padStart(fixedWidth)}) [${f(widgetSpec.low)} - ${f(widgetSpec.high)}]`;
            } else {
                const fixedWidth = String(widgetSpec.high - step).length;
                formattedRange = `(${String(value).padStart(fixedWidth)}) [${widgetSpec.low} - ${widgetSpec.high}]`;
            }
            return <div style={widgetBoxStyle}>
                <span style={liftedTextStyle}>{widgetSpec.name}:</span>
                <input
                    type="range"
                    min={low}
                    max={high}
                    value={sliderPos}
                    step={step}
                    onChange={(event) => {
                        let newValue = event.target.value;
                        if (widgetSpec.log) {
                            newValue = Math.exp(
                                Math.log(widgetSpec.low) + 1e-3 * newValue *
                                    (Math.log(widgetSpec.high) - Math.log(widgetSpec.low))
                            );
                            //newValue = Math.exp(newValue);
                        }
                        this.updateWidgetValue(widgetSpec, newValue);
                    }}
                />
                {widgetSpec.name === this.state.editBoxName ?
                    <input
                        autoFocus
                        type="text"
                        style={{marginLeft: "5px", width: "100px", transform: "translateY(-25%)"}}
                        value={this.state.editBoxContents}
                        onChange={(event) => {
                            let newValue = event.target.value;
                            this.setState({editBoxContents: newValue});
                        }}
                        onKeyDown={(event) => {
                            if (event.key === "Enter") {
                                const newValue = Number(this.state.editBoxContents);
                                if (!isNaN(newValue))
                                    this.updateWidgetValue(widgetSpec, newValue);
                                this.setState({editBoxName: null});
                            } else if (event.key === "Escape") {
                                this.setState({editBoxName: null});
                            }
                        }}
                    />
                :
                    <span
                        style={{...liftedTextStyle, whiteSpace: "pre-wrap", cursor: "pointer"}}
                        onDoubleClick={(event) => this.onDoubleClick(event, widgetSpec)}
                    >
                        {formattedRange}
                    </span>
                }
            </div>;
        } else if (widgetSpec.kind === "checkbox") {
            const value = this.getWidgetValue(widgetSpec.name, widgetSpec.default_value);
            return <div style={widgetBoxStyle}>
                <span style={widgetTextStyle}>{widgetSpec.name}:</span>
                <input
                    type="checkbox"
                    checked={value}
                    onChange={(event) => { this.updateWidgetValue(widgetSpec, event.target.checked); }}
                    style={{transform: "scale(1.5)", marginRight: "10px"}}
                />
            </div>;
        } else if (widgetSpec.kind === "selector") {
            const value = this.getWidgetValue(widgetSpec.name, widgetSpec.default_value);
            return <div style={widgetBoxStyle}>
                <span style={widgetTextStyle}>{widgetSpec.name}:</span>
                {widgetSpec.selections.map((optionName, i) =>
                    <span
                        style={{
                            ...widgetTextStyle,
                            border: value === i ? "2px solid green" : "2px solid black",
                            backgroundColor: value === i ? "#464" : "#444",
                            borderRadius: "10px",
                            padding: "5px",
                            marginLeft: "4px",
                            marginRight: "4px",
                            userSelect: "none",
                        }}
                        key={i}
                        onClick={() => {
                            this.updateWidgetValue(widgetSpec, i);
                        }}
                    >
                        {optionName}
                    </span>
                )}
            </div>;
        } else if (widgetSpec.kind === "optimizer") {
            return <OptimizerBox
                parent={this.props.parent}
                widgetSpec={widgetSpec}
                ref={this.optimizerRef}
            />;
        }
        return <div style={{backgroundColor: "#833"}}>Bad widget spec: <code>{JSON.stringify(widgetSpec)}</code></div>;
    }

    // FIXME: This is currently impure.
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
                {this.props.plotStructs.map((plotSpec, i) =>
                    <div key={i} style={{margin: "2px"}}>
                        <Plot data={plotSpec.data} layout={plotSpec.layout}/>
                    </div>
                )}
            </div>
            <div style={{...boxStyle, marginTop: "10px"}}>
                {this.props.widgetSpecs.map((widgetSpec, i) =>
                    <div key={i} style={{margin: "5px"}}>
                        {this.renderWidget(widgetSpec)}
                    </div>
                )}
            </div>
        </div>;
    }
}

// TODO: Actually implement this.
class ButcherIntegrator {
    constructor(butcherTableau, state, statePrime, callback) {
        this.butcherTableau = butcherTableau;
    }
}

//const libwebode = require("./libwebode.js");

function produceAggregatedEnvelope(xPitch, traces) {
    const fingers = [];
    for (let i = 0; i < traces.length; i++)
        fingers.push(0);

    let maxX = Math.max(...traces.flatMap(tr => tr.x));
    const totalSteps = 1 + Math.ceil(maxX / xPitch);
    //console.log("Total steps:", totalSteps, maxX, xPitch);
    const n = () => new Float64Array(totalSteps);
    const x = n(), yMean = n(), yMin = n(), yMax = n(), yFirstQuintile = n(), yForthQuintile = n();
    const allValues = new Float64Array(fingers.length);
    for (let timeIndex = 0; timeIndex < totalSteps; timeIndex++) {
        const now = xPitch * timeIndex;
        x[timeIndex] = now;
        let mean = 0.0, min = Infinity, max = -Infinity;
        for (let i = 0; i < fingers.length; i++) {
            const traceX = traces[i].x;
            // Catch this finger up up with now.
            while (fingers[i] < traceX.length && traceX[fingers[i]] < now)
                fingers[i]++;
            if (fingers[i] > 0)
                fingers[i]--;
            // Aggregate its data.
            // TODO: lerp here instead.
            const fingerTimeL = traceX[fingers[i]];
            const fingerTimeR = traceX[fingers[i] + 1];
            const fingerValL  = traces[i].y[fingers[i]];
            const fingerValR  = traces[i].y[fingers[i] + 1];
            //const val = traces[i].y[fingers[i]];
            const lerpCoef = (now - fingerTimeL) / (fingerTimeR - fingerTimeL);
            const val = (1 - lerpCoef) * fingerValL + lerpCoef * fingerValR;
            mean += val;
            min = Math.min(min, val);
            max = Math.max(max, val);
            allValues[i] = val;
        }
        yMean[timeIndex] = mean / fingers.length;
        yMin[timeIndex] = min;
        yMax[timeIndex] = max;
        allValues.sort();
        const qi1 = Math.floor(allValues.length / 5);
        let qi4 = Math.ceil(4 * allValues.length / 5);
        if (qi4 >= allValues.length)
            qi4 = allValues.length - 1;
        yFirstQuintile[timeIndex] = allValues[qi1];
        yForthQuintile[timeIndex] = allValues[qi4];
    }
    return {x, yMean, yMin, yMax, yFirstQuintile, yForthQuintile};
}

// From https://stackoverflow.com/a/44727682
const plotlyDefaultColors = [
    "#1f77b4", // muted blue
    "#ff7f0e", // safety orange
    "#2ca02c", // cooked asparagus green
    "#d62728", // brick red
    "#9467bd", // muted purple
    "#8c564b", // chestnut brown
    "#e377c2", // raspberry yogurt pink
    "#7f7f7f", // middle gray
    "#bcbd22", // curry yellow-green
    "#17becf", // blue-teal
];
function getPlotlyColor(i) {
    i %= plotlyDefaultColors.length;
    return plotlyDefaultColors[i];
}

class App extends React.Component {
    constructor() {
        super();
        this.editorRef = React.createRef();
        this.parametersRef = React.createRef();
        this.state = {val: -1, plotStructs: [], widgetSpecs: [], compilationParameters: {}};
        this.simData = null;
        this.simCtx = null;
        this.simRNGStartingState = null;
        this.currentlyOptimizingCounter = 0;
    }

    componentDidMount() {
        //this.updateVal();
        this.onReload();
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
        this.setState({widgetSpecs: this.simData.widgets});
        await this.rerunSimulation(true);
    }

    async rerunSimulation(reseed, updatePlots, objectiveAggregation) {
        updatePlots = updatePlots === undefined ? true : updatePlots;
        objectiveAggregation = objectiveAggregation === undefined ? false : objectiveAggregation;
        /*
        if (this.simData.settings.mcsamples <= 1) {
            const result = this.rerunSimulationCore(reseed, updatePlots, computeObjective);
        }
        */
        //console.log("Starting rerun:", this.simData.settings.mcsamples);
        const results = [];
        for (let sampleIndex = 0; sampleIndex < this.simData.settings.mcsamples; sampleIndex++) {
            const firstBehavior = reseed ? true : "mc-first";
            results.push(this.rerunSimulationCore(
                sampleIndex === 0 ? firstBehavior : "mc",
                updatePlots,
                objectiveAggregation !== false,
            ));
        }

        if (updatePlots) {
            const plotStructs = [];
            for (const plotName of Object.keys(this.simData.plots)) {
                const plotSpec = this.simData.plots[plotName];
                const data = [];
                let nextColor = 0;
                plotSpec.dataTemplates.map((dataTemplate, i) => {
                    const showMainLegend = plotSpec.dataTemplates.length > 1;

                    // There are several cases for how to aggregate this dataTemplate.
                    if (this.simData.settings.mcenvelope && dataTemplate.type === "scatter") {
                        const name = dataTemplate.hasOwnProperty("name") ? dataTemplate.name : "trace" + i;
                        let color;
                        if (dataTemplate.hasOwnProperty("line") && dataTemplate.line.hasOwnProperty("color")) {
                            color = dataTemplate.line.color;
                        } else {
                            color = getPlotlyColor(nextColor++);
                        }
                        console.log("Color:", color);
                        let fillcolor = d3.rgb(color);
                        console.log("Fill Color:", fillcolor);
                        fillcolor = `rgba(${fillcolor.r}, ${fillcolor.g}, ${fillcolor.b}, 0.3)`;
                        console.log("Fill Color:", fillcolor);
                        const traces = results.map(r => r.plotData[plotName][i]);
                        const envelope = produceAggregatedEnvelope(this.simData.settings.plotperiod, traces);

                        data.push({
                            x: envelope.x, y: envelope.yMax, ...dataTemplate,
                            showlegend: false,
                            name: name + " max",
                            line: {color},
                        });
                        data.push({
                            showlegend: showMainLegend,
                            x: envelope.x, y: envelope.yMean, ...dataTemplate,
                            fill: "tonexty",  fillcolor,
                            name: name + " mean",
                            line: {color},
                        });
                        data.push({
                            showlegend: false,
                            x: envelope.x, y: envelope.yMin, ...dataTemplate,
                            fill: "tonexty",  fillcolor,
                            name: name + " min",
                            line: {color},
                        });
                        //*
                        data.push({
                            x: envelope.x, y: envelope.yFirstQuintile, ...dataTemplate,
                            showlegend: false,
                            name: name + " 20%",
                            line: {color: "rgba(0, 0, 0, 0.1)"},
                        });
                        data.push({
                            showlegend: false,
                            x: envelope.x, y: envelope.yForthQuintile, ...dataTemplate,
                            fill: "tonexty", fillcolor,
                            name: name + " 80%",
                            line: {color: "rgba(0, 0, 0, 0.1)"},
                        });
                        //*/
                    } else {
                        let maxTraceCount = Math.min(results.length, this.simData.settings.mctraces);
                        // For heatmaps we can only render one.
                        // TODO: Possibly just average the heatmaps here.
                        if (dataTemplate.type === "heatmap")
                            maxTraceCount = 1;
                        for (let traceIndex = 0; traceIndex < maxTraceCount; traceIndex++) {
                            data.push({
                                showlegend: showMainLegend,
                                ...results[traceIndex].plotData[plotName][i],
                                ...dataTemplate,
                            });
                        }
                    }
                });
                /*
                plotSpec.dataTemplates.map((dataTemplate, i) => ({
                    ...results[0].plotData[plotName][i],
                    ...dataTemplate,
                }))
                */
                // The plotSpec has two fields, dataTemplates, and layout.
                plotStructs.push({data, layout: plotSpec.layout});
            }
            this.setState({plotStructs});
        }

        const objectives = results.map(r => r.objective);
        if (objectiveAggregation === "mean") {
            return objectives.reduce((x, y) => x + y) / objectives.length;
        } else if (objectiveAggregation === "min") {
            return Math.min(...objectives);
        } else if (objectiveAggregation === "max") {
            return Math.max(...objectives);
        }
    }

    rerunSimulationCore(reseed, computePlotValues, computeObjective) {
        if (this.simData === null)
            return;
        if (!["euler", "rk4", "cash-karp"].includes(this.simData.settings.integrator)) {
            this.setDialog("Unsupported integrator " + this.simData.settings.integrator + ", must select one of: euler, rk4, cash-karp");
            return;
        }
        //console.log("Starting inner run", reseed, this.simRNGStartingState);
        if (reseed === true || (reseed === "mc-first" && this.simRNGStartingState === null)) {
            //console.log("Reseeding");
            if (this.simData.settings.randomseed !== null) {
                const seed = this.simData.settings.randomseed;
                this.simData.setRNGState([123 + seed, 314, 159, 265]);
            } else {
                this.simData.reseedRNG();
            }
            this.simRNGStartingState = this.simData.getRNGState();
        } else if (this.simRNGStartingState !== null && reseed !== "mc") {
            //console.log("Restoring seed")
            this.simData.setRNGState(this.simRNGStartingState);
        }
        this.simData.initialize(this.simCtx);
        let t = 0.0;
        let stepSize = Math.max(1e-6, this.simData.settings.stepsize);
        const {state, statePrime} = this.simCtx;
        // This first getDerivative is to fill in zeroth order variables for plotting.
        this.simData.getDerivative(this.simCtx, t, stepSize, state, statePrime);
        this.simData.extractPlotDatum(this.simCtx, t, stepSize);
        let lastPlotGrab = t;

        // RK4 data structures.
        let startingPointBuffer = null;
        const derivativeBuffers = [];
        if (this.simData.settings.integrator === "rk4") {
            startingPointBuffer = new Float64Array(state.length);
            for (let i = 0; i < 3; i++)
                derivativeBuffers.push(new Float64Array(statePrime.length));
        }

        // Cash-Karp integrator.
        let cashKarpIntegrator = null;
        if (this.simData.settings.integrator === "cash-karp") {
            const cashKarpStep = (dydt, y, t) => {
                this.simData.getDerivative(this.simCtx, t, stepSize, y, dydt);
            };
            const cashKarpOptions = {
                tol: this.simData.settings.tolerance,
                dtMinMag: this.simData.settings.minstep,
                dtMaxMag: this.simData.settings.maxstep,
            };
            //console.log("Cash-Karp options:", cashKarpOptions);
            cashKarpIntegrator = ode45(
                state, cashKarpStep, 0.0, stepSize, cashKarpOptions,
            );
        }

        // The Cash-Karp integrator tries to avoid overstepping, so we allow a slight fudge factor to avoid
        // getting stuck in an infinite loop where the integrator thinks it's done, but we don't.
        const fudgeFactor = 1 - 1e-5;
        let keepGoing = true;
        while (t < this.simData.settings.simtime * fudgeFactor && keepGoing) {
            if (this.simData.settings.integrator === "euler") {
                // Do a first-order Euler step.
                this.simData.getDerivative(this.simCtx, t, stepSize, state, statePrime);
                for (let i = 0; i < state.length; i++)
                    state[i] += stepSize * statePrime[i];
                t += stepSize;
            } else if (this.simData.settings.integrator === "rk4") {
                // Copy the starting state.
                startingPointBuffer.set(state);

                // First derivative evaluation.
                this.simData.getDerivative(this.simCtx, t, stepSize / 2, state, derivativeBuffers[0]);

                // Second derivative evaluation.
                for (let i = 0; i < state.length; i++)
                    state[i] += (stepSize / 2) * statePrime[i];
                this.simData.getDerivative(this.simCtx, t + stepSize / 2, stepSize / 2, state, derivativeBuffers[1]);

                // Third derivative evaluation.
                state.set(startingPointBuffer);
                for (let i = 0; i < state.length; i++)
                    state[i] += (stepSize / 2) * statePrime[i];
                this.simData.getDerivative(this.simCtx, t + stepSize / 2, stepSize / 2, state, derivativeBuffers[2]);

                // Fourth derivative evaluation.
                state.set(startingPointBuffer);
                for (let i = 0; i < state.length; i++)
                    state[i] += stepSize * statePrime[i];
                this.simData.getDerivative(this.simCtx, t, stepSize / 2, state, statePrime);

                // Final mixture.
                state.set(startingPointBuffer);
                for (let i = 0; i < state.length; i++)
                    state[i] += (stepSize / 6) * (derivativeBuffers[0][i] + 2 * derivativeBuffers[1][i] + 2 * derivativeBuffers[2][i] + statePrime[i]);

                t += stepSize;
            } else if (this.simData.settings.integrator === "cash-karp") {
                // Ugh, for some reason their integrator interface has no method that just takes a single step.
                // (For example .steps(n) wants n to be the limit on the *total* number of steps so far.)
                // Here I hack in a time limit that we definitely won't hit in one step.
                const notDone = cashKarpIntegrator.step(this.simData.settings.simtime);
                if (!notDone)
                    keepGoing = false;
                t = cashKarpIntegrator.t;
                stepSize = cashKarpIntegrator.stepSizeActuallyTaken;
            }

            if (computePlotValues && (
                // Update if it's been long enough since our last plot period...
                t >= lastPlotGrab + this.simData.settings.plotperiod
                // ... or we're on the last sample.
                || t >= this.simData.settings.simTime
            )) {
                this.simData.extractPlotDatum(this.simCtx, t, stepSize);
                lastPlotGrab = t;
            }
        }

        return {
            plotData: this.simCtx.plotData,
            objective: computeObjective ? this.simData.getObjective(this.simCtx, t, stepSize, state) : null,
        };
    }

    async activateOptimizer(optimizerWidgetSpec, optimizerBox) {
        // XXX: FIXME: The proper thing to do here is use a counter,
        // so that there are no race conditions if you start, stop, start again.
        // But even more proper would be using a web worker, and doing this in another thread.
        if (this.currentlyOptimizing) {
            this.currentlyOptimizing = false;
            return;
        }
        this.currentlyOptimizing = true;
        //console.log("Optimizer widgetSpec:", optimizerWidgetSpec);
        const nameToWidgetSpec = {};
        for (const widgetSpec of this.state.widgetSpecs)
            nameToWidgetSpec[widgetSpec.name] = widgetSpec;
        const optimizationSpace = {};
        const isLogParameter = {};
        for (const tunableName of optimizerWidgetSpec.tunable) {
            if (!nameToWidgetSpec.hasOwnProperty(tunableName)) {
                this.setDialog("BUG: Missing tunable parameter for optimizer: " + tunableName);
                return;
            }
            const tunableWidgetSpec = nameToWidgetSpec[tunableName];
            if (tunableWidgetSpec.kind !== "slider") {
                this.setDialog("Currently optimization may only be done over sliders");
                return;
            }
            // Optimize in log-space for log sliders.
            let low = tunableWidgetSpec.low, high = tunableWidgetSpec.high;
            if (tunableWidgetSpec.log) {
                low = Math.log(low);
                high = Math.log(high);
            }
            optimizationSpace[tunableName] = [low, high];
            isLogParameter[tunableName] = tunableWidgetSpec.log;
        }
        const makeRandom = () => {
            const candidate = {};
            for (const key of Object.keys(optimizationSpace)) {
                const [low, high] = optimizationSpace[key]
                candidate[key] = low + Math.random() * (high - low);
            }
            return candidate;
        };
        const applySettings = (settings, updateGUI) => {
            //console.log("Applying:", settings);
            for (const tunableName of Object.keys(settings)) {
                let value = settings[tunableName];
                if (isLogParameter[tunableName])
                    value = Math.exp(value);
                this.setSimParameter(tunableName, value, false);
                if (updateGUI && this.parametersRef.current)
                    this.parametersRef.current._updateWidgetValueNoCheck(tunableName, value);
            }
        };
        let objectiveCalls = [0];
        let lastRerender = [performance.now()];
        let bestSeenObjective = [Infinity];
        let bestSeenSettings = [{}];
        let objectivePlot = {x: [], y: []};
        const computeObjective = async (settings) => {
            if (!this.currentlyOptimizing)
                return "early-stop";
            applySettings(settings, false);
            const now = performance.now();
            // Rate limit the rerendering.
            const updatePlots = now > (lastRerender[0] + 1000 * optimizerWidgetSpec.options.plotperiod);
            objectiveCalls[0]++;
            const obj = await this.rerunSimulation(false, updatePlots, optimizerWidgetSpec.options.objectiveaggregation);
            if (obj < bestSeenObjective[0]) {
                bestSeenObjective[0] = obj;
                bestSeenSettings[0] = {...settings};
                objectivePlot.x.push(objectiveCalls[0]);
                objectivePlot.y.push(bestSeenObjective[0]);
            }
            if (updatePlots) {
                // Update all widgets.
                optimizerBox.updateOptimizerPlot(objectivePlot, objectiveCalls[0]);
                // Do a slight sleep to allow rerender. This is super duper hacky!
                await sleep(2);
                // Set the rerender time *after* rerendering.
                lastRerender[0] = performance.now();
                applySettings(settings, true);
            }
            return obj;
        };
        await this.differentialEvolution(
            optimizerWidgetSpec.options,
            optimizationSpace,
            makeRandom,
            computeObjective,
        );
        optimizerBox.updateOptimizerPlot(objectivePlot, objectiveCalls[0]);
        applySettings(bestSeenSettings[0], true);
        await this.rerunSimulation();
        this.currentlyOptimizing = false;
    }

    async differentialEvolution(options, optimizationSpace, makeRandom, computeObjective) {
        if (options.populationsize < 4) {
            this.setDialog("Differential evolution requires a populationsize of at least 4");
            return;
        }
        // Make an initial pool of agents.
        const agents = [];
        for (let i = 0; i < options.populationsize; i++) {
            agents.push({
                "settings": makeRandom(),
                "objective": null,
            });
        }
        // Fill in the initial objective values.
        for (const agent of agents) {
            agent.objective = await computeObjective(agent.settings);
        }
        // Do optimization.
        const allProblemDimensions = Object.keys(agents[0].settings);
        console.log(allProblemDimensions);
        let bestObjectiveEverSeen = Infinity;
        let patience = options.patience;
        optimization_loop:
        for (let optimizationStep = 0; optimizationStep < options.maxsteps; optimizationStep++) {
            console.log("Optimization step:", optimizationStep, bestObjectiveEverSeen);
            patience--;
            for (let xIndex = 0; xIndex < agents.length; xIndex++) {
                // Rejection sample three more distinct indices.
                // Golly gee, ECMAScript has an impoverished standard library. :(
                const indices = [xIndex, 0, 0, 0];
                for (let i = 1; i < 4; i++) {
                    while (true) {
                        // Pick a random agent.
                        indices[i] = Math.floor(Math.random() * agents.length);
                        // Check if the agent is distinct from all previous.
                        let isDistinct = true;
                        for (let j = 0; j < i; j++)
                            if (indices[i] === indices[j])
                                isDistinct = false;
                        if (isDistinct)
                            break;
                    }
                }
                const xAgent = agents[xIndex].settings;
                const aAgent = agents[indices[1]].settings;
                const bAgent = agents[indices[2]].settings;
                const cAgent = agents[indices[3]].settings;
                // Crossover a new candidate.
                const R = Math.floor(Math.random() * allProblemDimensions.length);
                const newSettings = {};
                for (let i = 0; i < allProblemDimensions.length; i++) {
                    const dim = allProblemDimensions[i];
                    const r = Math.random();
                    let val = xAgent[dim];
                    if (r < options.crossoverprob || i === R) {
                        val = aAgent[dim] + options.diffweight * (bAgent[dim] - cAgent[dim]);
                    }
                    // Constrain the value.
                    // In order to prevent accumulation against a constraint boundary
                    // pick a random value between the original val and the constrained val.
                    // However, in order to converge a bit more quickly towards a constraint,
                    // we square the lerp coefficient.
                    const [boundLow, boundHigh] = optimizationSpace[dim];
                    const constrainedVal = Math.max(boundLow, Math.min(boundHigh, val));
                    const lerp = Math.pow(Math.random(), 2);
                    newSettings[dim] = lerp * xAgent[dim] + (1 - lerp) * constrainedVal;
                }
                // Evaluate the new candidate, and keep it if better.
                const newObjective = await computeObjective(newSettings);
                if (newObjective === "early-stop")
                    break optimization_loop;
                if (newObjective <= agents[xIndex].objective)
                    agents[xIndex] = {settings: newSettings, objective: newObjective};
                const patienceThreshold = bestObjectiveEverSeen > 0 ?
                    bestObjectiveEverSeen * (1 - options.patienceFactor) :
                    bestObjectiveEverSeen * (1 + options.patienceFactor);
                if (newObjective < patienceThreshold * options.patiencefactor) {
                    bestObjectiveEverSeen = newObjective;
                    patience = options.patience;
                }
            }
            if (patience === 0) {
                break optimization_loop;
            }
        }
        // Do a final application of our best settings.
        let bestAgent = agents[0];
        for (const agent of agents)
            if (agent.objective < bestAgent.objective)
                bestAgent = agent;
        return bestAgent;
    }

    setDialog(message, timeout) {
        if (this.editorRef.current)
            this.editorRef.current.setDialog(message, timeout);
        else
            alert(message);
    }

    setSimParameter(name, value, isCompilationParameter) {
        if (this.simData === null)
            return;
        //console.log(name, value, isCompilationParameter);
        if (isCompilationParameter) {
            // Determine if this is a change.
            const oldValue = this.state.compilationParameters[name];
            this.setState(
                {compilationParameters: {...this.state.compilationParameters, [name]: value}},
                () => {
                    if (oldValue !== value && this.editorRef.current) {
                        this.onCompile(this.editorRef.current.state.code);
                    }
                },
            );
            return;
        }
        if (!this.simData.parameterTable.hasOwnProperty(name)) {
            //this.setDialog("BUG BUG BUG: Attempt to set invalid parameter: " + name);
            return;
        }
        this.simCtx.parameters[this.simData.parameterTable[name]] = value;
    }

    onSaveCode = async (code) => {
        const response = await fetch(SERVER_HOST + "/save", {
            method: "POST",
            headers: {"Content-Type": "application/json"},
            body: JSON.stringify({code}),
        });
        const result = await response.json();
        if (!result.error)
            this.setDialog("Saved!", 750);
    }

    onReload = async () => {
        const response = await fetch(SERVER_HOST + "/reload", {
            method: "POST",
            headers: {"Content-Type": "application/json"},
            body: JSON.stringify({}),
        });
        const result = await response.json();
        if (!result.error && this.editorRef.current) {
            this.editorRef.current.setState({code: result.code});
        }
    }

    onCompile = async (code) => {
        const response = await fetch(SERVER_HOST + "/compile", {
            method: "POST",
            headers: {"Content-Type": "application/json"},
            body: JSON.stringify({
                code,
                compilation_parameters: this.state.compilationParameters,
            }),
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
                if (result.print_output !== "")
                    errorMessage = "Before the error, we printed:\n" + result.print_output + "\n\n" + errorMessage;
                this.setDialog(errorMessage);
            }
        } else {
            if (result.print_output !== "") {
                this.setDialog(result.print_output);
            } else {
                // We were successful, so clear the messages.
                this.setDialog(null);
            }
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
                    plotStructs={this.state.plotStructs}
                    ref={this.parametersRef}
                />
            </div>
        </div>;
    }
}

export default App;
