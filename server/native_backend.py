
import time, math, random
import numpy as np
import dsl
import native_backend

class NativeBackend:
    def __init__(self, ctx):
        self.ctx = ctx
        self.mcsamples = ctx.settings["mcsamples"]
        self.realized_process_length = 1 + math.ceil(ctx.settings["simtime"] / ctx.settings["processscale"])
        self.state = np.zeros([self.mcsamples, ctx.degrees_of_freedom])
        self.stateBackup = np.zeros([self.mcsamples, ctx.degrees_of_freedom])
        self.statePrime = np.zeros([
            self.mcsamples,
            4, # Four derivative evaluations in RK4
            ctx.degrees_of_freedom,
        ])
        self.scratch = np.zeros([self.mcsamples, ctx.scratch_buffer_size])
        self.parameters = np.zeros([self.mcsamples, len(ctx.adjustable_parameters)])
        self.realized_processes = np.zeros([
            self.mcsamples,
            len(ctx.realized_processes),
            self.realized_process_length,
        ])

        # Count up all the plot traces we need to do.
        self.plot_trace_count = 0
        # At least it's not the case that 2/3rds of your stories are a segue to 
        for plot_name, plot_spec in ctx.plots.items():
            for template in plot_spec["dataTemplates"]:
                self.plot_trace_count += 1 # Add one for the x series.
                if "rowExprs" in template:
                    # Add for all the various z serieses.
                    self.plot_trace_count += len(template["rowExprs"])
                else:
                    self.plot_trace_count += 1 # Add one for the y series.

        # Right here I implicitly assume that the child backend isn't adaptive.
        self.total_steps = math.ceil(ctx.settings["simtime"] / ctx.settings["stepsize"])
        self.plot_trace_length = 1 + min(self.total_steps, ctx.settings["maxplotpoints"])
        self.plotting_traces = np.zeros([
            self.mcsamples,
            self.plot_trace_count,
            self.plot_trace_length,
        ])
        self.objectives = np.zeros([self.mcsamples])

        self.sim_rng_starting_state = None

    def fulfill_request(self, request):
        self.parameters[:, :] = request["parameters"]

        reseed = request["reseed"]
        if reseed is True or (reseed == "mc-first" and self.sim_rng_starting_state is None):
            if self.ctx.settings["randomseed"] is not None:
                quadruple = [123 + self.ctx.settings["randomseed"], 314, 159, 265]
            else:
                quadruple = [max(1, random.getrandbits(32)) for i in range(4)]
            self.set_rng_state(quadruple)
            self.sim_rng_starting_state = quadruple
        elif self.sim_rng_starting_state is not None and reseed != "mc":
            self.set_rng_state(self.sim_rng_starting_state)

        plot_length = self.do_computation(request)

        assert plot_length <= self.plot_trace_length, \
            "Bad plot length: %i (buffer size: %i)\nWe probably just overflowed a buffer. :(" % (
                plot_length, self.plot_trace_length
            )

        aggregated_objective = None
        if request["computeObjective"]:
            aggregated_objective = float({
                "mean": self.objectives.mean,
                "min": self.objectives.min,
                "max": self.objectives.max,
            }[request["objectiveAggregation"]]())

        # If the other side doesn't want plot values then just aggregate everything down.
        if not request["computePlotValues"]:
            # Aggregate the objective ourself.
            return {"simResults": [{"objective": aggregated_objective}]}

        def floatify(l):
            return [float(i) for i in l]

        out = {
            "simResults": [],
            "preAggregated": {},
        }

        # If the other side wants enveloped data, then give them that.
        if self.ctx.settings["mcenvelope"]:
            # TODO: FIXME: Horrible repeated code here, that's the same as below. :(
            plot_trace_index = 0
            plotData = {}
            out["simResults"].append({
                "plotData": plotData,
                "objective": aggregated_objective,
            })
            for plot_name, plot_spec in self.ctx.plots.items():
                plotData[plot_name] = []
                for i, template in enumerate(plot_spec["dataTemplates"]):
                    x = self.plotting_traces[0, plot_trace_index, :plot_length]
                    plot_trace_index += 1
                    if "rowExprs" in template:
                        entry = {}
                        plotData[plot_name].append(entry)
                        z_count = len(template["rowExprs"])
                        z = self.plotting_traces[0, plot_trace_index:plot_trace_index + z_count, :plot_length]
                        plot_trace_index += z_count
                        entry["x"] = floatify(x)
                        entry["z"] = [floatify(row) for row in z]
                    else:
                        # Aggregate!
                        row = np.sort(self.plotting_traces[:, plot_trace_index, :plot_length].T)
                        qi1 = math.floor(row.shape[-1] / 5)
                        qi4 = min(row.shape[-1] - 1 , math.ceil(4 * row.shape[-1] / 5))
                        out["preAggregated"]["%s,%i" % (plot_name, i)] = {
                            "x":               floatify(x),
                            "yMax":            floatify(row.max(axis=-1)),
                            "yFourthQuintile": floatify(row[:, qi4]),
                            "yMean":           floatify(row.mean(axis=-1)),
                            "yFirstQuintile":  floatify(row[:, qi1]),
                            "yMin":            floatify(row.min(axis=-1)),
                        }
                        plot_trace_index += 1
            return out

        # This is the really slow code path, where we just have to send everything over.
        # It's slow mostly because we might end up serializing/deserializing megabytes of JSON. :(
        for samp in range(min(self.mcsamples, self.ctx.settings["mctraces"])):
            plot_trace_index = 0
            plotData = {}
            out["simResults"].append({
                "plotData": plotData,
                "objective": float(self.objectives[samp]),
            })
            for plot_name, plot_spec in self.ctx.plots.items():
                plotData[plot_name] = []
                for template in plot_spec["dataTemplates"]:
                    entry = {}
                    plotData[plot_name].append(entry)
                    x = self.plotting_traces[samp, plot_trace_index, :plot_length]
                    plot_trace_index += 1
                    if "rowExprs" in template:
                        z_count = len(template["rowExprs"])
                        z = self.plotting_traces[samp, plot_trace_index:plot_trace_index + z_count, :plot_length]
                        plot_trace_index += z_count
                        entry["x"] = floatify(x)
                        entry["z"] = [floatify(row) for row in z]
                    else:
                        y = self.plotting_traces[samp, plot_trace_index, :plot_length]
                        plot_trace_index += 1
                        entry["x"] = floatify(x)
                        entry["y"] = floatify(y)

        return out

        #self.invoke_numba
        #for i in range(self.settings["mcsamples"]):
        #    pass
        #return {
        #    "simResults": [],
        #    "preAggregated": {},
        #}
        return {
            #"simResults": [],
            "simResults": [{
                "plotData": {
                    "plot": [
                        {
                            "x": [1, 2, 3],
                            "y": [random.random() * 10 for _ in range(3)],
                        },
                    ],
                },
            } for _ in range(self.mcsamples)],
            "preAggregated": {
                "q,0": {
                    "x": [1, 2, 3],
                    "yMax": [7, 8, 9],
                    "yFourthQuintile": [4.5, 5.5, 6.5],
                    "yMean": [4, 5, 6],
                    "yFirstQuintile": [3.5, 4.5, 5.5],
                    "yMin": [1, 2, 3],
                },
            },
        }
