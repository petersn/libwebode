(() => {
    let xoshiro128ss_state = [[1, 2, 3, 4]];
    const RANDOM_EPSILON = 0.5 / 4294967296;
    const reseedRNG = () => {
        const newState = [];
        for (let i = 0; i < 4; i++)
            newState.push(Math.floor(Math.random() * 4294967296) | 0);
        xoshiro128ss_state[0] = newState;
    }
    reseedRNG();
    function myRandom() {
        let [a, b, c, d] = xoshiro128ss_state[0];
        var t = b << 9, r = a * 5; r = (r << 7 | r >>> 25) * 9;
        c ^= a; d ^= b;
        b ^= c; a ^= d; c ^= t;
        d = d << 11 | d >>> 21;
        xoshiro128ss_state[0] = [a, b, c, d];
        return (r >>> 0) / 4294967296;
    }
    function uniformRandom(low, high) {
        return low + (high - low) * myRandom();
    }
    const boxMullerCache = [null];
    function gaussianRandom() {
        if (boxMullerCache[0] === null) {
            // Use Box-Muller transform, and stash the other sample for later.
            const u = RANDOM_EPSILON + myRandom(), v = myRandom();
            const scale = Math.sqrt(-2 * Math.log(u));
            const arg = 2 * Math.PI * v;
            boxMullerCache[0] = scale * Math.cos(arg);
            return scale * Math.sin(arg);
        }
        const val = boxMullerCache[0];
        boxMullerCache[0] = null;
        return val;
    }
    function gammaRandom(alpha, beta) {
        // Translated from the Python source code for random.py.
        if (alpha > 1.0) {
            // Uses R.C.H. Cheng, "The generation of Gamma
            // variables with non-integral shape parameters",
            // Applied Statistics, (1977), 26, No. 1, p71-74
            const ainv = Math.sqrt(2 * alpha - 1);
            const bbb = alpha - Math.log(4);
            const ccc = alpha + ainv;
            const SG_MAGICCONST = 1 + Math.log(4.5);
            while (true) {
                const u1 = myRandom()
                if (u1 < 1e-7 || u1 > 0.9999999)
                    continue;
                const u2 = 1 - myRandom();
                const v = Math.log(u1 / (1 - u1)) / ainv;
                const x = alpha * Math.exp(v);
                const z = u1 * u1 * u2;
                const r = bbb + ccc * v - x;
                if (r + SG_MAGICCONST - 4.5 * z >= 0.0 || r >= Math.log(z))
                    return x * beta;
            }
        } else if (alpha === 1.0) {
            return -Math.log(RANDOM_EPSILON + myRandom()) * beta;
        } else {
            // Uses ALGORITHM GS of Statistical Computing - Kennedy & Gentle
            let x;
            while (true) {
                const u = myRandom();
                const b = (Math.E + alpha) / Math.E;
                const p = b * u;
                if (p <= 1.0) {
                    x = Math.pow(p, 1 / alpha);
                } else {
                    x = -Math.log((b - p) / alpha);
                }
                const u1 = myRandom();
                if (p > 1.0) {
                    if (u1 <= Math.pow(x, alpha - 1))
                        break;
                } else if (u1 <= Math.exp(x)) {
                    break;
                }
            }
            return x * beta;
        }
    }
    function betaRandom(alpha, beta) {
        // Translated from the Python source code for random.py.
        const y = gammaRandom(alpha, 1.0);
        if (y === 0.0)
            return 0.0;
        return y / (y + gammaRandom(beta, 1.0));
    }
    function frechetRandom(alpha) {
        // From manually inverting the CDF for the Frechet distribution.
        // I think I did this right?
        return Math.pow(-Math.log(RANDOM_EPSILON + myRandom()), -1 / alpha);
    }
    function exponentialRandom() {
        return -Math.log(RANDOM_EPSILON + myRandom());
    }
    function wienerDerivativeUnstable(dt) {
        return gaussianRandom() / Math.sqrt(dt);
    }
    const realizedProcessLength = 1 + Math.ceil(%(simtime)s / %(processscale)s);
    function simpleLerp(realization) {
        return (t) => {
            const moment = Math.max(0, Math.min(realizedProcessLength - 2, t / %(processscale)s));
            const i = Math.floor(moment);
            // Numerical stability isn't a huge concern here. lerpCoef always being 0 is safe.
            const lerpCoef = moment - i;
            return realization[i] * (1 - lerpCoef) + realization[i + 1] * lerpCoef;
        };
    }
    function makePoissonProcess(realization, rate) {
        let v = 0.0;
        if (rate <= 0.0)
            throw "PoissonProcess rate ended up negative!";
        let nextStepTime = exponentialRandom() / rate;
        for (let i = 0; i < realizedProcessLength; i++) {
            realization[i] = v;
            nextStepTime -= %(processscale)s;
            while (nextStepTime <= 0.0) {
                nextStepTime += exponentialRandom() / rate
                v += 1.0;
            }
        }
        return simpleLerp(realization);
    }
    function makeWienerProcess(realization) {
        const scaling = Math.sqrt(%(processscale)s);
        let v = 0.0;
        for (let i = 0; i < realizedProcessLength; i++) {
            realization[i] = v;
            v += gaussianRandom() * scaling;
        }
        return simpleLerp(realization);
    }
    function makeWienerDerivative(realization) {
        const scaling = Math.sqrt(%(processscale)s);
        // Fill in the Wiener process that we're integrating up.
        for (let i = 0; i < realizedProcessLength; i++) {
            realization[i] = gaussianRandom() * scaling;
        }
        return (t) => {
            const moment = Math.max(0, Math.min(realizedProcessLength - 1, t / %(processscale)s));
            const i = Math.floor(moment);
            return realization[i] / %(processscale)s;
            //// Naively we could just return: realization[i] / %(processscale)s
            //// But the discontinuity thereof makes our adaptive integration rule go crazy.
            //// So instead we return a triangular hat that has the same integral, and thus twice the peak value.
            //// Here the coefficient is 4 because we need a factor of 2 to make our peak twice as high,
            //// and then another factor of 2 because Math.min(lerpCoef, 1 - lerpCoef) peaks at 0.5.
            ////const c = 4 * realization[i] / %(processscale)s;
            ////return c * Math.min(lerpCoef, 1 - lerpCoef);
        };
    }
    return {
        allocate: () => {
            const state = new Float64Array(%(degrees_of_freedom)s);
            const statePrime = new Float64Array(%(degrees_of_freedom)s);
            const scratch = new Float64Array(%(scratch_buffer_size)s);
            const parameters = new Float64Array(%(parameter_count)s);
            const plotData = {};
            const realizedProcesses = [];
            const realizedProcessFunctions = [];
            const ctx = {
                state, statePrime,
                scratch, parameters,
                plotData,
                realizedProcesses, realizedProcessFunctions,
                deallocate: () => {},
            };
            %(allocate_code)s
            return new Promise(resolve => resolve(ctx));
        },
        initialize: (ctx) => {
            ctx.plotData = %(plot_data_initial)s;
            const {state, scratch, parameters, realizedProcesses, realizedProcessFunctions} = ctx;
            // Ugh, this is ugly. This is to deal with how crappily I map seed -> state in the front-end.
            for (let i = 0; i < 100; i++)
                myRandom();
            %(initialization_code)s
        },
        getDerivative: (ctx, t, dt, state, statePrime) => {
            // NB: We don't necessarily use the statePrime from ctx!
            const {scratch, parameters, realizedProcessFunctions} = ctx;
            %(derivative_code)s
        },
        extractPlotDatum: (ctx, t, dt) => {
            const {state, scratch, parameters, plotData, realizedProcessFunctions} = ctx;
            let fillRow = null;
            %(extract_plot_datum)s
        },
        getObjective: (ctx, t, dt, state) => {
            const {scratch, parameters, realizedProcessFunctions} = ctx;
            return %(objective_code)s;
        },
        reseedRNG,
        getRNGState: () => [...xoshiro128ss_state[0]],
        setRNGState: (state) => {
            xoshiro128ss_state[0] = [...state];
            boxMullerCache[0] = null;
        },
        plots: %(plots)s,
        widgets: %(widgets)s,
        parameterTable: %(parameter_table)s,
        settings: %(settings)s,
        // These two tables aren't used by the demo app, but you might want to use yourself.
        zerothOrderTable: %(scratch_table)s,
        stateTable: %(state_table)s,
        compilationId: %(compilation_id)s,
    };
})()