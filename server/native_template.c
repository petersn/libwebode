// This is the default template for C.

#include <stdbool.h>
#include <stdint.h>
#include <math.h>

#ifndef M_PI
    #define M_PI 3.14159265358979323846
#endif
#ifndef M_E
    #define M_E 2.718281828459045235360
#endif

#define RANDOM_EPSILON (0.5 / 4294967296)

#define MCSAMPLES %(mcsamples)s
#define DEGREES_OF_FREEDOM %(degrees_of_freedom)s
#define SCRATCH_BUFFER_SIZE %(scratch_buffer_size)s
#define ADJUSTABLE_PARAMETERS %(adjustable_parameters)s
#define REALIZED_PROCESSES %(realized_processes)s
#define REALIZED_PROCESS_LENGTH %(realized_process_length)s
#define PLOT_TRACE_COUNT %(plot_trace_count)s
#define PLOT_TRACE_LENGTH %(plot_trace_length)s
#define PROCESS_SCALE %(process_scale)s
#define STEP_SIZE %(step_size)s
#define TOTAL_STEPS %(total_steps)s
#define PLOT_COOLDOWN_INCREMENT %(plot_cooldown_increment)s
// We also depend on (init_code), (compute_deriv{0,1,2,3}), (extract_plot_datum), and (objective_expression).

#define myRandom xoshiro128ss_random
#define gaussianRandom gaussian_random
#define uniformRandom uniform_random
#define gammaRandom gamma_random
#define betaRandom beta_random
#define frechetRandom frechet_random

uint32_t a, b, c, d;

void set_rng_state(int _a, int _b, int _c, int _d) {
    a = _a;
    b = _b;
    c = _c;
    d = _d;
}

static inline double xoshiro128ss_random() {
    uint32_t t = b << 9;
    uint32_t r = a * 5;
    r = (r << 7 | r >> 25) * 9;
    c ^= a; d ^= b;
    b ^= c; a ^= d; c ^= t;
    d = d << 11 | d >> 21;
    return r / 4294967296.0;
}

bool box_muller_cache_full = false;
double box_muller_cache;

static double uniform_random(double low, double high) {
    return low + (high - low) * xoshiro128ss_random();
}

static double gaussian_random() {
    if (box_muller_cache_full) {
        box_muller_cache_full = false;
        return box_muller_cache;
    }
    double u = RANDOM_EPSILON + xoshiro128ss_random();
    double v = xoshiro128ss_random();
    double scale = sqrt(-2 * log(u));
    double arg = 2 * M_PI * v;
    box_muller_cache_full = true;
    box_muller_cache = scale * cos(arg);
    return scale * sin(arg);
}

static double gamma_random(double alpha, double beta) {
    // Translated from the Python source code for random.py.
    if (alpha > 1.0) {
        // Uses R.C.H. Cheng, "The generation of Gamma
        // variables with non-integral shape parameters",
        // Applied Statistics, (1977), 26, No. 1, p71-74
        double ainv = sqrt(2 * alpha - 1);
        double bbb = alpha - log(4);
        double ccc = alpha + ainv;
        double SG_MAGICCONST = 1 + log(4.5);
        while (true) {
            double u1 = xoshiro128ss_random();
            if (u1 < 1e-7 || u1 > 0.9999999)
                continue;
            double u2 = 1 - xoshiro128ss_random();
            double v = log(u1 / (1 - u1)) / ainv;
            double x = alpha * exp(v);
            double z = u1 * u1 * u2;
            double r = bbb + ccc * v - x;
            if (r + SG_MAGICCONST - 4.5 * z >= 0.0 || r >= log(z))
                return x * beta;
        }
    } else if (alpha == 1.0) {
        return -log(RANDOM_EPSILON + xoshiro128ss_random()) * beta;
    } else {
        // Uses ALGORITHM GS of Statistical Computing - Kennedy & Gentle
        double x;
        while (true) {
            double u = xoshiro128ss_random();
            double b = (M_E + alpha) / M_E;
            double p = b * u;
            x = p <= 1.0 ? pow(p, 1 / alpha) : -log((b - p) / alpha);
            double u1 = xoshiro128ss_random();
            if (p > 1.0) {
                if (u1 <= pow(x, alpha - 1))
                    break;
            } else if (u1 <= exp(x)) {
                break;
            }
        }
        return x * beta;
    }
}

static double beta_random(double alpha, double beta) {
    double y = gamma_random(alpha, 1.0);
    if (y == 0)
        return 0.0;
    return y / (y + gamma_random(beta, 1.0));
}

static double frechet_random(double alpha) {
    return pow(-log(RANDOM_EPSILON + xoshiro128ss_random()), -1 / alpha);
}

static double exponential_random() {
    return -log(RANDOM_EPSILON + xoshiro128ss_random());
}

static void make_PoissonProcess(double* realization, double rate) {
    double v = 0.0;
    double next_time_step = exponential_random() / rate;
    for (int i = 0; i < REALIZED_PROCESS_LENGTH; i++) {
        realization[i] = v;
        next_time_step -= PROCESS_SCALE;
        while (next_time_step <= 0.0) {
            next_time_step += exponential_random() / rate;
            v += 1.0;
        }
    }
}

static void make_WienerProcess(double* realization) {
    double scaling = sqrt(PROCESS_SCALE);
    double v = 0.0;
    for (int i = 0; i < REALIZED_PROCESS_LENGTH; i++) {
        realization[i] = v;
        v += gaussian_random() * scaling;
    }
}

static void make_WienerDerivative(double* realization) {
    double scaling = 1 / sqrt(PROCESS_SCALE);
    for (int i = 0; i < REALIZED_PROCESS_LENGTH; i++)
        realization[i] = gaussian_random() * scaling;
}

static inline double wiener_derivative_unstable() {
    return gaussian_random() / sqrt(STEP_SIZE);
}

static inline double fetch_lerp(double* const restrict realization, double t) {
    double moment = t / PROCESS_SCALE;
    if (moment < 0)
        moment = 0;
    if (moment > REALIZED_PROCESS_LENGTH - 2)
        moment = REALIZED_PROCESS_LENGTH - 2;
    int i = moment;
    double lerp_coef = moment - i;
    return realization[i] * (1 - lerp_coef) + realization[i + 1] * lerp_coef;
}

static inline double fetch_floor(double* const restrict realization, double t) {
    double moment = t / PROCESS_SCALE;
    int i = moment;
    if (i < 0)
        i = 0;
    if (i > REALIZED_PROCESS_LENGTH - 1)
        i = REALIZED_PROCESS_LENGTH - 1;
    return realization[i];
}

void initialize(
    double* big_state,
    double* big_parameters,
    double* big_realized_processes
) {
    for (int samp = 0; samp < MCSAMPLES; samp++) {
        double* const restrict state              = big_state + samp * DEGREES_OF_FREEDOM;
        double* const restrict parameters         = big_parameters + samp * ADJUSTABLE_PARAMETERS;
        double* const restrict realized_processes = big_realized_processes + samp * REALIZED_PROCESSES * REALIZED_PROCESS_LENGTH;
        // Ugh, this is ugly. This is to deal with how crappily I map seed -> state in the front-end.
        for (int i = 0; i < 100; i++)
            xoshiro128ss_random();
        %(init_code)s
    }
}

int run_simulation(
    double* big_state,              // [mcsamples, degrees_of_freedom]
    double* big_state_backup,       // [mcsamples, degrees_of_freedom]
    double* big_state_prime,        // [mcsamples, 4, degrees_of_freedom]
    double* big_scratch,            // [mcsamples, scratch_buffer_size]
    double* big_parameters,         // [mcsamples, adjustable_parameters]
    double* big_realized_processes, // [mcsamples, realized_processes, realized_process_length]
    double* big_plotting_traces,    // [mcsamples, plot_trace_count, plot_trace_length]
    double* big_objectives          // [mcsamples]
) {
    const double dt = STEP_SIZE;
    int overall_plot_counter;

    #pragma omp parallel for
    for (int samp = 0; samp < MCSAMPLES; samp++) {
        double t = 0.0;
        double plot_cooldown = 0.0;
        int plot_counter = 0;

        // Get pointers to the subarrays for just this Monte-Carlo sample.
        double* const restrict state              = big_state + samp * DEGREES_OF_FREEDOM;
        double* const restrict state_backup       = big_state_backup + samp * DEGREES_OF_FREEDOM;
        double* const restrict state_prime0       = big_state_prime + samp * 4 * DEGREES_OF_FREEDOM + 0 * DEGREES_OF_FREEDOM;
        double* const restrict state_prime1       = big_state_prime + samp * 4 * DEGREES_OF_FREEDOM + 1 * DEGREES_OF_FREEDOM;
        double* const restrict state_prime2       = big_state_prime + samp * 4 * DEGREES_OF_FREEDOM + 2 * DEGREES_OF_FREEDOM;
        double* const restrict state_prime3       = big_state_prime + samp * 4 * DEGREES_OF_FREEDOM + 3 * DEGREES_OF_FREEDOM;
        double* const restrict scratch            = big_scratch + samp * SCRATCH_BUFFER_SIZE;
        double* const restrict parameters         = big_parameters + samp * ADJUSTABLE_PARAMETERS;
        double* const restrict realized_processes = big_realized_processes + samp * REALIZED_PROCESSES * REALIZED_PROCESS_LENGTH;
        double* const restrict plotting_traces    = big_plotting_traces + samp * PLOT_TRACE_COUNT * PLOT_TRACE_LENGTH;

        for (int step = 0; step < TOTAL_STEPS; step++) {
            for (int i = 0; i < DEGREES_OF_FREEDOM; i++)
                state_backup[i] = state[i];

            // Derivative at t = 0
            %(compute_deriv0)s

            // Derivative at t = 1/2
            for (int i = 0; i < DEGREES_OF_FREEDOM; i++)
                state[i] = state_backup[i] + (0.5 * STEP_SIZE) * state_prime0[i];
            t += 0.5 * STEP_SIZE;
            %(compute_deriv1)s

            // Derivative at t = 1/2
            for (int i = 0; i < DEGREES_OF_FREEDOM; i++)
                state[i] = state_backup[i] + (0.5 * STEP_SIZE) * state_prime1[i];
            %(compute_deriv2)s

            // Derivative at t = 1
            for (int i = 0; i < DEGREES_OF_FREEDOM; i++)
                state[i] = state_backup[i] + STEP_SIZE * state_prime2[i];
            t += 0.5 * STEP_SIZE;
            %(compute_deriv3)s

            // Take a step.
            for (int i = 0; i < DEGREES_OF_FREEDOM; i++) {
                state[i] = state_backup[i] + (STEP_SIZE / 6.0) * (
                            state_prime0[i]
                    + 2.0 * state_prime1[i]
                    + 2.0 * state_prime2[i]
                    +       state_prime3[i]
                );
            }

            plot_cooldown -= 1.0;
            if (plot_cooldown <= 0.0) {
                plot_cooldown += PLOT_COOLDOWN_INCREMENT;

                %(extract_plot_datum)s

                plot_counter++;
            }
        }

        big_objectives[samp] = %(objective_expression)s;

        // OpenMP's behavior here is to give us the final loop iteration's value, which
        // is fine by us because every loop iteration should be producing the same value.
        overall_plot_counter = plot_counter;
    }

    return overall_plot_counter;
}
