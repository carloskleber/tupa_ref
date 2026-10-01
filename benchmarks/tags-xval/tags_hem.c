/*
 * Harmonic input impedance of an electrode system with TAGS (mHEM), for the
 * cross-code comparison of ROADMAP Phase 10 item 5 (docs/validation/tags-xval.md).
 *
 * Adapted from benchmarks/tags/examples/harmonic_impedance.c: same kernels and
 * linear algebra (mHEM integrals, zsysv on the nodal admittance system), but
 * a homogeneous *linear* soil (sigma, eps_r, mu_r given on the command line)
 * instead of the Alipio-Visacro model, and a selectable longitudinal image
 * reflection coefficient.
 *
 *   tags_hem <sigma S/m> <eps_r> <inj_node 1..N> <ref_l: one|gamma> <dir> [signed]
 *
 * <dir> holds electrodes.csv, nodes.csv, frequencies.csv (TAGS formats); the
 * result is <dir>/zh.csv, one "re,im" line per frequency.
 *
 * ref_l = "one"   : TAGS default, longitudinal image with Gamma_l = 1
 * ref_l = "gamma" : Gamma_t on both parcels (the Matlab reference and
 *                   PRTL-mHEM choice, theory.md section 5; what TUPA does)
 *
 * TAGS evaluates Z_l with |cos(theta)| for the direct AND the image parcel
 * (electrode.c: cost = fabs(cost)); TUPA uses the signed cosine, which
 * differs for the image of a vertical electrode (cos = -1) and for
 * anti-parallel pairs (theory.md section 9.6). The optional last argument
 * "signed" restores the sign of the *image* parcel (potzli *= sign(cos
 * between electrode k and the image of electrode m)), isolating that single
 * convention difference. Direct-parcel signs are handled by orienting the
 * electrodes (xval.py).
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <complex.h>
#include "auxiliary.h"
#include "electrode.h"
#include "linalg.h"
#include "cubature.h"
#include "grid.h"

static size_t count_lines(const char *path)
{
    FILE *f = fopen(path, "r");
    if (!f) { fprintf(stderr, "cannot open %s\n", path); exit(2); }
    size_t n = 0;
    int c, last = '\n';
    while ((c = getc(f)) != EOF) { if (c == '\n') n++; last = c; }
    if (last != '\n') n++;
    fclose(f);
    return n;
}

int main(int argc, char *argv[])
{
    if (argc != 6 && argc != 7) {
        fprintf(stderr, "usage: %s sigma eps_r inj_node ref_l(one|gamma) dir [signed]\n", argv[0]);
        return 1;
    }
    const int signed_image_cos = (argc == 7 && strcmp(argv[6], "signed") == 0);
    const double sigma = atof(argv[1]);
    const double epsr = atof(argv[2]);
    const size_t inj = (size_t)atoi(argv[3]) - 1;
    const int gamma_l = strcmp(argv[4], "gamma") == 0;
    const char *dir = argv[5];
    char p[512];

    snprintf(p, sizeof p, "%s/electrodes.csv", dir); size_t ne = count_lines(p);
    Electrode *electrodes = malloc(sizeof(Electrode) * ne);
    if (electrodes_file(p, electrodes, ne) != 0) return 3;
    snprintf(p, sizeof p, "%s/nodes.csv", dir); size_t nn = count_lines(p);
    double *nodes = malloc(nn * 3 * sizeof(double));
    if (nodes_file(p, nodes, nn) != 0) return 3;
    snprintf(p, sizeof p, "%s/frequencies.csv", dir); size_t ns = count_lines(p);
    double *freq = malloc(ns * sizeof(double));
    FILE *ff = fopen(p, "r");
    for (size_t i = 0; i < ns; i++) if (fscanf(ff, "%lf", freq + i) != 1) return 4;
    fclose(ff);

    Electrode *images = malloc(sizeof(Electrode) * ne);
    for (size_t m = 0; m < ne; m++) {
        populate_electrode(images + m, electrodes[m].start_point, electrodes[m].end_point,
                           electrodes[m].radius);
        images[m].start_point[2] = -images[m].start_point[2];
        images[m].end_point[2] = -images[m].end_point[2];
        images[m].middle_point[2] = -images[m].middle_point[2];
    }

    const size_t ne2 = ne * ne, nn2 = nn * nn;
    _Complex double *potzl = malloc(ne2 * sizeof(_Complex double));
    _Complex double *potzt = malloc(ne2 * sizeof(_Complex double));
    _Complex double *potzli = malloc(ne2 * sizeof(_Complex double));
    _Complex double *potzti = malloc(ne2 * sizeof(_Complex double));
    _Complex double *a = malloc(ne * nn * sizeof(_Complex double));
    _Complex double *b = malloc(ne * nn * sizeof(_Complex double));
    if (fill_incidence_adm(a, b, electrodes, ne, nodes, nn) != 0) { fprintf(stderr, "incidence failed\n"); return 5; }
    const size_t max_eval = 0;
    const double req_abs_error = 1e-6, req_rel_error = 1e-4;
    if (calculate_impedances(potzl, potzt, electrodes, ne, 0.0, 0.0, 0.0, 0.0, max_eval,
                             req_abs_error, req_rel_error, INTG_MHEM) != 0) fprintf(stderr, "integration error\n");
    if (impedances_images(potzli, potzti, electrodes, images, ne, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                          max_eval, req_abs_error, req_rel_error, INTG_MHEM) != 0) fprintf(stderr, "integration error\n");

    if (signed_image_cos) {
        for (size_t m = 0; m < ne; m++) {
            for (size_t k = 0; k < ne; k++) {
                double dot = 0.0;
                for (int c = 0; c < 3; c++) {
                    double dk = electrodes[k].end_point[c] - electrodes[k].start_point[c];
                    double dm = images[m].end_point[c] - images[m].start_point[c];
                    dot += dk * dm;
                }
                if (dot < 0.0) potzli[m * ne + k] = -potzli[m * ne + k];
            }
        }
    }

    _Complex double *zl = malloc(ne2 * sizeof(_Complex double));
    _Complex double *zt = malloc(ne2 * sizeof(_Complex double));
    _Complex double *yn = malloc(nn2 * sizeof(_Complex double));
    _Complex double *ie = malloc(nn * sizeof(_Complex double));
    int *ipiv = malloc(nn * sizeof(int));
    snprintf(p, sizeof p, "%s/zh.csv", dir);
    FILE *out = fopen(p, "w");

    for (size_t i = 0; i < ns; i++) {
        const _Complex double s = freq[i] * TWO_PI * I;
        const _Complex double kappa = sigma + s * epsr * EPS0;       /* soil complex conductivity */
        const _Complex double gamma = csqrt(s * MU0 * kappa);        /* soil propagation constant */
        const _Complex double iwu_4pi = s * MU0 / FOUR_PI;           /* mu_r = 1 */
        const _Complex double one_4pik = 1.0 / (FOUR_PI * kappa);
        const _Complex double ref_t = (kappa - s * EPS0) / (kappa + s * EPS0);
        const _Complex double ref_l = gamma_l ? ref_t : 1.0;
        for (size_t m = 0; m < ne; m++) {
            for (size_t k = m; k < ne; k++) {
                double rbar = vector_length(electrodes[k].middle_point, electrodes[m].middle_point);
                _Complex double e = cexp(-gamma * rbar);
                zl[m * ne + k] = e * potzl[m * ne + k];
                zt[m * ne + k] = e * potzt[m * ne + k];
                rbar = vector_length(electrodes[k].middle_point, images[m].middle_point);
                e = cexp(-gamma * rbar);
                zl[m * ne + k] += ref_l * e * potzli[m * ne + k];
                zt[m * ne + k] += ref_t * e * potzti[m * ne + k];
                zl[m * ne + k] *= iwu_4pi;
                zt[m * ne + k] *= one_4pik;
            }
        }
        fill_impedance_adm(yn, zl, zt, a, b, ne, nn);
        for (size_t m = 0; m < nn; m++) ie[m] = 0.0;
        ie[inj] = 1.0;
        int nn1 = (int)nn, nrhs = 1, info, lwork = -1;
        char uplo = 'L';
        _Complex double wkopt;
        zsysv_(&uplo, &nn1, &nrhs, yn, &nn1, ipiv, ie, &nn1, &wkopt, &lwork, &info);
        lwork = (int)creal(wkopt);
        _Complex double *work = malloc(lwork * sizeof(_Complex double));
        zsysv_(&uplo, &nn1, &nrhs, yn, &nn1, ipiv, ie, &nn1, work, &lwork, &info);
        free(work);
        if (info != 0) { fprintf(stderr, "zsysv info=%d at f=%g\n", info, freq[i]); return 6; }
        fprintf(out, "%.12e,%.12e\n", creal(ie[inj]), cimag(ie[inj]));
    }
    fclose(out);
    return 0;
}
