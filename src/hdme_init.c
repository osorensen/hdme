#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .Call calls */
extern SEXP _hdme_fit_mu_lasso(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);


static const R_CallMethodDef CallEntries[] = {
    {"_hdme_fit_mu_lasso", (DL_FUNC) &_hdme_fit_mu_lasso, 6},
    {NULL, NULL, 0}
};


void R_init_hdme(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
