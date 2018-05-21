#include <gsl/gsl_cqp.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>

#include <wrappers.h>
#include <mlgsl_vector_double.h>
#include <mlgsl_matrix_double.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value solve_cqp_native
  (value vn, value Q, value q,
   value vme, value A, value b,
   value vmi, value C, value d,
   value v1, value v2, value v3) {
  int n = Int_val(vn);
  _DECLARE_MATRIX(Q);
  _CONVERT_MATRIX(Q);
  _DECLARE_VECTOR(q);
  _CONVERT_VECTOR(q);
  int me = Int_val(vme);
  _DECLARE_MATRIX(A);
  _CONVERT_MATRIX(A);
  _DECLARE_VECTOR(b);
  _CONVERT_VECTOR(b);
  int mi = Int_val(vmi);
  _DECLARE_MATRIX(C);
  _CONVERT_MATRIX(C);
  _DECLARE_VECTOR(d);
  _CONVERT_VECTOR(d);
  _DECLARE_VECTOR(v1);
  _CONVERT_VECTOR(v1);
  _DECLARE_VECTOR(v2);
  _CONVERT_VECTOR(v2);
  _DECLARE_VECTOR(v3);
  _CONVERT_VECTOR(v3);

  const size_t max_iter = 1000;
  size_t iter=1;
  int status;

  const gsl_cqpminimizer_type * T;
  gsl_cqpminimizer *s;

	gsl_cqp_data *cqp_data; 
	cqp_data = malloc(sizeof(gsl_cqp_data));
  cqp_data->Q = &m_Q;
  cqp_data->q = &v_q;
  cqp_data->A = &m_A;
  cqp_data->b = &v_b;
  cqp_data->C = &m_C;
  cqp_data->d = &v_d;

  T = gsl_cqpminimizer_mg_pdip;
  s = gsl_cqpminimizer_alloc(T, n, me, mi);	

  status = gsl_cqpminimizer_set(s, cqp_data);

  printf("== Itn ======= f ======== ||gap|| ==== ||residual||\n\n");

  do {
    status = gsl_cqpminimizer_iterate(s);
    status = gsl_cqpminimizer_test_convergence(s, 1e-10, 1e-10);

    printf("%4zd   %14.8f  %13.6e  %13.6e\n", iter, gsl_cqpminimizer_f(s), gsl_cqpminimizer_gap(s), gsl_cqpminimizer_residuals_norm(s));

    if(status == GSL_SUCCESS) {
      gsl_vector_memcpy(&v_v1, gsl_cqpminimizer_x(s));
      gsl_vector_memcpy(&v_v2, gsl_cqpminimizer_lm_eq(s));
      gsl_vector_memcpy(&v_v3, gsl_cqpminimizer_lm_ineq(s));
    } else {
      iter++;
    }
  } while(status == GSL_CONTINUE && iter <= max_iter);

  gsl_cqpminimizer_free(s);
  free(cqp_data);
  return Val_unit;
}

CAMLprim value solve_cqp_bytecode( value* argv, int argn) {
  return solve_cqp_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9], argv[10], argv[11]);
}
