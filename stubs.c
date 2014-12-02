#include <math.h>
#include <caml/fail.h>
#include <caml/alloc.h>
value ml_fma(value x, value y, value z) {
	return copy_double(fma(Double_val(x), Double_val(y), Double_val(z)));
}
value ml_fdim(value x, value y) {
	return copy_double(fdim(Double_val(x), Double_val(y)));
}
value ml_nextafter(value x, value y) {
	return copy_double(nextafter(Double_val(x), Double_val(y)));
}
value ml_remainder(value x, value y) {
	return copy_double(remainder(Double_val(x), Double_val(y)));
}
value ml_trunc(value x) { return copy_double(trunc(Double_val(x))); }
value ml_round(value x) { return copy_double(round(Double_val(x))); }
value ml_nearbyint(value x) { return copy_double(nearbyint(Double_val(x))); }
value ml_tgamma(value x) { return copy_double(tgamma(Double_val(x))); }
value ml_lgamma(value x) { return copy_double(lgamma(Double_val(x))); }
value ml_erfc(value x) { return copy_double(erfc(Double_val(x))); }
value ml_erf(value x) { return copy_double(erf(Double_val(x))); }
value ml_hypot(value x, value y) {
	return copy_double(hypot(Double_val(x), Double_val(y)));
}
value ml_cbrt(value x) { return copy_double(cbrt(Double_val(x))); }
value ml_logb(value x) { return copy_double(logb(Double_val(x))); }
value ml_log2(value x) { return copy_double(log2(Double_val(x))); }
value ml_log1p(value x) { return copy_double(log1p(Double_val(x))); }
value ml_expm1(value x) { return copy_double(expm1(Double_val(x))); }
value ml_exp2(value x) { return copy_double(exp2(Double_val(x))); }
value ml_atanh(value x) { return copy_double(atanh(Double_val(x))); }
value ml_asinh(value x) { return copy_double(asinh(Double_val(x))); }
value ml_acosh(value x) { return copy_double(acosh(Double_val(x))); }
value ml_scalbn(value x, value y) {
	return copy_double(scalbn(Double_val(x), Int_val(y)));
}
value ml_ilogb(value x) {
	return Int_val(ilogb(Double_val(x)));
}
value ml_j0(value x) { return copy_double(j0(Double_val(x))); }
value ml_j1(value x) { return copy_double(j1(Double_val(x))); }
value ml_y0(value x) { return copy_double(y0(Double_val(x))); }
value ml_y1(value x) { return copy_double(y1(Double_val(x))); }
