#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix roll_stats_kernel(const NumericVector& x,
                                const IntegerVector& id,
                                int win) {
  int n = x.size();
  if (id.size() != n) stop("x e id deben tener la misma longitud");
  if (win < 1) stop("win debe ser >= 1");

  // out: columnas = mean, sd, cv, min, max, prop_gt0, max_streak_gt0
  NumericMatrix out(n, 7);

  for (int i = 0; i < n; i++) {
    // determinar inicio de ventana respetando id y tamaño win
    int start = i;
    int used  = 1;

    while (used < win && start > 0 && id[start - 1] == id[i]) {
      start--;
      used++;
    }

    double sum   = 0.0;
    double sumSq = 0.0;
    int    cnt   = 0;   // cantidad de valores no-NA
    double minv  = R_PosInf;
    double maxv  = R_NegInf;
    int    cnt_gt0      = 0;
    int    streak       = 0;
    int    bestStreak   = 0;

    for (int j = start; j <= i; j++) {
      double v = x[j];

      if (R_IsNA(v)) {
        // NA corta la racha
        streak = 0;
        continue;
      }

      cnt++;
      sum   += v;
      sumSq += v * v;

      if (v < minv) minv = v;
      if (v > maxv) maxv = v;

      if (v > 0.0) {
        cnt_gt0++;
        streak++;
      } else {
        streak = 0;
      }
      if (streak > bestStreak) bestStreak = streak;
    }

    double mean = NA_REAL;
    double sd   = NA_REAL;
    double cv   = NA_REAL;
    double prop = NA_REAL;

    if (cnt > 0) {
      mean = sum / cnt;

      if (cnt > 1) {
        double var = (sumSq - (sum * sum) / cnt) / (cnt - 1);
        sd = (var > 0.0) ? std::sqrt(var) : 0.0;
      } else {
        sd = NA_REAL;
      }

      if (!R_IsNA(mean) && mean != 0.0 && !R_IsNA(sd)) {
        cv = sd / mean;
      }

      prop = static_cast<double>(cnt_gt0) / cnt;

      if (minv == R_PosInf) minv = NA_REAL;
      if (maxv == R_NegInf) maxv = NA_REAL;
    } else {
      minv = NA_REAL;
      maxv = NA_REAL;
      bestStreak = NA_INTEGER;
    }

    out(i, 0) = mean;
    out(i, 1) = sd;
    out(i, 2) = cv;
    out(i, 3) = minv;
    out(i, 4) = maxv;
    out(i, 5) = prop;
    out(i, 6) = bestStreak;
  }

  colnames(out) = CharacterVector::create(
    "mean", "sd", "cv", "min", "max", "prop_gt0", "max_streak_gt0"
  );
  return out;
}
