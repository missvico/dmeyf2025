#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde)
{
  double  x[100];
  double  y[100];

  int n = pcolumna.size();
  NumericVector out(5*n);

  for(int i = 0; i < n; i++)
  {
    if( pdesde[i]-1 < i )  out[i + 4*n]  =  pcolumna[i-1];
    else                   out[i + 4*n]  =  NA_REAL;

    int libre = 0;
    int xvalor = 1;

    for(int j = pdesde[i]-1; j <= i; j++)
    {
       double a = pcolumna[j];

       if( !R_IsNA(a) )
       {
          y[libre] = a;
          x[libre] = xvalor;
          libre++;
       }
       xvalor++;
    }

    if( libre > 1 )
    {
      double  xsum  = x[0];
      double  ysum  = y[0];
      double  xysum = xsum * ysum;
      double  xxsum = xsum * xsum;
      double  vmin  = y[0];
      double  vmax  = y[0];

      for( int h=1; h<libre; h++)
      {
        xsum  += x[h];
        ysum  += y[h];
        xysum += x[h]*y[h];
        xxsum += x[h]*x[h];

        if( y[h] < vmin )  vmin = y[h];
        if( y[h] > vmax )  vmax = y[h];
      }

      out[i]       =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum);
      out[i + n]   =  vmin;
      out[i + 2*n] =  vmax;
      out[i + 3*n] =  ysum / libre;
    }
    else
    {
      out[i]       = NA_REAL;
      out[i + n]   = NA_REAL;
      out[i + 2*n] = NA_REAL;
      out[i + 3*n] = NA_REAL;
    }
  }

  return out;
}
