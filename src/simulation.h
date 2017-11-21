#ifndef SIMULATION_H
#define SIMULATION_H

#include <Rcpp.h>
using namespace Rcpp;
#include <math.h>
#include <vector>
#include <cmath>

NumericVector border_effect (unsigned int,
                             unsigned int,
                             double,
                             double,
                             double);

NumericVector distance2segment(double , 
                               double , 
                               double ,
                               double , 
                               double , 
                               double );

double grad_potential_func (double ,
                            double ,
                            double ,
                            double ,
                            double );

#endif /*SIMULATION_H*/