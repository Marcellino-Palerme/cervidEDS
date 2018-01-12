#ifndef SIMULATION_H
#define SIMULATION_H

#include <Rcpp.h>
using namespace Rcpp;
#include "simulation_const.h"
#include <math.h>
#include <vector>
#include <cmath>
#include <boost/random.hpp>
#include <map>
#include <list>
#include <algorithm>

typedef boost::normal_distribution<double> Normal;
typedef boost::mt19937 RandomGenerator;
typedef boost::uniform_01<RandomGenerator&> Uni;
typedef boost::variate_generator<RandomGenerator&, Normal> my_Generator;

NumericVector border_effect (unsigned int,
                             unsigned int,
                             double,
                             double,
                             double);

NumericVector distance2point(double, 
                             double, 
                             double,
                             double);

NumericVector shorter_distance(double, 
                               double, 
                               double,
                               double,
                               double,
                               double);

NumericVector distance2segment(double , 
                               double , 
                               double ,
                               double , 
                               double , 
                               double );

double potential_func(double,
                      double,
                      double,
                      double);

double grad_potential_func (double ,
                            double ,
                            double ,
                            double ,
                            double );

double alpha_func(double,
                  double,
                  double,
                  double);

double potential_value (NumericMatrix,
                        NumericMatrix);

NumericVector potential_effect (NumericMatrix,
                                NumericMatrix);

NumericVector all_effect (NumericVector,
                          unsigned int,
                          unsigned int,
                          double,
                          NumericMatrix,
                          NumericMatrix,
                          double);

double diffusion (double,
                  double);

#endif /*SIMULATION_H*/