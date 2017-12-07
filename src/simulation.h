#ifndef SIMULATION_H
#define SIMULATION_H

#include <Rcpp.h>
using namespace Rcpp;
#include <math.h>
#include <vector>
#include <cmath>
#include <boost/random.hpp>

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

NumericVector potential_effect (NumericMatrix,
                                NumericVector,
                                NumericVector,
                                double,
                                double,
                                double,
                                bool);

NumericVector repulsive_effect (NumericMatrix,
                                NumericVector,
                                double);

NumericVector attractive_effect (NumericMatrix,
                                 NumericVector,
                                 double);

NumericVector all_effect (NumericVector,
                          unsigned int,
                          unsigned int,
                          double,
                          NumericMatrix,
                          double,
                          NumericMatrix,
                          double,
                          double);

double diffusion (double,
                  double);
#endif /*SIMULATION_H*/