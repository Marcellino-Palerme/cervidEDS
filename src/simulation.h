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

double potential_value (NumericMatrix,
                        NumericVector,
                        NumericVector,
                        bool);

NumericVector potential_effect (NumericMatrix,
                                NumericVector,
                                NumericVector,
                                bool);

double repulsive_value (NumericMatrix,
                        NumericVector);

NumericVector repulsive_effect (NumericMatrix,
                                NumericVector);

double attractive_value (NumericMatrix,
                        NumericVector);

NumericVector attractive_effect (NumericMatrix,
                                 NumericVector);

double all_value (NumericVector,
                  NumericMatrix,
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

NumericVector next_coord (NumericVector,
                          unsigned int,
                          unsigned int,
                          double,
                          NumericMatrix,
                          NumericMatrix,
                          double);
#endif /*SIMULATION_H*/