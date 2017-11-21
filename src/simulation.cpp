#include "simulation.h"

//'@title Border effect
//'@description This function calculate the effect of border on mouvement
//' 
//' @param ui_land_width (unsigned int) width of landscape.
//' @param ui_land_heigth (unsigned int) heigth of landscape.
//' @param d_sigma (double) repulsif effect adaptor
//' @param d_pos_x (double) position in x
//' @param d_pos_y (double) position in y
//' @param ad_effect_xy (double[2]) effect in x and y
//' @export
// [[Rcpp::export]]
NumericVector border_effect (unsigned int ui_width,
                             unsigned int ui_heigth,
                             double d_sigma,
                             double d_pos_x,
                             double d_pos_y)
{
   double d_coeff;
   NumericVector ad_effect_xy = NumericVector::create(_["x"] = 0,
                                                      _["y"] = 0 );
   
   d_coeff = d_sigma * 10;
   //calculate effect in x
   ad_effect_xy["x"] = d_coeff * (pow(d_pos_x, -1) + pow(d_pos_x - ui_width, -1));
   //calculate effect in y
   ad_effect_xy["y"] = d_coeff * (pow(d_pos_y, -1) + pow(d_pos_y - ui_heigth, -1));
   
   return ad_effect_xy;
}

//' @title Distance to Segment
//' @description Give distance a point to segment and derivate in x and y
//' 
//' @param x (double) x of point 
//' @param y (double) y of point 
//' @param x1 (vector) x1 of segment 
//' @param y1 (vector) y1 of segment
//' @param x2 (vector) x2 of segment
//' @param y2 (vector) y2 of segment
//' @return (vector) distance to segment (dist),
//'                  derivate in x (dx),
//'                  derivate in y (dy)
//' @export
// [[Rcpp::export]]
NumericVector distance2segment(double x, 
                               double y, 
                               double x1,
                               double y1, 
                               double x2, 
                               double y2)
{
   NumericVector return_values = NumericVector::create(_["dist"] = 0,
                                                       _["dx"] = 0,
                                                       _["dy"] = 0);
   double sx, sy;
   double ux, uy;
   double dp;
   double sn2;
   double ah2;
   double un2;
   // case segment is a point
   if (x1==x2 && y1==y2)
   {
      return_values["dist"] = (pow(x1 - x, 2) + pow(y1 - y, 2));
      return_values["dx"] = (-2 * (x1 - x));
      return_values["dy"] = (-2 * (y1 - y));
      return return_values;
   }
   sx = x2 - x1;
   sy = y2 - y1;
   ux = x - x1;
   uy = y - y1;
   dp = sx * ux + sy * uy;
   if (dp < 0) 
   {
      return_values["dist"] = (pow(x1 - x, 2) + pow(y1 - y, 2));
      return_values["dx"] = (-2 * (x1 - x));
      return_values["dy"] = (-2 * (y1 - y));
      return return_values;
   }
   sn2 = sx * sx + sy * sy;
   if (dp > sn2)
   {
      return_values["dist"] = (pow(x2 - x, 2) + pow(y2 - y, 2));
      return_values["dx"] = (-2 * (x2 - x));
      return_values["dy"] = (-2 * (y2 - y));
      return return_values;
   }
   ah2 = dp * dp / sn2;
   un2 = ux * ux + uy * uy;
   return_values["dist"] = (un2 - ah2);
   return_values["dx"] = (2 * (sy * sy / sn2) * ux - 2 * sx * sy * uy / sn2);
   return_values["dy"] = (2 * (sx * sx / sn2) * uy - 2 * sx * sy * ux / sn2);
   
   return return_values;
}

//' @title gradient potential function
//' @description Fonction gradient du potentiel précédent
//' 
//' @param alpha_t (double)
//' @param beta (double) spatial display of potential
//' @param dist (double) distance 
//' @param puiss (double)
//' @param deriv (double)
//' @export
// [[Rcpp::export]]
double grad_potential_func (double alpha_t,
                            double beta,
                            double dist,
                            double puiss,
                            double deriv)
{
   return -puiss * beta * alpha_t * deriv * pow(dist, puiss - 1) * 
          exp(-beta * pow(dist, puiss));
}

//' @title alpha function
//' @description evolution of maximum of potential in time
//' 
//' @param alpha1 (double) maximum potential amplitude
//' @param alpha2 (double) spatial display of potential in time
//' @param alpha3 (double) time when potential amplitude is maximum
//' @export
// [[Rcpp::export]]
double alpha_func(double alpha1,
                  double alpha2,
                  double alpha3,
                  double t)
{
  return alpha1 * exp(-0.5 * pow(log(t / alpha3) / alpha2, 2));
}

//' @title effect potential
//' 
//' @param nm_coords_rep (NumericMatrix) all coordinates of repulsif segment 
//'                                      Matrix N*4 (x1,y1,x2,y2)
//' @param nv_coords_point (NumericVector) coordinates of point (x,y)
//' @param bound (NumericVector) limit of effect potential ("min", "max")
//' @param alpha (double) potential amplitude
//' @param beta (double) spatial display of potential
//' @param power (double)
//' @param b_sum_sub (bool) use +gradiant(true) or -gradiant(false) (not effect 
//'        on bound)
NumericVector potential_effect (NumericMatrix nm_coords_element,
                                NumericVector nv_coords_point,
                                NumericVector bound,
                                double alpha,
                                double beta,
                                double power,
                                bool b_sum_sub)
{
  NumericVector effect = NumericVector::create(_["x"] = 0,
                                               _["y"] = 0);
  NumericVector dist_grad_xy;
  int sum_sub = -1;
  
  if(b_sum_sub)
  {
    sum_sub = 1;
  }
  
  for (int i = 0; i < nm_coords_element.nrow(); i++)
  {
    // Calculate distant point to segment and derivate in x and y
    dist_grad_xy = distance2segment(nv_coords_point["x"],
                                    nv_coords_point["y"],
                                    nm_coords_element(i, 0),
                                    nm_coords_element(i, 1),
                                    nm_coords_element(i, 2),
                                    nm_coords_element(i, 3));
    //
    effect["x"] = effect["x"] + sum_sub * grad_potential_func(alpha,
                                                              beta,
                                                              dist_grad_xy["dist"],
                                                              power,
                                                              dist_grad_xy["dx"]);

    effect["y"] = effect["y"] + sum_sub * grad_potential_func(alpha,
                                                              beta,
                                                              dist_grad_xy["dist"],
                                                              power,
                                                              dist_grad_xy["dy"]);
  }
  // bound sum
  effect["x"] = fmin(fmax(bound["min"], effect["x"]), bound["max"]);
  effect["y"] = fmin(fmax(bound["min"], effect["y"]), bound["max"]);
  
  return effect;
}

//' @title repulsive effect
//' @description Give effect on x and y of all replusive elements 
//' 
//' @param nm_coords_rep (NumericMatrix) all coordinates of repulsif segment 
//'                                      Matrix N*4 (x1,y1,x2,y2)
//' @param nv_coords_point (NumericVector) coordinates of point (x,y)
//' 
NumericVector repulsive_effect (NumericMatrix nm_coords_rep,
                                NumericVector nv_coords_point,
                                double alpha1)
{
  NumericVector bound = NumericVector::create(_["min"] = DBL_MIN,
                                              _["max"] = DBL_MAX);
  return potential_effect(nm_coords_rep,
                          nv_coords_point,
                          bound,
                          alpha1,
                          0.1,
                          2,
                          true);
}
