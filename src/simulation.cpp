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
   // only if it isn't on border
   // if it is on or out border there isn't border effect
   if(d_pos_x < ui_width && d_pos_x > 0 )
   {
     ad_effect_xy["x"] = d_coeff * (pow(d_pos_x, -1) + 
                                    pow(d_pos_x - ui_width, -1));
   }
   
   //calculate effect in y
   // only if it isn't on border
   // if it is on border there isn't border effect
   if(d_pos_y < ui_heigth && d_pos_y > 0)
   {
     ad_effect_xy["y"] = d_coeff * (pow(d_pos_y, -1) + 
                                    pow(d_pos_y - ui_heigth, -1)); 
   }
   
   return ad_effect_xy;
}

//' @title Distance to point
//' @description Give distance a point to point and derivate in x and y
//' 
//' @param x (double) x of first point 
//' @param y (double) y of first point 
//' @param x1 (vector) x1 of second point 
//' @param y1 (vector) y1 of second point
//' @return (vector) distance to segment (dist),
//'                  derivate in x (dx),
//'                  derivate in y (dy)
//' @export
// [[Rcpp::export]]
NumericVector distance2point(double x, 
                             double y, 
                             double x1,
                             double y1)
{
  NumericVector return_values = NumericVector::create(_["dist"] = 0,
                                                      _["dx"] = 0,
                                                      _["dy"] = 0);
  // Case 2 points are same point
  if(x == x1 && y == y1)
  {
    return return_values;
  }
  return_values["dist"] = sqrt(pow(x1 - x, 2) + pow(y1 - y, 2));
  return_values["dx"] = (-2 * (x1 - x)) /
                        (2 * sqrt(pow(x1 - x, 2) + pow(y1 - y, 2)));
  return_values["dy"] = (-2 * (y1 - y)) /
                        (2 * sqrt(pow(x1 - x, 2) + pow(y1 - y, 2)));
  return return_values;
}

//' @title shoter distance
//' @description Give shoter distance a point to extrem point of segment and 
//'              derivate in x and y
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
NumericVector shorter_distance(double x, 
                               double y, 
                               double x1,
                               double y1,
                               double x2,
                               double y2)
{
  NumericVector distA, distB;
  double tmpA, tmpB;

  distA = distance2point(x, y, x1, y1);
  distB = distance2point(x, y, x2, y2);
  tmpA = distA["dist"];
  tmpB = distB["dist"];

  // shorter distance is point to A(x1,y1) or B(x2,y2)
  if (tmpA < tmpB)
  {
    return distA;
  }
  return distB;
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
   double slot_seg;
   double slot_ortho;
   double vert_inter_seg;
   double xh, yh;
   double limit_h [2];

   // case segment is a point
   if (x1==x2 && y1==y2)
   {
      return distance2point(x, y, x1, y1);
   }

   // case segment is vertical
   if( x1 == x2)
   {
     // if project of point is on segment
     if((y< y1 && y> y2) || (y> y1 && y< y2))
     {
        return distance2point(x, y, x1, y);
     }
     
     return shorter_distance(x, y, x1, y1, x2, y2);
   }
   // case segment is horizontal
   if( y1 == y2)
   {
     // if project of point is on segment
     if((x< x1 && x> x2) || (x> x1 && x< x2))
     {
       return distance2point(x, y, x, y1);
     }
     
     return shorter_distance(x, y, x1, y1, x2, y2);
   }
   sx = x2 - x1;
   sy = y2 - y1;
   // calculate slot of segment
   slot_seg = sy / sx;
   // Calculate slot of all orthogonal line to line segment
   slot_ortho = -1 / slot_seg;

   limit_h[0] = ((slot_ortho * x) + y1 - slot_ortho * x1);
   limit_h[1] = ((slot_ortho * x) + y2 - slot_ortho * x2);
   // case point is not between two orthogonal line to segment passing to 
   // A(x1,y1) and B(x2,y2)
   if( (y < limit_h[0] && y < limit_h[1])
       || (y > limit_h[0] && y > limit_h[1]))
   {
     // shorter distance is point to A(x1,y1) or B(x2,y2)
     return shorter_distance(x, y, x1, y1, x2, y2);
   }
   
   // calculate vertical intercept of line segment
   vert_inter_seg = y1 - slot_seg * x1;
   // Case point is on segment
   if( y == ((slot_seg * x) + vert_inter_seg))
   {
     // Distance is zero
     return return_values;
   }
   
   // Other case
   // Calculate coordonate of orthogonal projection of point on line segment
   xh = ((y - slot_ortho * x) - vert_inter_seg) / (slot_seg - slot_ortho);
   yh = slot_seg * xh + vert_inter_seg;
   
   // shorter distanc is point and this orthogonal projection on line segment
   return distance2point(x, y, xh, yh);
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
//' @param alpha1 (double) maximum potential amplitude
//' @export
// [[Rcpp::export]]
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
