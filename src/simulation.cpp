#include "simulation.h"
#include <stdio.h>

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

//' @title potential function
//' @description potential function
//' 
//' @param alpha_t (double)
//' @param beta (double) spatial display of potential
//' @param dist (double) distance 
//' @param power (double)
//' @return (double) potential
//' @export
// [[Rcpp::export]]
double potential_func(double alpha_t,
                      double beta,
                      double dist,
                      double power)
{
  return alpha_t * exp(-beta * pow(dist, power));
}

//' @title gradient potential function
//' @description Fonction gradient du potentiel précédent
//' 
//' @param alpha_t (double)
//' @param beta (double) spatial display of potential
//' @param dist (double) distance 
//' @param power (double)
//' @param deriv (double)
//' @return (double) gradiant of potential
//' @export
// [[Rcpp::export]]
double grad_potential_func (double alpha_t,
                            double beta,
                            double dist,
                            double power,
                            double deriv)
{
   return -power * beta * alpha_t * deriv * pow(dist, power - 1) * 
          exp(-beta * pow(dist, power));
}

//' @title alpha function
//' @description evolution of maximum of potential in time
//' 
//' @param alpha1 (double) maximum potential amplitude
//' @param alpha2 (double) spatial display of potential in time
//' @param alpha3 (double) time when potential amplitude is maximum
//' @return (double) miximum of potential at t
//' @export
// [[Rcpp::export]]
double alpha_func(double alpha1,
                  double alpha2,
                  double alpha3,
                  double t)
{
  if (alpha2 == 0 || alpha3 == 0 || t == 0 || t*alpha3 < 0)
  {
    return 0;
  }
  return alpha1 * exp(-0.5 * pow(log(t / alpha3) / alpha2, 2));
}

// create dictionnary to indicate which line containt which type
std::map<int, int> map_type (NumericMatrix nm_info_type) 
{
  std::map<int, int> index_type;
  for (int i = 0; i < nm_info_type.nrow(); i++)
  {
    index_type[nm_info_type(i, INFO_TYPE_COL_ID)] = i;
  }

  return index_type;
}

//' @title value potential
//' 
//' @param nv_coords_point (NumericVector) coordinates of point (x,y)
//' @param nm_coords_element (NumericMatrix) all coordinates of segment 
//'                                          Matrix N*5 (x1,y1,x2,y2,Id_type)
//' @param nm_info_type (NumericMatrix) descript each type
//'                                     Matrix M*5 (Id, rep (-1) or attr(1), 
//'                                                 alpha, beta, power)
//' @return (double) value potential
//' @export
// [[Rcpp::export]]
double potential_value (NumericVector nv_coords_point,
                        NumericMatrix nm_coords_element,
                        NumericMatrix nm_info_type)
{
  double value = 0;
  NumericVector dist_grad_xy;
  std::map<int, int> index_type = map_type(nm_info_type);
  int line_type = 0;
  
  // Verify if element is correct
  if (nm_coords_element.ncol() != ELEMENT_NB_COL)
  {
    return value;
  }
  for (int i = 0; i < nm_coords_element.nrow(); i++)
  {
    // Calculate distant point to segment and derivate in x and y
    dist_grad_xy = distance2segment(nv_coords_point[0],
                                    nv_coords_point[1],
                                    nm_coords_element(i, ELEMENT_COL_X1),
                                    nm_coords_element(i, ELEMENT_COL_Y1),
                                    nm_coords_element(i, ELEMENT_COL_X2),
                                    nm_coords_element(i, ELEMENT_COL_Y2));
    line_type = index_type[nm_coords_element(i, ELEMENT_COL_ID_TYPE)];

    value = value 
            - nm_info_type(line_type, INFO_TYPE_COL_REPATT)
            * potential_func(nm_info_type(line_type, INFO_TYPE_COL_ALPHA),
                             nm_info_type(line_type, INFO_TYPE_COL_BETA),
                             dist_grad_xy["dist"],
                             nm_info_type(line_type, INFO_TYPE_COL_POW));

  }

  return value;
}

//' @title effect potential
//' 
//' @param nv_coords_point (NumericVector) coordinates of point (x,y)
//' @param nm_coords_element (NumericMatrix) all coordinates of segment 
//'                                          Matrix N*5 (x1,y1,x2,y2,Id_type)
//' @param nm_info_type (NumericMatrix) descript each type
//'                                     Matrix M*5 (Id, rep (-1) or attr(1), 
//'                                                 alpha, beta, power)
//' @return (NumericVector) effect potential in x and y (['x'];['y'])
//' @export
// [[Rcpp::export]]
NumericVector potential_effect (NumericVector nv_coords_point,
                                NumericMatrix nm_coords_element,
                                NumericMatrix nm_info_type)
{
  NumericVector effect = NumericVector::create(_["x"] = 0,
                                               _["y"] = 0);
  NumericVector dist_grad_xy;
  std::map<int, int> index_type = map_type(nm_info_type);
  int line_type = 0;
  
  // Verify if element is correct
  if (nm_coords_element.ncol() != ELEMENT_NB_COL)
  {
    return effect;
  }
  for (int i = 0; i < nm_coords_element.nrow(); i++)
  {
    // Calculate distant point to segment and derivate in x and y
    dist_grad_xy = distance2segment(nv_coords_point[0],
                                    nv_coords_point[1],
                                    nm_coords_element(i, ELEMENT_COL_X1),
                                    nm_coords_element(i, ELEMENT_COL_Y1),
                                    nm_coords_element(i, ELEMENT_COL_X2),
                                    nm_coords_element(i, ELEMENT_COL_Y2));
    line_type = index_type[nm_coords_element(i, ELEMENT_COL_ID_TYPE)];
    effect["x"] = effect["x"] + 
                  nm_info_type(line_type, INFO_TYPE_COL_REPATT) * 
                  grad_potential_func(nm_info_type(line_type, INFO_TYPE_COL_ALPHA),
                                      nm_info_type(line_type, INFO_TYPE_COL_BETA),
                                      dist_grad_xy["dist"],
                                      nm_info_type(line_type, INFO_TYPE_COL_POW),
                                      dist_grad_xy["dx"]);
    
    effect["y"] = effect["y"] + 
                  nm_info_type(line_type, INFO_TYPE_COL_REPATT) * 
                  grad_potential_func(nm_info_type(line_type, INFO_TYPE_COL_ALPHA),
                                      nm_info_type(line_type, INFO_TYPE_COL_BETA),
                                      dist_grad_xy["dist"],
                                      nm_info_type(line_type, INFO_TYPE_COL_POW),
                                      dist_grad_xy["dy"]);

  }

  return effect;
}

//' @title all effect
//' @description Give complete effect of landscape on x and y
//' 
//' @param nv_coords_point (NumericVector) coordinates of point (x,y)
//' @param ui_land_width (unsigned int) width of landscape.
//' @param ui_land_heigth (unsigned int) heigth of landscape.
//' @param d_sigma (double) repulsif effect adaptor
//' @param nm_coords_element (NumericMatrix) all coordinates of segment 
//'                                          Matrix N*5 (x1,y1,x2,y2,Id_type)
//' @param nm_info_type (NumericMatrix) descript each type
//'                                     Matrix M*5 (Id, rep (-1) or attr(1), 
//'                                                 alpha, beta, power)
//' @param time_step (double) step of time
//' @return (NumericVector) effect in x and y (['x'];['y'])
//' @export
// [[Rcpp::export]]
NumericVector all_effect (NumericVector nv_coords_point,
                          unsigned int ui_width,
                          unsigned int ui_heigth,
                          double d_sigma,
                          NumericMatrix nm_coords_element,
                          NumericMatrix nm_info_type,
                          double time_step)
{
  NumericVector effect = NumericVector::create(_["x"] = 0,
                                               _["y"] = 0);
  NumericVector tmp_effect = NumericVector::create(_["x"] = 0,
                                                   _["y"] = 0);

  // border effect
  tmp_effect = border_effect (ui_width,
                              ui_heigth,
                              d_sigma,
                              nv_coords_point[0],
                              nv_coords_point[1]);

  effect["x"] = tmp_effect["x"];
  effect["y"] = tmp_effect["y"];

  // potential effect
  tmp_effect = potential_effect (nv_coords_point,
                                 nm_coords_element,
                                 nm_info_type);

  effect["x"] = effect["x"] + tmp_effect["x"];
  effect["y"] = effect["y"] + tmp_effect["y"];

  // effect proportional to step of time
  effect["x"] = effect["x"] * time_step;
  effect["y"] = effect["y"] * time_step;

  return effect;
}

//' @title diffusion
//' @description calculate the diffusion
//' 
//' @param d_sigma (double)
//' @param time_step (double) step of time
//' @return (double) diffusion
//' @export
// [[Rcpp::export]]
double diffusion (double d_sigma,
                  double time_step)
{
  double diffu;
  static RandomGenerator rng(0);
  const double mean = 0;
  const double sd = sqrt(time_step);
  Normal weiner(mean,sd);
  my_Generator my_weiner(rng,weiner);
  
  diffu = d_sigma * my_weiner();
  
  return diffu;
}

//' @title next coord
//' @description calculate next coordinates 
//' 
//' @param nv_coords_point (NumericVector) coordinates of point (x,y)
//' @param ui_land_width (unsigned int) width of landscape.
//' @param ui_land_heigth (unsigned int) heigth of landscape.
//' @param d_sigma (double) repulsif effect adaptor
//' @param nm_coords_element (NumericMatrix) all coordinates of segment 
//'                                          Matrix N*5 (x1,y1,x2,y2,Id_type)
//' @param nm_info_type (NumericMatrix) descript each type
//'                                     Matrix M*5 (Id, rep (-1) or attr(1), 
//'                                                 alpha, beta, power)
//' @param time_step (double) step of time
//' @return (NumericVector) next coord in x and y (['x'];['y'])
//' @export
// [[Rcpp::export]]

NumericVector next_coord (NumericVector nv_coords_point,
                          unsigned int ui_width,
                          unsigned int ui_heigth,
                          double d_sigma,
                          NumericMatrix nm_coords_element,
                          NumericMatrix nm_info_type,
                          double time_step)
{
  NumericVector effect;
  NumericVector new_coord = NumericVector::create(_["x"] = 0,
                                                  _["y"] = 0);
  effect = all_effect (nv_coords_point,
                       ui_width,
                       ui_heigth,
                       d_sigma,
                       nm_coords_element,
                       nm_info_type,
                       time_step);

  new_coord["x"] = nv_coords_point[0] + effect[0] + diffusion (d_sigma,
                                                               time_step);
  new_coord["y"] = nv_coords_point[1] + effect[1] + diffusion (d_sigma,
                                                               time_step);

  return new_coord;
}
