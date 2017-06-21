#------- utility.R----------#
#
#Provide several simply functions


#Give index of value in list
#Parameters
#value : (...) searching value
#the_list : (list of ...) where we search
#Return
#(int)
#0 : if not find value in the_list
get_index = function(value, the_list)
{
  index = match(TRUE, the_list == value)
  if (is.na(index))
  {
    index = 0
  }
  return(index)
}