% Use this editor as a MiniZinc scratch book
include "globals.mnz";

int: n;
array[i..n, 1..n] of int: dist; % distance between pairs of cities
int: start_city;
int: end_city;
array[1..n] of var 1..n: city;
array[1..n] of string: city_name;

constraint city[1] = start_city;
constraint city[n] = end_city;
constraint all_different(city);

var int: total_distance = sum(i in 2..n)(dist[city[i-1], city[i]]);
solve minimize total_distance;

output [city_name[fix(city[i])] ++ "->" | i in 1..n] ++ ["\nTotal hours travelled: ", show(total_distance)]

