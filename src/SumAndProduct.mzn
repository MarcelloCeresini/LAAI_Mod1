array[1..4] of var 1..711: numbers;

constraint sum(numbers) = 711;
constraint product(numbers) = 711*10*10*10;

solve satisfy;

output[show(numbers[i]) | i in 1..4];