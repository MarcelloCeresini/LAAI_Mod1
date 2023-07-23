var 0..100: banana; 
var 0..100: choc;

% flour
constraint 250*banana + 200*choc <= 4000;
% bananas
constraint 2*banana <= 6;
% sugar
constraint 75*banana + 150*choc <= 2000;
%butter
constraint 100*banana + 150*choc <= 500;
%cocoa
constraint 75*choc <= 500;


solve maximize 400*banana + 450* choc;

output [show(banana), " - ", show(choc)]