% alice.
% father(X). % X is a father.
% brother(X,Y). % X is a brother of Y.

% head. % a fact
% body. % another fact
% head :- body. % a rule (YOU HAVE TO DEFINE PREDICATES BEFORE YOU USE THEM)

% variables: X, Y
% constants: alice, bob
% functions: father
% predicate: brother(X,Y)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Arithmetic:
% X is Y. % X is the value of Y
% X =:= Y. % X is equal to Y
% X =\= Y. % X is not equal to Y
% X < Y. % X is less than Y

% Note that at the moment of evaluation, these variables must be instantiated

p(X) :- X is 3+5. % X is 8 --> MUST BE INSTANTIATED before evaluation
q(X) :- Y is 3*4, X is Y.
/** <examples>
?- p(X).
?- q(X).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Example: list length
%%% length cannot be modified because it's a built-in predicate %%%
s(N) :- N is N+1. % successor function --> VALUE OF N IS UNIFIED WITH VALUE OF N+1

% length(L, N) is true if N is the length of list L
length([],0).
length([_|L], s(N)) :- length(L,N). % _ is a don't care variable

% OR

length2([],0).
length2([_|L], N) :- length2(L,N1), N is N1+1.

/** <examples> 
% ?- length([a,b], R).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% member(X, L) is true if X is a member of list L
member(X, [X|_]). % true if it's the head of the list
member(X, [_|L]) :- member(X,L). % otherwise check the tail

/** <examples> 
?- member(a, [a|[b|[c|[]]]]). 
% shortcut: [a,b,c] is the same as [a|[b|[c|[]]]]
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% append(L1, L2, L) is true if L is the concatenation of L1 and L2
append([], L, L).
append([X|L1], L2, [X|L]) :- append(L1, L2, L). % start with the second list, then append one by one the elements of the first list

/** <examples> 
?- append([a,b], [c], Z).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % type control through the use of predicates
list([]).
list([_|L]) :- list(L).

/** <examples> 
% ?- list([a,b,c]). % true
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% delete(X, LB, LS) is true if LS is the list LB without the element unifying with X
delete(X, LB, LS) :-    append(F, [X|R], LB),
                        append(F, R, LS).

delete(X, [X|R], R). % base case (X is the head of the list)
delete(X, [Y|L], [Y|R]) :- delete(X, L, R). % recursive case (X is not the head of the list)

/** <examples>
?- delete(a, [c, d, a, b, a], Z).
?- delete(a, [X|Z], [a, b, a, c]).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% merge(L1, L2, L) is true if L is the merge of L1 and L2 (ordered lists)

merge([], L, L).
merge(L, [], L).
merge([X|L1], [Y|L2], [X|L]) :- X =< Y, merge(L1, [Y|L2], L).
merge([X|L1], [Y|L2], [Y|L]) :- X > Y,  merge([X|L1], L2, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% split(L, L1, L2) is true if L1 and L2 are the two halves of L
split([], [], []).
split([X|L], [X|L1], L2) :- split(L, L2, L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% build a tree
tree(Root, Left, Right). % defining a tree
empty(void). % defining an empty tree
build_tree(X, T1, T2, tree(X, T1, T2)). % tree(X, T1, T2) is the tree with root X, left subtree T1 and right subtree T2
left_tree(tree(Root, Left, Right), Left).

pre_visit(void, []).
pre_visit(tree(Root, Left, Right), [Root|L]) :- 
    pre_visit(Left, L1),
    pre_visit(Right, L2),
    append(L1, L2, L).

rotate_left(
    tree(Root1, A, tree(Root2, B, C)), 
    tree(Root2, tree(Root1, A, B), C)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% PREDICATE CUT !
% cut is used to avoid backtracking --> prune useless branches of the search tree

% example
compare(X, Y, minor) :- X < Y.
compare(X, Y, equal) :- X = Y.
compare(X, Y, major) :- X > Y.

% how does cut work?
% it FIXES all the previous choices, that cannot be changed anymore (so you cannot backtrack anymore)

/** <examples>
?- compare(10, 20, X), X=major
% this will return FALSE, but it will try to backtrack and find another solution
% however, the three options are mutually exclusive, so it's useless to backtrack
*/

% instead of:
member(X, [X|_]).
member(X, [_|L]) :- member(X,L).

/** <examples>
?- member(a, [c,Z]).
*/

% we can write:
member2(X, [X|_]) :- !. % if it's the head of the list, cut (and don't try and find other solutions)
member2(X, [_|L]) :- member2(X,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% fail predicate

not_member(X, []). % base case (X is not a member of the empty list)
not_member(X, [X|_]) :- !, fail. % if X is the head of the list, fail
not_member(X, [_|L]) :- not_member(X,L). % otherwise, check the tail

% when X unifies with the head of the list, the cut is evaluated. this doesn't allow backtracking, so the predicate fails

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set([], []). % base case
set([X|L], [X|S]) :- not_member(X, L), !, set(L, S). % if X is both in L and S then fail
set([X|L], S) :- set(L, S). % if the head of the list of L is not in S, then add it to S

/** <examples>
?- set([1,2,1], S)
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% flatten(L, F) is true if F is the flattened version of L

% flatten([X|L], F) :- flatten(X, F1), flatten(L, F2), append(F1, F2, F).
% flatten(X, [X]) :- constant(X), X=!=[].
% flatten([], []).

