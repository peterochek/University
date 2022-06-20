composite(1).

init(Num) :-
    build(2, Num).

build(Prime, Num) :-
    prime(Prime),
    Square is Prime * Prime,
    sieve(Square, Num, Prime).
build(Prev, Num) :-
    Prev < Num,
    Next is Prev + 1,
    build(Next, Num).
sieve(Comp, Num, Step) :-
    Comp < Num,
    assert(composite(Comp)),
    Next is Comp + Step,
    sieve(Next, Num, Step).

prime(X) :-
    \+ composite(X).

small(Num, Sqrt, Sqrt) :-
    0 is Num mod Sqrt, !.
small(Num, Rem, Prime) :-
    Next is Prime + 1,
    small(Num, Rem, Next).

prime_divisors(1, []) :- !.
prime_divisors(Num, [Num]) :-
    prime(Num), !.
prime_divisors(Num, List) :-
    number(Num), !,
    prime_divisors(Num, List, 2).
prime_divisors(Num, List) :-
    double_check(Num, List).

prime_divisors(Num, [Num], _) :-
    prime(Num), !.
prime_divisors(Num, [Head | Tail], Prime) :-
    small(Num, Head, Prime),
    Next is Num // Head,
    prime_divisors(Next, Tail, Head).

double_check(Rem, [Head1, Head2 | Tail]) :-
    Head1 =< Head2,
    prime(Head1),
    prime_divisors(Rem1, [Head2 | Tail]),
    Rem is Rem1 * Head1.

convert(0, _, []).

convert(Num, Base, [Rem|Xs]) :-
   Num > 0,
   Div is Num // Base,
   Rem is mod(Num, Base),
   convert(Div, Base, Xs).

prime_palindrome(Num, Base) :-
   prime(Num),
   convert(Num, Base, List),
   reverse(List, List).