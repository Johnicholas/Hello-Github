(* Examples of legitimate database statements are: *)
likes(john, X) :- likes(X,wine , female(X).
likes(joan,wine).
likes(alice,candy).
male(john).
female(alice).
female(joan).

(* An example of a legitimate query for this database would be: *)
?- likes(john,Who) .

