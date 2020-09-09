/* some comments
    /* Author: Tanishq */
*/
likes(john, mary). % john likes marry
likes(john, trains).
likes(peter, fast_cars).
likes(Person1, Person2):- hobby(Person1, Hobby), hobby(Person2, Hobby).
hobby(john, trainspotting).
hobby(tim, sailing).
hobby(helen, trainspotting).
hobby(simon, sailing).
