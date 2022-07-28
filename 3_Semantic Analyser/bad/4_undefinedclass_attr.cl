

class A {
(* Creating object of non-existent class *)
    a : C;
    b: Int;
	(*Use attribute not existing in scope *)
    f1() : Int {
        {
	b <- d;
	c <- b;
	0;
	}
    };
};

(* Class being inherited from does not exist *)

class B inherits A {
    b : Int;

};

class Main {
    main() : Int {
        0
    };
};

