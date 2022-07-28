(* Redefine attributes or methods in the same scope *)
(*Redefine classes *)

class A {
    a : Int;
    a : Int;
    f0():Int{0};
};

(*class A {
    a : String;
};*)


class B inherits A{
    b : Int;
    b : String;
    f0():String{"a"};
};


class C {
    f1() : Int {
        0
    };
    f1(x : Int) : Int {
        0
    };
};

class D {
    f2() : Int {
        0
    };
    f2() : Int {
        0
    };
};

class Main {
    main() : Int {
        0
    };
};
