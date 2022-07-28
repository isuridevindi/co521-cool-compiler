(* Repeat attribute in inherited class *)
(* Function redefinition with different signature or return type *)

class A {
    a : Int;
    f1: Int; --this is valid
    f1() : Int {
        0
    };

};

class B inherits A {
    a : Int;
    f1() : String {
        "Hello"
    };
};

class C {
    c : Int;
    f2(x : Int,y: Bool) : Int {
        0
    };
    f2:Int; --this is valid
};

class D inherits C {
    c : String;
    d : Int;
    d : Bool;

    f2(x : String,y:Int) : Int {
        0
    };
};

class E {
    f3(x : Int, y : Int) : Bool {
        true
    };
};

class F inherits E {
    f3(x : Int) : Bool {
        true
    };
    f: Int;
    x: Int;
    f: Int;
    f4():Int{a};
};

class Main {
    main() : Int {
        0
    };
};

