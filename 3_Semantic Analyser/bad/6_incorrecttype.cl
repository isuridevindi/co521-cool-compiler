(* Initialize attribute with non-conforming type *)

class A {
    a : Int;
};

class B inherits A {
    b : Int;
};

class C inherits B {
    c : Int;
};

class D {
    d : Int;
   
};

class E {
    a1 : B <- new B;
    a2 : B <- new A;
    a3 : B <- new C;
    a4 : B <- new D;

   f1(): Int{
    if a4<=0 then 0 else 1 fi
   };

   method2(num1 : B, num2 : Int) : B {  -- plus
      (let x : Int in
	 {
            x <- num1 + num2;
	    
	 }
      )
   };
    
};

class Main {
    main() : Int {
        0
    };
};
