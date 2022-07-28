/*

GROUP 04
DEVINDI G.A.I	: E/17/058
LIYANAGE S.N 	: E/17/190

*/

/*
*  cool.y
*  Parser definition for the COOL language.
*
*/

%{
  #include <iostream>
  #include "cool-tree.h"
  #include "stringtab.h"
  #include "utilities.h"
  
  extern char *curr_filename;
  
  
  /* Locations */
  #define YYLTYPE int              /* the type of locations */
  #define cool_yylloc curr_lineno  /* use the curr_lineno from the lexer
  for the location of tokens */
    
    extern int node_lineno;          /* set before constructing a tree node
    to whatever you want the line number
    for the tree node to be */
      
      
      #define YYLLOC_DEFAULT(Current, Rhs, N) \
      Current = Rhs[1]; \
      node_lineno = Current;
    
    
    #define SET_NODELOC(Current) \
    node_lineno = Current;
    
    /* IMPORTANT NOTE ON LINE NUMBERS
    *********************************
    * The above definitions and macros cause every terminal in your grammar to 
    * have the line number supplied by the lexer. The only task you have to
    * implement for line numbers to work correctly, is to use SET_NODELOC()
    * before constructing any constructs from non-terminals in your grammar.
    * Example: Consider you are matching on the following very restrictive 
    * (fictional) construct that matches a plus between two integer constants. 
    * (SUCH A RULE SHOULD NOT BE  PART OF YOUR PARSER):
    
    plus_consts	: INT_CONST '+' INT_CONST 
    
    * where INT_CONST is a terminal for an integer constant. Now, a correct
    * action for this rule that attaches the correct line number to plus_const
    * would look like the following:
    
    plus_consts	: INT_CONST '+' INT_CONST 
    {
      // Set the line number of the current non-terminal:
      // ***********************************************
      // You can access the line numbers of the i'th item with @i, just
      // like you acess the value of the i'th exporession with $i.
      //
      // Here, we choose the line number of the last INT_CONST (@3) as the
      // line number of the resulting expression (@$). You are free to pick
      // any reasonable line as the line number of non-terminals. If you 
      // omit the statement @$=..., bison has default rules for deciding which 
      // line number to use. Check the manual for details if you are interested.
      @$ = @3;
      
      
      // Observe that we call SET_NODELOC(@3); this will set the global variable
      // node_lineno to @3. Since the constructor call "plus" uses the value of 
      // this global, the plus node will now have the correct line number.
      SET_NODELOC(@3);
      
      // construct the result node:
      $$ = plus(int_const($1), int_const($3));
    }
    
    */
    
    
    
    void yyerror(char *s);        /*  defined below; called for each parse error */
    extern int yylex();           /*  the entry point to the lexer  */
    
    /************************************************************************/
    /*                DONT CHANGE ANYTHING IN THIS SECTION                  */
    
    Program ast_root;	      	/* the result of the parse  */
    Classes parse_results;      /* for use in semantic analysis */
    int omerrs = 0;             /* number of errors in lexing and parsing */
    %}
    
    /* A union of all the types that can be the result of parsing actions. */
    %union {
      Boolean boolean;
      Symbol symbol;
      Program program;
      Class_ class_;
      Classes classes;
      Feature feature;
      Features features;
      Formal formal;
      Formals formals;
      Case case_;
      Cases cases;
      Expression expression;
      Expressions expressions;
      char *error_msg;
    }
    
    /* 
    Declare the terminals; a few have types for associated lexemes.
    The token ERROR is never used in the parser; thus, it is a parse
    error when the lexer returns it.
    
    The integer following token declaration is the numeric constant used
    to represent that token internally.  Typically, Bison generates these
    on its own, but we give explicit numbers to prevent version parity
    problems (bison 1.25 and earlier start at 258, later versions -- at
    257)
    */
    %token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
    %token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
    %token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
    %token <symbol>  STR_CONST 275 INT_CONST 276 
    %token <boolean> BOOL_CONST 277
    %token <symbol>  TYPEID 278 OBJECTID 279 
    %token ASSIGN 280 NOT 281 LE 282 ERROR 283
    
    /*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
    /**************************************************************************/
    
    /* Complete the nonterminal list below, giving a type for the semantic
    value of each non terminal. (See section 3.6 in the bison 
    documentation for details). */
    
    
    /* Declare types for the grammar's non-terminals. */
    /* phyla are defined in cool-tree.aps */
    
    /* %type <type> nonterminal */
    
    %type <program> program						// complete program
    %type <classes> class_list					// collection of classes
    %type <class_> class						// single class
	
    %type <feature> feature						// features - methods, attributes
    %type <features> feature_list 				// list of features
    
    %type <formal> formal						// formal parameters		
    %type <formals> formal_list 				// list of formals
    
    %type <expression> expr 					// expression
    %type <expression> let_expr 				//  expression with let
    %type <expression> dispatch_expr			// dispatch expression
    %type <expressions> expr_list 				// list of expressions
    %type <expressions> block_expr				// block of expressions
    
    %type <case_> case_branch					// single case branch 
    %type <cases> case_branches					// set of case branches
    
    
    /* Precedence declarations go here. */
    
    %right ASSIGN			// assign is right associative
    %left NOT				// binary operators are left associative
    %nonassoc LE '<' '='	// comparisons are non associative
    %left '+' '-'
    %left '*' '/'
    %left ISVOID
    %left '~'
    %left '@'
    %left '.'
    %left '('
    %precedence LET			// avoid shift-reduce conflict involving the productions for let
    
    %%
    
    /* 
    	$n - semantic value n
    	@n - location of sub expression n
    	@$ - location of the whole grouping
    	$$ - holds the result of the rule
    */
    
    
    
    /*___________________________ Grammer rules ___________________________*/
    
   
    /* Save the root of the abstract syntax tree in a global variable. */   
    
    program: class_list 
    	{ 
          	@$ = @1 ;
            ast_root = program($1);
		};
		
		
	/* Production for set of classes [[class;]]+  */
	class_list:
		//single class 
		class 					
		{
			$$ = single_Classes($1);
			parse_results = $$;
		}
		// several classes-> left recursive rule
		| class_list class				
		{
			$$ = append_Classes($1, single_Classes($2));
			parse_results= $$;
		}
		// upon an error skip until ';' is read and assign nil_class as the result
		| error ';'						
			{ $$ = nil_Classes();}
		;
    
    
    
     /* Production for class -> class TYPE [inherits TYPE] { [[feature;]]*}  */
     /* function --> Class_ class_(Symbol name, Symbol parent, Features features, Symbol filename)*/
     
	class : 
		//class inheriting from object class with zero features
		CLASS TYPEID '{' '}' ';'
			// If no parent is specified, the class inherits from the Object class
			{$$ = class_($2, idtable.add_string("Object"), nil_Features(), stringtable.add_string(curr_filename));}
			
		//class inheriting from object class with features	
		| CLASS TYPEID '{' feature_list '}' ';'
			// If no parent is specified, the class inherits from the Object class.
			{ $$ = class_($2, idtable.add_string("Object"), $4, stringtable.add_string(curr_filename));}
		
		// class inheriting from another class with zero features	
      	| CLASS TYPEID INHERITS TYPEID '{' '}' ';'
      		//If a parent is specified, the class inherits from it -> value = $4
      		{ $$ = class_($2, $4, nil_Features(), stringtable.add_string(curr_filename));}
      		
      	// class inheriting from another class with features
		| CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';'
			//If a parent is specified, the class inherits from it -> value = $4
      		{ $$ = class_($2, $4, $6, stringtable.add_string(curr_filename));}
      	;
      	
     
     
    /* Production to define a set of features -> [[feature;]]*  */
      	
 	feature_list : 
 		// a feature followed by ; is a single feature
 		feature
     		{ $$ = single_Features($1);}
     	
     	// several features -> right recursive rule
     	| feature feature_list
     		{ $$ = append_Features(single_Features($1), $2);}
     	
     	// feature_list upon an error return empty features	
     	| error
     		{ $$ = nil_Features();}
     	;
     
    /*Productions to define a feature it can be either a method or an attribute */	
    /* function --> Feature method(Symbol name, Formals formals, Symbol return_type, Expression expr)*/ 
    /* function --> Feature attr(Symbol name, Symbol type_decl, Expression init) */
    	
	feature: 
		// methods without formal parameters -> ID ( ) : TYPE {expr}
		OBJECTID '(' ')' ':' TYPEID '{' expr '}' ';'
     		{ $$ = method($1, nil_Formals(), $5, $7);}
     		
     	// methods with formal parameters -> ID ([formal [[,formal]]*]) : TYPE {expr}
     	| OBJECTID '(' formal_list ')' ':' TYPEID '{' expr'}' ';'
     		{ $$ = method($1, $3, $6, $8);}
     		
     	// attributes without initialization -> ID : TYPE	
        | OBJECTID ':' TYPEID ASSIGN expr ';'
        	{ $$ = attr($1, $3, $5);}
        	
        //attributes with initialization -> ID : TYPE <- expr	
        | OBJECTID ':' TYPEID ';'
        	{ $$ = attr($1, $3, no_expr());}
        ;
     
     
     
     
    /* Productions to define set of formal parameters -> [formal [[,formal]]*]  */
    
 	formal_list: 
 		//list with single parameter
 		formal
     		{ $$ = single_Formals($1);}
     		
     	//several parameters separated by , -> right recursive rule	
     	| formal ',' formal_list
     		{ $$ = append_Formals(single_Formals($1), $3);}
     		
     	// feature_list upon an error return empty formals		
     	| error
     		{ $$ = nil_Formals();}
     	;
    
    
    /*Productions to define a formal parameter -> formal = ID:TYPE */ 	
    /* function --> Formal formal(Symbol name, Symbol type_decl)*/ 	
    
   	formal : OBJECTID ':' TYPEID
       		{ $$ = formal($1, $3); }
       	;
       	
     
				
	/*Productions to define expression	*/ 	
	 			
	expr : 
		OBJECTID ASSIGN expr
          	{ $$ = assign($1, $3); }

   		| IF expr THEN expr ELSE expr FI
          	{ $$ = cond($2, $4, $6); }

     	| WHILE expr LOOP expr POOL
          	{ $$ = loop($2, $4); }

      	| '{' block_expr '}'
          	{ $$ = block($2); }

    	| let_expr
          
      	| dispatch_expr
          
      	| CASE expr OF case_branches ESAC
  	        { $$ = typcase($2, $4); }

       	| NEW TYPEID
  		        { $$ = new_($2); }

       	| ISVOID expr
  	        { $$ = isvoid($2); }

		| expr '+' expr
		    { $$ = plus($1, $3); }

		| expr '-' expr
		    { $$ = sub($1, $3); }

		| expr '*' expr
		    { $$ = mul($1, $3); }

		| expr '/' expr
		    { $$ = divide($1, $3); }

	  	| '~' expr
		    { $$ = neg($2); }

	  	| expr '<' expr
		    { $$ = lt($1, $3); }

	  	| expr LE expr
		    { $$ = leq($1, $3); }

	  	| expr '=' expr
		    { $$ = eq($1, $3); }

	  	| NOT expr
		    { $$ = comp($2); }

	  	| '(' expr ')'
		    { $$ = $2; }

	  	| OBJECTID
		    { $$ = object($1); }

	  	| INT_CONST
		    { $$ = int_const($1); }

		| STR_CONST
		    { $$ = string_const($1); }

		| BOOL_CONST
		    { $$ = bool_const($1); }
		  
		;
      
      
    /*Production to define a list of expressions separated by, -> expr [[ ,expr]]*  */
      
  	expr_list : 
  		// only one expr
  		expr
     		{ $$ = single_Expressions($1);}
     	
     	// list of expr
		| expr_list ',' expr
     		{ $$ = append_Expressions($1, single_Expressions($3));}
     		
     	// expr_list upon an error return empty expressions
     	| error
     		{ $$ = nil_Expressions(); }
     	;
     	
     	
     	
    /* Productions to define dispatch expressions of the form 
		expr[@TYPEID].OBJECTID(expr [[,expr]]*]) and 
		ID ([expr [[,expr]]*)
    */
    /* fuction -->  dispatch(Expression expr, Symbol name, Expressions actual) */
    
    dispatch_expr : 
    	// expr.OBJECTID()  
  	 	expr '.' OBJECTID '(' ')'
  	        { $$ = dispatch($1, $3, nil_Expressions()); }
  	        
  	    //expr.OBJECTID( expr[[,expr]]*)
  		| expr '.' OBJECTID '(' expr_list ')'
  	        { $$ = dispatch($1, $3, $5); }  

    	//expr @TYPEID.OBJECTID() 
    	| expr '@' TYPEID '.' OBJECTID '(' ')'
  	        { $$ = static_dispatch($1, $3, $5, nil_Expressions()); }
  	 	
  	 	//expr @TYPEID.OBJECTID(expr [[,expr]]*])
  	 	| expr '@' TYPEID '.' OBJECTID '(' expr_list ')'
  	        { $$ = static_dispatch($1, $3, $5, $7); } 
  	    
		//ID ()
        | OBJECTID '(' ')'
			{ $$ = dispatch(object(idtable.add_string("self")), $1, nil_Expressions()); }
		//ID ( [expr [[,expr]]* )	
		| OBJECTID '(' expr_list ')'
			{ $$ = dispatch(object(idtable.add_string("self")), $1, $3); }
		;
  	
  	
  	
  	
  	/*Production to define branches in case expression-> [[ID: TYPE => expr;]]+  */
  	
  	case_branches : 
  		// single case branch of the form ID:TYPE => expr;
  		case_branch ';'
  			{ $$ = single_Cases($1);}
  			
  		//multiple case branches [[ID:TYPE => expr;]]	
        | case_branches case_branch ';' 
    		{ $$ = append_Cases($1, single_Cases($2));}
    	;
           
              
    /*production of a case branch ID:TYPE => expr */  
    /* function --> Case branch(Symbol name, Symbol type_decl, Expression expr) */
          
   	case_branch: 
   		OBJECTID ':' TYPEID DARROW expr
     		{ $$ = branch($1, $3, $5); }
     	;
     		
     
     
     
    /*Production to define let construct
	to give precedence to the let constructs from end to begining %prec is used*/	
	/* function --> Expression let(Symbol identifier, Symbol type_decl, Expression init, Expression body) */
			
   	let_expr : 
   		LET OBJECTID ':' TYPEID IN expr %prec LET
            { $$ = let($2, $4, no_expr(), $6); }
   
   		| LET OBJECTID ':' TYPEID ASSIGN expr IN expr %prec LET
     		{ $$ = let($2, $4, $6, $8); }
        ;
     
     
	 	 			
    /*Production to define a block of expressions separated by ; -> [[expr;]]+ */
     			
    block_expr :  
    	// single expression
   		expr ';'
  	   		{ $$ = single_Expressions($1); }
  	   		
  	   	// multiple expressions
  	  	| block_expr expr ';'  
        	{ $$ = append_Expressions($1, single_Expressions($2)); }
  	    
  	    // expr_list upon an error return empty expressions      
  	   	| error
	       	{ $$ = nil_Expressions(); }
	  	;
	  	
	  	
	  	
    /*___________________________ End of Grammer ___________________________*/
    
    
    %%
    
    /* This function is called automatically when Bison detects a parse error. */
    void yyerror(char *s)
    {
      extern int curr_lineno;
      
      cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
      << s << " at or near ";
      print_cool_token(yychar);
      cerr << endl;
      omerrs++;
      
      if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
    }
    
    