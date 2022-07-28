/*
_________________________________

GROUP 04
DEVINDI G.A.I	: E/17/058
LIYANAGE S.N 	: E/17/190	
_________________________________

*/
#include <algorithm>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <vector>
#include <stack>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

/*Define a macro to increase the error count and log the error
with the file name and line number when an error is found.


class_node: class containing the error

node:The node at which the error occurred
The line number is taken as the line number of the node 
with the error.
*/
#define LOG_ERROR(class_node,node)\
    semant_error();                                     \
    error_stream                                        \
    << class_node->get_filename()         \
    << ":" << node->get_line_number() << ": "


//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    classes_table.enterscope();
    symbol_table.enterscope();
    method_table.enterscope();
    //Record the basic classes, attributes and methods
    classes = install_basic_classes(classes);

    int classes_len = classes->len();
    
    /* Install all classes defined in the program and store
        them in class table if there are no errors
    */
    for (int i =  0; i < classes_len; i++) {
        class__class* current_class = static_cast<class__class*>(classes->nth(i));
        /*check if the class is previously defined and stored in the class table*/
        class__class* previous_def = static_cast<class__class*>(classes_table.lookup(current_class->get_name()));
       
        /*Error handling in class names
         1.Error 1:  SELF_TYPE is an invalid name for a class
         2.Error 2: Classes cannot be redefined
        */
        if (current_class->get_name() == SELF_TYPE) {
            LOG_ERROR(current_class,current_class)
                << "SELF_TYPE is an invalid name for a class." << endl;
            return;
        }

        if (previous_def != NULL) {
            LOG_ERROR(current_class,current_class)
                << "Class " << current_class->get_name() << " was previously defined." <<endl;
            return;
        }

        /*If no errors occur add the class to the class table*/
        classes_table.addid(current_class->get_name(), current_class);
    }

    // Check the validity of the inheritance graph
    bool is_inheritance_graph_valid = true;


    for (int i = 0; i < classes_len; i++) {
        class__class* current_class = static_cast<class__class*>(classes->nth(i));
        std::vector<Symbol> parents;
        /*For each class (except Object), go up the tree and get the parents and check the validity of parents
        Note: if Object class is reached, the root of the inheritance graph is reached-> terminate
        If all parents are valid, check for cycles in the inheritance graph
        */
        while (current_class->get_name() != Object) {
            parents.push_back(current_class->get_name());
            Symbol parent_symbol = current_class->get_parent();
            class__class* parent = static_cast<class__class*>(classes_table.lookup(parent_symbol));
           
           /*Error handling in parent validity checking
            1.Error 1: If parent is not in class table-> parent not defined
            2.Error 2: If parent is the class Integer, String, Bool or SELF_TYPE -> invalid parent
            */
           
            if (parent == NULL)  {
                is_inheritance_graph_valid = false;
                LOG_ERROR(current_class,current_class)
                << "Class " << current_class->get_name()
			    << " inherits from an undefined class " << parent_symbol << "."
                << endl;
                break;
            }

            if (parent_symbol == Int ||
                parent_symbol == Str ||
                parent_symbol == Bool ||
                parent_symbol == SELF_TYPE) {
                is_inheritance_graph_valid = false;
                LOG_ERROR(current_class,current_class)
                << "Class " << current_class->get_name()
                << " inherits from " << parent_symbol
                << ", which is not allowed."
                << endl;
                break;
            }

            //Check for inheritance cycles by going through all the classes in parents vector for a particular class

            /*Errors handled
                1. Error : If the parent of a class X is already in its parents vector
                          -> graph contains cycles 
                          -> log the error with the class involved in the cycle.

            */
            bool inheritance_cycle = false;
            for (size_t j = 0; j < parents.size(); j++) {
                if (parents[j] ==  parent_symbol) {
                    inheritance_cycle = true;
                    LOG_ERROR(current_class,current_class)
                   << "Class " << current_class->get_name()
			       << " or an ancestor of " << current_class->get_name()
                   << " is involved in an inheritance cycle." 
			       << endl;
                    break;
                }
            }

            //If inheritance cycles are present, the inheritance graph is invalid
            if (inheritance_cycle) {
                is_inheritance_graph_valid = false;
                break;
            }
            
            //If the current class is not already stored as a child in parent,
            //store it as a child
            if (!parent->has_child(current_class->get_name())) {
                parent->children.push_back(current_class->get_name());
            }
            //Move to the parent of the current class and move up the tree
            //and check the validity of inheritance graph
            //until root is reached
            current_class = parent;
        }
    }

    /*If the inheritance graph is invalid, 
    quit semantic analysing further
    */
    if (!is_inheritance_graph_valid) {
        return;
    }

    /*If inheritance graph is valid and class definitions are correct
    proceed to type checking by traversing the AST and gathering details
    of methods and attributes in every class
    */

    /* For each class starting from root, traverse the AST 
        and add the methods and attributes to the symbol tables
    */
    class__class* root_class = static_cast<class__class*>(classes_table.lookup(Object));
    decl_class(root_class);
    
    /*Before moving to type checking, 
    check for possible errors invloving Main class and main method*/

    //Get the Main class from the classes table
    class__class* main_class = static_cast<class__class*>(classes_table.lookup(Main));
    //If Main class is not present-> log error
    if (main_class == NULL) {
        semant_error();
        error_stream << "Class Main is not defined." << endl;
    }

    //If main class is present, check for the validity in main method
    else{
        //retrieve the methods defined in Main class scope
        method_table = method_tables_by_classes[main_class -> get_name()];
        //if main method is not defined in Main class-> log error
        if( method_table.lookup(main_meth) == NULL){
            LOG_ERROR(main_class,main_class)
            << " No 'main' method in class Main. " 
            << endl;
        }

        //if main method is defined, check the validity of the method
        else{
            //Get the details of the formal parameters and return type of the main method
            MethodDeclarations mainmethods = method_table.lookup(main_meth);
               /*Error handling:
                1. Error: main method in Main class should not contain formal parameters
               */
                for (MethodDeclaration def : *mainmethods) {

                    if( def.argument_types.size() != 0){
                        LOG_ERROR(main_class,main_class) 
                        << "'main' method in class Main should have no arguments." << endl;
                    

                    }
            
                }
        }

    }

    /*If main method in Main class is valid, 
      proceed to type checking in all other classes
      starting from the root_class (Object)
    */
    type_check_class(root_class);

}


/*
decl_class method: Recursive method to go through the inheritance
graph and 
for each class go through the feature list,
extract the attributes and methods separately and
store them in separate symbol tables
*/
void ClassTable::decl_class(class__class * current_class) {
    //Get the feature list of a class
    Features features = current_class->get_features();
    int features_len = features->len();

    /*
      Enter a new scope of both symbol table and method table 
      (to store the attributes and methods of the current class),
      with the parent of the new scope as the scope of the previous class
      Ultimately the scopes of children will recide inside parent's scope
    */
    symbol_table.enterscope();
    method_table.enterscope();
    
    /*
    Go through all the features in a class 
    and declare them and store them in symbol table or method table
    depending on the feature type (attr/method)
    */
    for (int i = 0; i < features_len; i++) {
        Feature feature = features->nth(i);
        switch (feature->feature_type()) {
        case FeatureType::attr:
            /*If feature type is attribute, store the attribute in symbol table
            using the decl_attr method
            */
            decl_attr(static_cast<attr_class*>(feature), current_class);
            break;
        case FeatureType::method:
            /*If feature type is method, store the method in method table
            using the decl_method method
            */
            decl_method(static_cast<method_class*>(feature), current_class);
            break;
        }
    }

    /*Since method table and symbol table are already in the scope of current class,
    store them in method_tables_by_classes and symbol_tables_by_classes
    that store the methods and symbols with class name as the key
    --> This way we can easily extract the attributes and methods belonging to
    a specific class 
    */
    method_tables_by_classes.emplace(current_class->get_name(), method_table);
    symbol_tables_by_classes.emplace(current_class->get_name(), symbol_table);

    /*Move on to the child of the current class
    and recursively call the decl_class method to
    record all the attributes and methods of all the classes 
    present in the program.
    (go through the inheritance graph from root to leaves)
    */
    for(Symbol child: current_class->children) {
        Class__class* child_class = classes_table.lookup(child);
        decl_class(static_cast<class__class*>(child_class));
    }
    
    /*
        Exit from the current scope in method and symbol tables.
       `exitscope' makes the table point to the parent scope of the
       current scope without deallocating the old child scope.
       This way all the scopes of children will recide inside parent scopes.  
    */
    method_table.exitscope();
    symbol_table.exitscope();
}

/*
decl_attr method:
for each attribute of a class, check the validity of the 
attribute and 
if attribute is valid, store it in the current scope of the 
symbol table.

According to cool manual,
No attribute name may be defined multiple times in a class, 
but a method and an attribute may have the same name.

Error identified:
1. Error 1: attribute type is not defined in the program
2. Error 2: attribute cannot be named as "self"
3. Error 3: attribute cannot be redefined inside same class
4. Error 4: an inherited attribute cannot be redefined inside child class
*/
void ClassTable::decl_attr(attr_class* current_attr, class__class* current_class) {
    //Get the attribute type
    Symbol attr_type = current_attr->get_type();
    //Check if attribute type (the Class) is defined in the program
    Class_ type = classes_table.lookup(attr_type);

    /*If the attribute type class is not defined (type==NULL) and it's not SELF_TYPE 
     Error 1: attribute type is not defined in the program

     According to cool manual: SELF_TYPE can be  the declared type of an attribute
    */

    if (attr_type != SELF_TYPE && type == NULL) {
        LOG_ERROR(current_class, current_attr)
	    << "Class " << attr_type << " of attribute " << current_attr-> get_name()
            << " is undefined. " <<  endl;
        return;
    }

    /*
        According to cool manual:  It is illegal to have attributes named self.
        Error 2: attribute cannot be named as "self"
    */
 
    if (current_attr->get_name() == self) {
        LOG_ERROR(current_class,current_attr)
            << "'self' cannot be the name of an attribute" << endl;
        return;
    }
    
    //Check if attribute is previously defined in somewhere in the program. If defined previously, previous_def != NULL
    Symbol previous_def = symbol_table.lookup(current_attr->get_name());

    // check if the attribute in same class is repeated. If the attribute in same class is repeated probing_val != NULL
    Symbol probing_val = symbol_table.probe(current_attr->get_name()); 

    /*
    If previous_def != NULL and probing_val!=NULL->
    The attribute is redefined in the same class (not redefining an inherited attr)
    
    Error 3: attribute cannot be redefined inside same class
    */

    if (previous_def != NULL && probing_val != NULL) {

        LOG_ERROR(current_class, current_attr)
	    << "Attribute " <<  current_attr->get_name() 
            << " is multiply defined in class " << current_class->get_name()
            << "." 
            << endl;
        return;
    }

     /*
    If previous_def != NULL and probing_val==NULL->
    An inherited attribute is redefined in the child class 
    
    Error 4: an inherited attribute cannot be redefined inside child class
    */
    if (previous_def != NULL && probing_val == NULL){

        LOG_ERROR(current_class, current_attr)
	    << "Class " << current_class->get_name()
	    << " is redefining the attribute " << current_attr->get_name() 
	    << " in an inherited class."
	    << endl;
        return;


    }

    /*
    If attribute is valid, add it to the symbol table under the scope
    of the current class
    */
    symbol_table.addid(current_attr->get_name(), current_attr->get_type());
   
}

/*
decl_method method:
for each method in a class, check the validity of the 
method definition and formal parameters
if method declaration and parameters are valid, store in
current scope of method table

According to cool manual,
No method name may be defined multiple times in a class, 
but a method and an attribute may have the same name.

Error identified:
 * Formal parameter errors:

    1. Error 1: formal parameter name cannot be "self"
    2. Error 2: formal parameters cannot be defined multiple times for the same method
   
 * Method declaration errors:
    3: Error 3: cannot have return type or argument type that is not defined in program
    4. Error 4: cannot redefine method in same class

    According to cool manual:
   " To ensure type safety, there are restrictions on the redefinition of inherited methods. The rule is
    simple: If a class C inherits a method f from an ancestor class P, then C may override the inherited
    definition of f provided the number of arguments, the types of the formal parameters, and the return
    type are exactly the same in both definitions "

    5. Error 5: if inherited method is redefined, they cannot have different return types
    6. Error 6: if inherited method is redefined, they cannot have different number of formal parameters
    7. Error 7: if inherited method is redefined, the formal parameters cannot have different types
*/
void ClassTable::decl_method(method_class* current_method, class__class* current_class)  {
    //Get the formal parameters of the method
    Formals formals = current_method->get_formals();
    int formals_len = formals->len();
    //Vector to keep track of the valid parameters in method
    std::vector<Symbol> arg_names;
    //Go through each formal parameter
    for(int i = 0; i < formals_len; i++) {
        //Get the name of the formal parameter
        Symbol name = (static_cast<formal_class*>(formals->nth(i)))->get_name();
       
        /*
        Error 1: formal parameter name cannot be "self"
        */
        if (name == self) {
            LOG_ERROR(current_class, current_method)
		    << "'self' cannot be the name of a formal parameter as shown in method " 
            << current_method->get_name()
            << " in Class " << current_class->get_name() << "." << endl;
        }
        /*
        Go through the valid parameters stored in arg_names.
        If a parameter already exists in arg_names with the same name as the current parameter,
        it's defined multiple times in the same method.
         
        Error 2: formal parameters cannot be defined multiple times for the same method

        */
        for (Symbol other_name : arg_names)  {
            if (name == other_name) {
                LOG_ERROR(current_class, current_method)
                << "Formal parameter "<< name 
                << " is multiply defined in method "<< current_method->get_name()
                << " in Class " << current_class->get_name()
                << "." << endl;
                return;
            }
        }

        //If formal parameter is well defined, put it in arg_names vector
        arg_names.push_back(name);
    }

    /*Store the details in the current method in a MethodDeclaration structer using
    the method from_method_class()
    extract the return type, formal parameter identifiers and their types in the variables
    of the MethodDeclaration struct's variables return_type, identifiers (vector), and argument_types(vector)
    */
    MethodDeclaration new_def = MethodDeclaration::from_method_class(current_method);

    /*
    Check whether the return type or any formal parameter types are undefined
    using the get_undeclared_types() method in MethodDeclaration struct
    if the size of the vector returned by get_undeclared_types() is not 0, 
    there are undeclared types in the method.

    Error 3: cannot have return type or argument type that is not defined in program
    */
    std::vector<Symbol> undeclared_types = new_def.get_undeclared_types(classes_table);
    if (undeclared_types.size() > 0) {
        for (Symbol type: undeclared_types) {
            LOG_ERROR(current_class,current_class)
                << "Undeclared type " << type << endl;
        }
        return;
    }

    /*
    Check whether the method is already in the method table 

    */
    MethodDeclarations method_def = method_table.lookup(current_method->get_name());

    /*
    if method is not in the method table, method is not previously defined-> method_def==NULL
    therefore add it to the method table
    */
    if (method_def == NULL)  {
        method_def = new std::vector<MethodDeclaration>;
        method_table.addid(current_method->get_name(), method_def);
    } 
    
    /*
    if method_def is not NULL, the method is previously defined-> either in the same class
                                                            or in a parent class

    */
    else {
	
	    /*Check if method is repeated from same class, 
        if so probe != NULL
        */
        MethodDeclarations method_prob = method_table.probe(current_method->get_name());

        /*
        If method_prob !=NULL, method is already defined in the same class.
        Error 4: cannot redefine method in same class
        */
        if(method_prob != NULL){ 
            LOG_ERROR(current_class,current_method) 
                << "Method " << current_method->get_name()
                << " is multiply defined in class " << current_class->get_name()<<"." << endl;

            return;
        }

        /*Proceed to next section
        If method_prob == NULL &&  method_def != NULL, 
        method from parent is redefined in child */

        for (MethodDeclaration def : *method_def) {
            /*
            According to the cool manual, 
            the return type must not be changed in redefinition of inherited method
            Error 5: if inherited method is redefined, they cannot have different return types
            */
	        if (new_def.return_type != def.return_type) {
                  	LOG_ERROR(current_class,current_method) 
                    << "In redefined method " << current_method->get_name()
                    << ", return type " << new_def.return_type
                    << " is different from original return type " << def.return_type << "." << endl;

                    return;
            }

            /*
            According to the cool manual, 
            number of formal parameters must be compatible with the original definition.

            Error 6: if inherited method is redefined, they cannot have different number of formal parameters
             */
 	        if (!new_def.has_same_args(def) && new_def.argument_types.size() != def.argument_types.size()) {
                    LOG_ERROR(current_class,current_method) 
			        << "Incompatible number of formal parameters in redefined method "
                    << current_method->get_name()
                    << " in Class " << current_class->get_name()
                    << "."  << endl;
                    return;
	        }

            /*
            According to the cool manual, 
            formal parameters types must be compatible with the original definition.

            Error 7: if inherited method is redefined, the formal parameters cannot have different types
            */
            if (!new_def.has_same_args(def)) {
                    LOG_ERROR(current_class,current_method) 
                    << "In redefined method " << current_method->get_name()
                    << ", parameter type(s) are different from original type(s)."
                    << endl;
                     return;
            }
	
        }
    }
    //if no errors are encountered, add the new method to the existing set of methods in the class
    method_def->push_back(new_def);
}

/*
type_check_class:
Recursive Method to check the correctness of the types in expressions of all user defined classes
starting from the root

For each class, check the type correctness of attributes using  type_check_attr() method
and check the type correctness of methods using  type_check_method() method

then move on to the child class and repeat the same procedure
*/

void ClassTable::type_check_class(class__class* current_class) {
    //Get the name of the current class
    Symbol class_name = current_class->get_name();
    //Check if the class is a basic class
    bool should_type_check = class_name != Object &&
        class_name != Int &&
        class_name != Str &&
        class_name != Bool &&
        class_name != IO;

    //If class is not a basic class and is user defined, proceed to type checking
    if (should_type_check) {
        //get the features of the class
        Features features = current_class->get_features();
        int features_len = features->len();

        //Change the scope of the symbol table and method table to the current class
        symbol_table = symbol_tables_by_classes[class_name];
        method_table = method_tables_by_classes[class_name];

        /*
        For each feature, call the correct type checking method depending on the feature type
            * if Feature is an attribute:type_check_attr()
            * if Feature is a method :type_check_ method()
        */
        for (int i = 0; i < features_len; i++) {
            Feature feature = features->nth(i);
            switch (feature->feature_type()) {
            case FeatureType::attr:
                type_check_attr(static_cast<attr_class*>(feature), current_class);
                break;
            case FeatureType::method:
                type_check_method(static_cast<method_class*>(feature), current_class);
                break;
            }
        }
    }

    // Proceed to the children classes and Type check children classes
    for(Symbol child: current_class->children) {
        Class__class* child_class = classes_table.lookup(child);
        type_check_class(static_cast<class__class*>(child_class));
    }
}

/*
type_check_attr(): 
method to check the type correctness of an attribute and its initialization
According to the cool manual:
If initialization is supplied,
the static type of the expression must conform to the declared type of the attribute.

Errors handled:
    1. Error: If type inferred by the initialization expression doesn't match with the 
    declared type
*/
void ClassTable::type_check_attr(attr_class* current_attr, class__class* current_class) {
    //get the initilization expression of the attribute
    Expression init = current_attr->get_init();
    //Get the declared type of the attribute
    Symbol attr_type = current_attr->get_type();
    Symbol return_type;

    /* if initialization expression is present (initialization expression is not a no_expr type), 
    check the type of the expression, compare if with the declared attr_type
    */
    if (init->expression_type() != ExpressionType::no_expr) {
        /*Get the infered type of the initialization expression 
            using type_check_expression() method
        */
        return_type= type_check_expression(init, current_class, attr_type);
        /* 
        if the inferred type (return_type) has the declared_type(attr_type) as an ancestor->
        inferred type is a subtype (child) of the declared type-> then the types are correct

        if not, 
        Error: If type inferred by the initialization expression doesn't match with the 
    declared type
        */
        if( !is_descendant(return_type, attr_type, current_class) ){

            LOG_ERROR(current_class, current_attr)
            << "Inferred type " << return_type
            << " of initialization of attribute " << current_attr-> get_name()
            << " does not conform to declared type " << attr_type <<"." << endl;
	    }
    }
}

/*
type_check_method(): 
method to check the type correctness of a method

According to the cool manual:
The type of the method body must conform to the declared return type

Errors handled:
    1. Error: If type inferred by the expressions in the method body doesn't match with the 
    declared return type

*/

void ClassTable::type_check_method(method_class* current_method, class__class* current_class) {
    //Get the expressions in the method body
    Expression expr = current_method->get_expression();
    //Get the declared return type of the method
    Symbol return_type = current_method->get_return_type();

    /*Enter the scope of the current class in the symbol table to
        access the attributes defined for the current class
    */
    symbol_table.enterscope();
	
    //Get the formal parameters of the method
    Formals formals = current_method->get_formals();
    int formals_len = formals->len();

    /*Add the formal parameters of the method to the symbol table 
    inside the scope of the current class
    */
    for(int i = 0; i < formals_len; i++) {
        Symbol name = (static_cast<formal_class*>(formals->nth(i)))->get_name();
        Symbol type = (static_cast<formal_class*>(formals->nth(i)))->get_type();
        symbol_table.addid(name, type);
    }

    /*
    With the formal parameters of the method added to the symbol table,
    proceed to check the type correctness of the expressions present in the method body
    and store the inferred type of the body as fin_type
    */
    Symbol fin_type = type_check_expression(expr, current_class, return_type);

    /* 
    if the inferred type (fin_type) has the declared_type(return_type) as an ancestor->
    inferred type is a subtype (child) of the declared type-> then the types are correct

    if not, 
    Error: If type inferred by the expressions in the method body doesn't match with the 
    declared return type

    */

    if (No_type != fin_type && !is_descendant(fin_type, return_type, current_class)) {
        LOG_ERROR(current_class, current_method)
            << "Inferred return type " << fin_type <<" of method "<< current_method-> get_name()<< " does not conform to declared return type " << return_type <<"."<< endl;
        fin_type = Object;

    }
    
    //Exit from the scope of the current class in symbol table after typechecking the method
    symbol_table.exitscope();
}
/*
Create three macros for code clarity in the type_check_expression() method

 * TYPE_CHECK_ARITH_EXPR_SAME_TYPE() & TYPE_CHECK_ARITH_EXPR(): define a token string to type check the arithmetic operations
 * TYPE_CHECK_UNARY(): define a token string to type check unary operators ~ and not
*/
#define TYPE_CHECK_ARITH_EXPR_SAME_TYPE(EXPR, CLASS, TYPE)  TYPE_CHECK_ARITH_EXPR(EXPR, CLASS, TYPE,  TYPE)

/*
According to the cool manual: 
    For arithmetic expressions,
    the left and right side expressions are first evaluated.
    The static types of the two sub-expressions must be Int.
    The static type of the expression is Int.

Error: type(s) of sub-expressions is/are not Int
*/
#define TYPE_CHECK_ARITH_EXPR(EXPR, CLASS, TYPE, OP_TYPE)   {   \
    final_type = TYPE; \
    Expression left_operand = static_cast<CLASS*>(EXPR)->get_left_operand(); \
    Expression right_operand = static_cast<CLASS*>(EXPR)->get_right_operand(); \
    left_type = type_check_expression(left_operand, current_class, OP_TYPE); \
    right_type = type_check_expression(right_operand, current_class, OP_TYPE); \
    ExpressionType exprtype = static_cast<Expression>(EXPR)-> expression_type();\
    if (left_type != Int || right_type != Int){\
    switch (static_cast<Expression>(EXPR)-> expression_type()){\
        case ExpressionType::lt:\
		LOG_ERROR(current_class,static_cast<CLASS*>(EXPR))\
		<< "non-Int arguments: " << left_type << " < "<< right_type << endl;\
		break;\
        case ExpressionType::leq:\
		LOG_ERROR(current_class,static_cast<CLASS*>(EXPR))\
		<< "non-Int arguments: " << left_type << " <= "<< right_type << endl;\
		break;\
	case ExpressionType::plus:\
		LOG_ERROR(current_class,static_cast<CLASS*>(EXPR))\
		<< "non-Int arguments: " << left_type << " + "<< right_type << endl;\
		break;\
	case ExpressionType::sub:\
		LOG_ERROR(current_class,static_cast<CLASS*>(EXPR))\
		<< "non-Int arguments: " << left_type << " - "<< right_type << endl;\
		break;\
	case ExpressionType::mul:\
		LOG_ERROR(current_class,static_cast<CLASS*>(EXPR))\
		<< "non-Int arguments: " << left_type << " * "<< right_type << endl;\
		break;\
	case ExpressionType::divide:\
		LOG_ERROR(current_class,static_cast<CLASS*>(EXPR))\
		<< "non-Int arguments: " << left_type << " / "<< right_type << endl;\
		break;\
	default:\
		break;\
    }\
}\
}

/*
For unary operations (~ and not),
    * The expression ~<expr>: <expr> must have static type Int
                            and the entire expression has static type Int. 
    * The expression not <expr>: <expr> must have static type Bool 
                            and the entire expression has static type Bool.

Error: expr type for ~ and not are Int and Bool
*/
#define TYPE_CHECK_UNARY(EXPR, CLASS, TYPE)   {   \
	final_type = TYPE; \
	expr_type = type_check_expression((static_cast<CLASS*>(EXPR))->get_operand(), current_class, TYPE);\
	if(final_type !=expr_type && !is_descendant(final_type,expr_type, current_class)){\
		switch (static_cast<Expression>(EXPR)-> expression_type()){\
			case ExpressionType::neg:\
				LOG_ERROR(current_class, static_cast<CLASS*>(EXPR))\
            			<< " Argument of '~' has type " << expr_type <<" instead of " << final_type<< "."<< endl;\
				break;\
			case ExpressionType::comp:\
				LOG_ERROR(current_class, static_cast<CLASS*>(EXPR))\
            			<< " Argument of 'not' has type " << expr_type <<" instead of " << final_type<< "."<< endl;\
				break;\
			default:\
				break;\
		}\
	}\
}

/*
type_check_expression:
method to check type correctness of the expressions related to a method or an attribute initialization
For each type of expression, 
specific type check methods are called 
and errors are identified for the specific expression type
*/
Symbol ClassTable::type_check_expression(Expression expr, class__class* current_class, Symbol expected_type) {
    /*
    final_type = type resolved for the specific expression (inferred)
    left_type = type of the left operand (if exists)
    right_type = type of the right operand (if exists)
    expr_type = type of the expression evaluated
    */
    
    Symbol final_type,left_type, right_type, name, expr_type; 
     
    // Handle different expressions
    switch(expr->expression_type()) {
    case ExpressionType::assign:
	    final_type = type_check_assign(static_cast<assign_class*>(expr), current_class);
	    break;
    case ExpressionType::static_dispatch:
	    final_type = type_check_static_dispatch(static_cast<static_dispatch_class*>(expr), current_class);
        break;
    case ExpressionType::dispatch:
        final_type = type_check_dispatch(static_cast<dispatch_class*>(expr), current_class);
        break;
    case ExpressionType::cond:
        final_type = type_check_cond(static_cast<cond_class*>(expr), current_class);
        break;
    case ExpressionType::typcase:
        final_type = type_check_typcase(static_cast<typcase_class*>(expr), current_class);
        break;
    case ExpressionType::block:
        final_type = type_check_block(static_cast<block_class*>(expr), current_class);
        break;
    case ExpressionType::loop:
        final_type =  Object;
        type_check_loop(static_cast<loop_class*>(expr), current_class);
        break;
    case ExpressionType::new_:
        final_type = type_check_new_(static_cast<new__class*>(expr), current_class);
        break;
    case ExpressionType::object:
        final_type = type_check_object(static_cast<object_class*>(expr), current_class);
        break;
    case ExpressionType::let:
        final_type = type_check_let(static_cast<let_class*>(expr), current_class);
        break;
    case ExpressionType::eq:
        final_type = Bool;
        type_check_eq(static_cast<eq_class*>(expr), current_class);
        break;
    case ExpressionType::lt:
    //will be replaced by the token string in macro definition
        TYPE_CHECK_ARITH_EXPR(expr, lt_class, Bool, Int);
        break;
    case ExpressionType::leq:
    //will be replaced by the token string in macro definition
        TYPE_CHECK_ARITH_EXPR(expr, leq_class, Bool, Int);
        break;
    case ExpressionType::neg:
    //will be replaced by the token string in macro definition
	    TYPE_CHECK_UNARY(expr, neg_class, Int);  
        break;
    case ExpressionType::comp:
    //will be replaced by the token string in macro definition
	    TYPE_CHECK_UNARY(expr, comp_class, Bool);
        break;
    case ExpressionType::plus:
    //will be replaced by the token string in macro definition
        TYPE_CHECK_ARITH_EXPR_SAME_TYPE(expr, plus_class, Int);
        break;
    case ExpressionType::sub:
    //will be replaced by the token string in macro definition
        TYPE_CHECK_ARITH_EXPR_SAME_TYPE(expr, sub_class, Int);
        break;
    case ExpressionType::mul:
    //will be replaced by the token string in macro definition
        TYPE_CHECK_ARITH_EXPR_SAME_TYPE(expr, mul_class, Int);
        break;
    case ExpressionType::divide:
    //will be replaced by the token string in macro definition
        TYPE_CHECK_ARITH_EXPR_SAME_TYPE(expr, divide_class, Int);
        break;
    case ExpressionType::isvoid:
    //final_type must be Bool
        final_type = Bool;
        type_check_expression(static_cast<isvoid_class*>(expr)->get_operand(), current_class, Object);
        break;
    //For string, bool, int constants the final type must be string,bool, int
    case ExpressionType::string_const:
        final_type = Str;
        break;
    case ExpressionType::bool_const:
        final_type = Bool;
        break;
    case ExpressionType::int_const:
        final_type = Int;
        break;
    //If there's no expression, the final type is set to No_type
    case ExpressionType::no_expr:
        final_type = No_type;
        break;
    //Log errors if invalid or unhandled expression types are encountered
    case ExpressionType::invalid:
        final_type = Object;
        cerr << "Invalid expression" << endl;
        break;
    default:
        cerr << "Unhandled expression type " << endl;
        break;
    }

    //Store the final_type to be returned
    Symbol return_fin = final_type;
    /* If final_type is not No_type and the final_type is not a descendent of expected type,
        it's a type error-> 
        As the error recovery mechanism, assign type Object to ill typed expressions
    */
    if (No_type != final_type && !is_descendant(final_type, expected_type, current_class)) {
       final_type = Object;
    }
    //Annotate the node with the type
    expr->type = final_type;
    return return_fin;
}

/*
type_check_assign():
Method to check the type correctness of an assignment

According to the cool manual:
   1. The static type of the expression must conform to the declared type of the identifier
   2. It is an error to assign to self
   3. It is an error to assign to an undeclared variable

Therefore the errors handled:
   1. Error 1: the name of the assignment identifier is 'self'
   2. Error 2: assigning to an undeclared variable
   3. Error 3: static type of expression not conforming to the declared type 

*/
Symbol ClassTable::type_check_assign(assign_class* assign, class__class* current_class) {
    Symbol type = Object;
    //Get the name of the identifier of assigment
    Symbol name = assign->get_name();
    /*
    Error 1: the name of the assignment identifier is 'self'
    */
    if (name == self) {
        LOG_ERROR(current_class, assign)
        << "Cannot assign to 'self'." <<  endl;
	    type = SELF_TYPE;
    } 
    else {  
        //check if the variable is defined       
        type = symbol_table.lookup(name);
        /*
        If variable is not defined,
        Error 2: assigning to an undeclared variable
        */
        if (type == NULL) {
            LOG_ERROR(current_class,assign)
            << "Assignment to undeclared variable " << name <<"."<< endl;
            type = Object;
	    }
    }
 
    /*
    check the type of the expression in the assignment
    */
    Symbol expr_type = type_check_expression(assign->get_expression(), current_class, type);

    /*
    If the expression type is not a subtype (descendent) of the type of the declared type of the identifier,
    and expression type is not No_type (no expression),
    then the assignment is ill formed.

    Error 3: static type of expression not conforming to the declared type 
    */
   if (!is_descendant(expr_type, type, current_class) && expr_type != No_type) {
        LOG_ERROR(current_class, assign)
	    << "Type " << expr_type 
    	<< " of assigned expression does not conform to declared type " << type 
	    << " of identifier " << name <<"."<< endl;
    }
    
    //Return the expr_type as the final type of the assignment expression
    return expr_type;
}

/*
type_check_static_dispatch:
check the type correctness of a dispatch in the form 
    <expr>@<dispatch_type>.f(arguments)

The correctness of the types involving the method (f) and the arguments (arguments/actual)
is checked by handle_dispatch() helper method.

According to the cool manual:
    For this form of dispatch, the static type to the left of
    “@”must conform to the type specified to the right of “@”.

Therefore, errors handled in this method:
    1. Error 1: dispatch_type class is not defined in the program 
    2. Error 2: the static type to the left of "@" not conforming to the
                type specified to the right of "@"

*/
Symbol ClassTable::type_check_static_dispatch(static_dispatch_class* static_dispatch, class__class* current_class) {
    //Get the type specified to the right of "@"
    Symbol dispatch_type = static_dispatch->get_type();
    //Check if the dispatch_type is defined in the program
    class__class* type_ptr = static_cast<class__class*>(classes_table.lookup(dispatch_type));
    /*
    if dispatch_type not specified-> 
    Error 1: dispatch_type class is not defined in the program 
    */
    if (type_ptr == NULL) {
        LOG_ERROR(current_class,static_dispatch)
        << "Static dispatch to undefined class "<< dispatch_type <<"."<< endl;
        return Object;
    }

    /*If dispatch type is defined, proceed to check the validity of the method
    and its arguments using handle_dispatch
    static_dispatch->get_object() = expr
    static_dispatch->get_name() = method f
    static_dispatch->get_arguments() = formal parameters of f
    */
    Symbol return_type = handle_dispatch(
        static_dispatch->get_object(),
        static_dispatch->get_name(),
        static_dispatch->get_arguments(),
        dispatch_type,
        current_class);

    //Proceed to check the validity of the dispatch expression 
    
    Expression expr = static_dispatch->get_object();
    //Check the type of the expression and store it in object_type
    Symbol object_type = type_check_expression(expr, current_class, dispatch_type);
    
    /*
     if method f has return type SELF TYPE, 
     then the static type of the dispatch is object_type
    */
    if (return_type == SELF_TYPE) {
        return_type = object_type;
    }

    /*
    However, if expression type (expression to the left of "@") is not a descendent
    of the dispatch_type (type specified to the right of "@"), error in static dispatch.
    Error 2: the static type to the left of "@" not conforming to the
                type specified to the right of "@"
    */
    if (!is_descendant(object_type,dispatch_type,current_class)){
        LOG_ERROR(current_class,static_dispatch)
        << "Expression type "<<object_type
        <<" does not conform to declared static dispatch type "
        << dispatch_type<<"."<<endl;
	    return Object;
	}

    /*
    If no errors occurr,
    The ultimate return type of a static dispatch is the return type of the method f or object_type
    */
    return return_type;
}

/*
type_check_dispatch:
check the type correctness of a dispatch in the form 
    <expr>.f(arguments)
            or 
    f(arguments) == self.f(arguments)
The correctness of the types involving the method (f) and the arguments/actual
is checked by handle_dispatch() helper method.

The final type of the dispatch expression is either
    * the type of the return type of the function f
    * type of the expression (if return type of function is SELF_TYPE)

errors related to the method and arguments are handled in handle_dispatch() method
*/
Symbol ClassTable::type_check_dispatch(dispatch_class* dispatch, class__class* current_class) {
    //Check the type of the expression and store it in object_type
    Symbol object_type = type_check_expression(dispatch->get_object(), current_class, Object);
   
    /*Proceed to check the validity of the method
    and its arguments using handle_dispatch
    static_dispatch->get_object() = expr
    static_dispatch->get_name() = method f
    static_dispatch->get_arguments() = formal parameters of f
    */
    Symbol return_type = handle_dispatch(
        dispatch->get_object(),
        dispatch->get_name(),
        dispatch->get_arguments(),
        object_type,
        current_class);
    /*
     if method f has return type SELF TYPE, 
     then the static type of the dispatch is object_type
    */
    if (return_type == SELF_TYPE) {
        return_type = object_type;
    }

    /*
    If no errors occurr,
    The ultimate return type of a dispatch is the return type of the method f or object_type
    */
    return return_type;
}
/*
handle_dispatch():
method used to evaluate the method and its arguments in a static or normal dispatch

According to the cool manual:

    Type checking a dispatch involves several steps.
        * Assume expr has static type A. 
        (expr can have a subclass of A, lets name it as C inferred at runtime)
        * Class A must have a method f,
        the dispatch and the definition of f must have the same number of arguments, 
        and the static type of the ith actual parameter must conform to the 
        declared type of the ith formal parameter
        * If f has return type B and B is a class name, 
        then the static type of the dispatch is B.
        * Otherwise, if f has return type SELF TYPE, 
        then the static type of the dispatch is A.

Therefore, the errors detected:
    1. Error 1: method f is not defined under the class of the type of expr
    2. Error 2: method invoked with wrong number of arguments in dispatch
    3. Error 3: ith formal parameter of the method doesn't conform to the declared type
*/
Symbol ClassTable::handle_dispatch(Expression expr,Symbol name, Expressions arguments,Symbol dispatch_type, class__class* current_class) {
    /*Store the types of the formal parameters given in dispatch method f
    Go through the argument list,
    check the type correctness of each expression
    and store the types
    */
    std::vector<Symbol> args;
    for (int i = 0; i < arguments->len(); i++) {
        Expression arg = arguments->nth(i);
        Symbol arg_type = type_check_expression(arg, current_class, Object);
        if (arg_type == SELF_TYPE) {
            arg_type = current_class->get_name();
        }
        args.push_back(arg_type);

    }

    /*
    If dispatch_type is SELF_TYPE-> it's the type of the current class
    */
    if (dispatch_type == SELF_TYPE) {
        dispatch_type = current_class->get_name();
    }

    /*
    Get the method_table for the dispatch_type class
    and check if the method given by name (f) is in the table
    */
   
    SymbolTable<Symbol, MethodDeclarations_> table = method_tables_by_classes[dispatch_type];
    MethodDeclarations method_decls = table.lookup(name);
  
   /*
    If the specified method is not in the dispatch_type class,

    Error 1: method f is not defined under the class of the type of expr
    */
    if (method_decls == NULL) {
        LOG_ERROR(current_class, expr)
        << "Dispatch to undefined method "<< name<< "." << endl;
        return Object;
	
    }

    class__class* dispatch_class = static_cast<class__class*>(classes_table.lookup(dispatch_type));

    Symbol return_type = nullptr;
   
    //If the method is defined, proceed to check the validity of the method declared in dispatch
    for (MethodDeclaration current_decl : *method_decls) {
        bool matches = true; //remains true if match method declaration is found
        /*
        If the method in dispatch has incompatible number of formal parameters 
        Error 2: method invoked with wrong number of arguments in dispatch
        */
        if( current_decl.argument_types.size() != args.size()){
            matches = false;
            LOG_ERROR(current_class, expr)
            << "Method " << name 
            << " invoked with wrong number of arguments." << endl;
            return_type = No_type;
            break;
            
        }

        // If number of formal parameters match, proceed to check whether the types match
        for (size_t i = 0; i < args.size(); i++) {
            /*
            If the type of the formal parameter declared in dispatch (args[i])
            is not an ancestor( subtype) of the formal parameter type in method declaration (current_decl.argument_types[i]),

            Error 3: ith formal parameter of the method doesn't conform to the declared type

            */
            if (!is_descendant(args[i], current_decl.argument_types[i], dispatch_class)) {
                matches = false;
                LOG_ERROR(current_class, expr)
                << "In call of method " << name 
                <<", type " << args[i] << " of parameter "<< current_decl.identifiers[i]
                <<" does not conform to declared type "<< current_decl.argument_types[i] 
                <<"."<< endl;
                return_type = No_type;
                
            }
        }

        /*If a match for the method exists, the return type of the dispatch 
        is the return type of the method
        */
        if (matches) {
            return_type = current_decl.return_type;
            break;
        }
    }
    //Return the return type
    return return_type;   
}
/*
type_union:
Helper function used in type checking conditional and case expressions
This function is used to find the least upper bound -> least common ancestor
for two types 

This is done by walking through the inheritance tree of the two types from top to bottom
and finding the type from which the paths of two types go in separate ways
*/
Symbol ClassTable::type_union(Symbol t1, Symbol t2, class__class* current_class) {
    //define variables to store the inheritance trees of the two types
    std::stack<Symbol> t1_heritance_stack, t2_heritance_stack;
    /*
    Starting from the class t1, go up the inheritance tree to the root
    and store the parents in the stack
    the root will be at the top of the stack and the class t1 will be in the bottom
    */
    class__class* curr = static_cast<class__class*>(classes_table.lookup(t1));
    while (curr != NULL) {
        t1_heritance_stack.push(curr->get_name());
        curr = static_cast<class__class*>(classes_table.lookup(curr->get_parent()));
    }
    /*
    Starting from the class t2, go up the inheritance tree to the root
    and store the parents in the stack
    the root will be at the top of the stack and the class t1 will be in the bottom
    */
    curr = static_cast<class__class*>(classes_table.lookup(t2));
    while (curr != NULL) {
        t2_heritance_stack.push(curr->get_name());
        curr = static_cast<class__class*>(classes_table.lookup(curr->get_parent()));
    }

    /*Get the root of the inheritance tree of t1 as head_type_t1 
    and remove it from t1_heritance_stack
    */
    Symbol head_type_t1 = t1_heritance_stack.top();
    t1_heritance_stack.pop();

    /*Get the root of the inheritance tree of t2 as head_type_t2 
    and remove it from t2_heritance_stack
    */
    Symbol head_type_t2 = t2_heritance_stack.top();
    t2_heritance_stack.pop();

    //take the common_type as root of t1 tree (at the begining this will be Object)
    Symbol common_type = head_type_t1;
    /*
    Go through the parents of the two types from root to leaves,
    until both have common ancestors (head_type_t2==head_type_t1)
    and both stacks are not empty

    When the loop stops, the common_type will contain the least common ancestor 
    of the two types t1 and t2
    */
    while (head_type_t2 == head_type_t1 && !t1_heritance_stack.empty() && !t2_heritance_stack.empty()) {
         head_type_t1 = t1_heritance_stack.top();
         t1_heritance_stack.pop();
         head_type_t2 = t2_heritance_stack.top();
         t2_heritance_stack.pop();
         if (head_type_t2 == head_type_t1) {
             common_type = head_type_t1;
         }
    }
    //Return the least common type
    return common_type;
}

/*
type_check_cond:
Method to check the type correctness of a conditional statement (it-then-else)

According to the cool manual:
    *The predicate must have static type Bool. 

    *Let T and F be the static types of the branches of the conditional. 
    Then the static type of the conditional is type_union(T, F) 
    ( Walk towards Object from each of T and F until the paths meet.)

Therefore, error handled:
    1. Error: Predicate is not type Bool
*/
Symbol ClassTable::type_check_cond(cond_class* cond, class__class* current_class) {
    //Check the type of the predicate
    Symbol pred_type= type_check_expression(cond->get_predicate(), current_class, Bool);
    /*
    If predicate type is not Bool:
     Error: Predicate is not type Bool
    */
    if(pred_type != Bool){
        LOG_ERROR( current_class, cond)
	    << "Predicate of 'if' does not have type Bool."<< endl;
    }
    //Get the types of then and else branches
    Symbol then_type = type_check_expression(cond->get_then_expression(), current_class, Object);
    Symbol else_type = type_check_expression(cond->get_else_expression(), current_class, Object);
    /*Return the least upper bound (least common ancestor)
    of two branches as the type of the conditional statement
    */
    return type_union(then_type, else_type, current_class);
}

/*
type_check_case:
method to check the type correctness of a case statement

According to the cool manual:
    * It is an error to bind 'self' in a case
    * Types in case branch must be defined
    * The variables declared on each branch of a case must all have distinct types.
    *  The type of the entire case is the join of the types of its branches.
    (Least upper bound/ least common ancestor of types of all branches)

Therefore, the errors handled:
    1. Error 1: identifier in case branch is bound to 'self'
    2. Error 2: type in case branch undefined
    3. Error 3: case branches have duplicate types
*/

Symbol ClassTable::type_check_typcase(typcase_class* typcase, class__class* current_class) {
    //Evaluate the type of the expression in case
    type_check_expression(typcase->get_expression(), current_class, Object);
    //Get the case branches
    Cases cases = typcase->get_cases();
    Symbol return_type = nullptr;
    //Vector to store the types in branches
    std::vector<Symbol> branches_types;

    //Go through each case branch and check validity
    for (int i = 0 ; i < cases->len() ; i++) {
        //Get the i th branch
        branch_class* branch = static_cast<branch_class*>(cases->nth(i));
        //identifier of the i th branch
        Symbol identifier = branch->get_name();
        //Type of the i th branch
        Symbol type = branch->get_type();

        //Check if the type in the branch is defined
        class__class* type_ptr = static_cast<class__class*>(classes_table.lookup(type));

        /*If the identifier is bound to self,
            Error 1: identifier in case branch is bound to 'self'
        */
        if ( identifier == self ) {
            LOG_ERROR(current_class, branch)
            << "'self' bound in 'case'. " << endl;
        }

        /*
        if the identifier is not self but the type is not defined (type_ptr == NULL)
         Error 2: type in case branch undefined
        */
        else if (type_ptr == NULL) {
            LOG_ERROR (current_class, branch)
            << "Undefined type in case branch " << type << endl;
        }

        /*
        if the identifier is not self and type is defined,
        but the type is not unique to the branch,
            Error 3: case branches have duplicate types
        */
        else if (std::find(begin(branches_types), end(branches_types), type) != end(branches_types)) {
            LOG_ERROR (current_class, branch)
            << "Duplicate branch "<< type
            <<" in case statement."
            << endl;
            
        }

        //Store the type of the branch in branches_types
        branches_types.push_back(type);
        //Enter the scope of the current class in symbol tabel to access the attributes
        symbol_table.enterscope();
        /*
        According to the cool manual:
            The identifier id introduced by a branch of a case 
            hides any variable or attribute definition for id visible
            in the containing scope.
        Therefore, add the identifier and type to the scope of the 
        current class in symbol table        
        */
        symbol_table.addid(identifier, type);
        /*Check the type correctness of the expression in the branch
        with the new definitions of the identifiers 
        */
        Symbol branch_type = type_check_expression(branch->get_expression(), current_class, Object);
        //exit from the scope of the current class
        symbol_table.exitscope();

        /*if the return_type is null (happens when evaluating the first branch),
            assign the branch type
        */
        if (return_type == nullptr) {
            return_type = branch_type;
        }
        /*
        Update the return type with the type union of the branch types 
        after each branch evaluation
        */
        return_type = type_union(return_type, branch_type, current_class);
    }
    //Return the type union of all the case branches
    return return_type;
}

/*
type_check_block:
check the type correctness of a block in the form
                    { <expr>; ... <expr>; }

According to the cool manual:

    * The expressions are evaluated in left-to-right order
    * The static type of a block is the static type of the last expression
*/

Symbol ClassTable::type_check_block(block_class*  block, class__class* current_class) {
    Symbol last_type;
    //Go through all the expressions in a block
    for (int i = 0; i < block->get_body()->len(); i++) {
        //Update the last type with the type of the expression
        last_type = type_check_expression(block->get_body()->nth(i), current_class, Object);
    }
    //Return the type of the last expression in the block
    return last_type;
}

/*
type_check_loop:
check the type correctness of a loop in the form
    
            while <expr> loop <expr> pool

According to the cool manual:   
    * The predicate must have static type Bool. 
    * The body may have any static type. 
    * The static type of a loop expression is Object.

Therefore, error handled:
    1. Error: predicate is not of type Bool
*/
void ClassTable::type_check_loop(loop_class* loop, class__class* current_class) {
    //Check the type of the predicate
    Symbol pred_type= type_check_expression(loop->get_predicate(), current_class, Bool);
    /*If predicate is not Bool, error */
    if(pred_type != Bool){
        LOG_ERROR( current_class, loop)
        << "Loop condition does not have type Bool."<< endl;
    }
    //Type check the expressions in the body of the loop
    type_check_expression(loop->get_body(), current_class, Object);
    /*Since the static type of loop is Object, it's given in final_type in 
    the case statement in type_check_expression
    and therefore, no need of returning it from here
    */
}

/*
type_check_new:
method to check the type correctness of a new expression of the form
                        new <type>
The static type is <type>.

Error handled : type is undefined

*/
Symbol ClassTable::type_check_new_(new__class* new_, class__class* current_class) {
    //If the type is SELF_TYPE, return it
    if (new_->get_type() == SELF_TYPE) {
        return SELF_TYPE;
    }
    //If not, check if the type is  defined
    Symbol type = new_->get_type();
    class__class* type_ptr = static_cast<class__class*>(classes_table.lookup(type));
    /*If type not defined-> error
        for error recovery, return the type as Object
    */
    if (type_ptr == NULL) {
        LOG_ERROR(current_class,new_)
        << "Undefined type in new expression " << type << endl;
        return Object;
    }

    //If no error occurr, return the type
    return type;
}


/*
type_check_object:
method to check whether the type of an object identifier is declared as an attribute

Error: undeclared identifier used

*/
Symbol ClassTable::type_check_object(object_class* object, class__class* current_class) {
    //if identifier is 'self', type is SELF_TYPE
    if (object->get_name() == self) {
        return SELF_TYPE;
    }

    //If not self, check whether the identifier is defined
    Symbol type = symbol_table.lookup(object->get_name());

    //If not defined -> error
    if (type == NULL) {
        LOG_ERROR(current_class,object)
        << "Undeclared identifier " << object->get_name() << endl;
        type = Object;
    }
    //Return the identifier/Object type as the type
    return type;
}

/*
type_check_let:
method to check the type correctness of a let expression of the form,

    let <id1> : <type1> [<- <expr1>],..., <idn> : <typen> [<- <exprn>] in <expr>

According to the cool manual:

    * First <expr1> is evaluated and the result bound to <id1>. 
    * Then <expr2> is evaluated and the result bound to <id2>, and so on,
     until all of the variables in the let are initialized. 
     (If the initialization of <idk> is omitted, 
     the default initialization of type <typek> is used.) 
    * Next the body of the let is evaluated. 
  

    * The let identifiers <id1>,...,<idn> are visible in the body of the let. 
    * If an identifier is defined multiple times in a let, 
      later bindings hide earlier ones. 
    * Identifiers introduced by let also hide any definitions for the same names
      in containing scopes. 
    * The type of an initialization expression must conform to the declared type
      of the identifier. 
    * The type of let is the type of the body.

Therefore, errors handled:
    1. Error 1: Identifier type is undefined
    2. Error 2: Idenitifier name is "self"
    3. Error 3: Initialization type of identifier not conforming to the declared type
*/
Symbol ClassTable::type_check_let(let_class* let, class__class* current_class) {
    /*Enter the scope of current class of symbol table to access the attributes in
        the class
    */
    symbol_table.enterscope();
    //Get the declared type of the idenitifer
    Symbol type = let->get_type();
    //Check if the declared type is defined in the program
    class__class* type_ptr = static_cast<class__class*>(classes_table.lookup(type));
    //Get the name of the identifier
    Symbol identifier = let->get_identifier();

    /*  if type is not SELF_TYPE and the declared type is not defined,
            Error 1: Identifier type is undefined
    */
    if (type != SELF_TYPE && type_ptr == NULL) {
        LOG_ERROR(current_class, let)
        << "Undefined type in let binding " << type << endl;
        return Object;
    }
    /*
    If identifier name is self,
        Error 2: Idenitifier name is "self"
    */
    if (identifier == self) {
        LOG_ERROR(current_class,let)
        << "'self' cannot be bound in a 'let' expression." << endl;
    }
    //add the identifiers in let expression to the current scope
    symbol_table.addid(identifier, type);
    //check the type of initialization expression
    Symbol init_type = type_check_expression(let->get_init(), current_class, type);

    /*Init expression is optional, 
    therefore if init_type = no_type, 
    the initialization expression is not present.
    If initialization is present, check if it conforms to the declared type.
    If not,
    Error 3: Initialization type of identifier not conforming to the declared type    
    */
    if( init_type !=No_type && !is_descendant(init_type,type,  current_class) ){
        LOG_ERROR(current_class, let)
        <<"Inferred type "<< init_type
        <<" of initialization of "<< identifier
        <<" does not conform to identifier's declared type "
        << type<<"."<<endl;
	}
  
    /*
    After type checking all the identifiers, get the type of the let body
    */
    Symbol let_type = type_check_expression(let->get_body(), current_class, Object);
    symbol_table.exitscope();
    //type of let is the type of the body
    return let_type;
}

/*
invalid_comparison:
Helper function to check the validity of expressions involved in the comparison
if expressions have basic types.

According to the cool manula:
If either <expr1> or <expr2> has static type Int, Bool, or String, 
then the other must have the same static type.

Therefore, invalid_comparison() function returns
    1. True: if either of the expressions in comparison  has a basic type and both doesn't have
    the same basic type
    2. False: either if the expressions are not basic type and they are not of same type, or both

*/
bool invalid_comparison(Symbol a, Symbol b) {
    bool has_basic_type = a == Int || a == Str || a == Bool;
    return has_basic_type  && a != b;
}

/*
type_check_eq:
check type correctness of the comparison expression with the aid of function invalid_comparison()

According to cool manual:

 * If either <expr1> or <expr2> has static type Int, Bool, or String, 
    then the other must have the same static type. 
 * Any other types, including SELF TYPE, may be freely compared. 
 * On non-basic objects, equality simply checks for pointer equality 

Therefore, the error handled :
    1. Error: If either of the expressions is basic 
                but the other doesn't have the same type->
                illegal comparison between basic types.
*/
void ClassTable::type_check_eq(eq_class* eq, class__class* current_class) {
    Symbol left_type = type_check_expression(eq->get_left_operand(), current_class, Object);
    Symbol right_type = type_check_expression(eq->get_right_operand(), current_class, Object);
    if (invalid_comparison(left_type, right_type) || invalid_comparison(right_type, left_type)) {
            LOG_ERROR(current_class, eq)
                << "Illegal comparison between " << left_type << " and " << right_type << endl;
    }
}

/*
is_descendant: helper function used to check if given two types
have a descendant-ancestor relationship
*/

bool ClassTable::is_descendant(Symbol desc, Symbol ancestor, class__class* current_class) {
    if (desc == ancestor) {
        return true;
    }
    if (desc == SELF_TYPE) {
        desc = current_class->get_name();
    }
    class__class *current_type = static_cast<class__class*>(classes_table.lookup(desc));
    while (current_type != NULL) {
        if (current_type->get_name()  == ancestor) {
            return true;
        }
        Symbol parent_symbol = current_type->get_parent();
        class__class *parent_type = static_cast<class__class*>(classes_table.lookup(parent_symbol));
        current_type = parent_type;
    }
    return false;
}

Classes ClassTable::install_basic_classes(Classes classes) {
    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    classes_table.addid(prim_slot, class_(prim_slot, NULL, nil_Features(), filename));

    return append_Classes(
        single_Classes(Object_class),
        append_Classes(
            single_Classes(IO_class),
            append_Classes(
                single_Classes(Int_class),
                append_Classes(
                    single_Classes(Bool_class),
                    append_Classes(
                        single_Classes(Str_class),
                        classes)))));
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 

/*   This is the entry point to the semantic checker.
     Your checker should do the following two things:
     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')
     You are free to first do 1), make sure you catch all semantic
     errors. Part 2)  be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* Instatitiate a classtable object
    where the program does the following tasks.
    1. Traverse the AST and store the basic and custom classes in a classtable
    2. Build the inheritance graph and check if its well formed
    3. Traverse the AST and gather the methods and attributes
    4. Check type correctness
    5. Annotate the AST with types
     */
    ClassTable *classtable = new ClassTable(classes);

   
    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}
/*_________________________________________________________________________________
Implementations of methods declared for the MethodDeclaration structure in semant.h
*/


/*
from_method_class(): method used to extract the return type, formal parameter ids and types
from a method declaration class and store in variables of a MethodDeclaration object
*/
MethodDeclaration MethodDeclaration::from_method_class(method_class * method) {
    MethodDeclaration result;
    //Store the return type of method in return_type variable in result
    result.return_type = method->get_return_type();
    Formals formals = method->get_formals();
    /*
    Go through all the formal parameters and store the types and parameter names in 
    argument_types and identifiers vectors in MethodDeclaration structure
    */
    for (int i = 0; i < formals->len(); i++ ) {
        formal_class* formal = static_cast<formal_class*>(formals->nth(i));
        result.argument_types.push_back(formal->get_type());
	    result.identifiers.push_back(formal->get_name());
    }
        
    return result;
}

/*
has_same_args(): Method returns true if the two methods have same number of args and types
with the aid of the helper function matches()
*/
bool MethodDeclaration::has_same_args(MethodDeclaration  & other) {
    return matches(other.argument_types);
}

/*
matches(): Method takes arguments from another method and compares them with 
the arguments of the current method
if number of arguments and respective parameter types match returns true
*/
bool MethodDeclaration::matches(std::vector<Symbol> & args) {
    if (this->argument_types.size() != args.size())  {
        return false;
    }
  for (size_t i = 0; i < this->argument_types.size(); i++)  {
        if (this->argument_types[i] != args[i]) {
            return false;
        }
    }
  return true;
}

/*
get_undeclared_types(): method returns a vector of argument types which are not 
defined in the program
it also checkes whether the return type of the method is declared
*/
std::vector<Symbol> MethodDeclaration::get_undeclared_types(SymbolTable<Symbol, Class__class> & types) {
    std::vector<Symbol> undeclared_types;
    // SELF_TYPE is allowed as a  return type
    if (return_type != SELF_TYPE && types.lookup(return_type) == NULL) {
        undeclared_types.push_back(return_type);
    }
    for (Symbol arg_type : argument_types) {
        if (types.lookup(arg_type) == NULL) {
            undeclared_types.push_back(arg_type);
        }
    }
    return undeclared_types;
}


