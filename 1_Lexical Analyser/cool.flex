/*
GROUP 04
E/17/058 : DEVINDI G.A.I
E/17/190 : LIYANAGE S.N
*/



/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants 1025 */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

int nested_comment_count = 0;/*count the number of nested comments*/

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

/*function to check the length of the string exceeds max len*/
bool isLong();

/*function to print error if maximum length is exceeded*/
int maxlen_error();

%}

	/*________________________DEFINE START CONDITIONS_________________________ */


/* Exclusive start condition for COMMENT*/
%x COMMENT 
/* Exclusive start condition for STRING*/
%x STRING
/* Exclusive start condition for INVALID_STRING*/
%x INVALID_STRING


	/*________________________DEFINE NAMES AND REGEX___________________________*/
	



/*White spaces are considered as a blank or \t (tab)*/
WHITESPACE 	[ \t]*

/*Dash comment that start with -- followed by any sequence of chars upto next line*/
DASHCOM		--.*\n

/* An integer is any sequence of digits between 0-9*/
DIGIT 		[0-9]

/*Since Indentifiers contain underscores as well, defile LETTER as the union of all alphabetical chars + Underscore*/
LETTER 		[a-zA-Z_]

/*An object identifier starts with a lowercase letter, followed by any sequence of alphabetical chars, digits and underscores*/   
OBJ_ID		[a-z]({LETTER}|{DIGIT})*

/*A type identifier starts with an uppercase letter, followed by any sequence of alphabetical chars, digits and underscores*/ 	
TYPE_ID		[A-Z]({LETTER}|{DIGIT})*

/*Define case insensitive keywords using the option i*/
CLASS		(?i:class)
ELSE		(?i:else)
FI			(?i:fi)
IF			(?i:if)
IN			(?i:in)
INHERITS	(?i:inherits)
ISVOID		(?i:isvoid)
LET			(?i:let)
LOOP		(?i:loop)
POOL		(?i:pool)
THEN		(?i:then)
WHILE		(?i:while)
CASE		(?i:case)
ESAC		(?i:esac)
NEW			(?i:new)
OF			(?i:of)
NOT			(?i:not)

/*The first letter of true & false MUST be lower case, and the rest are case insensitive*/
TRUE		[t](?i:rue)
FALSE		[f](?i:alse)

/*Special symbols that contain more than one char (darrow,less than or equal and assign)*/
DARROW          =>
LE		<=
ASSIGN		<-

	/*________________________END OF DEFINITIONS______________________________________*/

%%



 	/*_________________________BEGINING OF RULES_____________________________________*/


 /*_________________________________________________________________________________

    		Identify Comments and nested comments.
   _________________________________________________________________________________
 */


 /* If an umatched is *) encountered outside the COMMENT state-> ERROR*/
"*)"		{
		cool_yylval.error_msg = "Unmatched *)";
		return ERROR;
		}

 /* If (* found, start exclusive start condition COMMENT and increase the comment count by 1*/

"(*"	{
			nested_comment_count++;
			BEGIN (COMMENT);
		}


 /*If an <<EOF>> is encountered while in COMMENT state, go to the initial state after stating ERROR: EOF in comment*/ 
<COMMENT><<EOF>> 	{
				BEGIN(INITIAL);
				cool_yylval.error_msg = "EOF in comment";
				return ERROR;
			}
<COMMENT>"(*"	{
		nested_comment_count++;
		BEGIN (COMMENT);
		}
 /*If a next line is encountered inside the COMMENT state, increase line count*/
<COMMENT>\n 	{curr_lineno++;}

 /*Ignore anything, including nested comment content except new line in comments*/
<COMMENT>.	{}

 /*If closing comment *) is encountered when in COMMENT state, return to INITIAL-> End of comment*/
<COMMENT>"*)"	{

					if (--nested_comment_count>0); // if count is not 0, there are nested comments remaining that has to be closed
					else BEGIN(INITIAL);
				}

 /*Rule for comments starting with --. Increase curr_lineno by one when a DASHCOMMENT is encountered without tokenizing the content*/
{DASHCOM} 	{curr_lineno++;}

  /*_________________________________________________________________________________________________________________________________

	Identify Strings and errors associated with them.
   __________________________________________________________________________________________________________________________________

  * Note:
	 Escape sequence \c is accepted for all characters c. Except for \n \t \b \f, the result is c.
   
  */


 /*If an inverted comma " is detected, enter the exclusive start condition STRING state.
	To keep track of the characters in the string,
	assign the start address of the string_buf to the buffer pointer "string_buf_ptr"
 */

\" 			{	
			BEGIN(STRING);
			/*assign the buffer address to the buffer pointer 
			so that chars can be stored in buffer by increasing the string_buf_ptr
			*/
			string_buf_ptr = string_buf;
			}

 /* If an EOF is encoutered while string is not terminated, Go to INITIAL state after stating the ERROR: EOF in string constant */

<STRING><<EOF>>		{
			cool_yylval.error_msg = "EOF in string constant";
			return ERROR;
			BEGIN(INITIAL);
			}

 /*If an inverted comma " is encountered when in STRING state, return to INITIAL-> End of string

	After checking the string length is within the allowed limit, clear the buffer by writing '\0'
	and store the string in the stringtable and return STR_CONST token along with the string

 */
<STRING>\"		{
			BEGIN(INITIAL);

			//check whether the string length exceeds the maximum allowed, if exceeds -> ERROR
			if (isLong()){
				return maxlen_error();
			}
			*string_buf_ptr = '\0'; //clear buffer

			//Store string and return
			cool_yylval.symbol = stringtable.add_string(string_buf);
			
			return STR_CONST;
			}


 /*Capture escaped newlines in STRING (a slash \ followed by \n) and increase line number*/
<STRING>\\\n		{
			//check whether the string length exceeds the maximum allowed, if exceeds -> ERROR
			if (isLong()){
				return maxlen_error();
			}
			//Capture the next line char and store in buffer
			*string_buf_ptr++ = '\n';
			curr_lineno++;
			}

 /*Capture the character that's not n,b,t,f which is followed by the slash \  */
<STRING>\\[^nbtf]	{
			//check whether the string length exceeds the maximum allowed, if exceeds -> ERROR
			if (isLong()){
				return maxlen_error();
			}
			*string_buf_ptr++= yytext[1];

			}

 /*If string has an unterminated newline->ERROR: Unterminated string constant
	Return to initial state (to resume lexing from next line) after increasing the line number 
 */

<STRING>\n		{
			curr_lineno++;
			BEGIN(INITIAL);
			cool_yylval.error_msg = "Unterminated string constant";
			return ERROR;
			}


 /* Invalid null char encounted inside STRING can be \0 followed by zero or more \
	state ERROR: String contains null character
	and go to INVALID_STRING state to to avoid tokenizing the rest of the string and begin lexing after end of string

*/
<STRING>\\?\0		{
			BEGIN(INVALID_STRING);
			cool_yylval.error_msg = "String contains null character";
			return ERROR;
			}


 /*

	If \n \t \b \f chars are found in string, record the ASCII value as a single entry in the string table.
	the strings containing escape characters entered with keypresses will be taken in their ASCII value by flex

*/


<STRING>\\[n] 		{
			//check whether the string length exceeds the maximum allowed, if exceeds -> ERROR
			if (isLong()){
				return maxlen_error();
			}
			//printf("next line typed %c here in two %c. char stored previously %cis not a tab\n",yytext[0],yytext[1], *(string_buf_ptr-1));
			
			
			*string_buf_ptr++ = '\n';
			}

<STRING>\\[t] 		{
			//check whether the string length exceeds the maximum allowed, if exceeds -> ERROR
			if (isLong()){
				return maxlen_error();
			}
			
			*string_buf_ptr++= '\t';
			
			}

<STRING>\\[b] 		{
			//check whether the string length exceeds the maximum allowed, if exceeds -> ERROR
			if (isLong()){
				return maxlen_error();
			}
			
			*string_buf_ptr++ = '\b';
			}

<STRING>\\[f] 		{
			//check whether the string length exceeds the maximum allowed, if exceeds -> ERROR
			if (isLong()){
				return maxlen_error();
			}
			
			*string_buf_ptr++ = '\f';
			}


 /*If any character that deosn't belong to above rules are encountered inside a string-> valid token*/
<STRING>.		{
			//check whether the string length exceeds the maximum allowed, if exceeds -> ERROR
			if (isLong()){
				return maxlen_error();
			}
			//store the token available in yytext variable in the buffer
			*string_buf_ptr++ = *yytext;
			}


 /*_________________________________________________________________________________________________________________________________

	The INVALID_STRING state that's used to deal with the string after invalid component is detected.
   __________________________________________________________________________________________________________________________________

  */

 /* If an inverted comma " is detected, the end of the invalid string is encountered. Therefore, return to INITIAL state*/
<INVALID_STRING>\" 	{BEGIN(INITIAL);}

 /*If a next line is detected inside an invalid string, increase line number and go to INITIAL state to start tokenizing the next line*/
<INVALID_STRING>\n 	{ 
			 curr_lineno++;
			 BEGIN(INITIAL);
			}

 /*Capture escaped newline inside invalid string, and increase line number*/
<INVALID_STRING>\\\n	{curr_lineno++;}

 /* If any char encountered after \, stay in this state because the string is not yet terminated*/
<INVALID_STRING>\\.	;

 /*If any character except \,\n," encountered, stay in this state because the string is not yet terminated*/
<INVALID_STRING>[^\\\n\"]+ ;



 /*_______________________________________________________________________________________________

  *  				Identify character operators and return appropriate token code
  *_______________________________________________________________________________________________
  */

{DARROW}		{ return (DARROW); }
{LE}			{ return (LE);}
{ASSIGN}		{ return (ASSIGN);}
"."			{ return '.';}
"@"			{ return '@';}
"~"			{ return '~';}
"*"			{ return '*';}
"/"			{ return '/';}
"+"			{ return '+';}
"-"			{ return '-';}
"<"			{ return '<';}
"="			{ return '=';}
","			{ return ',';}
"{"			{ return '{';}
"}"			{ return '}';}
"("			{ return '(';}
")"			{ return ')';}
":"			{ return ':';}
";"			{ return ';';}


 /*__________________________________________________________________________________________________________

  *  				Identify Integer constants: any non empty sequence of DIGIT is an Integer
  *__________________________________________________________________________________________________________
  */

{DIGIT}+ 		{
			cool_yylval.symbol = inttable.add_string(yytext);
			return INT_CONST;
			}

 /*____________________________________________________________________________________________________________

  *  				Identify Keywords. All except true and false are case insensitive
  *___________________________________________________________________________________________________________
  */


{CLASS} 		{ return CLASS;}
{ELSE} 			{ return ELSE;}
{IF} 			{ return IF;}
{FI} 			{ return FI;}
{IN}	 		{ return IN;}
{INHERITS} 		{ return INHERITS;}
{ISVOID} 		{ return ISVOID;}
{LET} 			{ return LET;}
{LOOP} 			{ return LOOP;}
{POOL}		 	{ return POOL;}
{THEN} 		        { return THEN;}
{WHILE}			{ return WHILE;}
{CASE} 			{ return CASE;}
{ESAC}	 		{ return ESAC;}
{NEW}	 		{ return NEW;}
{OF}	 		{ return OF;}
{NOT}	 		{ return NOT;}

{FALSE} 		{
			//Store the value of the boolean constant
			  cool_yylval.boolean = false;
			  return BOOL_CONST;
			}


{TRUE} 			{
			//Store the value of the boolean constant
			  cool_yylval.boolean = true;
			  return BOOL_CONST;
			}

 /*_________________________________________________________________________________

  *  			Detect Object identifiers. Begin with lower case letter
  *_________________________________________________________________________________
  */


{OBJ_ID}		{
			cool_yylval.symbol = idtable.add_string(yytext);
			return OBJECTID;

			}
 /*_________________________________________________________________________________

  *  			Detect Type identifiers. Begin with upper case letter
  *_________________________________________________________________________________
  */

{TYPE_ID}		{
			cool_yylval.symbol = idtable.add_string(yytext);
			return TYPEID;

			}



 /*Increase curr_lineno by 1 when new line is encountered*/
\n			{curr_lineno++;}

 /*Whitespaces are ignored*/
{WHITESPACE}		{}

 

 /*if any character except new line which do not adhere to above rules are encountered-> ERROR: |THE ERRONEOUES TOKEN|*/
.			{
			//return pointer to the msg
			cool_yylval.error_msg = strdup(yytext);
			return ERROR;
			}

	/*___________________________________________END OF RULES__________________________________________________*/ 

%%


	/* ___________________________Helper functions used in STRING state________________________________________*/


/*function to check the length of the string*/
bool isLong(){

	/* String length =  ending pointer position of string - starting pointer position of string +1
			 = string_buf_ptr - string_buf+1
	
	if string length > MAX_STR_CONST -> return TRUE
	if string length < MAX_STR_CONST -> return FALSE
	*/
	return (string_buf_ptr - string_buf + 1 > MAX_STR_CONST);

}

/*function to print error if maximum length is exceeded*/
int maxlen_error(){
	BEGIN(INVALID_STRING); ///should continue reading the string
	cool_yylval.error_msg = "String constant too long";
	
	return ERROR;
}


	/* _____________________________________END OF THE FLEX SPECIFICATION FOR COOL LEXER______________________*/
