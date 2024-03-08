grammar Cobra;

BLOCK_START : '{';
BLOCK_END : '}';
ID : [a-zA-Z] ( [a-zA-Z_] | [0-9] )*;
WS  : [ \t\r\n]+ -> skip ;
NUMBER
    : [0-9]+(.[0-9]+)?
    ;

script : fn* runner? EOF;

fn  : 'def' ID '(' (ID (',' ID)*)? ')' BLOCK_START text? return BLOCK_END;
runner  : 'main' BLOCK_START text BLOCK_END;

text : line | line text;
line  : alloc | blocks | fnAlloc | return | io | map | reduce;

map : ID '=' 'map' '(' lambda ',' (list | ID) ')';
reduce : ID '=' 'reduce' '(' lambda ',' (list | ID) ',' NUMBER')';

lambda: '(' ID (',' ID)? ')' '=>' expr;
list: '[' (((expr (',' expr)*)?) | (NUMBER ':' NUMBER (':' NUMBER)?)) ']';

and: cmpOut | and '&&' cmpOut;
or: and | or '||' and;

cmpOut: cmpInn;

cmpInn
	: '(' or ')' #brackets
	| ID '==' ID #eq
	| ID '!=' ID #ne
	| ID '>' ID #gt
	| ID '>=' ID #gte
	| ID '<' ID #lt
	| ID '<=' ID #lte
	;

blocks
	: 'if' '(' or ')' '{' text '}' ('else' '{' text '}')? #ifElse
	| 'while' '(' or ')' '{' text '}' #whileCond
	;

alloc : ID '=' (expr | list);

fnAlloc : ID '=' ID '(' ID (',' ID)? ')';
io : 'read' '(' ID (',' ID)? ')' | 'print' '(' ID (',' ID)? ')' | 'printarr' '(' ID (',' ID)? ')';
return : 'return' expr;

expr : term | expr ('+' | '-') term;
term : factor | term ('*' | '/') factor;
factor : eval | '-' factor;
eval : ID | NUMBER | '(' expr ')';
