%option c++ prefix="mml_scanner_" outfile="mml_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <ast/variable_declaration_node.h>
#include <ast/function_node.h>
#include <ast/block_node.h>
#include <ast/call_node.h>
#include <cdk/ast/basic_node.h>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "mml_parser.tab.h"

// don't change this
#define yyerror LexerError
%}

%x X_COMMENT
%x X_STRING X_BACKSLASH

%%
  yydebug=1; set_debug(1);

  /* ====================================================================== */
  /* 3.1 - WHITESPACE                                                       */
  /* ====================================================================== */

[ \t\n\r]+ ;

  /* ====================================================================== */
  /* 3.2 - COMMENTS                                                         */
  /* ====================================================================== */

"//".*            ;
"/*"              yy_push_state(X_COMMENT);
<X_COMMENT>"*/"   yy_pop_state();
<X_COMMENT>.|"\n" ;

  /* ====================================================================== */
  /* 3.3 - KEYWORDS                                                         */
  /* ====================================================================== */

"foreign" return tFOREIGN;
"forward" return tFORWARD;
"public"  return tPUBLIC;
"auto"    return tAUTO;

"if"      return tIF;
"elif"    return tELIF;
"else"    return tELSE;
"while"   return tWHILE;
"stop"    return tSTOP;
"next"    return tNEXT;
"return"  return tRETURN;

"input"   return tINPUT;
"sizeof"  return tSIZEOF;
"null"    return tNULL;

"begin"   return tBEGIN;
"end"     return tEND;

  /* ====================================================================== */
  /* 3.4 - TYPES                                                            */
  /* ====================================================================== */

"int"    return tINT;
"double" return tDOUBLE;
"string" return tSTRING;
"void"   return tVOID;

  /* ====================================================================== */
  /* FUNCTIONAL                                                             */
  /* ====================================================================== */

"->" return tGIVES;
@    return *yytext;

  /* ====================================================================== */
  /* 3.5 - OPERATORS                                                        */
  /* ====================================================================== */

">="             return tGE;
"<="             return tLE;
"=="             return tEQ;
"!="             return tNE;
"&&"             return tAND;
"||"             return tOR;
[-+~<>=*/%?\[\]] return *yytext;

  /* ====================================================================== */
  /* 3.6 - DELIMITERS AND TERMINATORS                                       */
  /* ====================================================================== */

"!!"      return tPRINTLN;
[,;!(){}] return *yytext;

  /* ====================================================================== */
  /* 3.7 - IDENTIFIERS                                                      */
  /* ====================================================================== */

[A-Za-z][A-Za-z0-9_]* yylval.s = new std::string(yytext); return tIDENTIFIER;

  /* ====================================================================== */
  /* 3.8.1 - INTEGERS                                                       */
  /* ====================================================================== */

[1-9][0-9]*  yylval.i = strtol(yytext, nullptr, 10); return tLINTEGER;
0[0-7]*      yylval.i = strtol(yytext, nullptr, 8); return tLINTEGER;
0[0-7]*[8-9] yyerror("Invalid octal number");

  /* ====================================================================== */
  /* 3.8.2 - FLOATING POINT NUMBERS                                         */
  /* ====================================================================== */

[0-9]+[eE][+-]?[0-9]+                             yylval.d = strtod(yytext, NULL); return tLDOUBLE;
([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)? yylval.d = strtod(yytext, NULL); return tLDOUBLE;

  /* ====================================================================== */
  /* 3.8.3 - STRINGS                                                        */
  /* ====================================================================== */

\"                      yy_push_state(X_STRING); yylval.s = new std::string("");
<X_STRING>\"            yy_pop_state(); return tLSTRING;
<X_STRING>\\            yy_push_state(X_BACKSLASH);
<X_STRING>.             *yylval.s += yytext;
<X_STRING>\n            yyerror("Newline in string");

<X_BACKSLASH>n          yy_pop_state(); *yylval.s += '\n';
<X_BACKSLASH>r          yy_pop_state(); *yylval.s += '\r';
<X_BACKSLASH>t          yy_pop_state(); *yylval.s += '\t';
<X_BACKSLASH>[0-7]{1,3} *yylval.s += strtoul(yytext, NULL, 8);
<X_BACKSLASH>.          yy_pop_state(); *yylval.s += yytext;

  /* ====================================================================== */
  /* 3.8.4 - POINTERS                                                       */
  /* ====================================================================== */

  /* Defined in 3.3 - KEYWORDS to precede 3.7 - IDENTIFIERS */

  /* ====================================================================== */
  /* CATCH ALL                                                              */
  /* ====================================================================== */

. yyerror("Unknown character");

%%
