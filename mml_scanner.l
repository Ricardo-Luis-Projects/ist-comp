%option c++ prefix="mml_scanner_" outfile="mml_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <ast/variable_declaration_node.h>
#include <ast/function_node.h>
#include <ast/block_node.h>
#include <cdk/ast/lvalue_node.h>
#include "mml_parser.tab.h"

// don't change this
#define yyerror LexerError

// utility function to prevent duplicating logic on integer literals
bool parseint(const std::string& str, int base, int& out);

// utility function to prevent duplicating logic on floating point literals
bool parsedouble(const std::string& str, double& out);
%}

%x X_COMMENT
%x X_STRING X_NULL X_BACKSLASH

%%
  /* ====================================================================== */
  /* 3.1 - WHITESPACE                                                       */
  /* ====================================================================== */

[ \t\n\r]+ ;

  /* ====================================================================== */
  /* 3.2 - COMMENTS                                                         */
  /* ====================================================================== */

"//".*            ;
"/*"              yy_push_state(X_COMMENT);
<X_COMMENT>"/*"   yy_push_state(X_COMMENT);
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

[A-Za-z][A-Za-z0-9]* yylval.s = new std::string(yytext); return tIDENTIFIER;

  /* ====================================================================== */
  /* 3.8.1 - INTEGERS                                                       */
  /* ====================================================================== */

[1-9][0-9]*        if (!parseint(yytext, 10, yylval.i)) yyerror("Integer overflow"); return tLINTEGER;
0[0-7]*            if (!parseint(yytext, 8, yylval.i)) yyerror("Integer overflow"); return tLINTEGER;
0[0-7]*[8-9][0-7]* yyerror("Invalid octal number");

  /* ====================================================================== */
  /* 3.8.2 - FLOATING POINT NUMBERS                                         */
  /* ====================================================================== */

[0-9]+[eE][+-]?[0-9]+                             if (!parsedouble(yytext, yylval.d)) yyerror("Floating point overflow"); return tLDOUBLE;
([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)? if (!parsedouble(yytext, yylval.d)) yyerror("Floating point overflow"); return tLDOUBLE;

  /* ====================================================================== */
  /* 3.8.3 - STRINGS                                                        */
  /* ====================================================================== */

\"                           yy_push_state(X_STRING); yylval.s = new std::string("");
<X_STRING>\"                 yy_pop_state(); return tLSTRING;
<X_STRING>\\                 yy_push_state(X_BACKSLASH);
<X_STRING>\0                 yyerror("Null byte in string");
<X_STRING>.                  *yylval.s += yytext;
<X_STRING>\n                 yyerror("Newline in string");

<X_NULL>\"                   yy_pop_state(); yy_pop_state(); return tLSTRING;
<X_NULL>\0                   yyerror("Null byte in string");
<X_NULL>.|\\\"|\\\\          ;
<X_NULL>\n                   yyerror("Newline in string");

<X_BACKSLASH>n               yy_pop_state(); *yylval.s += '\n';
<X_BACKSLASH>r               yy_pop_state(); *yylval.s += '\r';
<X_BACKSLASH>t               yy_pop_state(); *yylval.s += '\t';
<X_BACKSLASH>0{1,3}          yy_pop_state(); yy_push_state(X_NULL);
<X_BACKSLASH>[0-3][0-7]{0,2} yy_pop_state(); *yylval.s += strtoul(yytext, NULL, 8);
<X_BACKSLASH>[4-7][0-7]{2}   yyerror("Special character overflow");
<X_BACKSLASH>.               yy_pop_state(); *yylval.s += yytext;

  /* ====================================================================== */
  /* 3.8.4 - POINTERS                                                       */
  /* ====================================================================== */

  /* Defined in 3.3 - KEYWORDS to precede 3.7 - IDENTIFIERS */

  /* ====================================================================== */
  /* CATCH ALL                                                              */
  /* ====================================================================== */

. yyerror("Unknown character");

%%

bool parseint(const std::string& str, int base, int& out) {
  try {
    out = std::stoi(str, nullptr, base);
    return true;
  } catch (std::out_of_range&) {
    return false;
  }
}

bool parsedouble(const std::string& str, double& out) {
  try {
    out = std::stod(str, nullptr);
    return true;
  } catch (std::out_of_range&) {
    return false;
  }
}
