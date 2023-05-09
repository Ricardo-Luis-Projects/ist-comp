%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  bool         b;
  int          i;
  double       d;
  std::string *s;

  std::vector<std::shared_ptr<cdk::basic_type>> *types;

  mml::variable_declaration_node *variable_declaration;
  mml::function_node             *function;
  mml::block_node                *block;
  cdk::basic_node                *basic;
  cdk::sequence_node             *sequence;
  cdk::expression_node           *expression;
  cdk::lvalue_node               *lvalue;
  cdk::integer_node              *linteger;
};

%token <i> tFOREIGN tFORWARD tPUBLIC tAUTO
%token tIF tELIF tELSE tWHILE tSTOP tNEXT tRETURN
%token tINPUT tSIZEOF
%token tBEGIN tEND

%token tINT tDOUBLE tSTRING tVOID
%token tGIVES

%token tOR tAND tEQ tNE tGE tLE

%token tPRINTLN

%token <s> tIDENTIFIER

%token <i> tLINTEGER
%token <d> tLDOUBLE
%token <s> tLSTRING

%token tNULL

%nonassoc tIFX
%nonassoc tELIF
%nonassoc tELSE

%type <sequence> file global_declarations declarations instructions
%type <sequence> opt_expressions expressions opt_arguments arguments
%type <variable_declaration> global_declaration declaration argument
%type <i> qualifier
%type <type> type function_type
%type <types> types
%type <function> program function
%type <block> block inner_block
%type <basic> instruction conditional
%type <b> print_opt_newline
%type <expression> opt_initializer initializer expression call literal
%type <expression> expression_primary expression_unary expression_mul
%type <expression> expression_add expression_comp expression_eq
%type <expression> expression_not expression_and expression_or
%type <lvalue> lvalue
%type <linteger> opt_linteger
%type <s> string

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file : global_declarations         { compiler->ast($$ = $1); }
     | global_declarations program { compiler->ast($$ = new cdk::sequence_node(LINE, $2, $1)); }
     ;

global_declarations : /* empty */                                { $$ = new cdk::sequence_node(LINE); }
                    | global_declarations global_declaration ';' { $$ = new cdk::sequence_node(LINE, $2, $1); }
                    ;

global_declaration : tPUBLIC   opt_auto tIDENTIFIER initializer { $$ = new mml::variable_declaration_node(LINE, tPUBLIC, *$3, $4, nullptr); }
                   | qualifier type     tIDENTIFIER             { $$ = new mml::variable_declaration_node(LINE, $1, *$3, nullptr, $2); }
                   | declaration                                { $$ = $1; }
                   ;

declarations : /* empty */                  { $$ = new cdk::sequence_node(LINE); }
             | declarations declaration ';' { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

declaration : tAUTO tIDENTIFIER initializer     { $$ = new mml::variable_declaration_node(LINE, 0, *$2, $3, nullptr); }
            | type  tIDENTIFIER opt_initializer { $$ = new mml::variable_declaration_node(LINE, 0, *$2, $3, $1); }
            ;

qualifier : tPUBLIC  { $$ = tPUBLIC; }
          | tFORWARD { $$ = tFORWARD; }
          | tFOREIGN { $$ = tFOREIGN; }
          ;

opt_initializer : /* empty */ { $$ = nullptr; }
                | initializer { $$ = $1; }
                ;

initializer : '=' expression { $$ = $2; }
            ;

opt_auto : /* empty */ 
         | tAUTO
         ;

type : tINT          { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
     | tDOUBLE       { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
     | tSTRING       { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
     | tVOID         { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
     | function_type { $$ = $1; }
     | '[' type ']'  {
          bool is_void_pointer =
               $2->name() == cdk::TYPE_POINTER &&
               cdk::reference_type::cast($2)->referenced()->name() == cdk::TYPE_VOID;

          if (is_void_pointer) {
               $$ = $2;
          } else {
               $$ = cdk::reference_type::create(4, $2);
          }
     }
     ;

function_type : type '<' types '>' { $$ = cdk::functional_type::create(*$3, $1); }
              ;

types : /* empty */ { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); }
      | types type  { $$ = $1; $1->push_back($2); }
      ;

program : tBEGIN inner_block tEND { $$ = mml::function_node::create_main(LINE, $2); }
	   ;

block : '{' inner_block '}' { $$ = $2; }

inner_block : declarations instructions { $$ = new mml::block_node(LINE, $1, $2); }
            ;

instructions : /* empty */              { $$ = new cdk::sequence_node(LINE); }
	        | instructions instruction { $$ = new cdk::sequence_node(LINE, $2, $1); }
	        ;

instruction : block                                 { $$ = $1; }
            | expression ';'                        { $$ = new mml::evaluation_node(LINE, $1); }
            | expressions print_opt_newline         { $$ = new mml::print_node(LINE, $1, $2); }
            | tSTOP opt_linteger ';'                { $$ = new mml::stop_node(LINE, $2); }
            | tNEXT opt_linteger ';'                { $$ = new mml::next_node(LINE, $2); }
            | tRETURN expression ';'                { $$ = new mml::return_node(LINE, $2); }
            | tWHILE '(' expression ')' instruction { $$ = new mml::while_node(LINE, $3, $5); }
            | tIF conditional                       { $$ = $2; }
            ;

print_opt_newline : '!'      { $$ = false; }
                  | tPRINTLN { $$ = true; }
                  ;

opt_linteger : /* empty */ { $$ = new cdk::integer_node(LINE, 0); }
             | tLINTEGER   { $$ = new cdk::integer_node(LINE, $1); }
             ;

conditional : '(' expression ')' instruction %prec tIFX        { $$ = new mml::if_node(LINE, $2, $4); }
            | '(' expression ')' instruction tELIF conditional { $$ = new mml::if_else_node(LINE, $2, $4, $6); }
            | '(' expression ')' instruction tELSE instruction { $$ = new mml::if_else_node(LINE, $2, $4, $6); }
            ;

expressions : expression                 { $$ = new cdk::sequence_node(LINE, $1); }
            | expressions ',' expression { $$ = new cdk::sequence_node(LINE, $3, $1); }
            ;

expression : lvalue '=' expression { $$ = new cdk::assignment_node(LINE, $1, $3); }
           | expression_or         { $$ = $1; } /* fallback */
           ;

expression_or : expression_or tOR expression_and { $$ = new cdk::or_node(LINE, $1, $3); }
              | expression_and                    { $$ = $1; } /* fallback */
              ;

expression_and : expression_and tAND expression_not { $$ = new cdk::and_node(LINE, $1, $3); }
               | expression_not                     { $$ = $1; } /* fallback */
               ;

expression_not : '~' expression_not { $$ = new cdk::not_node(LINE, $2); }
               | expression_eq     { $$ = $1; } /* fallback */
               ;

expression_eq : expression_eq   tEQ expression_comp { $$ = new cdk::eq_node(LINE, $1, $3); }
              | expression_eq   tNE expression_comp { $$ = new cdk::ne_node(LINE, $1, $3); }
              | expression_comp                     { $$ = $1; } /* fallback */
              ;

expression_comp : expression_comp tGE expression_add { $$ = new cdk::ge_node(LINE, $1, $3); }
                | expression_comp tLE expression_add { $$ = new cdk::le_node(LINE, $1, $3); }
                | expression_comp '>' expression_add { $$ = new cdk::gt_node(LINE, $1, $3); }
                | expression_comp '<' expression_add { $$ = new cdk::lt_node(LINE, $1, $3); }
                | expression_add                    { $$ = $1; } /* fallback */
                ;

expression_add : expression_add '+' expression_mul { $$ = new cdk::add_node(LINE, $1, $3); }
               | expression_add '-' expression_mul { $$ = new cdk::sub_node(LINE, $1, $3); }
               | expression_mul                    { $$ = $1; } /* fallback */
               ;

expression_mul : expression_mul '*' expression_unary { $$ = new cdk::mul_node(LINE, $1, $3); }
               | expression_mul '/' expression_unary { $$ = new cdk::div_node(LINE, $1, $3); }
               | expression_mul '%' expression_unary { $$ = new cdk::mod_node(LINE, $1, $3); }
               | expression_unary                      { $$ = $1; } /* fallback */
               ;

expression_unary : '+' expression_unary { $$ = new mml::identity_node(LINE, $2); }
                 | '-' expression_unary { $$ = new mml::identity_node(LINE, $2); }
                 | expression_primary   { $$ = $1; } /* fallback */
                 ;

expression_primary : literal                    { $$ = $1; }
                   | function                   { $$ = $1; }
                   | call                       { $$ = $1; }
                   | lvalue                     { $$ = new cdk::rvalue_node(LINE, $1); }
                   | lvalue '?'                 { $$ = new mml::address_of_node(LINE, $1); }
                   | tINPUT                     { $$ = new mml::input_node(LINE); }
                   | '[' expression ']'         { $$ = new mml::stack_alloc_node(LINE, $2); }
                   | '(' expression ')'         { $$ = $2; }
                   | tSIZEOF '(' expression ')' { $$ = new mml::sizeof_node(LINE, $3); }
                   ;

literal : tLINTEGER { $$ = new cdk::integer_node(LINE, $1); }
        | tLDOUBLE  { $$ = new cdk::double_node(LINE, $1); }
        | string    { $$ = new cdk::string_node(LINE, $1); }
        | tNULL     { $$ = new mml::null_node(LINE); }
        ;

string : tLSTRING        { $$ = $1; }
       | string tLSTRING { $$ = new std::string(*$1 + *$2); }
       ;

function : '(' opt_arguments ')' tGIVES type block { $$ = mml::function_node::create(LINE, $2, $6, $5); }
         ;

opt_arguments : /* empty */ { $$ = new cdk::sequence_node(LINE); }
              | arguments   { $$ = $1; }
              ;

arguments : argument               { $$ = new cdk::sequence_node(LINE, $1); }
          | arguments ',' argument { $$ = new cdk::sequence_node(LINE, $3, $1); }
          ;

argument : type tIDENTIFIER { $$ = new mml::variable_declaration_node(LINE, 0, *$2, nullptr, $1); }
         ;

call : expression_primary '(' opt_expressions ')' { $$ = new mml::call_node(LINE, $1, $3); }
     | '@'                '(' opt_expressions ')' { $$ = new mml::recursive_call_node(LINE, $3); }
     ;

lvalue : tIDENTIFIER                           { $$ = new cdk::variable_node(LINE, $1); }
       | expression_primary '[' expression ']' { $$ = new mml::index_node(LINE, $1, $3); }
       ;

opt_expressions : /* empty */ { $$ = new cdk::sequence_node(LINE); }
                | expressions { $$ = $1; }
                ;

%%
