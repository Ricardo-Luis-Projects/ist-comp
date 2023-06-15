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
};

%token tFOREIGN tFORWARD tPUBLIC tUNQUALIFIED tAUTO
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

%right '='
%left tOR
%left tAND
%nonassoc '~'
%left tEQ tNE
%left '<' '>' tLE tGE
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY '?'
%nonassoc '(' '['

%type <sequence> file global_declarations declarations finalized_instructions instructions
%type <sequence> opt_expressions expressions opt_arguments arguments
%type <variable_declaration> global_declaration declaration argument
%type <i> forward_or_foreign opt_nesting
%type <type> type non_void_type function_type
%type <types> argument_types
%type <function> program function
%type <block> block inner_block
%type <basic> any_instruction final_instruction instruction conditional
%type <b> print_opt_newline
%type <expression> opt_global_initializer global_initializer opt_initializer initializer call literal opt_expression expression
%type <lvalue> lvalue
%type <s> string

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file : /* empty */                 { compiler->ast($$ = new cdk::sequence_node(LINE)); }
     | global_declarations         { compiler->ast($$ = $1); }
     | global_declarations program { compiler->ast($$ = new cdk::sequence_node(LINE, $2, $1)); }
     |                     program { compiler->ast($$ = new cdk::sequence_node(LINE, $1)); }
     ;

global_declarations :                     global_declaration ';' { $$ = new cdk::sequence_node(LINE, $1); }
                    | global_declarations global_declaration ';' { $$ = new cdk::sequence_node(LINE, $2, $1); }
                    ;

global_declaration : tPUBLIC            opt_auto      tIDENTIFIER global_initializer     { $$ = new mml::variable_declaration_node(LINE, tPUBLIC, *$3, $4); delete $3; }
                   | tPUBLIC            non_void_type tIDENTIFIER opt_global_initializer { $$ = new mml::variable_declaration_node(LINE, tPUBLIC, *$3, $4, $2); delete $3; }
                   | forward_or_foreign non_void_type tIDENTIFIER                        { $$ = new mml::variable_declaration_node(LINE, $1, *$3, $2); delete $3; }
                   | tAUTO                            tIDENTIFIER global_initializer     { $$ = new mml::variable_declaration_node(LINE, tUNQUALIFIED, *$2, $3); delete $2; }
                   | non_void_type                    tIDENTIFIER opt_global_initializer { $$ = new mml::variable_declaration_node(LINE, tUNQUALIFIED, *$2, $3, $1); delete $2; }
                   ;

declarations :              declaration ';' { $$ = new cdk::sequence_node(LINE, $1); }
             | declarations declaration ';' { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

declaration : tAUTO         tIDENTIFIER initializer     { $$ = new mml::variable_declaration_node(LINE, tUNQUALIFIED, *$2, $3); delete $2; }
            | non_void_type tIDENTIFIER opt_initializer { $$ = new mml::variable_declaration_node(LINE, tUNQUALIFIED, *$2, $3, $1); delete $2; }
            ;

forward_or_foreign : tFORWARD { $$ = tFORWARD; }
                   | tFOREIGN { $$ = tFOREIGN; }
                   ;

opt_global_initializer : /* empty */        { $$ = nullptr; }
                       | global_initializer { $$ = $1; }
                       ;

global_initializer : '=' literal  { $$ = $2; }
                   | '=' function { $$ = $2; }
                   ;

opt_initializer : /* empty */ { $$ = nullptr; }
                | initializer { $$ = $1; }
                ;

initializer : '=' expression { $$ = $2; }
            ;

opt_auto : /* empty */ 
         | tAUTO
         ;

type : non_void_type { $$ = $1; }
     | tVOID         { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
     ;

non_void_type : tINT          { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
              | tDOUBLE       { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
              | tSTRING       { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
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

function_type : type '<' '>'                { $$ = cdk::functional_type::create($1); }
              | type '<' argument_types '>' { $$ = cdk::functional_type::create(*$3, $1); delete $3; }
              ;

argument_types :                    non_void_type { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>({$1}); }
               | argument_types ',' non_void_type { $$ = $1; $1->push_back($3); }
               ;

program : tBEGIN inner_block tEND { $$ = new mml::function_node(LINE, $2); }
	   ;

block : '{' inner_block '}' { $$ = $2; }

inner_block : /* empty */                         { $$ = new mml::block_node(LINE, nullptr, nullptr); }
            | declarations                        { $$ = new mml::block_node(LINE, $1, nullptr); }
            | declarations finalized_instructions { $$ = new mml::block_node(LINE, $1, $2); }
            |              finalized_instructions { $$ = new mml::block_node(LINE, nullptr, $1); }
            ;

instructions : instruction              { $$ = new cdk::sequence_node(LINE, $1); }
	         | instructions instruction { $$ = new cdk::sequence_node(LINE, $2, $1); }
	         ;

finalized_instructions : instructions                   { $$ = $1; }
                       |              final_instruction { $$ = new cdk::sequence_node(LINE, $1); }
                       | instructions final_instruction { $$ = new cdk::sequence_node(LINE, $2, $1); }
                       ;

any_instruction : instruction       { $$ = $1; }
                | final_instruction { $$ = $1; }
                ;

instruction : block                                     { $$ = $1; }
            | expression ';'                            { $$ = new mml::evaluation_node(LINE, $1); }
            | expressions print_opt_newline             { $$ = new mml::print_node(LINE, $1, $2); }
            | tWHILE '(' expression ')' any_instruction { $$ = new mml::while_node(LINE, $3, $5); }
            | tIF conditional                           { $$ = $2; }
            ;

final_instruction : tSTOP opt_nesting ';'      { $$ = new mml::stop_node(LINE, $2); }
                  | tNEXT opt_nesting ';'      { $$ = new mml::next_node(LINE, $2); }
                  | tRETURN opt_expression ';' { $$ = new mml::return_node(LINE, $2); }
                  ;

print_opt_newline : '!'      { $$ = false; }
                  | tPRINTLN { $$ = true; }
                  ;

opt_nesting : /* empty */ { $$ = 1; }
            | tLINTEGER   { $$ = $1; }
            ;

opt_expression : /* empty */ { $$ = nullptr; }
               | expression  { $$ = $1; }
               ;

conditional : '(' expression ')' any_instruction %prec tIFX            { $$ = new mml::if_node(LINE, $2, $4); }
            | '(' expression ')' any_instruction tELIF conditional     { $$ = new mml::if_else_node(LINE, $2, $4, $6); }
            | '(' expression ')' any_instruction tELSE any_instruction { $$ = new mml::if_else_node(LINE, $2, $4, $6); }
            ;

expressions : expression                 { $$ = new cdk::sequence_node(LINE, $1); }
            | expressions ',' expression { $$ = new cdk::sequence_node(LINE, $3, $1); }
            ;

expression : literal                     { $$ = $1; }
           | function                    { $$ = $1; }
           | call                        { $$ = $1; }
           | lvalue                      { $$ = new cdk::rvalue_node(LINE, $1); }
           | lvalue '?'                  { $$ = new mml::address_of_node(LINE, $1); }
           | tINPUT                      { $$ = new mml::input_node(LINE); }
           | '[' expression ']'          { $$ = new mml::stack_alloc_node(LINE, $2); }
           | '(' expression ')'          { $$ = $2; }
           | '~' expression              { $$ = new cdk::not_node(LINE, $2); }
           | '+' expression %prec tUNARY { $$ = new mml::identity_node(LINE, $2); }
           | '-' expression %prec tUNARY { $$ = new cdk::neg_node(LINE, $2); }
           | tSIZEOF '(' expression ')'  { $$ = new mml::sizeof_node(LINE, $3); }
           | expression tAND expression  { $$ = new cdk::and_node(LINE, $1, $3); }
           | expression tOR  expression  { $$ = new cdk::or_node(LINE, $1, $3); }
           | expression tEQ  expression  { $$ = new cdk::eq_node(LINE, $1, $3); }
           | expression tNE  expression  { $$ = new cdk::ne_node(LINE, $1, $3); }
           | expression tGE  expression  { $$ = new cdk::ge_node(LINE, $1, $3); }
           | expression tLE  expression  { $$ = new cdk::le_node(LINE, $1, $3); }
           | expression '>'  expression  { $$ = new cdk::gt_node(LINE, $1, $3); }
           | expression '<'  expression  { $$ = new cdk::lt_node(LINE, $1, $3); }
           | expression '+'  expression  { $$ = new cdk::add_node(LINE, $1, $3); }
           | expression '-'  expression  { $$ = new cdk::sub_node(LINE, $1, $3); }
           | expression '*'  expression  { $$ = new cdk::mul_node(LINE, $1, $3); }
           | expression '/'  expression  { $$ = new cdk::div_node(LINE, $1, $3); }
           | expression '%'  expression  { $$ = new cdk::mod_node(LINE, $1, $3); }
           | lvalue     '='  expression  { $$ = new cdk::assignment_node(LINE, $1, $3); }
           ;

literal : tLINTEGER { $$ = new cdk::integer_node(LINE, $1); }
        | tLDOUBLE  { $$ = new cdk::double_node(LINE, $1); }
        | string    { $$ = new cdk::string_node(LINE, $1); }
        | tNULL     { $$ = new mml::null_node(LINE); }
        ;

string : tLSTRING        { $$ = $1; }
       | tLSTRING string { $$ = new std::string(*$1 + *$2); delete $1; delete $2; }
       ;

function : '(' opt_arguments ')' tGIVES type block { $$ = new mml::function_node(LINE, $2, $6, $5); }
         ;

opt_arguments : /* empty */ { $$ = new cdk::sequence_node(LINE); }
              | arguments   { $$ = $1; }
              ;

arguments : argument               { $$ = new cdk::sequence_node(LINE, $1); }
          | arguments ',' argument { $$ = new cdk::sequence_node(LINE, $3, $1); }
          ;

argument : non_void_type tIDENTIFIER { $$ = new mml::variable_declaration_node(LINE, tUNQUALIFIED, *$2, $1); delete $2; }
         ;

call : expression '(' opt_expressions ')' { $$ = new mml::call_node(LINE, $3, $1); }
     | '@'        '(' opt_expressions ')' { $$ = new mml::call_node(LINE, $3); }
     ;

lvalue : tIDENTIFIER                   { $$ = new cdk::variable_node(LINE, $1); }
       | expression '[' expression ']' { $$ = new mml::index_node(LINE, $1, $3); }
       ;

opt_expressions : /* empty */ { $$ = new cdk::sequence_node(LINE); }
                | expressions { $$ = $1; }
                ;

%%
