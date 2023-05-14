#ifndef __MML_AST_CALL_NODE_H__
#define __MML_AST_CALL_NODE_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>

namespace mml {

  /**
   * Class for describing function call nodes. Recurses if function is null.
   */
  class call_node : public cdk::expression_node {
    cdk::sequence_node *_arguments;
    cdk::expression_node *_function;

  public:
    inline call_node(int lineno, cdk::sequence_node *arguments, cdk::expression_node *function = nullptr) :
        cdk::expression_node(lineno), _arguments(arguments), _function(function) {
    }

  public:
    inline cdk::sequence_node *arguments() {
      return _arguments;
    }

    inline cdk::expression_node *function() {
      return _function;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_call_node(this, level);
    }

  };

} // mml

#endif
