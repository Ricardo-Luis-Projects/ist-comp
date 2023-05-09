#ifndef __MML_AST_RECURSIVE_CALL_NODE_H__
#define __MML_AST_RECURSIVE_CALL_NODE_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>

namespace mml {

  /**
   * Class for describing recursive call nodes.
   */
  class recursive_call_node : public cdk::expression_node {
    cdk::sequence_node *_arguments;

  public:
    inline recursive_call_node(int lineno, cdk::sequence_node *arguments) :
        cdk::expression_node(lineno), _arguments(arguments) {
    }

  public:
    inline cdk::sequence_node *arguments() {
      return _arguments;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_recursive_call_node(this, level);
    }

  };

} // mml

#endif
