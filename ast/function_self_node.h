#ifndef __MML_AST_FUNCTION_SELF_NODE_H__
#define __MML_AST_FUNCTION_SELF_NODE_H__

#include <cdk/ast/expression_node.h>

namespace mml {

  /**
   * Class for describing function self nodes ('@').
   */
  class function_self_node: public cdk::expression_node {
  public:
    inline function_self_node(int lineno) :
        cdk::expression_node(lineno) {
    }

  public:
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_self_node(this, level);
    }

  };

} // mml

#endif
