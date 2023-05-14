#ifndef __MML_AST_NULL_NODE_H__
#define __MML_AST_NULL_NODE_H__

#include <cdk/ast/expression_node.h>

namespace mml {

  /**
   * Class for describing null nodes. 
   */
  class null_node: public virtual cdk::expression_node {
  public:
    inline null_node(int lineno) :
        expression_node(lineno) {
    }

  public:
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_null_node(this, level);
    }

  };

} // mml

#endif
