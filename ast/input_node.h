#ifndef __MML_AST_INPUT_NODE_H__
#define __MML_AST_INPUT_NODE_H__

#include <cdk/ast/lvalue_node.h>

namespace mml {

  /**
   * Class for describing input nodes.
   */
  class input_node: public cdk::basic_node {
    cdk::lvalue_node *_argument;

  public:
    inline input_node(int lineno, cdk::lvalue_node *argument) :
        cdk::basic_node(lineno), _argument(argument) {
    }

  public:
    inline cdk::lvalue_node *argument() {
      return _argument;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_input_node(this, level);
    }

  };

} // mml

#endif
