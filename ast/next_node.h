#ifndef __MML_AST_NEXT_NODE_H__
#define __MML_AST_NEXT_NODE_H__

#include "ast/loop_flow_control_node.h"

namespace mml {

  /**
   * Class for describing next nodes.
   */
  class next_node : public mml::loop_flow_control_node {

  public:
    inline next_node(int lineno) :
        mml::loop_flow_control_node(lineno) {
    }

    inline next_node(int lineno, cdk::integer_node *nesting) :
        mml::loop_flow_control_node(lineno, nesting) {
    }

  public:

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_next_node(this, level);
    }
  };

} // mml

#endif