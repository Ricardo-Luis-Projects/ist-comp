#ifndef __MML_AST_STOP_NODE_H__
#define __MML_AST_STOP_NODE_H__

#include "ast/loop_flow_control_node.h"

namespace mml {

  /**
   * Class for describing stop nodes.
   */
  class stop_node : public mml::loop_flow_control_node {

  public:
    inline stop_node(int lineno) :
        mml::loop_flow_control_node(lineno) {
    }

    inline stop_node(int lineno, cdk::integer_node *nesting) :
        mml::loop_flow_control_node(lineno, nesting) {
    }

  public:

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_stop_node(this, level);
    }
  };

} // mml

#endif