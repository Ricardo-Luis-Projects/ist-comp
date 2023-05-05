#ifndef __MML_AST_LOOP_FLOW_CONTROL_NODE_H__
#define __MML_AST_LOOP_FLOW_CONTROL_NODE_H__

#include <cdk/ast/basic_node.h>
#include <cdk/ast/integer_node.h>

namespace mml {

  /**
   * Class for describing loop flow control nodes.
   */
  class loop_flow_control_node : public cdk::basic_node {
    cdk::integer_node *_cycle;

  public:
    inline loop_flow_control_node(int lineno) :
        cdk::basic_node(lineno), _cycle(new cdk::integer_node(lineno, 1)) {
    }

    inline loop_flow_control_node(int lineno, cdk::integer_node *cycle) :
        cdk::basic_node(lineno), _cycle(cycle) {
    }

  public:
    inline cdk::integer_node *cycle() {
      return _cycle;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_loop_flow_control_node(this, level);
    }
  };

} // mml

#endif