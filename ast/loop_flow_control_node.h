#ifndef __MML_AST_LOOP_FLOW_CONTROL_NODE_H__
#define __MML_AST_LOOP_FLOW_CONTROL_NODE_H__

#include <cdk/ast/basic_node.h>
#include <cdk/ast/integer_node.h>

namespace mml {

  /**
   * Class for describing loop flow control nodes.
   */
  class loop_flow_control_node : public cdk::basic_node {
    cdk::integer_node *_nesting;

  protected:
    inline loop_flow_control_node(int lineno) :
        cdk::basic_node(lineno), _nesting(new cdk::integer_node(lineno, 1)) {
    }

    inline loop_flow_control_node(int lineno, cdk::integer_node *nesting) :
        cdk::basic_node(lineno), _nesting(nesting) {
    }

  public:
    /**
     * @return the nesting level of the loop that is being controlled
    */
    inline cdk::integer_node *nesting() {
      return _nesting;
    }
  };

} // mml

#endif