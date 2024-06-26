#ifndef __MML_AST_STOP_NODE_H__
#define __MML_AST_STOP_NODE_H__

#include <cdk/ast/basic_node.h>

namespace mml {

  /**
   * Class for describing stop nodes.
   */
  class stop_node : public cdk::basic_node {
    int _nesting;

  public:
    inline stop_node(int lineno, int nesting) :
        cdk::basic_node(lineno), _nesting(nesting) {
    }

  public:
    inline int nesting() {
      return _nesting;
    }
    
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_stop_node(this, level);
    }

  };

} // mml

#endif
