#ifndef __MML_AST_NEXT_NODE_H__
#define __MML_AST_NEXT_NODE_H__

#include <cdk/ast/basic_node.h>

namespace mml {

  /**
   * Class for describing next nodes.
   */
  class next_node : public cdk::basic_node {
    int _nesting;

  public:
    inline next_node(int lineno, int nesting) :
       cdk::basic_node(lineno), _nesting(nesting) {
    }

  public:
    inline int nesting() {
      return _nesting;
    }
    
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_next_node(this, level);
    }

  };

} // mml

#endif
