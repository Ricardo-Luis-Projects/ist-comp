#ifndef __MML_AST_NULLPTR_H__
#define __MML_AST_NULLPTR_H__

#include <cdk/ast/literal_node.h>

namespace mml {

  /**
   * Class for describing null nodes. 
   */
  class null_node: public virtual cdk::literal_node<std::nullptr_t> {
  public:
    inline null_node(int lineno) :
        cdk::literal_node<std::nullptr_t>(lineno, nullptr) {
    }

  public:
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_null_node(this, level);
    }

  };

} // mml

#endif
