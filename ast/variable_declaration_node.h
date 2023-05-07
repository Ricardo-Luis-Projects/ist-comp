#ifndef __MML_AST_VARIABLE_DECLARATION_NODE_H__
#define __MML_AST_VARIABLE_DECLARATION_NODE_H__

#include <cdk/ast/expression_node.h>

namespace mml {

  /**
   * Class for describing variable declaration nodes.
   */
  class variable_declaration_node: public cdk::typed_node {
  public:
    int _qualifier;
    std::string _name;
    cdk::expression_node* _initializer;

    inline variable_declaration_node(int lineno, int qualifier, const std::string& name, cdk::expression_node* initializer, std::shared_ptr<cdk::basic_type> vartype = nullptr) :
        cdk::typed_node(lineno), _qualifier(qualifier), _name(name), _initializer(initializer) {
      type(vartype);
    }

  public:
    inline int qualifier() {
      return _qualifier;
    }

    inline const std::string& name() {
      return _name;
    }

    inline cdk::expression_node* initializer() {
      return _initializer;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_variable_declaration_node(this, level);
    }

  };

} // mml

#endif
