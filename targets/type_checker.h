#ifndef __MML_TARGETS_TYPE_CHECKER_H__
#define __MML_TARGETS_TYPE_CHECKER_H__

#include "targets/basic_ast_visitor.h"

#include <cdk/types/functional_type.h>

namespace mml {

  /**
   * Print nodes as XML elements to the output stream.
   */
  class type_checker: public basic_ast_visitor {
    cdk::symbol_table<mml::symbol> &_symtab;

    basic_ast_visitor *_parent;

    std::shared_ptr<cdk::functional_type> _currentFunctionType;

  public:
    type_checker(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<mml::symbol> &symtab, basic_ast_visitor *parent) :
        basic_ast_visitor(compiler), _symtab(symtab), _parent(parent), _currentFunctionType(nullptr) {
    }

  public:
    ~type_checker() {
      os().flush();
    }

  protected:
    void processUnaryExpression(cdk::unary_operation_node *const node, int lvl);
    void processBinaryExpression(cdk::binary_operation_node *const node, int lvl);
    template<typename T>
    void process_literal(cdk::literal_node<T> *const node, int lvl) {
    }

    /** Asserts that a type can be casted to another type. */
    void assert_cast(std::shared_ptr<cdk::basic_type> from, std::shared_ptr<cdk::basic_type> to);

  public:
    // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
    // do not edit these lines: end

  };

} // mml

//---------------------------------------------------------------------------
//     HELPER MACRO FOR TYPE CHECKING
//---------------------------------------------------------------------------

#define CHECK_TYPES(compiler, symtab, node) { \
  try { \
    mml::type_checker checker(compiler, symtab, this); \
    (node)->accept(&checker, 0); \
  } \
  catch (const std::string &problem) { \
    std::cerr << (node)->lineno() << ": " << problem << std::endl; \
    return; \
  } \
}

#define ASSERT_SAFE_EXPRESSIONS CHECK_TYPES(_compiler, _symtab, node)

#endif
