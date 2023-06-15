#ifndef __MML_TARGETS_FRAME_SIZE_CALCULATOR_H__
#define __MML_TARGETS_FRAME_SIZE_CALCULATOR_H__

#include "targets/basic_ast_visitor.h"
#include <cdk/types/functional_type.h>

namespace mml {

  /**
   * Finds the size of a function's frame.
   */
  class frame_size_calculator: public basic_ast_visitor {
    cdk::symbol_table<mml::symbol> &_symtab;
    std::shared_ptr<cdk::functional_type> _functionType;
    std::size_t _size;
    bool _isMain;

  public:
    frame_size_calculator(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<mml::symbol> &symtab, std::shared_ptr<cdk::functional_type> functionType, bool isMain)
      : basic_ast_visitor(compiler)
      , _symtab(symtab)
      , _functionType(functionType)
      , _size(0)
      , _isMain(isMain) {
    }

  public:
    std::size_t size() {
      return _size;
    }

  public:
    // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
    // do not edit these lines: end
  };

} // mml

#endif
