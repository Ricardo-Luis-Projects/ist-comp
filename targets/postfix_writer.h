#ifndef __MML_TARGETS_POSTFIX_WRITER_H__
#define __MML_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"

#include <sstream>
#include <queue>
#include <vector>
#include <unordered_set>
#include <cdk/emitters/basic_postfix_emitter.h>
#include <cdk/types/functional_type.h>

namespace mml {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class postfix_writer: public basic_ast_visitor {
    cdk::symbol_table<mml::symbol> &_symtab;
    std::queue<std::pair<int, mml::function_node*>> _deferredFunctions;
    std::vector<std::pair<int, int>> _loopLabels; // (next, stop)
    std::shared_ptr<cdk::functional_type> _functionType;
    int _function;
    bool _isMain;
    std::unordered_set<std::string> _externSymbols;
    cdk::basic_postfix_emitter &_pf;
    int _lbl;
    long _offset;

  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<mml::symbol> &symtab,
                   cdk::basic_postfix_emitter &pf) :
        basic_ast_visitor(compiler), _symtab(symtab), _functionType(nullptr), _isMain(false), _pf(pf), _lbl(0), _offset(0) {
    }

  public:
    ~postfix_writer() {
      os().flush();
    }

  private:
    void cast(std::shared_ptr<cdk::basic_type> from, std::shared_ptr<cdk::basic_type> to);
    void visitCast(cdk::expression_node* from, std::shared_ptr<cdk::basic_type> to, int lvl);
    void processCmpExpression(cdk::binary_operation_node *const node, int lvl);
    void externIfNeeded(std::string symbol);

    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
      std::ostringstream oss;
      if (lbl < 0)
        oss << ".L" << -lbl;
      else
        oss << "_L" << lbl;
      return oss.str();
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
