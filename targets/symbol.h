#ifndef __MML_TARGETS_SYMBOL_H__
#define __MML_TARGETS_SYMBOL_H__

#include <string>
#include <memory>
#include <cdk/types/basic_type.h>

namespace mml {
  class variable_declaration_node;

  class symbol {
    variable_declaration_node* _node;
    /** when offset is 0, symbol is global */
    long _offset;

  public:
    symbol(variable_declaration_node* node) :
        _node(node), _offset(0) {
    }

    virtual ~symbol() {
      // EMPTY
    }

  public:
    variable_declaration_node* node() const {
      return _node;
    }
    void offset(long value) {
      _offset = value;
    }
    long offset() const {
      return _offset;
    }
  };

} // mml

#endif
