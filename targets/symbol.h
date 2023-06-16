#ifndef __MML_TARGETS_SYMBOL_H__
#define __MML_TARGETS_SYMBOL_H__

#include <string>
#include <memory>
#include <cdk/types/basic_type.h>

namespace mml {
  class variable_declaration_node;

  class symbol {
    variable_declaration_node* _node;
    /** when offset is 1, symbol is global */
    long _offset;

  public:
    symbol(variable_declaration_node* node, long offset = 1) :
        _node(node), _offset(offset) {
    }

    virtual ~symbol() {
      // EMPTY
    }

  public:
    void node(variable_declaration_node* node) {
      _node = node;
    }
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
