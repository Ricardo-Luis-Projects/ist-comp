#ifndef __MML_AST_BLOCK_NODE_H__
#define __MML_AST_BLOCK_NODE_H__

#include <cdk/ast/basic_node.h>
#include <cdk/ast/sequence_node.h>

namespace mml {

  /**
   * Class for describing block nodes.
   */
  class block_node : public cdk::basic_node {
    cdk::sequence_node *_declarations, *_instructions;

  public:
    /**
     * Declarations and instructions are optional - if nullptr is passed, an empty sequence node is
     * automatically created.
     */
    inline block_node(int lineno, cdk::sequence_node *declarations, cdk::sequence_node *instructions) :
        cdk::basic_node(lineno), _declarations(declarations), _instructions(instructions) {
      if (_declarations == nullptr) {
        _declarations = new cdk::sequence_node(lineno);
      }

      if (_instructions == nullptr) {
        _instructions = new cdk::sequence_node(lineno);
      }
    }

  public:
    inline cdk::sequence_node *declarations() {
      return _declarations;
    }
    inline cdk::sequence_node *instructions() {
      return _instructions;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_block_node(this, level);
    }
  };

} // mml

#endif
