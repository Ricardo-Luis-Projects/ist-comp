#ifndef __MML_AST_FUNCTION_NODE_H__
#define __MML_AST_FUNCTION_NODE_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>
#include <ast/block_node.h>

namespace mml {

  /**
   * Class for describing function nodes.
   */
  class function_node : public cdk::expression_node {
    cdk::sequence_node *_arguments;
    mml::block_node *_block;
    bool _main;

  public:
    inline function_node(int lineno, cdk::sequence_node *arguments, mml::block_node *block, bool main) :
        cdk::expression_node(lineno), _arguments(arguments), _block(block), _main(main) {
      type(cdk::primitive_type::create(0, cdk::TYPE_VOID));
    }

    inline function_node(int lineno, cdk::sequence_node *arguments, mml::block_node *block, bool main, std::shared_ptr<cdk::basic_type> funtype) :
        cdk::expression_node(lineno), _arguments(arguments), _block(block), _main(main) {
      type(funtype);
    }

  public:
    inline cdk::sequence_node *arguments() {
      return _arguments;
    }

    inline mml::block_node *block() {
      return _block;
    }

    inline bool main() {
      return _main;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_node(this, level);
    }

  };

} // mml

#endif
