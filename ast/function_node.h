#ifndef __MML_AST_FUNCTION_NODE_H__
#define __MML_AST_FUNCTION_NODE_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>
#include <cdk/types/functional_type.h>
#include <ast/block_node.h>
#include <ast/variable_declaration_node.h>

namespace mml {

  /**
   * Class for describing function nodes.
   */
  class function_node : public cdk::expression_node {
    cdk::sequence_node *_arguments;
    mml::block_node *_block;
    bool _main;

  public:
    inline function_node(int lineno, cdk::sequence_node *arguments, mml::block_node *block, std::shared_ptr<cdk::basic_type> output) :
        cdk::expression_node(lineno), _arguments(arguments), _block(block), _main(false) {
      // Extract the input types from the arguments sequence.
      std::vector<std::shared_ptr<cdk::basic_type>> inputs{};
      for (auto* node : arguments->nodes()) {
        auto* argument = dynamic_cast<mml::variable_declaration_node*>(node);
        inputs.push_back(argument->type());
      }

      type(cdk::functional_type::create(inputs, output));
    }

    /**
     * Special constructor for the main function.
     */
    inline function_node(int lineno, mml::block_node *block) :
        cdk::expression_node(lineno), _arguments(new cdk::sequence_node(lineno)), _block(block), _main(true) {
      type(cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_INT)));
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

    inline std::shared_ptr<cdk::structured_type> input_types() {
      return std::dynamic_pointer_cast<cdk::functional_type>(type())->input();
    }

    inline std::shared_ptr<cdk::basic_type> output_type() {
      return std::dynamic_pointer_cast<cdk::functional_type>(type())->output(0);
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_node(this, level);
    }
  };

} // mml

#endif
