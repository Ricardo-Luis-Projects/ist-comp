#ifndef __MML_AST_FUNCTION_NODE_H__
#define __MML_AST_FUNCTION_NODE_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>
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
    inline function_node(int lineno, cdk::sequence_node *arguments, mml::block_node *block, std::shared_ptr<cdk::basic_type> funtype) :
        function_node(lineno, arguments, block, funtype, false) {
    }

  protected:
    inline function_node(int lineno, cdk::sequence_node *arguments, mml::block_node *block, std::shared_ptr<cdk::basic_type> funtype, bool main) :
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

  public:
     /**
     * Creates a new function node to represent the main function. 
     */
    static function_node* create_main(int lineno, mml::block_node *block) {
      auto arguments = new cdk::sequence_node(lineno);
      auto type = cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_INT));
      return new function_node(lineno, arguments, block, type, true);
    }

    static function_node* create(int lineno, cdk::sequence_node *arguments, mml::block_node *block, std::shared_ptr<cdk::basic_type> output) {
      std::vector<std::shared_ptr<cdk::basic_type>> inputs{};
      for (auto* node : arguments->nodes()) {
        auto* argument = dynamic_cast<mml::variable_declaration_node*>(node);
        inputs.push_back(argument->type());
      }

      auto type = cdk::functional_type::create(inputs, output);
      return new function_node(lineno, arguments, block, type);
    }
  };

} // mml

#endif
