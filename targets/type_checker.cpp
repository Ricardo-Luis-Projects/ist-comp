#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

void mml::type_checker::assert_cast(std::shared_ptr<cdk::basic_type> from, std::shared_ptr<cdk::basic_type> to) {
  if (from->name() == cdk::TYPE_INT && to->name() == cdk::TYPE_DOUBLE) {
    // Integers can be casted to doubles.
    return;
  } else if (from->name() != to->name()) {
    throw std::string("cannot cast '" + cdk::to_string(from) + "' to '" + cdk::to_string(to) + "'");
  }

  try {
    if (from->name() == cdk::TYPE_POINTER) {
      auto fromPtr = std::dynamic_pointer_cast<cdk::reference_type>(from);
      auto toPtr = std::dynamic_pointer_cast<cdk::reference_type>(to);

      // If either is a void pointer, we can cast - nested void pointers are parsed as just one void pointer.
      if (fromPtr->referenced()->name() == cdk::TYPE_VOID || toPtr->referenced()->name() == cdk::TYPE_VOID) {
        return;
      }

      assert_cast(fromPtr->referenced(), toPtr->referenced());
    } else if (from->name() == cdk::TYPE_FUNCTIONAL) {
      auto fromFunc = std::dynamic_pointer_cast<cdk::functional_type>(from);
      auto toFunc = std::dynamic_pointer_cast<cdk::functional_type>(to);

      // Must have the same number of arguments.
      if (fromFunc->input_length() != toFunc->input_length()) {
        throw std::string("different number of arguments");
      }

      for (size_t i = 0; i < fromFunc->input_length(); i++) {
        assert_cast(toFunc->input(i), fromFunc->input(i));
      }

      assert_cast(fromFunc->output(), toFunc->output());
    }
  } catch (std::string &e) {
    e = "cannot cast '" + cdk::to_string(from) + "' to '" + cdk::to_string(to) + "'\n" + e;
    throw e;
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void mml::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void mml::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void mml::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

void mml::type_checker::do_null_node(mml::null_node *const node, int lvl) {
  // TODO: should we use TYPE_UNSPEC instead?
  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_VOID)));
}

//---------------------------------------------------------------------------

void mml::type_checker::processUnaryExpression(cdk::unary_operation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
  node->type(node->argument()->type());
}

void mml::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type in argument of not operator");
  }
}

void mml::type_checker::do_neg_node(cdk::neg_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
  if (!node->argument()->is_typed(cdk::TYPE_INT) && !node->argument()->is_typed(cdk::TYPE_DOUBLE)) {
    throw std::string("wrong type in argument of negate expression");
  }
}

void mml::type_checker::do_identity_node(mml::identity_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
  if (!node->argument()->is_typed(cdk::TYPE_INT) && !node->argument()->is_typed(cdk::TYPE_DOUBLE)) {
    throw std::string("wrong type in argument of identity expression");
  }
}

void mml::type_checker::do_sizeof_node(mml::sizeof_node *const node, int lvl) {
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_stack_alloc_node(mml::stack_alloc_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
  // TODO: expect argument type to be int
  // TODO: should we use TYPE_UNSPEC instead?
  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_VOID)));
}

//---------------------------------------------------------------------------

void mml::type_checker::processBinaryExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in left argument of binary expression");

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in right argument of binary expression");

  // in MML, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  // EMPTY
}
void mml::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void mml::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<mml::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->type());
  } else {
    throw id;
  }
}

void mml::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

void mml::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;

  try {
    node->lvalue()->accept(this, lvl);
  } catch (const std::string &id) {
    auto symbol = std::make_shared<mml::symbol>(cdk::primitive_type::create(4, cdk::TYPE_INT), id, 0);
    _symtab.insert(id, symbol);
    _parent->set_new_symbol(symbol);  // advise parent that a symbol has been inserted
    node->lvalue()->accept(this, lvl);  //DAVID: bah!
  }

  node->rvalue()->accept(this, lvl + 2);

  try {
    assert_cast(node->rvalue()->type(), node->lvalue()->type());
  } catch (std::string &msg) {
    msg = "invalid cast in assignment expression\n" + msg;
    throw msg;
  }

  node->type(node->lvalue()->type());
}

//---------------------------------------------------------------------------

void mml::type_checker::do_block_node(mml::block_node *const node, int lvl) {
  _symtab.push();
  node->declarations()->accept(this, lvl + 2);
  node->instructions()->accept(this, lvl + 2);
  _symtab.pop();
}

void mml::type_checker::do_variable_declaration_node(mml::variable_declaration_node *const node, int lvl) {
  if (node->initializer()) {
    node->initializer()->accept(this, lvl + 2);
    if (node->type() == nullptr) {
      node->type(node->initializer()->type());
    } else {
      // TODO: make sure that the type of the initializer is compatible with the variable's type
    }
  }

  auto symbol = std::make_shared<mml::symbol>(node->type(), node->name(), 0);
  _symtab.insert(node->name(), symbol);
}

void mml::type_checker::do_index_node(mml::index_node *const node, int lvl) {
  // TODO: expect base type to be a pointer.
  // TODO: expect index type to be int.
}

void mml::type_checker::do_address_of_node(mml::address_of_node *const node, int lvl) {
  // TODO: find variable and set type to pointer to variable type.
}

void mml::type_checker::do_evaluation_node(mml::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void mml::type_checker::do_print_node(mml::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);
  // TODO: make sure the arguments are int, real or string
}

void mml::type_checker::do_input_node(mml::input_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void mml::type_checker::do_function_node(mml::function_node *const node, int lvl) {
  // TODO: set current function type to this function's type.
  _symtab.push();
  node->arguments()->accept(this, lvl + 2);
  node->block()->accept(this, lvl + 2);
  _symtab.pop();
}

void mml::type_checker::do_call_node(mml::call_node *const node, int lvl) {
  // TODO: if function() is null, find type of the current function
  // TODO: check argument types.
  // TODO: set type to function return type.
}

void mml::type_checker::do_return_node(mml::return_node *const node, int lvl) {
  // TODO: make sure that the return type is compatible with the current function's return type
}

//---------------------------------------------------------------------------

void mml::type_checker::do_while_node(mml::while_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 2);
  // TODO: expect condition type to be int
}

void mml::type_checker::do_stop_node(mml::stop_node *const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_next_node(mml::next_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void mml::type_checker::do_if_node(mml::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 2);
  // TODO: expect condition type to be int
}

void mml::type_checker::do_if_else_node(mml::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 2);
  // TODO: expect condition type to be int
}

