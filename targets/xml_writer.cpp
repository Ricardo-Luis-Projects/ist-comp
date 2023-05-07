#include <string>
#include "targets/xml_writer.h"
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include "mml_parser.tab.h"

static const char* qualifier_name(int qualifier) {
  switch (qualifier) {
    case tFORWARD:
      return "forward";
    case tFOREIGN:
      return "foreign";
    case tPUBLIC:
      return "public";
    default:
      return "unknown";
  }
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}
void mml::xml_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}
void mml::xml_writer::do_double_node(cdk::double_node * const node, int lvl) {
  // EMPTY
}
void mml::xml_writer::do_not_node(cdk::not_node * const node, int lvl) {
  // EMPTY
}
void mml::xml_writer::do_and_node(cdk::and_node * const node, int lvl) {
  // EMPTY
}
void mml::xml_writer::do_or_node(cdk::or_node * const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  os() << std::string(lvl, ' ') << "<sequence_node size='" << node->size() << "'>" << std::endl;
  for (size_t i = 0; i < node->size(); i++)
    node->node(i)->accept(this, lvl + 2);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  process_literal(node, lvl);
}

void mml::xml_writer::do_string_node(cdk::string_node * const node, int lvl) {
  process_literal(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_unary_operation(cdk::unary_operation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->argument()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_neg_node(cdk::neg_node * const node, int lvl) {
  do_unary_operation(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_binary_operation(cdk::binary_operation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_add_node(cdk::add_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_div_node(cdk::div_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_le_node(cdk::le_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  os() << std::string(lvl, ' ') << "<" << node->label() << ">" << node->name() << "</" << node->label() << ">" << std::endl;
}

void mml::xml_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->lvalue()->accept(this, lvl + 4);
  closeTag(node, lvl);
}

void mml::xml_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);

  node->lvalue()->accept(this, lvl);
  reset_new_symbol();

  node->rvalue()->accept(this, lvl + 4);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_block_node(mml::block_node * const node, int lvl) {
  openTag(node, lvl);
  node->declarations()->accept(this, lvl + 4);
  node->instructions()->accept(this, lvl + 4);
  closeTag(node, lvl);
}

void mml::xml_writer::do_return_node(mml::return_node *const node, int lvl) {
  openTag(node, lvl);
  node->retval()->accept(this, lvl + 4);
  closeTag(node, lvl);
}

void mml::xml_writer::do_stop_node(mml::stop_node *const node, int lvl) {
  openTag(node, lvl);
  node->nesting()->accept(this, lvl + 4);
  closeTag(node, lvl);
}

void mml::xml_writer::do_next_node(mml::next_node *const node, int lvl) {
  openTag(node, lvl);
  node->nesting()->accept(this, lvl + 4);
  closeTag(node, lvl);
}

void mml::xml_writer::do_null_node(mml::null_node *const node, int lvl) {
  openTag(node, lvl);
  closeTag(node, lvl);
}

void mml::xml_writer::do_sizeof_node(mml::sizeof_node *const node, int lvl) {
  openTag(node, lvl);
  node->argument()->accept(this, lvl + 4);
  closeTag(node, lvl);
}

void mml::xml_writer::do_variable_declaration_node(mml::variable_declaration_node *const node, int lvl) {
  os() << std::string(lvl, ' ') << "<" << node->label() << " name='" << node->name() << '\'';
  if (node->qualifier() != 0) {
    os() << " qualifier='" << qualifier_name(node->qualifier()) << '\'';
  }
  if (node->type() != nullptr) {
    os() << " type='" << cdk::to_string(node->type()) << '\'';
  }
  os() << ">" << std::endl;

  if (node->initializer()) {
    openTag("initializer", lvl);
    node->initializer()->accept(this, lvl + 4);
    closeTag("initializer", lvl);
  }

  closeTag(node, lvl);
}

void mml::xml_writer::do_function_call_node(mml::function_call_node *const node, int lvl) {
  openTag(node, lvl);
  openTag("function", lvl + 2);
  node->function()->accept(this, lvl + 4);
  closeTag("function", lvl + 2);
  openTag("arguments", lvl + 2);
  node->arguments()->accept(this, lvl + 4);
  closeTag("arguments", lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_function_self_node(mml::function_self_node *const node, int lvl) {
  os() << std::string(lvl, ' ') << "<" << node->label() << "></" << node->label() << ">" << std::endl;
}

void mml::xml_writer::do_identity_node(mml::identity_node *const node, int lvl) {
  openTag(node, lvl);
  node->argument()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_index_node(mml::index_node *const node, int lvl) {
  openTag(node, lvl);
  openTag("base", lvl);
  node->base()->accept(this, lvl + 2);
  closeTag("base", lvl);
  openTag("index", lvl);
  node->index()->accept(this, lvl + 2);
  closeTag("index", lvl);
  closeTag(node, lvl);
}

void mml::xml_writer::do_address_of_node(mml::address_of_node *const node, int lvl) {
  openTag(node, lvl);
  node->lvalue()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_stack_alloc_node(mml::stack_alloc_node *const node, int lvl) {
  openTag(node, lvl);
  node->argument()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_function_node(mml::function_node * const node, int lvl) {
  openTag(node->label() + (node->main() ? " main=true" : " main=false"), lvl);
  node->arguments()->accept(this, lvl + 4);
  node->block()->accept(this, lvl + 4);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_evaluation_node(mml::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->argument()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_print_node(mml::print_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  openTag(node->label() + (node->newline() ? " newline=true" : " newline=false"), lvl);
  node->arguments()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_input_node(mml::input_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_while_node(mml::while_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("condition", lvl + 2);
  node->condition()->accept(this, lvl + 4);
  closeTag("condition", lvl + 2);
  openTag("block", lvl + 2);
  node->block()->accept(this, lvl + 4);
  closeTag("block", lvl + 2);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_if_node(mml::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("condition", lvl + 2);
  node->condition()->accept(this, lvl + 4);
  closeTag("condition", lvl + 2);
  openTag("then", lvl + 2);
  node->block()->accept(this, lvl + 4);
  closeTag("then", lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_if_else_node(mml::if_else_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("condition", lvl + 2);
  node->condition()->accept(this, lvl + 4);
  closeTag("condition", lvl + 2);
  openTag("then", lvl + 2);
  node->thenblock()->accept(this, lvl + 4);
  closeTag("then", lvl + 2);
  openTag("else", lvl + 2);
  node->elseblock()->accept(this, lvl + 4);
  closeTag("else", lvl + 2);
  closeTag(node, lvl);
}
