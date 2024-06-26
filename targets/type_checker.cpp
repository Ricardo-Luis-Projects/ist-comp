#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include "mml_parser.tab.h"
#include <cdk/types/primitive_type.h>

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

std::string mml::to_string(std::shared_ptr<cdk::basic_type> type)
{
  if (type->name() == cdk::TYPE_VOID) {
    return "void";
  }

  if (type->name() == cdk::TYPE_INT) {
    return "int";
  }

  if (type->name() == cdk::TYPE_DOUBLE) {
    return "double";
  }

  if (type->name() == cdk::TYPE_STRING) {
    return "string";
  }

  if (type->name() == cdk::TYPE_UNSPEC) {
    return "unspec";
  }

  if (type->name() == cdk::TYPE_POINTER) {
    auto ptr = std::dynamic_pointer_cast<cdk::reference_type>(type);
    return '[' + mml::to_string(ptr->referenced()) + ']';
  }
  
  if (type->name() == cdk::TYPE_FUNCTIONAL) {
    auto ptr = std::dynamic_pointer_cast<cdk::functional_type>(type);
    std::string ret = "(";
    for (size_t i = 0; i < ptr->input_length(); i++) {
      ret += mml::to_string(ptr->input(i));
      if (i != ptr->input_length() - 1)
        ret += ", ";
    }
    ret += ") -> " + mml::to_string(ptr->output(0));
    return ret;
  }

  return "unknown";
}

static std::shared_ptr<cdk::primitive_type> create_unspec() {
  return cdk::primitive_type::create(0, cdk::TYPE_UNSPEC);
}

static std::shared_ptr<cdk::primitive_type> create_int() {
  return cdk::primitive_type::create(4, cdk::TYPE_INT);
}

static std::shared_ptr<cdk::primitive_type> create_double() {
  return cdk::primitive_type::create(8, cdk::TYPE_DOUBLE);
}

static std::shared_ptr<cdk::reference_type> create_pointer(std::shared_ptr<cdk::basic_type> referenced) {
  return cdk::reference_type::create(4, referenced);
}

bool mml::is_same(std::shared_ptr<cdk::basic_type> lhs, std::shared_ptr<cdk::basic_type> rhs) {
  if (lhs->name() == cdk::TYPE_POINTER && rhs->name() == cdk::TYPE_POINTER) {
    return mml::is_same(cdk::reference_type::cast(lhs)->referenced(), cdk::reference_type::cast(rhs)->referenced());
  }

  if (lhs->name() == cdk::TYPE_FUNCTIONAL && rhs->name() == cdk::TYPE_FUNCTIONAL) {
    auto lhs_func = cdk::functional_type::cast(lhs);
    auto rhs_func = cdk::functional_type::cast(rhs);

    if (lhs_func->input_length() != rhs_func->input_length()) {
      return false;
    }

    for (size_t i = 0; i < lhs_func->input_length(); i++) {
      if (!mml::is_same(lhs_func->input(i), rhs_func->input(i))) {
        return false;
      }
    }

    return mml::is_same(lhs_func->output(0), rhs_func->output(0));
  }

  return lhs->name() == rhs->name();
}

static std::pair<std::shared_ptr<cdk::basic_type>, std::shared_ptr<cdk::basic_type>> unify(
    std::shared_ptr<cdk::basic_type> from,
    std::shared_ptr<cdk::basic_type> to) {
  if (from->name() == cdk::TYPE_UNSPEC) {
    if (to->name() == cdk::TYPE_POINTER && cdk::reference_type::cast(to)->referenced()->name() == cdk::TYPE_VOID) {
      return {create_pointer(from), to};
    }

    return {to, to};
  }
  
  if (to->name() == cdk::TYPE_UNSPEC) {
    if (from->name() == cdk::TYPE_POINTER && cdk::reference_type::cast(from)->referenced()->name() == cdk::TYPE_VOID) {
      return {from, create_pointer(to)};
    }

    return {from, from};
  }

  if (from->name() == to->name() && (to->name() == cdk::TYPE_VOID || to->name() == cdk::TYPE_INT || to->name() == cdk::TYPE_DOUBLE || to->name() == cdk::TYPE_STRING)) {
    return {from, to};
  }

  if (from->name() == cdk::TYPE_INT && to->name() == cdk::TYPE_DOUBLE) {
    return {from, to};
  }
  
  try {
    if (from->name() == cdk::TYPE_POINTER && to->name() == cdk::TYPE_POINTER) {
      auto fromReferenced = cdk::reference_type::cast(from)->referenced();
      auto toReferenced = cdk::reference_type::cast(to)->referenced();

      if ((fromReferenced->name() == cdk::TYPE_POINTER && toReferenced->name() == cdk::TYPE_POINTER) ||
           fromReferenced->name() == cdk::TYPE_UNSPEC || toReferenced->name() == cdk::TYPE_UNSPEC) {
        auto [newFromReferenced, newToReferenced] = unify(fromReferenced, toReferenced);

        if (newFromReferenced.get() != fromReferenced.get()) {
          from = create_pointer(newFromReferenced);
        }

        if (newToReferenced.get() != toReferenced.get()) {
          to = create_pointer(newToReferenced);
        }
      } else if (fromReferenced->name() != cdk::TYPE_VOID && toReferenced->name() != cdk::TYPE_VOID) {
        if (!mml::is_same(fromReferenced, toReferenced)) {
          throw std::string("cannot unify pointers to different types");
        }
      }

      return {from, to};
    }
    
    if (from->name() == cdk::TYPE_FUNCTIONAL && to->name() == cdk::TYPE_FUNCTIONAL) {
      auto fromFunc = cdk::functional_type::cast(from);
      auto toFunc = cdk::functional_type::cast(to);

      if (fromFunc->input_length() != toFunc->input_length()) {
        throw std::string("cannot unify functions with different arguments");
      }

      std::vector<std::shared_ptr<cdk::basic_type>> fromInputs;
      std::vector<std::shared_ptr<cdk::basic_type>> toInputs;
      bool fromInputChanged = false;
      bool toInputChanged = false;

      for (std::size_t i = 0; i < fromFunc->input_length(); i++) {
        auto [newTo, newFrom] = unify(toFunc->input(i), fromFunc->input(i));
        fromInputs.push_back(newFrom);
        toInputs.push_back(newTo);
        fromInputChanged |= newFrom.get() != fromFunc->input(i).get();
        toInputChanged |= newTo.get() != toFunc->input(i).get();
      }

      auto [fromOutput, toOutput] = unify(fromFunc->output(0), toFunc->output(0));
      if (fromOutput.get() != fromFunc->output(0).get() || fromInputChanged) {
        from = cdk::functional_type::create(fromInputs, fromOutput);
      }
      if (toOutput.get() != toFunc->output(0).get() || toInputChanged) {
        to = cdk::functional_type::create(toInputs, toOutput);
      }

      return {from, to};
    }
  } catch (std::string& e) {
    e = "cannot cast '" + mml::to_string(from) + "' to '" + mml::to_string(to) + "'\n" + e;
    throw e;
  }

  throw std::string("cannot cast '" + mml::to_string(from) + "' to '" + mml::to_string(to) + "'");
}

/** Creates a new type where all unspecified types are converted to int. */
static std::shared_ptr<cdk::basic_type> default_to_int(std::shared_ptr<cdk::basic_type> type) {
  if (type->name() == cdk::TYPE_UNSPEC) {
    return cdk::primitive_type::create(4, cdk::TYPE_INT);
  } else if (type->name() == cdk::TYPE_POINTER) {
    auto reference = std::dynamic_pointer_cast<cdk::reference_type>(type);
    auto referenced = default_to_int(reference->referenced());
    if (reference->referenced().get() != referenced.get()) {
      return cdk::reference_type::create(4, referenced);
    } else {
      return type;
    }
  } else if (type->name() == cdk::TYPE_FUNCTIONAL) {
    auto functional = std::dynamic_pointer_cast<cdk::functional_type>(type);

    std::vector<std::shared_ptr<cdk::basic_type>> inputs;
    bool inputChanged = false;
    for (auto& input : functional->input()->components()) {
      auto newInput = default_to_int(input);
      inputChanged |= newInput.get() != input.get();
      inputs.push_back(newInput);
    }

    auto output = default_to_int(functional->output(0));
    if (output.get() != functional->output(0).get() || inputChanged) {
      return cdk::functional_type::create(inputs, output);
    } else {
      return type;
    }
  } else {
    return type;
  }
}

/** Unifies the type of the given node to the given type, returns the unified version of the given type. */
std::shared_ptr<cdk::basic_type> mml::type_checker::unify_node_to_type(cdk::typed_node *const node, std::shared_ptr<cdk::basic_type> to, int lvl) {
  auto oldFrom = node->type();
  
  try {
    auto [newFrom, newTo] = unify(oldFrom, to);
    if (newFrom.get() != oldFrom.get()) {
      node->type(newFrom);
      propagate(node, lvl + 2);
      if (_isTesting) {
        node->type(oldFrom);
      }
    }

    return newTo;
  } catch (std::string& e) {
    node->type(oldFrom);
    e = "failed to unify '" + node->label() + "' from type '" + mml::to_string(oldFrom) + "' to '" + mml::to_string(to) + "'\n" + e;
    throw e;
  }
}

void mml::type_checker::unify_node_to_node(cdk::typed_node *const from, cdk::typed_node *const to, int lvl)
{
  auto oldTo = to->type();

  try {
    auto newTo = unify_node_to_type(from, oldTo, lvl);
    if (newTo.get() != oldTo.get()) {
      to->type(newTo);
      propagate(to, lvl + 2);
      if (_isTesting) {
        to->type(oldTo);
      }
    }
  } catch (std::string&) {
    to->type(oldTo);
    throw;
  }
}

bool mml::type_checker::test_unify_node_to_type(cdk::typed_node *const node, std::shared_ptr<cdk::basic_type> to, int lvl) {
  _isTesting = true;
  try {
    unify_node_to_type(node, to, lvl);
    _isTesting = false;
    return true;
  } catch (std::string&) {
    _isTesting = false;
    return false;
  }
}

void mml::type_checker::default_node_to_int(cdk::typed_node *const node, int lvl) {
  unify_node_to_type(node, default_to_int(node->type()), lvl);
}

void mml::type_checker::propagate(cdk::typed_node *const node, int lvl) {
  if (_isPropagating) {
    node->accept(this, lvl);
    return;
  }

  _isPropagating = true;
  node->accept(this, lvl);
  _isPropagating = false;
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
  for (auto& node : node->nodes()) {
    node->accept(this, lvl + 2);
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(create_int());
}

void mml::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(create_double());
}

void mml::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

void mml::type_checker::do_null_node(mml::null_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(create_pointer(create_unspec()));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);
  unify_node_to_type(node->argument(), create_int(), lvl + 2);
  node->type(node->argument()->type());
}

void mml::type_checker::processUnaryExpression(cdk::unary_operation_node *const node, int lvl) {
  if (_isPropagating) {
    unify_node_to_type(node->argument(), node->type(), lvl + 2);
    return;
  }

  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);

  if (node->argument()->is_typed(cdk::TYPE_DOUBLE) || node->argument()->is_typed(cdk::TYPE_INT) || node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(node->argument()->type());
  } else {
    throw std::string("expected int or double in argument of unary operator, found " + mml::to_string(node->argument()->type()));
  }
}

void mml::type_checker::do_neg_node(cdk::neg_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

void mml::type_checker::do_identity_node(mml::identity_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

void mml::type_checker::do_sizeof_node(mml::sizeof_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);
  default_node_to_int(node->argument(), lvl + 2);

  node->type(create_int());
}

void mml::type_checker::do_stack_alloc_node(mml::stack_alloc_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);

  unify_node_to_type(node->argument(), create_int(), lvl + 2);

  node->type(create_pointer(create_unspec()));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  // U + U = U
  // U + I = U
  // U + D = D (U <- D)
  // U + P = P (U <- I)
  // I + U = U
  // I + I = I
  // I + D = D
  // I + P = P
  // D + U = D (U <- D)
  // D + I = D
  // D + D = D
  // D + P = boom
  // P + U = P (U <- I)
  // P + I = P
  // P + D = boom
  // P + P = boom

  if (_isPropagating) {
    if (node->is_typed(cdk::TYPE_POINTER)) {
        // Either U + U, U + I or I + U.
        if (node->left()->is_typed(cdk::TYPE_INT)) {
          // I + U = P (U <- P).
          unify_node_to_type(node->right(), node->type(), lvl + 2);
        } else if (node->right()->is_typed(cdk::TYPE_INT)) {
          // U + I = P (U <- P).
          unify_node_to_type(node->left(), node->type(), lvl + 2);
        } else {
          // U1 + U2 = P.
          // Either U1 or U2 must unify with I - we check which is possible.
          if (test_unify_node_to_type(node->left(), create_int(), lvl + 2) &&
              test_unify_node_to_type(node->right(), node->type(), lvl + 2)) {
            unify_node_to_type(node->left(), create_int(), lvl + 2);
            unify_node_to_type(node->right(), node->type(), lvl + 2);
          } else {
            unify_node_to_type(node->left(), node->type(), lvl + 2);
            unify_node_to_type(node->right(), create_int(), lvl + 2);
          }
        }
    } else {
      // Both arguments unify with the expression type, removing any previous unspecified type.
      unify_node_to_type(node->left(), node->type(), lvl + 2);
      unify_node_to_type(node->right(), node->type(), lvl + 2);
    }

    return;
  }

  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  // Given x + y = z
  if (node->left()->is_typed(cdk::TYPE_POINTER)) {
    // If x = P, then y <- I, z = P
    unify_node_to_type(node->right(), create_int(), lvl + 2);
    node->type(node->left()->type()); // Results in a pointer.
  } else if (node->right()->is_typed(cdk::TYPE_POINTER)) {
    // Else if y = P, then x <- I, z = P
    unify_node_to_type(node->left(), create_int(), lvl + 2);
    node->type(node->right()->type()); // Results in a pointer.
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    // Else if, x = D, then y <- D, z = D
    unify_node_to_type(node->right(), create_double(), lvl + 2);
    node->type(node->left()->type()); // Results in a double.
  } else if (node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    // Else if, y = D, then x <- D, z = D
    unify_node_to_type(node->left(), create_double(), lvl + 2);
    node->type(node->right()->type()); // Results in a double.
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT)) {
    // Else if, x = I and y = I, then z = I
    node->type(node->left()->type()); // Results in an int.
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    // Else if, x = I and y = U, then z = U
    node->type(node->right()->type()); // Remains unspecified - leave decision between int and double to the parent.
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) && node->right()->is_typed(cdk::TYPE_INT)) {
    // Else if, x = U and y = I, then z = U
    node->type(node->left()->type()); // Remains unspecified - leave decision between int and double to the parent.
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) && node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    // Else if, x = U and y = U, then z = U
    node->type(node->left()->type()); // Remains unspecified - leave decision between int and double to the parent.
  } else {
    throw std::string("invalid operand types for addition expression");
  }
}

void mml::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  // U - U = U
  // U - I = U
  // U - D = D (U <- D)
  // U - P = I (U <- P)
  // I - U = U
  // I - I = I
  // I - D = D
  // I - P = boom
  // D - U = D (U <- D)
  // D - I = D
  // D - D = D
  // D - P = boom
  // P - U = U
  // P - I = P
  // P - D = boom
  // P - P = I

  if (_isPropagating) {
    if (node->is_typed(cdk::TYPE_POINTER)) {
      // X - Y = P, X <- P, Y <- I
      unify_node_to_type(node->left(), node->type(), lvl + 2);
      unify_node_to_type(node->right(), create_int(), lvl + 2);
    } else if (node->is_typed(cdk::TYPE_INT)) {
      if (node->left()->is_typed(cdk::TYPE_POINTER)) {
        // P - U = I, U <- P
        unify_node_to_type(node->left(), create_pointer(create_unspec()), lvl + 2);
        unify_node_to_node(node->left(), node->right(), lvl + 2);
        default_node_to_int(node->left(), lvl + 2);
        default_node_to_int(node->right(), lvl + 2);
      } else {
        // U - U = I, U <- I
        unify_node_to_type(node->left(), node->type(), lvl + 2);
        unify_node_to_type(node->right(), node->type(), lvl + 2);
      }
    } else {
      // Both arguments unify with the expression type, removing any previous unspecified type.
      unify_node_to_type(node->left(), node->type(), lvl + 2);
      unify_node_to_type(node->right(), node->type(), lvl + 2);
    }
  }

  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  // Given x - y = z
  if (node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    // If x = D, then y <- D, z = D
    unify_node_to_type(node->right(), create_double(), lvl + 2);
    node->type(node->left()->type()); // Results in a double.
  } else if (node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    // Else if y = D, then x <- D, z = D
    unify_node_to_type(node->left(), create_double(), lvl + 2);
    node->type(node->right()->type()); // Results in a double.
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) && node->right()->is_typed(cdk::TYPE_POINTER)) {
    // Else if x = U and y = P, then x <- P, z = I
    unify_node_to_type(node->left(), node->right()->type(), lvl + 2);
    node->type(create_int()); // Results in an int.
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) || node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    // Else if x = U or y = U, then z = U
    node->type(create_unspec()); // Remains unspecified - leave decision to parent.
  } else if (node->left()->is_typed(cdk::TYPE_INT)) {
    // Else if x = I, then y <- I, z = I
    unify_node_to_type(node->right(), create_int(), lvl + 2);
    node->type(node->left()->type()); // Results in an int.
  } else if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    // Else if x = P and y = I, then z = P
    node->type(node->left()->type()); // Results in a pointer.
  } else if (node->left()->is_typed(cdk::TYPE_POINTER)) {
    // Else if x = P, then x <-> y, z = I
    unify_node_to_node(node->left(), node->right(), lvl + 2);
    node->type(create_int()); // Results in an int.
  } else {
    throw std::string("invalid operand types for subtraction expression");
  }
}

void mml::type_checker::processMulExpression(cdk::binary_operation_node *const node, int lvl) {
  // U * U = U
  // U * I = U
  // U * D = D (U <- D)
  // I * U = U
  // I * I = I
  // I * D = D
  // D * U = D (U <- D)
  // D * I = D
  // D * D = D

  if (_isPropagating) {
    if (!node->is_typed(cdk::TYPE_INT) && !node->is_typed(cdk::TYPE_DOUBLE)) {
      throw "multiplicative expression cannot produce a " + mml::to_string(node->type());
    }

    unify_node_to_type(node->left(), node->type(), lvl + 2);
    unify_node_to_type(node->right(), node->type(), lvl + 2);
    return;
  }

  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    // If X = D, then Y <- D, Z = D
    unify_node_to_type(node->right(), create_double(), lvl + 2);
    node->type(create_double()); // Results in a double.
  } else if (node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    // Else if Y = D, then X <- D, Z = D
    unify_node_to_type(node->left(), create_double(), lvl + 2);
    node->type(create_double()); // Results in a double.
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) || node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    // If either X or Y is U, then Z = U.
    node->type(create_unspec()); // Remains unspecified - leave decision to parent.
  } else {
    // Else, X <- I, Y <- I, Z = I.
    unify_node_to_type(node->left(), create_int(), lvl + 2);
    unify_node_to_type(node->right(), create_int(), lvl + 2);
    node->type(create_int()); // Results in an int.
  }
}

void mml::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  processMulExpression(node, lvl);
}
void mml::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  processMulExpression(node, lvl);
}

void mml::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  node->type(create_int()); // Always results in an int.

  unify_node_to_type(node->left(), create_int(), lvl + 2);
  unify_node_to_type(node->right(), create_int(), lvl + 2);
}

void mml::type_checker::processCmpExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  node->type(create_int()); // Always results in an int.

  if (node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    unify_node_to_type(node->right(), create_double(), lvl + 2);
  } else if (node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    unify_node_to_type(node->left(), create_double(), lvl + 2);
  } else {
    unify_node_to_type(node->left(), create_int(), lvl + 2);
    unify_node_to_type(node->right(), create_int(), lvl + 2);
  }
}

void mml::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processCmpExpression(node, lvl);
}
void mml::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processCmpExpression(node, lvl);
}
void mml::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processCmpExpression(node, lvl);
}
void mml::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processCmpExpression(node, lvl);
}

void mml::type_checker::processEqExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  node->type(create_int()); // Always results in an int.

  if (node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    unify_node_to_type(node->right(), create_double(), lvl + 2);
  } else if (node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    unify_node_to_type(node->left(), create_double(), lvl + 2);
  } else if (node->left()->is_typed(cdk::TYPE_POINTER) || node->right()->is_typed(cdk::TYPE_POINTER)) {
    unify_node_to_node(node->left(), node->right(), lvl + 2);
    default_node_to_int(node->left(), lvl + 2);
    default_node_to_int(node->right(), lvl + 2);
  } else {
    unify_node_to_type(node->left(), create_int(), lvl + 2);
    unify_node_to_type(node->right(), create_int(), lvl + 2);
  }
}

void mml::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processEqExpression(node, lvl);
}
void mml::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processEqExpression(node, lvl);
}

void mml::type_checker::processLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  node->type(create_int()); // Always results in an int.

  unify_node_to_type(node->left(), create_int(), lvl + 2);
  unify_node_to_type(node->right(), create_int(), lvl + 2);
}

void mml::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  processLogicalExpression(node, lvl);
}
void mml::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  processLogicalExpression(node, lvl);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<mml::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->node()->type());
  } else {
    throw "undeclared variable '" + id + "'";
  }
}

void mml::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  if (_isPropagating) {
    unify_node_to_type(node->lvalue(), node->type(), lvl + 2);
    return;
  }

  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl);
  node->type(node->lvalue()->type());
}

void mml::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  if (_isPropagating) {
    unify_node_to_type(node->lvalue(), node->type(), lvl + 2);
    unify_node_to_node(node->rvalue(), node->lvalue(), lvl + 2);
    return;
  }

  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl + 2);
  node->rvalue()->accept(this, lvl + 2);

  unify_node_to_node(node->rvalue(), node->lvalue(), lvl + 2);

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
      // Figure out the type of the variable through the initializer. Default unspecs to int.
      node->type(default_to_int(node->initializer()->type()));
    }
  
    unify_node_to_type(node->initializer(), node->type(), lvl + 2);
    default_node_to_int(node->initializer(), lvl + 2);

    if (node->is_typed(cdk::TYPE_VOID)) {
      throw std::string("variable '" + node->name() + "' cannot be of type void");
    }
  }

  auto symbol = _symtab.find_local(node->name());
  if (symbol != nullptr)
  {
    if (symbol->node() == node) {
      return;
    }

    if (!mml::is_same(node->type(), symbol->node()->type())) {
      throw std::string("variable '" + node->name() + "' redeclared with different type");
    }

    if (node->qualifier() == tFOREIGN || symbol->node()->qualifier() == tFOREIGN) {
      throw std::string("variable '" + node->name() + "' redeclared as foreign");
    }

    if (symbol->node()->qualifier() == tFORWARD) {
      symbol->node(node);
      return;
    }

    if (node->qualifier() == tFORWARD) {
      return;
    }

    throw std::string("variable '" + node->name() + "' redeclared on its scope");
  }

  if (node->qualifier() == tFOREIGN) {
    // Foreign variables must be functions.
    if (!node->is_typed(cdk::TYPE_FUNCTIONAL)) {
      throw std::string("foreign variable must be a function");
    }
  }

  _symtab.insert(node->name(), std::make_shared<mml::symbol>(node));
}

void mml::type_checker::do_index_node(mml::index_node *const node, int lvl) {
  if (_isPropagating) {
    // The base node must be a pointer to this node.
    unify_node_to_type(node->base(), create_pointer(node->type()), lvl + 2);
    return;
  }

  ASSERT_UNSPEC;

  node->base()->accept(this, lvl + 2);
  node->index()->accept(this, lvl + 2);

  // Base must be a pointer to something, so we unify it with a pointer to unspec.
  auto newType = unify_node_to_type(node->base(), create_pointer(create_unspec()), lvl + 2);

  // The type is what the referenced unspec was unified to.
  node->type(cdk::reference_type::cast(newType)->referenced());

  // The index must unify to an integer.
  unify_node_to_type(node->index(), create_int(), lvl + 2);
}

void mml::type_checker::do_address_of_node(mml::address_of_node *const node, int lvl) {
  if (_isPropagating) {
    // The lvalue node must be the referenced type of this node.
    unify_node_to_type(node->lvalue(), cdk::reference_type::cast(node->type())->referenced(), lvl + 2);
    return;
  }

  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl + 2);

  // No matter what, the type of this node is always a pointer to the lvalue.
  node->type(create_pointer(node->lvalue()->type()));
}

void mml::type_checker::do_evaluation_node(mml::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);

  // We must unify the argument with its type but where all instances of TYPE_UNSPEC are replaced
  // with TYPE_INT.
  default_node_to_int(node->argument(), lvl + 2);
}

void mml::type_checker::do_print_node(mml::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);

  for (const auto& argument : node->arguments()->nodes()) {
    auto typed = static_cast<cdk::typed_node*>(argument);

    // We only unify to int if the type is still unspecified.
    if (typed->is_typed(cdk::TYPE_UNSPEC)) {
      unify_node_to_type(typed, create_int(), lvl + 2);
    } else if (!typed->is_typed(cdk::TYPE_INT) && !typed->is_typed(cdk::TYPE_DOUBLE) && !typed->is_typed(cdk::TYPE_STRING) && !typed->is_typed(cdk::TYPE_VOID)) {
      throw std::string("wrong type in print expression");
    }
  }
}

void mml::type_checker::do_input_node(mml::input_node *const node, int lvl) {
  if (!_isPropagating) {
    ASSERT_UNSPEC;

    node->type(create_unspec());
  } else if (!node->is_typed(cdk::TYPE_INT) && !node->is_typed(cdk::TYPE_DOUBLE)) {
    throw std::string("input only supports integer or real numbers");
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_function_node(mml::function_node *const node, int lvl) {
  auto oldFunctionType = _functionType;
  auto oldIsMain = _isMain;
  _functionType = cdk::functional_type::cast(node->type());
  _isMain = node->main();

  _symtab.push();
  node->arguments()->accept(this, lvl + 2);
  node->block()->accept(this, lvl + 2);
  _symtab.pop();

  _functionType = oldFunctionType;
  _isMain = oldIsMain;
}

void mml::type_checker::do_call_node(mml::call_node *const node, int lvl) {
  std::shared_ptr<cdk::functional_type> functionType;
  
  if (_isPropagating) {
    // This never happens when its a recursive call, since propagations only happen when the node's
    // type is left unspecified.

    // Make sure that the function return type unifies with this node.
    functionType = cdk::functional_type::cast(node->function()->type());
    auto expectedFunctionType = cdk::functional_type::create(functionType->input()->components(), node->type());
    unify_node_to_type(node->function(), expectedFunctionType, lvl + 2);
    return;
  }

  ASSERT_UNSPEC;

  node->arguments()->accept(this, lvl + 2);

  if (node->function() == nullptr) {
    if (_functionType == nullptr || _isMain) {
      throw std::string("cannot call recurse outside of function definition");
    }

    functionType = _functionType;

    if (functionType->input_length() != node->arguments()->size()) {
      throw std::string("wrong number of arguments in recursive call");
    }

    // Unify the arguments to the expected input types.
    for (std::size_t i = 0; i < node->arguments()->size(); ++i) {
      auto typed = static_cast<cdk::typed_node*>(node->arguments()->node(i));
      unify_node_to_type(typed, functionType->input(i), lvl + 2);
      default_node_to_int(typed, lvl + 2);
    }
  } else {
    node->function()->accept(this, lvl + 2);

    // Create the expected function type.
    std::vector<std::shared_ptr<cdk::basic_type>> inputs;
    for (auto* node : node->arguments()->nodes()) {
      inputs.push_back(static_cast<cdk::typed_node*>(node)->type());
    }

    // Unify the function node with the expected type.
    auto expectedFunctionType = cdk::functional_type::create(inputs, create_unspec());
    functionType = cdk::functional_type::cast(unify_node_to_type(node->function(), expectedFunctionType, lvl + 2));
  
    // Unify the arguments to the expected input types.
    bool changed = false;
    for (std::size_t i = 0; i < node->arguments()->size(); ++i) {
      auto typed = static_cast<cdk::typed_node*>(node->arguments()->node(i));
      inputs[i] = unify_node_to_type(typed, default_to_int(functionType->input(i)), lvl + 2);
      changed |= inputs[i] != functionType->input(i);
    }

    if (changed) {
      unify_node_to_type(node->function(), cdk::functional_type::create(inputs, functionType->output(0)), lvl + 2);
    }
  }

  node->type(functionType->output(0));
}

void mml::type_checker::do_return_node(mml::return_node *const node, int lvl) {
  auto outputType = _functionType == nullptr ? create_int() : _functionType->output(0);

  if (outputType->name() != cdk::TYPE_VOID) {
    if (node->retval() == nullptr) {
      throw std::string("non-void function must return a value");
    }

    node->retval()->accept(this, lvl + 2);
    unify_node_to_type(node->retval(), outputType, lvl + 2);
    default_node_to_int(node->retval(), lvl + 2);
  } else if (node->retval() != nullptr) {
    throw std::string("void function cannot return a value");
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_while_node(mml::while_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 2);
  node->block()->accept(this, lvl + 2);
  unify_node_to_type(node->condition(), create_int(), lvl + 2);
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
  node->block()->accept(this, lvl + 2);
  unify_node_to_type(node->condition(), create_int(), lvl + 2);
}

void mml::type_checker::do_if_else_node(mml::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 2);
  node->thenblock()->accept(this, lvl + 2);
  node->elseblock()->accept(this, lvl + 2);
  unify_node_to_type(node->condition(), create_int(), lvl + 2);
}

