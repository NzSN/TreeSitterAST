import { strict as assert } from "assert";
export class SyntaticNode {
    evaluate(): string {
        throw Error("Interior or Leaf should implement evaluate().");
    }
}

export class SyntaticLeaf extends SyntaticNode {
    value_: string;
    constructor(value: string) {
        super();
        this.value_ = value;
    }
    evaluate(): string {
        return this.value_;
    }
}

export class SyntaticInterior extends SyntaticNode {
    evaluate(): string {
        throw Error("Instance of Interior should implment evaluate()");
    }
}
export class Yield_expression_T extends SyntaticInterior {
    expression_0_i: Expression_T;
    constructor() {
        super();
    }
}
export class With_statement_T extends SyntaticInterior {
    object_0_i: Parenthesized_expression_T;
    body_0_i: Statement_T;
    constructor() {
        super();
    }
}
export class While_statement_T extends SyntaticInterior {
    condition_0_i: Parenthesized_expression_T;
    body_0_i: Statement_T;
    constructor() {
        super();
    }
}
export class Variable_declarator_T extends SyntaticInterior {
    name_0_i: Identifier_T | _destructuring_pattern_T;
    _initializer_0_i: _initializer_T;
    constructor() {
        super();
    }
}
export class Variable_declaration_T extends SyntaticInterior {
    variable_declarator_0_i: Variable_declarator_T;
    variable_declarator_1_i: Variable_declarator_T;
    _semicolon_0_i: _semicolon_T;
    constructor() {
        super();
    }
}
export class Using_declaration_T extends SyntaticInterior {
    kind_0_i: undefined;
    variable_declarator_0_i: Variable_declarator_T;
    variable_declarator_1_i: Variable_declarator_T;
    _semicolon_0_i: _semicolon_T;
    constructor() {
        super();
    }
}
export class Update_expression_T extends SyntaticInterior {
    operator_0_i: undefined;
    argument_0_i: Expression_T;
    constructor() {
        super();
    }
}
export class Unescaped_single_string_fragment_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Unescaped_single_jsx_string_fragment_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Unescaped_double_string_fragment_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Unescaped_double_jsx_string_fragment_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Undefined_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Unary_expression_T extends SyntaticInterior {
    operator_0_i: undefined;
    argument_0_i: Expression_T;
    constructor() {
        super();
    }
}
export class Try_statement_T extends SyntaticInterior {
    handler_0_i: Catch_clause_T;
    finalizer_0_i: Finally_clause_T;
    body_0_i: Statement_block_T;
    constructor() {
        super();
    }
}
export class True_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Throw_statement_T extends SyntaticInterior {
    _semicolon_0_i: _semicolon_T;
    _expressions_0_i: _expressions_T;
    constructor() {
        super();
    }
}
export class This_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Ternary_expression_T extends SyntaticInterior {
    consequence_0_i: Expression_T;
    condition_0_i: Expression_T;
    alternative_0_i: Expression_T;
    constructor() {
        super();
    }
}
export class Template_substitution_T extends SyntaticInterior {
    _expressions_0_i: _expressions_T;
    constructor() {
        super();
    }
}
export class Template_string_T extends SyntaticInterior {
    template_substitution_0_i: Template_substitution_T;
    escape_sequence_0_i: Escape_sequence_T;
    constructor() {
        super();
    }
}
export class Switch_statement_T extends SyntaticInterior {
    value_0_i: Parenthesized_expression_T;
    body_0_i: Switch_body_T;
    constructor() {
        super();
    }
}
export class Switch_default_T extends SyntaticInterior {
    body_0_i: Statement_T;
    constructor() {
        super();
    }
}
export class Switch_case_T extends SyntaticInterior {
    value_0_i: _expressions_T;
    body_0_i: Statement_T;
    constructor() {
        super();
    }
}
export class Switch_body_T extends SyntaticInterior {
    switch_default_0_i: Switch_default_T;
    switch_case_0_i: Switch_case_T;
    constructor() {
        super();
    }
}
export class Super_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Subscript_expression_T extends SyntaticInterior {
    optional_chain_0_i: Optional_chain_T;
    object_0_i: Expression_T | Primary_expression_T;
    index_0_i: _expressions_T;
    constructor() {
        super();
    }
}
export class String_T extends SyntaticInterior {
    unescaped_single_string_fragment_0_i: Unescaped_single_string_fragment_T;
    unescaped_double_string_fragment_0_i: Unescaped_double_string_fragment_T;
    escape_sequence_0_i: Escape_sequence_T;
    constructor() {
        super();
    }
}
export class Statement_block_T extends SyntaticInterior {
    statement_0_i: Statement_T;
    _automatic_semicolon_0_i: _automatic_semicolon_T;
    constructor() {
        super();
    }
}
export class Statement_T extends SyntaticInterior {
    with_statement_0_i: With_statement_T;
    while_statement_0_i: While_statement_T;
    try_statement_0_i: Try_statement_T;
    throw_statement_0_i: Throw_statement_T;
    switch_statement_0_i: Switch_statement_T;
    statement_block_0_i: Statement_block_T;
    return_statement_0_i: Return_statement_T;
    labeled_statement_0_i: Labeled_statement_T;
    import_statement_0_i: Import_statement_T;
    if_statement_0_i: If_statement_T;
    for_statement_0_i: For_statement_T;
    for_in_statement_0_i: For_in_statement_T;
    expression_statement_0_i: Expression_statement_T;
    export_statement_0_i: Export_statement_T;
    empty_statement_0_i: Empty_statement_T;
    do_statement_0_i: Do_statement_T;
    declaration_0_i: Declaration_T;
    debugger_statement_0_i: Debugger_statement_T;
    continue_statement_0_i: Continue_statement_T;
    break_statement_0_i: Break_statement_T;
    constructor() {
        super();
    }
}
export class Spread_element_T extends SyntaticInterior {
    expression_0_i: Expression_T;
    constructor() {
        super();
    }
}
export class Sequence_expression_T extends SyntaticInterior {
    expression_0_i: Expression_T;
    expression_1_i: Expression_T;
    constructor() {
        super();
    }
}
export class Return_statement_T extends SyntaticInterior {
    _semicolon_0_i: _semicolon_T;
    _expressions_0_i: _expressions_T;
    constructor() {
        super();
    }
}
export class Rest_pattern_T extends SyntaticInterior {
    _lhs_expression_0_i: _lhs_expression_T;
    constructor() {
        super();
    }
}
export class Regex_pattern_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Regex_flags_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Regex_T extends SyntaticInterior {
    pattern_0_i: Regex_pattern_T;
    flags_0_i: Regex_flags_T;
    constructor() {
        super();
    }
}
export class Program_T extends SyntaticInterior {
    statement_0_i: Statement_T;
    hash_bang_line_0_i: Hash_bang_line_T;
    constructor() {
        super();
    }
}
export class Private_property_identifier_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Primary_expression_T extends SyntaticInterior {
    true_0_i: True_T;
    this_0_i: This_T;
    template_string_0_i: Template_string_T;
    super_0_i: Super_T;
    subscript_expression_0_i: Subscript_expression_T;
    string_0_i: String_T;
    regex_0_i: Regex_T;
    parenthesized_expression_0_i: Parenthesized_expression_T;
    object_0_i: Object_T;
    number_0_i: Number_T;
    null_0_i: Null_T;
    meta_property_0_i: Meta_property_T;
    member_expression_0_i: Member_expression_T;
    generator_function_0_i: Generator_function_T;
    function_expression_0_i: Function_expression_T;
    false_0_i: False_T;
    class_0_i: Class_T;
    call_expression_0_i: Call_expression_T;
    arrow_function_0_i: Arrow_function_T;
    array_0_i: Array_T;
    _reserved_identifier_0_i: _reserved_identifier_T;
    _identifier_0_i: _identifier_T;
    constructor() {
        super();
    }
}
export class Pattern_T extends SyntaticInterior {
    rest_pattern_0_i: Rest_pattern_T;
    _lhs_expression_0_i: _lhs_expression_T;
    constructor() {
        super();
    }
}
export class Parenthesized_expression_T extends SyntaticInterior {
    _expressions_0_i: _expressions_T;
    constructor() {
        super();
    }
}
export class Pair_pattern_T extends SyntaticInterior {
    value_0_i: Pattern_T | Assignment_pattern_T;
    key_0_i: _property_name_T;
    constructor() {
        super();
    }
}
export class Pair_T extends SyntaticInterior {
    value_0_i: Expression_T;
    key_0_i: _property_name_T;
    constructor() {
        super();
    }
}
export class Optional_chain_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Object_pattern_T extends SyntaticInterior {
    rest_pattern_0_i: Rest_pattern_T;
    pair_pattern_0_i: Pair_pattern_T;
    object_assignment_pattern_0_i: Object_assignment_pattern_T;
    identifier_0_i: Identifier_T;
    _reserved_identifier_0_i: _reserved_identifier_T;
    constructor() {
        super();
    }
}
export class Object_assignment_pattern_T extends SyntaticInterior {
    right_0_i: Expression_T;
    left_0_i: _reserved_identifier_T | Identifier_T | _destructuring_pattern_T;
    constructor() {
        super();
    }
}
export class Object_T extends SyntaticInterior {
    spread_element_0_i: Spread_element_T;
    pair_0_i: Pair_T;
    method_definition_0_i: Method_definition_T;
    identifier_0_i: Identifier_T;
    _reserved_identifier_0_i: _reserved_identifier_T;
    constructor() {
        super();
    }
}
export class Number_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Null_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class New_expression_T extends SyntaticInterior {
    constructor_0_i: Primary_expression_T | New_expression_T;
    arguments_0_i: Arguments_T;
    constructor() {
        super();
    }
}
export class Nested_identifier_T extends SyntaticInterior {
    property_0_i: Identifier_T;
    object_0_i: Identifier_T | Nested_identifier_T;
    constructor() {
        super();
    }
}
export class Namespace_import_T extends SyntaticInterior {
    identifier_0_i: Identifier_T;
    constructor() {
        super();
    }
}
export class Namespace_export_T extends SyntaticInterior {
    _module_export_name_0_i: _module_export_name_T;
    constructor() {
        super();
    }
}
export class Named_imports_T extends SyntaticInterior {
    import_specifier_0_i: Import_specifier_T;
    constructor() {
        super();
    }
}
export class Method_definition_T extends SyntaticInterior {
    parameters_0_i: Formal_parameters_T;
    name_0_i: _property_name_T;
    decorator_0_i: Decorator_T;
    body_0_i: Statement_block_T;
    constructor() {
        super();
    }
}
export class Meta_property_T extends SyntaticInterior {
    constructor() {
        super();
    }
}
export class Member_expression_T extends SyntaticInterior {
    property_0_i: Private_property_identifier_T | Identifier_T;
    optional_chain_0_i: Optional_chain_T;
    object_0_i: Expression_T | Primary_expression_T | Import_T;
    constructor() {
        super();
    }
}
export class Lexical_declaration_T extends SyntaticInterior {
    kind_0_i: undefined;
    variable_declarator_0_i: Variable_declarator_T;
    variable_declarator_1_i: Variable_declarator_T;
    _semicolon_0_i: _semicolon_T;
    constructor() {
        super();
    }
}
export class Labeled_statement_T extends SyntaticInterior {
    label_0_i: Identifier_T | _reserved_identifier_T;
    body_0_i: Statement_T;
    constructor() {
        super();
    }
}
export class Jsx_self_closing_element_T extends SyntaticInterior {
    name_0_i: _jsx_element_name_T;
    attribute_0_i: _jsx_attribute_T;
    constructor() {
        super();
    }
}
export class Jsx_opening_element_T extends SyntaticInterior {
    name_0_i: _jsx_element_name_T;
    attribute_0_i: _jsx_attribute_T;
    constructor() {
        super();
    }
}
export class Jsx_namespace_name_T extends SyntaticInterior {
    _jsx_identifier_0_i: _jsx_identifier_T;
    _jsx_identifier_1_i: _jsx_identifier_T;
    constructor() {
        super();
    }
}
export class Jsx_identifier_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Jsx_expression_T extends SyntaticInterior {
    spread_element_0_i: Spread_element_T;
    sequence_expression_0_i: Sequence_expression_T;
    expression_0_i: Expression_T;
    constructor() {
        super();
    }
}
export class Jsx_element_T extends SyntaticInterior {
    open_tag_0_i: Jsx_opening_element_T;
    close_tag_0_i: Jsx_closing_element_T;
    _jsx_child_0_i: _jsx_child_T;
    constructor() {
        super();
    }
}
export class Jsx_closing_element_T extends SyntaticInterior {
    name_0_i: _jsx_element_name_T;
    constructor() {
        super();
    }
}
export class Jsx_attribute_T extends SyntaticInterior {
    _jsx_attribute_value_0_i: _jsx_attribute_value_T;
    _jsx_attribute_name_0_i: _jsx_attribute_name_T;
    constructor() {
        super();
    }
}
export class Import_statement_T extends SyntaticInterior {
    source_0_i: String_T;
    import_clause_0_i: Import_clause_T;
    import_attribute_0_i: Import_attribute_T;
    _semicolon_0_i: _semicolon_T;
    _from_clause_0_i: _from_clause_T;
    constructor() {
        super();
    }
}
export class Import_specifier_T extends SyntaticInterior {
    name_0_i: _module_export_name_T | Identifier_T;
    alias_0_i: Identifier_T;
    constructor() {
        super();
    }
}
export class Import_clause_T extends SyntaticInterior {
    namespace_import_0_i: Namespace_import_T;
    named_imports_0_i: Named_imports_T;
    identifier_0_i: Identifier_T;
    constructor() {
        super();
    }
}
export class Import_attribute_T extends SyntaticInterior {
    object_0_i: Object_T;
    constructor() {
        super();
    }
}
export class Import_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class If_statement_T extends SyntaticInterior {
    consequence_0_i: Statement_T;
    condition_0_i: Parenthesized_expression_T;
    alternative_0_i: Else_clause_T;
    constructor() {
        super();
    }
}
export class Identifier_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Html_character_reference_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Hash_bang_line_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Generator_function_declaration_T extends SyntaticInterior {
    name_0_i: Identifier_T;
    body_0_i: Statement_block_T;
    _call_signature_0_i: _call_signature_T;
    _automatic_semicolon_0_i: _automatic_semicolon_T;
    constructor() {
        super();
    }
}
export class Generator_function_T extends SyntaticInterior {
    name_0_i: Identifier_T;
    body_0_i: Statement_block_T;
    _call_signature_0_i: _call_signature_T;
    constructor() {
        super();
    }
}
export class Function_expression_T extends SyntaticInterior {
    name_0_i: Identifier_T;
    body_0_i: Statement_block_T;
    _call_signature_0_i: _call_signature_T;
    constructor() {
        super();
    }
}
export class Function_declaration_T extends SyntaticInterior {
    name_0_i: Identifier_T;
    body_0_i: Statement_block_T;
    _call_signature_0_i: _call_signature_T;
    _automatic_semicolon_0_i: _automatic_semicolon_T;
    constructor() {
        super();
    }
}
export class Formal_parameters_T extends SyntaticInterior {
    _formal_parameter_0_i: _formal_parameter_T;
    constructor() {
        super();
    }
}
export class For_statement_T extends SyntaticInterior {
    initializer_0_i:
        | Empty_statement_T
        | _expressions_T
        | Lexical_declaration_T
        | Variable_declaration_T;
    increment_0_i: _expressions_T;
    condition_0_i: _expressions_T | Empty_statement_T;
    body_0_i: Statement_T;
    constructor() {
        super();
    }
}
export class For_in_statement_T extends SyntaticInterior {
    body_0_i: Statement_T;
    _for_header_0_i: _for_header_T;
    constructor() {
        super();
    }
}
export class Finally_clause_T extends SyntaticInterior {
    body_0_i: Statement_block_T;
    constructor() {
        super();
    }
}
export class Field_definition_T extends SyntaticInterior {
    property_0_i: _property_name_T;
    decorator_0_i: Decorator_T;
    _initializer_0_i: _initializer_T;
    constructor() {
        super();
    }
}
export class False_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Expression_statement_T extends SyntaticInterior {
    _semicolon_0_i: _semicolon_T;
    _expressions_0_i: _expressions_T;
    constructor() {
        super();
    }
}
export class Expression_T extends SyntaticInterior {
    yield_expression_0_i: Yield_expression_T;
    update_expression_0_i: Update_expression_T;
    unary_expression_0_i: Unary_expression_T;
    ternary_expression_0_i: Ternary_expression_T;
    primary_expression_0_i: Primary_expression_T;
    new_expression_0_i: New_expression_T;
    binary_expression_0_i: Binary_expression_T;
    await_expression_0_i: Await_expression_T;
    augmented_assignment_expression_0_i: Augmented_assignment_expression_T;
    assignment_expression_0_i: Assignment_expression_T;
    _jsx_element_0_i: _jsx_element_T;
    constructor() {
        super();
    }
}
export class Export_statement_T extends SyntaticInterior {
    value_0_i: Expression_T;
    decorator_0_i: Decorator_T;
    declaration_0_i: Declaration_T;
    namespace_export_0_i: Namespace_export_T;
    export_clause_0_i: Export_clause_T;
    _semicolon_0_i: _semicolon_T;
    _from_clause_0_i: _from_clause_T;
    constructor() {
        super();
    }
}
export class Export_specifier_T extends SyntaticInterior {
    name_0_i: _module_export_name_T;
    alias_0_i: _module_export_name_T;
    constructor() {
        super();
    }
}
export class Export_clause_T extends SyntaticInterior {
    export_specifier_0_i: Export_specifier_T;
    constructor() {
        super();
    }
}
export class Escape_sequence_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Empty_statement_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Else_clause_T extends SyntaticInterior {
    statement_0_i: Statement_T;
    constructor() {
        super();
    }
}
export class Do_statement_T extends SyntaticInterior {
    condition_0_i: Parenthesized_expression_T;
    body_0_i: Statement_T;
    _semicolon_0_i: _semicolon_T;
    constructor() {
        super();
    }
}
export class Decorator_member_expression_T extends SyntaticInterior {
    property_0_i: Identifier_T;
    object_0_i: Identifier_T | Decorator_member_expression_T;
    constructor() {
        super();
    }
}
export class Decorator_call_expression_T extends SyntaticInterior {
    function_0_i: Identifier_T | Decorator_member_expression_T;
    arguments_0_i: Arguments_T;
    constructor() {
        super();
    }
}
export class Decorator_T extends SyntaticInterior {
    identifier_0_i: Identifier_T;
    decorator_member_expression_0_i: Decorator_member_expression_T;
    decorator_call_expression_0_i: Decorator_call_expression_T;
    constructor() {
        super();
    }
}
export class Declaration_T extends SyntaticInterior {
    variable_declaration_0_i: Variable_declaration_T;
    using_declaration_0_i: Using_declaration_T;
    lexical_declaration_0_i: Lexical_declaration_T;
    generator_function_declaration_0_i: Generator_function_declaration_T;
    function_declaration_0_i: Function_declaration_T;
    class_declaration_0_i: Class_declaration_T;
    constructor() {
        super();
    }
}
export class Debugger_statement_T extends SyntaticInterior {
    _semicolon_0_i: _semicolon_T;
    constructor() {
        super();
    }
}
export class Continue_statement_T extends SyntaticInterior {
    label_0_i: Identifier_T;
    _semicolon_0_i: _semicolon_T;
    constructor() {
        super();
    }
}
export class Computed_property_name_T extends SyntaticInterior {
    expression_0_i: Expression_T;
    constructor() {
        super();
    }
}
export class Comment_T extends SyntaticLeaf {
    constructor(value: string) {
        super(value);
    }
}
export class Class_static_block_T extends SyntaticInterior {
    body_0_i: Statement_block_T;
    _automatic_semicolon_0_i: _automatic_semicolon_T;
    constructor() {
        super();
    }
}
export class Class_heritage_T extends SyntaticInterior {
    expression_0_i: Expression_T;
    constructor() {
        super();
    }
}
export class Class_declaration_T extends SyntaticInterior {
    name_0_i: Identifier_T;
    decorator_0_i: Decorator_T;
    body_0_i: Class_body_T;
    class_heritage_0_i: Class_heritage_T;
    _automatic_semicolon_0_i: _automatic_semicolon_T;
    constructor() {
        super();
    }
}
export class Class_body_T extends SyntaticInterior {
    member_0_i: Class_static_block_T | Field_definition_T | Method_definition_T;
    _semicolon_0_i: _semicolon_T;
    constructor() {
        super();
    }
}
export class Class_T extends SyntaticInterior {
    name_0_i: Identifier_T;
    decorator_0_i: Decorator_T;
    body_0_i: Class_body_T;
    class_heritage_0_i: Class_heritage_T;
    constructor() {
        super();
    }
}
export class Catch_clause_T extends SyntaticInterior {
    parameter_0_i: Identifier_T | _destructuring_pattern_T;
    body_0_i: Statement_block_T;
    constructor() {
        super();
    }
}
export class Call_expression_T extends SyntaticInterior {
    optional_chain_0_i: Optional_chain_T;
    function_0_i:
        | Primary_expression_T
        | New_expression_T
        | Expression_T
        | Import_T;
    arguments_0_i: Template_string_T | Arguments_T;
    constructor() {
        super();
    }
}
export class Break_statement_T extends SyntaticInterior {
    label_0_i: Identifier_T;
    _semicolon_0_i: _semicolon_T;
    constructor() {
        super();
    }
}
export class Binary_expression_T extends SyntaticInterior {
    right_0_i: Expression_T;
    operator_0_i: undefined;
    left_0_i: Expression_T | Private_property_identifier_T;
    constructor() {
        super();
    }
}
export class Await_expression_T extends SyntaticInterior {
    expression_0_i: Expression_T;
    constructor() {
        super();
    }
}
export class Augmented_assignment_expression_T extends SyntaticInterior {
    right_0_i: Expression_T;
    operator_0_i: undefined;
    left_0_i: _augmented_assignment_lhs_T;
    constructor() {
        super();
    }
}
export class Assignment_pattern_T extends SyntaticInterior {
    right_0_i: Expression_T;
    left_0_i: Pattern_T;
    constructor() {
        super();
    }
}
export class Assignment_expression_T extends SyntaticInterior {
    right_0_i: Expression_T;
    left_0_i: Parenthesized_expression_T | _lhs_expression_T;
    constructor() {
        super();
    }
}
export class Arrow_function_T extends SyntaticInterior {
    parameter_0_i: _reserved_identifier_T | Identifier_T;
    body_0_i: Expression_T | Statement_block_T;
    _call_signature_0_i: _call_signature_T;
    constructor() {
        super();
    }
}
export class Array_pattern_T extends SyntaticInterior {
    pattern_0_i: Pattern_T;
    assignment_pattern_0_i: Assignment_pattern_T;
    constructor() {
        super();
    }
}
export class Array_T extends SyntaticInterior {
    spread_element_0_i: Spread_element_T;
    expression_0_i: Expression_T;
    constructor() {
        super();
    }
}
export class Arguments_T extends SyntaticInterior {
    spread_element_0_i: Spread_element_T;
    expression_0_i: Expression_T;
    constructor() {
        super();
    }
}
export class _semicolon_T extends SyntaticInterior {
    _automatic_semicolon_0_i: _automatic_semicolon_T;
    constructor() {
        super();
    }
}
export class _reserved_identifier_T extends SyntaticInterior {
    constructor() {
        super();
    }
}
export class _property_name_T extends SyntaticInterior {
    string_0_i: String_T;
    private_property_identifier_0_i: Private_property_identifier_T;
    number_0_i: Number_T;
    identifier_0_i: Identifier_T;
    computed_property_name_0_i: Computed_property_name_T;
    _reserved_identifier_0_i: _reserved_identifier_T;
    constructor() {
        super();
    }
}
export class _module_export_name_T extends SyntaticInterior {
    string_0_i: String_T;
    identifier_0_i: Identifier_T;
    constructor() {
        super();
    }
}
export class _lhs_expression_T extends SyntaticInterior {
    subscript_expression_0_i: Subscript_expression_T;
    member_expression_0_i: Member_expression_T;
    _reserved_identifier_0_i: _reserved_identifier_T;
    _identifier_0_i: _identifier_T;
    _destructuring_pattern_0_i: _destructuring_pattern_T;
    constructor() {
        super();
    }
}
export class _jsx_string_T extends SyntaticInterior {
    unescaped_single_jsx_string_fragment_0_i: Unescaped_single_jsx_string_fragment_T;
    unescaped_double_jsx_string_fragment_0_i: Unescaped_double_jsx_string_fragment_T;
    html_character_reference_0_i: Html_character_reference_T;
    constructor() {
        super();
    }
}
export class _jsx_identifier_T extends SyntaticInterior {
    jsx_identifier_0_i: Jsx_identifier_T;
    identifier_0_i: Identifier_T;
    constructor() {
        super();
    }
}
export class _jsx_element_name_T extends SyntaticInterior {
    nested_identifier_0_i: Nested_identifier_T;
    jsx_namespace_name_0_i: Jsx_namespace_name_T;
    _jsx_identifier_0_i: _jsx_identifier_T;
    constructor() {
        super();
    }
}
export class _jsx_element_T extends SyntaticInterior {
    jsx_self_closing_element_0_i: Jsx_self_closing_element_T;
    jsx_element_0_i: Jsx_element_T;
    constructor() {
        super();
    }
}
export class _jsx_child_T extends SyntaticInterior {
    jsx_text_0_i: Jsx_text_T;
    jsx_expression_0_i: Jsx_expression_T;
    html_character_reference_0_i: Html_character_reference_T;
    _jsx_element_0_i: _jsx_element_T;
    constructor() {
        super();
    }
}
export class _jsx_attribute_value_T extends SyntaticInterior {
    jsx_expression_0_i: Jsx_expression_T;
    _jsx_string_0_i: _jsx_string_T;
    _jsx_element_0_i: _jsx_element_T;
    constructor() {
        super();
    }
}
export class _jsx_attribute_name_T extends SyntaticInterior {
    jsx_namespace_name_0_i: Jsx_namespace_name_T;
    _jsx_identifier_0_i: _jsx_identifier_T;
    constructor() {
        super();
    }
}
export class _jsx_attribute_T extends SyntaticInterior {
    jsx_expression_0_i: Jsx_expression_T;
    jsx_attribute_0_i: Jsx_attribute_T;
    constructor() {
        super();
    }
}
export class _initializer_T extends SyntaticInterior {
    value_0_i: Expression_T;
    constructor() {
        super();
    }
}
export class _identifier_T extends SyntaticInterior {
    undefined_0_i: Undefined_T;
    identifier_0_i: Identifier_T;
    constructor() {
        super();
    }
}
export class _from_clause_T extends SyntaticInterior {
    source_0_i: String_T;
    constructor() {
        super();
    }
}
export class _formal_parameter_T extends SyntaticInterior {
    pattern_0_i: Pattern_T;
    assignment_pattern_0_i: Assignment_pattern_T;
    constructor() {
        super();
    }
}
export class _for_header_T extends SyntaticInterior {
    right_0_i: _expressions_T;
    operator_0_i: undefined;
    left_0_i:
        | Identifier_T
        | _destructuring_pattern_T
        | _lhs_expression_T
        | Parenthesized_expression_T;
    kind_0_i: undefined;
    _initializer_0_i: _initializer_T;
    _automatic_semicolon_0_i: _automatic_semicolon_T;
    constructor() {
        super();
    }
}
export class _expressions_T extends SyntaticInterior {
    sequence_expression_0_i: Sequence_expression_T;
    expression_0_i: Expression_T;
    constructor() {
        super();
    }
}
export class _destructuring_pattern_T extends SyntaticInterior {
    object_pattern_0_i: Object_pattern_T;
    array_pattern_0_i: Array_pattern_T;
    constructor() {
        super();
    }
}
export class _call_signature_T extends SyntaticInterior {
    parameters_0_i: Formal_parameters_T;
    constructor() {
        super();
    }
}
export class _augmented_assignment_lhs_T extends SyntaticInterior {
    subscript_expression_0_i: Subscript_expression_T;
    parenthesized_expression_0_i: Parenthesized_expression_T;
    member_expression_0_i: Member_expression_T;
    identifier_0_i: Identifier_T;
    _reserved_identifier_0_i: _reserved_identifier_T;
    constructor() {
        super();
    }
}
