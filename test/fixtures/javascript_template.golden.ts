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
export class _augmented_assignment_lhs_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _augmented_assignment_lhs__reserved_identifier_T extends _augmented_assignment_lhs_T { _reserved_identifier_0_i : _reserved_identifier_T;
 constructor(_reserved_identifier_0 : _reserved_identifier_T) { super();this._reserved_identifier_0_i = _reserved_identifier_0; } evaluate(): string { return this._reserved_identifier_0_i.evaluate(); } }
export class _augmented_assignment_lhs_identifier_T extends _augmented_assignment_lhs_T { identifier_0_i : Identifier_T;
 constructor(identifier_0 : Identifier_T) { super();this.identifier_0_i = identifier_0; } evaluate(): string { return this.identifier_0_i.evaluate(); } }
export class _augmented_assignment_lhs_member_expression_T extends _augmented_assignment_lhs_T { member_expression_0_i : Member_expression_T;
 constructor(member_expression_0 : Member_expression_T) { super();this.member_expression_0_i = member_expression_0; } evaluate(): string { return this.member_expression_0_i.evaluate(); } }
export class _augmented_assignment_lhs_parenthesized_expression_T extends _augmented_assignment_lhs_T { parenthesized_expression_0_i : Parenthesized_expression_T;
 constructor(parenthesized_expression_0 : Parenthesized_expression_T) { super();this.parenthesized_expression_0_i = parenthesized_expression_0; } evaluate(): string { return this.parenthesized_expression_0_i.evaluate(); } }
export class _augmented_assignment_lhs_subscript_expression_T extends _augmented_assignment_lhs_T { subscript_expression_0_i : Subscript_expression_T;
 constructor(subscript_expression_0 : Subscript_expression_T) { super();this.subscript_expression_0_i = subscript_expression_0; } evaluate(): string { return this.subscript_expression_0_i.evaluate(); } }
export class _call_signature_T extends SyntaticInterior { formal_parameters_0_i : Formal_parameters_T;
 constructor(formal_parameters_0 : Formal_parameters_T) { super();this.formal_parameters_0_i = formal_parameters_0; } evaluate(): string { return this.formal_parameters_0_i.evaluate(); } }
export class _destructuring_pattern_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _destructuring_pattern_array_pattern_T extends _destructuring_pattern_T { array_pattern_0_i : Array_pattern_T;
 constructor(array_pattern_0 : Array_pattern_T) { super();this.array_pattern_0_i = array_pattern_0; } evaluate(): string { return this.array_pattern_0_i.evaluate(); } }
export class _destructuring_pattern_object_pattern_T extends _destructuring_pattern_T { object_pattern_0_i : Object_pattern_T;
 constructor(object_pattern_0 : Object_pattern_T) { super();this.object_pattern_0_i = object_pattern_0; } evaluate(): string { return this.object_pattern_0_i.evaluate(); } }
export class _expressions_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _expressions_expression_T extends _expressions_T { expression_0_i : Expression_T;
 constructor(expression_0 : Expression_T) { super();this.expression_0_i = expression_0; } evaluate(): string { return this.expression_0_i.evaluate(); } }
export class _expressions_sequence_expression_T extends _expressions_T { sequence_expression_0_i : Sequence_expression_T;
 constructor(sequence_expression_0 : Sequence_expression_T) { super();this.sequence_expression_0_i = sequence_expression_0; } evaluate(): string { return this.sequence_expression_0_i.evaluate(); } }
export class _for_header_T extends SyntaticInterior { _lhs_expression_0_i : _lhs_expression_T | undefined;
 parenthesized_expression_1_i : Parenthesized_expression_T | undefined;
 identifier_2_i : Identifier_T | undefined;
 _destructuring_pattern_3_i : _destructuring_pattern_T | undefined;
 _initializer_4_i : _initializer_T | undefined;
 identifier_5_i : Identifier_T | undefined;
 _destructuring_pattern_6_i : _destructuring_pattern_T | undefined;
 identifier_7_i : Identifier_T | undefined;
 _destructuring_pattern_8_i : _destructuring_pattern_T | undefined;
 _expressions_9_i : _expressions_T;
 constructor(_lhs_expression_0 : _lhs_expression_T | undefined,parenthesized_expression_1 : Parenthesized_expression_T | undefined,identifier_2 : Identifier_T | undefined,_destructuring_pattern_3 : _destructuring_pattern_T | undefined,_initializer_4 : _initializer_T | undefined,identifier_5 : Identifier_T | undefined,_destructuring_pattern_6 : _destructuring_pattern_T | undefined,identifier_7 : Identifier_T | undefined,_destructuring_pattern_8 : _destructuring_pattern_T | undefined,_expressions_9 : _expressions_T) { super();this._lhs_expression_0_i = _lhs_expression_0;this.parenthesized_expression_1_i = parenthesized_expression_1;this.identifier_2_i = identifier_2;this._destructuring_pattern_3_i = _destructuring_pattern_3;this._initializer_4_i = _initializer_4;this.identifier_5_i = identifier_5;this._destructuring_pattern_6_i = _destructuring_pattern_6;this.identifier_7_i = identifier_7;this._destructuring_pattern_8_i = _destructuring_pattern_8;this._expressions_9_i = _expressions_9; } evaluate(): string { return `(${(this._lhs_expression_0_i !== undefined ? this._lhs_expression_0_i.evaluate() : this.identifier_2_i !== undefined ? `var${(this.identifier_2_i !== undefined ? this.identifier_2_i.evaluate() : "of")}${(this._initializer_4_i !== undefined ? this._initializer_4_i.evaluate() : "")}` : this.identifier_5_i !== undefined ? `${("let")}${(this.identifier_5_i !== undefined ? this.identifier_5_i.evaluate() : "of")}${("")}` : this.identifier_7_i !== undefined ? `${("using")}${(this.identifier_7_i !== undefined ? this.identifier_7_i.evaluate() : "of")}${("")}` : (() => { throw new Error("No alternative matched in CHOICE node"); })())}${("in")}${this._expressions_9_i.evaluate()})`; } }
export class _formal_parameter_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _formal_parameter_assignment_pattern_T extends _formal_parameter_T { assignment_pattern_0_i : Assignment_pattern_T;
 constructor(assignment_pattern_0 : Assignment_pattern_T) { super();this.assignment_pattern_0_i = assignment_pattern_0; } evaluate(): string { return this.assignment_pattern_0_i.evaluate(); } }
export class _formal_parameter_pattern_T extends _formal_parameter_T { pattern_0_i : Pattern_T;
 constructor(pattern_0 : Pattern_T) { super();this.pattern_0_i = pattern_0; } evaluate(): string { return this.pattern_0_i.evaluate(); } }
export class _from_clause_T extends SyntaticInterior { string_0_i : String_T;
 constructor(string_0 : String_T) { super();this.string_0_i = string_0; } evaluate(): string { return `from${this.string_0_i.evaluate()}`; } }
export class _identifier_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _identifier_identifier_T extends _identifier_T { identifier_0_i : Identifier_T;
 constructor(identifier_0 : Identifier_T) { super();this.identifier_0_i = identifier_0; } evaluate(): string { return this.identifier_0_i.evaluate(); } }
export class _identifier_undefined_T extends _identifier_T { undefined_0_i : Undefined_T;
 constructor(undefined_0 : Undefined_T) { super();this.undefined_0_i = undefined_0; } evaluate(): string { return this.undefined_0_i.evaluate(); } }
export class _initializer_T extends SyntaticInterior { expression_0_i : Expression_T;
 constructor(expression_0 : Expression_T) { super();this.expression_0_i = expression_0; } evaluate(): string { return `=${this.expression_0_i.evaluate()}`; } }
export class _jsx_attribute_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _jsx_attribute_jsx_attribute_T extends _jsx_attribute_T { jsx_attribute_0_i : Jsx_attribute_T;
 constructor(jsx_attribute_0 : Jsx_attribute_T) { super();this.jsx_attribute_0_i = jsx_attribute_0; } evaluate(): string { return this.jsx_attribute_0_i.evaluate(); } }
export class _jsx_attribute_jsx_expression_T extends _jsx_attribute_T { jsx_expression_0_i : Jsx_expression_T;
 constructor(jsx_expression_0 : Jsx_expression_T) { super();this.jsx_expression_0_i = jsx_expression_0; } evaluate(): string { return this.jsx_expression_0_i.evaluate(); } }
export class _jsx_attribute_name_T extends _jsx_attribute_T {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _jsx_attribute_name__jsx_identifier_T extends _jsx_attribute_T { _jsx_identifier_0_i : _jsx_identifier_T;
 constructor(_jsx_identifier_0 : _jsx_identifier_T) { super();this._jsx_identifier_0_i = _jsx_identifier_0; } evaluate(): string { return this._jsx_identifier_0_i.evaluate(); } }
export class _jsx_attribute_name_jsx_namespace_name_T extends _jsx_attribute_T { jsx_namespace_name_0_i : Jsx_namespace_name_T;
 constructor(jsx_namespace_name_0 : Jsx_namespace_name_T) { super();this.jsx_namespace_name_0_i = jsx_namespace_name_0; } evaluate(): string { return this.jsx_namespace_name_0_i.evaluate(); } }
export class _jsx_attribute_value_T extends _jsx_attribute_T {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _jsx_attribute_value__jsx_element_T extends _jsx_attribute_T { _jsx_element_0_i : _jsx_element_T;
 constructor(_jsx_element_0 : _jsx_element_T) { super();this._jsx_element_0_i = _jsx_element_0; } evaluate(): string { return this._jsx_element_0_i.evaluate(); } }
export class _jsx_attribute_value__jsx_string_T extends _jsx_attribute_T { _jsx_string_0_i : _jsx_string_T;
 constructor(_jsx_string_0 : _jsx_string_T) { super();this._jsx_string_0_i = _jsx_string_0; } evaluate(): string { return this._jsx_string_0_i.evaluate(); } }
export class _jsx_attribute_value_jsx_expression_T extends _jsx_attribute_T { jsx_expression_0_i : Jsx_expression_T;
 constructor(jsx_expression_0 : Jsx_expression_T) { super();this.jsx_expression_0_i = jsx_expression_0; } evaluate(): string { return this.jsx_expression_0_i.evaluate(); } }
export class _jsx_child_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _jsx_child_0_T extends _jsx_child_T {  constructor() { super(); } evaluate(): string { return ""; } }
export class _jsx_child__jsx_element_T extends _jsx_child_T { _jsx_element_0_i : _jsx_element_T;
 constructor(_jsx_element_0 : _jsx_element_T) { super();this._jsx_element_0_i = _jsx_element_0; } evaluate(): string { return this._jsx_element_0_i.evaluate(); } }
export class _jsx_child_html_character_reference_T extends _jsx_child_T { html_character_reference_0_i : Html_character_reference_T;
 constructor(html_character_reference_0 : Html_character_reference_T) { super();this.html_character_reference_0_i = html_character_reference_0; } evaluate(): string { return this.html_character_reference_0_i.evaluate(); } }
export class _jsx_child_jsx_expression_T extends _jsx_child_T { jsx_expression_0_i : Jsx_expression_T;
 constructor(jsx_expression_0 : Jsx_expression_T) { super();this.jsx_expression_0_i = jsx_expression_0; } evaluate(): string { return this.jsx_expression_0_i.evaluate(); } }
export class _jsx_element_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _jsx_element_jsx_element_T extends _jsx_element_T { jsx_element_0_i : Jsx_element_T;
 constructor(jsx_element_0 : Jsx_element_T) { super();this.jsx_element_0_i = jsx_element_0; } evaluate(): string { return this.jsx_element_0_i.evaluate(); } }
export class _jsx_element_jsx_self_closing_element_T extends _jsx_element_T { jsx_self_closing_element_0_i : Jsx_self_closing_element_T;
 constructor(jsx_self_closing_element_0 : Jsx_self_closing_element_T) { super();this.jsx_self_closing_element_0_i = jsx_self_closing_element_0; } evaluate(): string { return this.jsx_self_closing_element_0_i.evaluate(); } }
export class _jsx_element_name_T extends _jsx_element_T {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _jsx_element_name__jsx_identifier_T extends _jsx_element_T { _jsx_identifier_0_i : _jsx_identifier_T;
 constructor(_jsx_identifier_0 : _jsx_identifier_T) { super();this._jsx_identifier_0_i = _jsx_identifier_0; } evaluate(): string { return this._jsx_identifier_0_i.evaluate(); } }
export class _jsx_element_name_jsx_namespace_name_T extends _jsx_element_T { jsx_namespace_name_0_i : Jsx_namespace_name_T;
 constructor(jsx_namespace_name_0 : Jsx_namespace_name_T) { super();this.jsx_namespace_name_0_i = jsx_namespace_name_0; } evaluate(): string { return this.jsx_namespace_name_0_i.evaluate(); } }
export class _jsx_element_name_nested_identifier_T extends _jsx_element_T { nested_identifier_0_i : Nested_identifier_T;
 constructor(nested_identifier_0 : Nested_identifier_T) { super();this.nested_identifier_0_i = nested_identifier_0; } evaluate(): string { return this.nested_identifier_0_i.evaluate(); } }
export class _jsx_identifier_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _jsx_identifier_identifier_T extends _jsx_identifier_T { identifier_0_i : Identifier_T;
 constructor(identifier_0 : Identifier_T) { super();this.identifier_0_i = identifier_0; } evaluate(): string { return this.identifier_0_i.evaluate(); } }
export class _jsx_identifier_jsx_identifier_T extends _jsx_identifier_T { jsx_identifier_0_i : Jsx_identifier_T;
 constructor(jsx_identifier_0 : Jsx_identifier_T) { super();this.jsx_identifier_0_i = jsx_identifier_0; } evaluate(): string { return this.jsx_identifier_0_i.evaluate(); } }
export class _jsx_string_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _jsx_string_0_T extends _jsx_string_T { unescaped_double_jsx_string_fragment_0_i : Unescaped_double_jsx_string_fragment_T | undefined;
 html_character_reference_1_i : Html_character_reference_T | undefined;
 constructor(unescaped_double_jsx_string_fragment_0 : Unescaped_double_jsx_string_fragment_T | undefined,html_character_reference_1 : Html_character_reference_T | undefined) { super();this.unescaped_double_jsx_string_fragment_0_i = unescaped_double_jsx_string_fragment_0;this.html_character_reference_1_i = html_character_reference_1; } evaluate(): string { return `\"${(this.unescaped_double_jsx_string_fragment_0_i !== undefined ? this.unescaped_double_jsx_string_fragment_0_i.evaluate() : this.html_character_reference_1_i !== undefined ? this.html_character_reference_1_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}\"`; } }
export class _jsx_string_1_T extends _jsx_string_T { unescaped_single_jsx_string_fragment_0_i : Unescaped_single_jsx_string_fragment_T | undefined;
 html_character_reference_1_i : Html_character_reference_T | undefined;
 constructor(unescaped_single_jsx_string_fragment_0 : Unescaped_single_jsx_string_fragment_T | undefined,html_character_reference_1 : Html_character_reference_T | undefined) { super();this.unescaped_single_jsx_string_fragment_0_i = unescaped_single_jsx_string_fragment_0;this.html_character_reference_1_i = html_character_reference_1; } evaluate(): string { return `'${(this.unescaped_single_jsx_string_fragment_0_i !== undefined ? this.unescaped_single_jsx_string_fragment_0_i.evaluate() : this.html_character_reference_1_i !== undefined ? this.html_character_reference_1_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}'`; } }
export class _lhs_expression_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _lhs_expression__destructuring_pattern_T extends _lhs_expression_T { _destructuring_pattern_0_i : _destructuring_pattern_T;
 constructor(_destructuring_pattern_0 : _destructuring_pattern_T) { super();this._destructuring_pattern_0_i = _destructuring_pattern_0; } evaluate(): string { return this._destructuring_pattern_0_i.evaluate(); } }
export class _lhs_expression__identifier_T extends _lhs_expression_T { _identifier_0_i : _identifier_T;
 constructor(_identifier_0 : _identifier_T) { super();this._identifier_0_i = _identifier_0; } evaluate(): string { return this._identifier_0_i.evaluate(); } }
export class _lhs_expression__reserved_identifier_T extends _lhs_expression_T { _reserved_identifier_0_i : _reserved_identifier_T;
 constructor(_reserved_identifier_0 : _reserved_identifier_T) { super();this._reserved_identifier_0_i = _reserved_identifier_0; } evaluate(): string { return this._reserved_identifier_0_i.evaluate(); } }
export class _lhs_expression_member_expression_T extends _lhs_expression_T { member_expression_0_i : Member_expression_T;
 constructor(member_expression_0 : Member_expression_T) { super();this.member_expression_0_i = member_expression_0; } evaluate(): string { return this.member_expression_0_i.evaluate(); } }
export class _lhs_expression_subscript_expression_T extends _lhs_expression_T { subscript_expression_0_i : Subscript_expression_T;
 constructor(subscript_expression_0 : Subscript_expression_T) { super();this.subscript_expression_0_i = subscript_expression_0; } evaluate(): string { return this.subscript_expression_0_i.evaluate(); } }
export class _module_export_name_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _module_export_name_2_T extends _module_export_name_T {  constructor() { super(); } evaluate(): string { return "default"; } }
export class _module_export_name_identifier_T extends _module_export_name_T { identifier_0_i : Identifier_T;
 constructor(identifier_0 : Identifier_T) { super();this.identifier_0_i = identifier_0; } evaluate(): string { return this.identifier_0_i.evaluate(); } }
export class _module_export_name_string_T extends _module_export_name_T { string_0_i : String_T;
 constructor(string_0 : String_T) { super();this.string_0_i = string_0; } evaluate(): string { return this.string_0_i.evaluate(); } }
export class _property_name_T extends SyntaticInterior { identifier_0_i : Identifier_T | undefined;
 _reserved_identifier_1_i : _reserved_identifier_T | undefined;
 private_property_identifier_2_i : Private_property_identifier_T | undefined;
 string_3_i : String_T | undefined;
 number_4_i : Number_T | undefined;
 computed_property_name_5_i : Computed_property_name_T | undefined;
 constructor(identifier_0 : Identifier_T | undefined,_reserved_identifier_1 : _reserved_identifier_T | undefined,private_property_identifier_2 : Private_property_identifier_T | undefined,string_3 : String_T | undefined,number_4 : Number_T | undefined,computed_property_name_5 : Computed_property_name_T | undefined) { super();this.identifier_0_i = identifier_0;this._reserved_identifier_1_i = _reserved_identifier_1;this.private_property_identifier_2_i = private_property_identifier_2;this.string_3_i = string_3;this.number_4_i = number_4;this.computed_property_name_5_i = computed_property_name_5; } evaluate(): string { if (this.identifier_0_i !== undefined) { return this.identifier_0_i.evaluate(); } if (this.private_property_identifier_2_i !== undefined) { return this.private_property_identifier_2_i.evaluate(); } if (this.string_3_i !== undefined) { return this.string_3_i.evaluate(); } if (this.number_4_i !== undefined) { return this.number_4_i.evaluate(); } if (this.computed_property_name_5_i !== undefined) { return this.computed_property_name_5_i.evaluate(); } throw new Error("No alternative matched in CHOICE node"); } }
export class _reserved_identifier_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _reserved_identifier_0_T extends _reserved_identifier_T {  constructor() { super(); } evaluate(): string { return "get"; } }
export class _reserved_identifier_1_T extends _reserved_identifier_T {  constructor() { super(); } evaluate(): string { return "set"; } }
export class _reserved_identifier_2_T extends _reserved_identifier_T {  constructor() { super(); } evaluate(): string { return "async"; } }
export class _reserved_identifier_3_T extends _reserved_identifier_T {  constructor() { super(); } evaluate(): string { return "await"; } }
export class _reserved_identifier_4_T extends _reserved_identifier_T {  constructor() { super(); } evaluate(): string { return "static"; } }
export class _reserved_identifier_5_T extends _reserved_identifier_T {  constructor() { super(); } evaluate(): string { return "export"; } }
export class _reserved_identifier_6_T extends _reserved_identifier_T {  constructor() { super(); } evaluate(): string { return "let"; } }
export class _semicolon_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class _semicolon_0_T extends _semicolon_T {  constructor() { super(); } evaluate(): string { return ""; } }
export class _semicolon_1_T extends _semicolon_T {  constructor() { super(); } evaluate(): string { return ";"; } }
export class Arguments_T extends SyntaticInterior { expression_0_i : Expression_T | undefined;
 spread_element_1_i : Spread_element_T | undefined;
 expression_2_i : Expression_T | undefined;
 spread_element_3_i : Spread_element_T | undefined;
 constructor(expression_0 : Expression_T | undefined,spread_element_1 : Spread_element_T | undefined,expression_2 : Expression_T | undefined,spread_element_3 : Spread_element_T | undefined) { super();this.expression_0_i = expression_0;this.spread_element_1_i = spread_element_1;this.expression_2_i = expression_2;this.spread_element_3_i = spread_element_3; } evaluate(): string { return `(${(this.expression_0_i !== undefined ? `${this.expression_0_i.evaluate()}${`,${(this.expression_2_i !== undefined ? this.expression_2_i.evaluate() : "")}`}` : "")})`; } }
export class Array_T extends SyntaticInterior { expression_0_i : Expression_T | undefined;
 spread_element_1_i : Spread_element_T | undefined;
 expression_2_i : Expression_T | undefined;
 spread_element_3_i : Spread_element_T | undefined;
 constructor(expression_0 : Expression_T | undefined,spread_element_1 : Spread_element_T | undefined,expression_2 : Expression_T | undefined,spread_element_3 : Spread_element_T | undefined) { super();this.expression_0_i = expression_0;this.spread_element_1_i = spread_element_1;this.expression_2_i = expression_2;this.spread_element_3_i = spread_element_3; } evaluate(): string { return `[${(this.expression_0_i !== undefined ? `${this.expression_0_i.evaluate()}${`,${(this.expression_2_i !== undefined ? this.expression_2_i.evaluate() : "")}`}` : "")}]`; } }
export class Array_pattern_T extends SyntaticInterior { pattern_0_i : Pattern_T | undefined;
 assignment_pattern_1_i : Assignment_pattern_T | undefined;
 pattern_2_i : Pattern_T | undefined;
 assignment_pattern_3_i : Assignment_pattern_T | undefined;
 constructor(pattern_0 : Pattern_T | undefined,assignment_pattern_1 : Assignment_pattern_T | undefined,pattern_2 : Pattern_T | undefined,assignment_pattern_3 : Assignment_pattern_T | undefined) { super();this.pattern_0_i = pattern_0;this.assignment_pattern_1_i = assignment_pattern_1;this.pattern_2_i = pattern_2;this.assignment_pattern_3_i = assignment_pattern_3; } evaluate(): string { return `[${(this.pattern_0_i !== undefined ? `${this.pattern_0_i.evaluate()}${`,${(this.pattern_2_i !== undefined ? this.pattern_2_i.evaluate() : "")}`}` : "")}]`; } }
export class Arrow_function_T extends SyntaticInterior { _reserved_identifier_0_i : _reserved_identifier_T | undefined;
 identifier_1_i : Identifier_T | undefined;
 _call_signature_2_i : _call_signature_T | undefined;
 expression_3_i : Expression_T | undefined;
 statement_block_4_i : Statement_block_T | undefined;
 constructor(_reserved_identifier_0 : _reserved_identifier_T | undefined,identifier_1 : Identifier_T | undefined,_call_signature_2 : _call_signature_T | undefined,expression_3 : Expression_T | undefined,statement_block_4 : Statement_block_T | undefined) { super();this._reserved_identifier_0_i = _reserved_identifier_0;this.identifier_1_i = identifier_1;this._call_signature_2_i = _call_signature_2;this.expression_3_i = expression_3;this.statement_block_4_i = statement_block_4; } evaluate(): string { return `${("async")}${(this._reserved_identifier_0_i !== undefined ? this._reserved_identifier_0_i.evaluate() : this._call_signature_2_i !== undefined ? this._call_signature_2_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}=>${(this.expression_3_i !== undefined ? this.expression_3_i.evaluate() : this.statement_block_4_i !== undefined ? this.statement_block_4_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}`; } }
export class Assignment_expression_T extends SyntaticInterior { parenthesized_expression_0_i : Parenthesized_expression_T | undefined;
 _lhs_expression_1_i : _lhs_expression_T | undefined;
 expression_2_i : Expression_T;
 constructor(parenthesized_expression_0 : Parenthesized_expression_T | undefined,_lhs_expression_1 : _lhs_expression_T | undefined,expression_2 : Expression_T) { super();this.parenthesized_expression_0_i = parenthesized_expression_0;this._lhs_expression_1_i = _lhs_expression_1;this.expression_2_i = expression_2; } evaluate(): string { return `${(this.parenthesized_expression_0_i !== undefined ? this.parenthesized_expression_0_i.evaluate() : this._lhs_expression_1_i !== undefined ? this._lhs_expression_1_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}=${this.expression_2_i.evaluate()}`; } }
export class Assignment_pattern_T extends SyntaticInterior { pattern_0_i : Pattern_T;
 expression_1_i : Expression_T;
 constructor(pattern_0 : Pattern_T,expression_1 : Expression_T) { super();this.pattern_0_i = pattern_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.pattern_0_i.evaluate()}=${this.expression_1_i.evaluate()}`; } }
export class Augmented_assignment_expression_T extends SyntaticInterior { _augmented_assignment_lhs_0_i : _augmented_assignment_lhs_T;
 expression_1_i : Expression_T;
 constructor(_augmented_assignment_lhs_0 : _augmented_assignment_lhs_T,expression_1 : Expression_T) { super();this._augmented_assignment_lhs_0_i = _augmented_assignment_lhs_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this._augmented_assignment_lhs_0_i.evaluate()}${("+=")}${this.expression_1_i.evaluate()}`; } }
export class Await_expression_T extends SyntaticInterior { expression_0_i : Expression_T;
 constructor(expression_0 : Expression_T) { super();this.expression_0_i = expression_0; } evaluate(): string { return `await${this.expression_0_i.evaluate()}`; } }
export class Binary_expression_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class Binary_expression_left_binary_equality_16_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}==${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_binary_equality_17_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}===${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_binary_equality_18_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}!=${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_binary_equality_19_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}!==${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_binary_plus_8_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}+${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_binary_plus_9_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}-${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_binary_relation_14_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}<${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_binary_relation_15_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}<=${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_binary_relation_20_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}>=${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_binary_relation_21_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}>${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_binary_relation_23_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}instanceof${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_binary_relation_24_T extends Binary_expression_T { expression_0_i : Expression_T | undefined;
 private_property_identifier_1_i : Private_property_identifier_T | undefined;
 expression_2_i : Expression_T;
 constructor(expression_0 : Expression_T | undefined,private_property_identifier_1 : Private_property_identifier_T | undefined,expression_2 : Expression_T) { super();this.expression_0_i = expression_0;this.private_property_identifier_1_i = private_property_identifier_1;this.expression_2_i = expression_2; } evaluate(): string { return `${(this.expression_0_i !== undefined ? this.expression_0_i.evaluate() : this.private_property_identifier_1_i !== undefined ? this.private_property_identifier_1_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}in${this.expression_2_i.evaluate()}`; } }
export class Binary_expression_left_binary_shift_2_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}>>${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_binary_shift_3_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}>>>${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_binary_shift_4_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}<<${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_binary_times_10_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}*${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_binary_times_11_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}/${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_binary_times_12_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}%${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_bitwise_and_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}&${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_bitwise_or_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}|${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_bitwise_xor_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}^${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_logical_and_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}&&${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_logical_or_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}||${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_left_ternary_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}??${this.expression_1_i.evaluate()}`; } }
export class Binary_expression_right_binary_exp_T extends Binary_expression_T { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}**${this.expression_1_i.evaluate()}`; } }
export class Break_statement_T extends SyntaticInterior { identifier_0_i : Identifier_T | undefined;
 _semicolon_1_i : _semicolon_T;
 constructor(identifier_0 : Identifier_T | undefined,_semicolon_1 : _semicolon_T) { super();this.identifier_0_i = identifier_0;this._semicolon_1_i = _semicolon_1; } evaluate(): string { return `break${(this.identifier_0_i !== undefined ? this.identifier_0_i.evaluate() : "")}${this._semicolon_1_i.evaluate()}`; } }
export class Call_expression_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class Call_expression_call_T extends Call_expression_T { expression_0_i : Expression_T | undefined;
 import_1_i : Import_T | undefined;
 arguments_2_i : Arguments_T;
 constructor(expression_0 : Expression_T | undefined,import_1 : Import_T | undefined,arguments_2 : Arguments_T) { super();this.expression_0_i = expression_0;this.import_1_i = import_1;this.arguments_2_i = arguments_2; } evaluate(): string { return `${(this.expression_0_i !== undefined ? this.expression_0_i.evaluate() : this.import_1_i !== undefined ? this.import_1_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}${this.arguments_2_i.evaluate()}`; } }
export class Call_expression_member_T extends Call_expression_T { primary_expression_0_i : Primary_expression_T;
 optional_chain_1_i : Optional_chain_T;
 arguments_2_i : Arguments_T;
 constructor(primary_expression_0 : Primary_expression_T,optional_chain_1 : Optional_chain_T,arguments_2 : Arguments_T) { super();this.primary_expression_0_i = primary_expression_0;this.optional_chain_1_i = optional_chain_1;this.arguments_2_i = arguments_2; } evaluate(): string { return `${this.primary_expression_0_i.evaluate()}${this.optional_chain_1_i.evaluate()}${this.arguments_2_i.evaluate()}`; } }
export class Call_expression_template_call_T extends Call_expression_T { primary_expression_0_i : Primary_expression_T | undefined;
 new_expression_1_i : New_expression_T | undefined;
 template_string_2_i : Template_string_T;
 constructor(primary_expression_0 : Primary_expression_T | undefined,new_expression_1 : New_expression_T | undefined,template_string_2 : Template_string_T) { super();this.primary_expression_0_i = primary_expression_0;this.new_expression_1_i = new_expression_1;this.template_string_2_i = template_string_2; } evaluate(): string { return `${(this.primary_expression_0_i !== undefined ? this.primary_expression_0_i.evaluate() : this.new_expression_1_i !== undefined ? this.new_expression_1_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}${this.template_string_2_i.evaluate()}`; } }
export class Catch_clause_T extends SyntaticInterior { identifier_0_i : Identifier_T | undefined;
 _destructuring_pattern_1_i : _destructuring_pattern_T | undefined;
 statement_block_2_i : Statement_block_T;
 constructor(identifier_0 : Identifier_T | undefined,_destructuring_pattern_1 : _destructuring_pattern_T | undefined,statement_block_2 : Statement_block_T) { super();this.identifier_0_i = identifier_0;this._destructuring_pattern_1_i = _destructuring_pattern_1;this.statement_block_2_i = statement_block_2; } evaluate(): string { return `catch${(this.identifier_0_i !== undefined ? `(${(this.identifier_0_i !== undefined ? this.identifier_0_i.evaluate() : this._destructuring_pattern_1_i !== undefined ? this._destructuring_pattern_1_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())})` : "")}${this.statement_block_2_i.evaluate()}`; } }
export class Class_T extends SyntaticInterior { decorator_0_i : Decorator_T;
 identifier_1_i : Identifier_T | undefined;
 class_heritage_2_i : Class_heritage_T | undefined;
 class_body_3_i : Class_body_T;
 constructor(decorator_0 : Decorator_T,identifier_1 : Identifier_T | undefined,class_heritage_2 : Class_heritage_T | undefined,class_body_3 : Class_body_T) { super();this.decorator_0_i = decorator_0;this.identifier_1_i = identifier_1;this.class_heritage_2_i = class_heritage_2;this.class_body_3_i = class_body_3; } evaluate(): string { return `${this.decorator_0_i.evaluate()}class${(this.identifier_1_i !== undefined ? this.identifier_1_i.evaluate() : "")}${(this.class_heritage_2_i !== undefined ? this.class_heritage_2_i.evaluate() : "")}${this.class_body_3_i.evaluate()}`; } }
export class Class_body_T extends SyntaticInterior { method_definition_0_i : Method_definition_T | undefined;
 field_definition_1_i : Field_definition_T | undefined;
 _semicolon_2_i : _semicolon_T | undefined;
 class_static_block_3_i : Class_static_block_T | undefined;
 constructor(method_definition_0 : Method_definition_T | undefined,field_definition_1 : Field_definition_T | undefined,_semicolon_2 : _semicolon_T | undefined,class_static_block_3 : Class_static_block_T | undefined) { super();this.method_definition_0_i = method_definition_0;this.field_definition_1_i = field_definition_1;this._semicolon_2_i = _semicolon_2;this.class_static_block_3_i = class_static_block_3; } evaluate(): string { return `{${(this.method_definition_0_i !== undefined ? `${this.method_definition_0_i.evaluate()}${(";")}` : this.field_definition_1_i !== undefined ? `${this.field_definition_1_i.evaluate()}${this._semicolon_2_i.evaluate()}` : this.class_static_block_3_i !== undefined ? this.class_static_block_3_i.evaluate() : ";")}}`; } }
export class Class_declaration_T extends SyntaticInterior { decorator_0_i : Decorator_T;
 identifier_1_i : Identifier_T;
 class_heritage_2_i : Class_heritage_T | undefined;
 class_body_3_i : Class_body_T;
 constructor(decorator_0 : Decorator_T,identifier_1 : Identifier_T,class_heritage_2 : Class_heritage_T | undefined,class_body_3 : Class_body_T) { super();this.decorator_0_i = decorator_0;this.identifier_1_i = identifier_1;this.class_heritage_2_i = class_heritage_2;this.class_body_3_i = class_body_3; } evaluate(): string { return `${this.decorator_0_i.evaluate()}class${this.identifier_1_i.evaluate()}${(this.class_heritage_2_i !== undefined ? this.class_heritage_2_i.evaluate() : "")}${this.class_body_3_i.evaluate()}${("")}`; } }
export class Class_heritage_T extends SyntaticInterior { expression_0_i : Expression_T;
 constructor(expression_0 : Expression_T) { super();this.expression_0_i = expression_0; } evaluate(): string { return `extends${this.expression_0_i.evaluate()}`; } }
export class Class_static_block_T extends SyntaticInterior { statement_block_0_i : Statement_block_T;
 constructor(statement_block_0 : Statement_block_T) { super();this.statement_block_0_i = statement_block_0; } evaluate(): string { return `static${("")}${this.statement_block_0_i.evaluate()}`; } }
export class Comment_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Computed_property_name_T extends SyntaticInterior { expression_0_i : Expression_T;
 constructor(expression_0 : Expression_T) { super();this.expression_0_i = expression_0; } evaluate(): string { return `[${this.expression_0_i.evaluate()}]`; } }
export class Continue_statement_T extends SyntaticInterior { identifier_0_i : Identifier_T | undefined;
 _semicolon_1_i : _semicolon_T;
 constructor(identifier_0 : Identifier_T | undefined,_semicolon_1 : _semicolon_T) { super();this.identifier_0_i = identifier_0;this._semicolon_1_i = _semicolon_1; } evaluate(): string { return `continue${(this.identifier_0_i !== undefined ? this.identifier_0_i.evaluate() : "")}${this._semicolon_1_i.evaluate()}`; } }
export class Debugger_statement_T extends SyntaticInterior { _semicolon_0_i : _semicolon_T;
 constructor(_semicolon_0 : _semicolon_T) { super();this._semicolon_0_i = _semicolon_0; } evaluate(): string { return `debugger${this._semicolon_0_i.evaluate()}`; } }
export class Declaration_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class Declaration_class_declaration_T extends Declaration_T { class_declaration_0_i : Class_declaration_T;
 constructor(class_declaration_0 : Class_declaration_T) { super();this.class_declaration_0_i = class_declaration_0; } evaluate(): string { return this.class_declaration_0_i.evaluate(); } }
export class Declaration_function_declaration_T extends Declaration_T { function_declaration_0_i : Function_declaration_T;
 constructor(function_declaration_0 : Function_declaration_T) { super();this.function_declaration_0_i = function_declaration_0; } evaluate(): string { return this.function_declaration_0_i.evaluate(); } }
export class Declaration_generator_function_declaration_T extends Declaration_T { generator_function_declaration_0_i : Generator_function_declaration_T;
 constructor(generator_function_declaration_0 : Generator_function_declaration_T) { super();this.generator_function_declaration_0_i = generator_function_declaration_0; } evaluate(): string { return this.generator_function_declaration_0_i.evaluate(); } }
export class Declaration_lexical_declaration_T extends Declaration_T { lexical_declaration_0_i : Lexical_declaration_T;
 constructor(lexical_declaration_0 : Lexical_declaration_T) { super();this.lexical_declaration_0_i = lexical_declaration_0; } evaluate(): string { return this.lexical_declaration_0_i.evaluate(); } }
export class Declaration_using_declaration_T extends Declaration_T { using_declaration_0_i : Using_declaration_T;
 constructor(using_declaration_0 : Using_declaration_T) { super();this.using_declaration_0_i = using_declaration_0; } evaluate(): string { return this.using_declaration_0_i.evaluate(); } }
export class Declaration_variable_declaration_T extends Declaration_T { variable_declaration_0_i : Variable_declaration_T;
 constructor(variable_declaration_0 : Variable_declaration_T) { super();this.variable_declaration_0_i = variable_declaration_0; } evaluate(): string { return this.variable_declaration_0_i.evaluate(); } }
export class Decorator_T extends SyntaticInterior { identifier_0_i : Identifier_T | undefined;
 decorator_member_expression_1_i : Decorator_member_expression_T | undefined;
 decorator_call_expression_2_i : Decorator_call_expression_T | undefined;
 constructor(identifier_0 : Identifier_T | undefined,decorator_member_expression_1 : Decorator_member_expression_T | undefined,decorator_call_expression_2 : Decorator_call_expression_T | undefined) { super();this.identifier_0_i = identifier_0;this.decorator_member_expression_1_i = decorator_member_expression_1;this.decorator_call_expression_2_i = decorator_call_expression_2; } evaluate(): string { return `@${(this.identifier_0_i !== undefined ? this.identifier_0_i.evaluate() : this.decorator_member_expression_1_i !== undefined ? this.decorator_member_expression_1_i.evaluate() : this.decorator_call_expression_2_i !== undefined ? this.decorator_call_expression_2_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}`; } }
export class Decorator_call_expression_T extends SyntaticInterior { identifier_0_i : Identifier_T | undefined;
 decorator_member_expression_1_i : Decorator_member_expression_T | undefined;
 arguments_2_i : Arguments_T;
 constructor(identifier_0 : Identifier_T | undefined,decorator_member_expression_1 : Decorator_member_expression_T | undefined,arguments_2 : Arguments_T) { super();this.identifier_0_i = identifier_0;this.decorator_member_expression_1_i = decorator_member_expression_1;this.arguments_2_i = arguments_2; } evaluate(): string { return `${(this.identifier_0_i !== undefined ? this.identifier_0_i.evaluate() : this.decorator_member_expression_1_i !== undefined ? this.decorator_member_expression_1_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}${this.arguments_2_i.evaluate()}`; } }
export class Decorator_member_expression_T extends SyntaticInterior { identifier_0_i : Identifier_T | undefined;
 decorator_member_expression_1_i : Decorator_member_expression_T | undefined;
 identifier_2_i : Identifier_T;
 constructor(identifier_0 : Identifier_T | undefined,decorator_member_expression_1 : Decorator_member_expression_T | undefined,identifier_2 : Identifier_T) { super();this.identifier_0_i = identifier_0;this.decorator_member_expression_1_i = decorator_member_expression_1;this.identifier_2_i = identifier_2; } evaluate(): string { return `${(this.identifier_0_i !== undefined ? this.identifier_0_i.evaluate() : this.decorator_member_expression_1_i !== undefined ? this.decorator_member_expression_1_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}.${this.identifier_2_i.evaluate()}`; } }
export class Do_statement_T extends SyntaticInterior { statement_0_i : Statement_T;
 parenthesized_expression_1_i : Parenthesized_expression_T;
 _semicolon_2_i : _semicolon_T | undefined;
 constructor(statement_0 : Statement_T,parenthesized_expression_1 : Parenthesized_expression_T,_semicolon_2 : _semicolon_T | undefined) { super();this.statement_0_i = statement_0;this.parenthesized_expression_1_i = parenthesized_expression_1;this._semicolon_2_i = _semicolon_2; } evaluate(): string { return `do${this.statement_0_i.evaluate()}while${this.parenthesized_expression_1_i.evaluate()}${(this._semicolon_2_i !== undefined ? this._semicolon_2_i.evaluate() : "")}`; } }
export class Else_clause_T extends SyntaticInterior { statement_0_i : Statement_T;
 constructor(statement_0 : Statement_T) { super();this.statement_0_i = statement_0; } evaluate(): string { return `else${this.statement_0_i.evaluate()}`; } }
export class Empty_statement_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Escape_sequence_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Export_clause_T extends SyntaticInterior { export_specifier_0_i : Export_specifier_T | undefined;
 export_specifier_1_i : Export_specifier_T | undefined;
 constructor(export_specifier_0 : Export_specifier_T | undefined,export_specifier_1 : Export_specifier_T | undefined) { super();this.export_specifier_0_i = export_specifier_0;this.export_specifier_1_i = export_specifier_1; } evaluate(): string { return `{${(this.export_specifier_0_i !== undefined ? `${this.export_specifier_0_i.evaluate()}${`,${this.export_specifier_1_i.evaluate()}`}` : "")}${(",")}}`; } }
export class Export_specifier_T extends SyntaticInterior { _module_export_name_0_i : _module_export_name_T;
 _module_export_name_1_i : _module_export_name_T | undefined;
 constructor(_module_export_name_0 : _module_export_name_T,_module_export_name_1 : _module_export_name_T | undefined) { super();this._module_export_name_0_i = _module_export_name_0;this._module_export_name_1_i = _module_export_name_1; } evaluate(): string { return `${this._module_export_name_0_i.evaluate()}${(this._module_export_name_1_i !== undefined ? `as${this._module_export_name_1_i.evaluate()}` : "")}`; } }
export class Export_statement_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class Export_statement_0_T extends Export_statement_T { _from_clause_0_i : _from_clause_T | undefined;
 namespace_export_1_i : Namespace_export_T | undefined;
 _from_clause_2_i : _from_clause_T | undefined;
 export_clause_3_i : Export_clause_T | undefined;
 _from_clause_4_i : _from_clause_T | undefined;
 export_clause_5_i : Export_clause_T | undefined;
 _semicolon_6_i : _semicolon_T;
 constructor(_from_clause_0 : _from_clause_T | undefined,namespace_export_1 : Namespace_export_T | undefined,_from_clause_2 : _from_clause_T | undefined,export_clause_3 : Export_clause_T | undefined,_from_clause_4 : _from_clause_T | undefined,export_clause_5 : Export_clause_T | undefined,_semicolon_6 : _semicolon_T) { super();this._from_clause_0_i = _from_clause_0;this.namespace_export_1_i = namespace_export_1;this._from_clause_2_i = _from_clause_2;this.export_clause_3_i = export_clause_3;this._from_clause_4_i = _from_clause_4;this.export_clause_5_i = export_clause_5;this._semicolon_6_i = _semicolon_6; } evaluate(): string { return `export${(this._from_clause_0_i !== undefined ? `*${this._from_clause_0_i.evaluate()}` : this.namespace_export_1_i !== undefined ? `${this.namespace_export_1_i.evaluate()}${this._from_clause_2_i.evaluate()}` : this.export_clause_3_i !== undefined ? `${this.export_clause_3_i.evaluate()}${this._from_clause_4_i.evaluate()}` : this.export_clause_5_i !== undefined ? this.export_clause_5_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}${this._semicolon_6_i.evaluate()}`; } }
export class Export_statement_1_T extends Export_statement_T { decorator_0_i : Decorator_T;
 declaration_1_i : Declaration_T | undefined;
 declaration_2_i : Declaration_T | undefined;
 expression_3_i : Expression_T | undefined;
 _semicolon_4_i : _semicolon_T | undefined;
 constructor(decorator_0 : Decorator_T,declaration_1 : Declaration_T | undefined,declaration_2 : Declaration_T | undefined,expression_3 : Expression_T | undefined,_semicolon_4 : _semicolon_T | undefined) { super();this.decorator_0_i = decorator_0;this.declaration_1_i = declaration_1;this.declaration_2_i = declaration_2;this.expression_3_i = expression_3;this._semicolon_4_i = _semicolon_4; } evaluate(): string { return `${this.decorator_0_i.evaluate()}export${(this.declaration_1_i !== undefined ? this.declaration_1_i.evaluate() : this.declaration_2_i !== undefined ? `default${(this.declaration_2_i !== undefined ? this.declaration_2_i.evaluate() : this.expression_3_i !== undefined ? `${this.expression_3_i.evaluate()}${this._semicolon_4_i.evaluate()}` : (() => { throw new Error("No alternative matched in CHOICE node"); })())}` : (() => { throw new Error("No alternative matched in CHOICE node"); })())}`; } }
export class Expression_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class Expression__jsx_element_T extends Expression_T { _jsx_element_0_i : _jsx_element_T;
 constructor(_jsx_element_0 : _jsx_element_T) { super();this._jsx_element_0_i = _jsx_element_0; } evaluate(): string { return this._jsx_element_0_i.evaluate(); } }
export class Expression_assignment_expression_T extends Expression_T { assignment_expression_0_i : Assignment_expression_T;
 constructor(assignment_expression_0 : Assignment_expression_T) { super();this.assignment_expression_0_i = assignment_expression_0; } evaluate(): string { return this.assignment_expression_0_i.evaluate(); } }
export class Expression_augmented_assignment_expression_T extends Expression_T { augmented_assignment_expression_0_i : Augmented_assignment_expression_T;
 constructor(augmented_assignment_expression_0 : Augmented_assignment_expression_T) { super();this.augmented_assignment_expression_0_i = augmented_assignment_expression_0; } evaluate(): string { return this.augmented_assignment_expression_0_i.evaluate(); } }
export class Expression_await_expression_T extends Expression_T { await_expression_0_i : Await_expression_T;
 constructor(await_expression_0 : Await_expression_T) { super();this.await_expression_0_i = await_expression_0; } evaluate(): string { return this.await_expression_0_i.evaluate(); } }
export class Expression_binary_expression_T extends Expression_T { binary_expression_0_i : Binary_expression_T;
 constructor(binary_expression_0 : Binary_expression_T) { super();this.binary_expression_0_i = binary_expression_0; } evaluate(): string { return this.binary_expression_0_i.evaluate(); } }
export class Expression_new_expression_T extends Expression_T { new_expression_0_i : New_expression_T;
 constructor(new_expression_0 : New_expression_T) { super();this.new_expression_0_i = new_expression_0; } evaluate(): string { return this.new_expression_0_i.evaluate(); } }
export class Expression_primary_expression_T extends Expression_T { primary_expression_0_i : Primary_expression_T;
 constructor(primary_expression_0 : Primary_expression_T) { super();this.primary_expression_0_i = primary_expression_0; } evaluate(): string { return this.primary_expression_0_i.evaluate(); } }
export class Expression_statement_T extends Expression_T { _expressions_0_i : _expressions_T;
 _semicolon_1_i : _semicolon_T;
 constructor(_expressions_0 : _expressions_T,_semicolon_1 : _semicolon_T) { super();this._expressions_0_i = _expressions_0;this._semicolon_1_i = _semicolon_1; } evaluate(): string { return `${this._expressions_0_i.evaluate()}${this._semicolon_1_i.evaluate()}`; } }
export class Expression_ternary_expression_T extends Expression_T { ternary_expression_0_i : Ternary_expression_T;
 constructor(ternary_expression_0 : Ternary_expression_T) { super();this.ternary_expression_0_i = ternary_expression_0; } evaluate(): string { return this.ternary_expression_0_i.evaluate(); } }
export class Expression_unary_expression_T extends Expression_T { unary_expression_0_i : Unary_expression_T;
 constructor(unary_expression_0 : Unary_expression_T) { super();this.unary_expression_0_i = unary_expression_0; } evaluate(): string { return this.unary_expression_0_i.evaluate(); } }
export class Expression_update_expression_T extends Expression_T { update_expression_0_i : Update_expression_T;
 constructor(update_expression_0 : Update_expression_T) { super();this.update_expression_0_i = update_expression_0; } evaluate(): string { return this.update_expression_0_i.evaluate(); } }
export class Expression_yield_expression_T extends Expression_T { yield_expression_0_i : Yield_expression_T;
 constructor(yield_expression_0 : Yield_expression_T) { super();this.yield_expression_0_i = yield_expression_0; } evaluate(): string { return this.yield_expression_0_i.evaluate(); } }
export class False_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Field_definition_T extends SyntaticInterior { decorator_0_i : Decorator_T;
 _property_name_1_i : _property_name_T;
 _initializer_2_i : _initializer_T | undefined;
 constructor(decorator_0 : Decorator_T,_property_name_1 : _property_name_T,_initializer_2 : _initializer_T | undefined) { super();this.decorator_0_i = decorator_0;this._property_name_1_i = _property_name_1;this._initializer_2_i = _initializer_2; } evaluate(): string { return `${this.decorator_0_i.evaluate()}${("static")}${this._property_name_1_i.evaluate()}${(this._initializer_2_i !== undefined ? this._initializer_2_i.evaluate() : "")}`; } }
export class Finally_clause_T extends SyntaticInterior { statement_block_0_i : Statement_block_T;
 constructor(statement_block_0 : Statement_block_T) { super();this.statement_block_0_i = statement_block_0; } evaluate(): string { return `finally${this.statement_block_0_i.evaluate()}`; } }
export class For_in_statement_T extends SyntaticInterior { _for_header_0_i : _for_header_T;
 statement_1_i : Statement_T;
 constructor(_for_header_0 : _for_header_T,statement_1 : Statement_T) { super();this._for_header_0_i = _for_header_0;this.statement_1_i = statement_1; } evaluate(): string { return `for${("await")}${this._for_header_0_i.evaluate()}${this.statement_1_i.evaluate()}`; } }
export class For_statement_T extends SyntaticInterior { lexical_declaration_0_i : Lexical_declaration_T | undefined;
 variable_declaration_1_i : Variable_declaration_T | undefined;
 _expressions_2_i : _expressions_T | undefined;
 empty_statement_3_i : Empty_statement_T | undefined;
 _expressions_4_i : _expressions_T | undefined;
 empty_statement_5_i : Empty_statement_T | undefined;
 _expressions_6_i : _expressions_T | undefined;
 statement_7_i : Statement_T;
 constructor(lexical_declaration_0 : Lexical_declaration_T | undefined,variable_declaration_1 : Variable_declaration_T | undefined,_expressions_2 : _expressions_T | undefined,empty_statement_3 : Empty_statement_T | undefined,_expressions_4 : _expressions_T | undefined,empty_statement_5 : Empty_statement_T | undefined,_expressions_6 : _expressions_T | undefined,statement_7 : Statement_T) { super();this.lexical_declaration_0_i = lexical_declaration_0;this.variable_declaration_1_i = variable_declaration_1;this._expressions_2_i = _expressions_2;this.empty_statement_3_i = empty_statement_3;this._expressions_4_i = _expressions_4;this.empty_statement_5_i = empty_statement_5;this._expressions_6_i = _expressions_6;this.statement_7_i = statement_7; } evaluate(): string { return `for(${(this.lexical_declaration_0_i !== undefined ? this.lexical_declaration_0_i.evaluate() : this._expressions_2_i !== undefined ? `${this._expressions_2_i.evaluate()};` : this.empty_statement_3_i !== undefined ? this.empty_statement_3_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}${(this._expressions_4_i !== undefined ? `${this._expressions_4_i.evaluate()};` : this.empty_statement_5_i !== undefined ? this.empty_statement_5_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}${(this._expressions_6_i !== undefined ? this._expressions_6_i.evaluate() : "")})${this.statement_7_i.evaluate()}`; } }
export class Formal_parameters_T extends SyntaticInterior { _formal_parameter_0_i : _formal_parameter_T | undefined;
 _formal_parameter_1_i : _formal_parameter_T | undefined;
 constructor(_formal_parameter_0 : _formal_parameter_T | undefined,_formal_parameter_1 : _formal_parameter_T | undefined) { super();this._formal_parameter_0_i = _formal_parameter_0;this._formal_parameter_1_i = _formal_parameter_1; } evaluate(): string { return `(${(this._formal_parameter_0_i !== undefined ? `${`${this._formal_parameter_0_i.evaluate()}${`,${this._formal_parameter_1_i.evaluate()}`}`}${(",")}` : "")})`; } }
export class Function_declaration_T extends SyntaticInterior { identifier_0_i : Identifier_T;
 _call_signature_1_i : _call_signature_T;
 statement_block_2_i : Statement_block_T;
 constructor(identifier_0 : Identifier_T,_call_signature_1 : _call_signature_T,statement_block_2 : Statement_block_T) { super();this.identifier_0_i = identifier_0;this._call_signature_1_i = _call_signature_1;this.statement_block_2_i = statement_block_2; } evaluate(): string { return `${("async")}function${this.identifier_0_i.evaluate()}${this._call_signature_1_i.evaluate()}${this.statement_block_2_i.evaluate()}${("")}`; } }
export class Function_expression_T extends SyntaticInterior { identifier_0_i : Identifier_T | undefined;
 _call_signature_1_i : _call_signature_T;
 statement_block_2_i : Statement_block_T;
 constructor(identifier_0 : Identifier_T | undefined,_call_signature_1 : _call_signature_T,statement_block_2 : Statement_block_T) { super();this.identifier_0_i = identifier_0;this._call_signature_1_i = _call_signature_1;this.statement_block_2_i = statement_block_2; } evaluate(): string { return `${("async")}function${(this.identifier_0_i !== undefined ? this.identifier_0_i.evaluate() : "")}${this._call_signature_1_i.evaluate()}${this.statement_block_2_i.evaluate()}`; } }
export class Generator_function_T extends SyntaticInterior { identifier_0_i : Identifier_T | undefined;
 _call_signature_1_i : _call_signature_T;
 statement_block_2_i : Statement_block_T;
 constructor(identifier_0 : Identifier_T | undefined,_call_signature_1 : _call_signature_T,statement_block_2 : Statement_block_T) { super();this.identifier_0_i = identifier_0;this._call_signature_1_i = _call_signature_1;this.statement_block_2_i = statement_block_2; } evaluate(): string { return `${("async")}function*${(this.identifier_0_i !== undefined ? this.identifier_0_i.evaluate() : "")}${this._call_signature_1_i.evaluate()}${this.statement_block_2_i.evaluate()}`; } }
export class Generator_function_declaration_T extends SyntaticInterior { identifier_0_i : Identifier_T;
 _call_signature_1_i : _call_signature_T;
 statement_block_2_i : Statement_block_T;
 constructor(identifier_0 : Identifier_T,_call_signature_1 : _call_signature_T,statement_block_2 : Statement_block_T) { super();this.identifier_0_i = identifier_0;this._call_signature_1_i = _call_signature_1;this.statement_block_2_i = statement_block_2; } evaluate(): string { return `${("async")}function*${this.identifier_0_i.evaluate()}${this._call_signature_1_i.evaluate()}${this.statement_block_2_i.evaluate()}${("")}`; } }
export class Hash_bang_line_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Html_character_reference_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Identifier_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class If_statement_T extends SyntaticInterior { parenthesized_expression_0_i : Parenthesized_expression_T;
 statement_1_i : Statement_T;
 else_clause_2_i : Else_clause_T | undefined;
 constructor(parenthesized_expression_0 : Parenthesized_expression_T,statement_1 : Statement_T,else_clause_2 : Else_clause_T | undefined) { super();this.parenthesized_expression_0_i = parenthesized_expression_0;this.statement_1_i = statement_1;this.else_clause_2_i = else_clause_2; } evaluate(): string { return `if${this.parenthesized_expression_0_i.evaluate()}${this.statement_1_i.evaluate()}${(this.else_clause_2_i !== undefined ? this.else_clause_2_i.evaluate() : "")}`; } }
export class Import_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Import_attribute_T extends SyntaticInterior { object_0_i : Object_T;
 constructor(object_0 : Object_T) { super();this.object_0_i = object_0; } evaluate(): string { return `with${this.object_0_i.evaluate()}`; } }
export class Import_clause_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class Import_clause_2_T extends Import_clause_T { identifier_0_i : Identifier_T;
 namespace_import_1_i : Namespace_import_T | undefined;
 named_imports_2_i : Named_imports_T | undefined;
 constructor(identifier_0 : Identifier_T,namespace_import_1 : Namespace_import_T | undefined,named_imports_2 : Named_imports_T | undefined) { super();this.identifier_0_i = identifier_0;this.namespace_import_1_i = namespace_import_1;this.named_imports_2_i = named_imports_2; } evaluate(): string { return `${this.identifier_0_i.evaluate()}${(this.namespace_import_1_i !== undefined ? `,${(this.namespace_import_1_i !== undefined ? this.namespace_import_1_i.evaluate() : this.named_imports_2_i !== undefined ? this.named_imports_2_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}` : "")}`; } }
export class Import_clause_named_imports_T extends Import_clause_T { named_imports_0_i : Named_imports_T;
 constructor(named_imports_0 : Named_imports_T) { super();this.named_imports_0_i = named_imports_0; } evaluate(): string { return this.named_imports_0_i.evaluate(); } }
export class Import_clause_namespace_import_T extends Import_clause_T { namespace_import_0_i : Namespace_import_T;
 constructor(namespace_import_0 : Namespace_import_T) { super();this.namespace_import_0_i = namespace_import_0; } evaluate(): string { return this.namespace_import_0_i.evaluate(); } }
export class Import_specifier_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class Import_specifier_0_T extends Import_specifier_T { identifier_0_i : Identifier_T;
 constructor(identifier_0 : Identifier_T) { super();this.identifier_0_i = identifier_0; } evaluate(): string { return this.identifier_0_i.evaluate(); } }
export class Import_specifier_1_T extends Import_specifier_T { _module_export_name_0_i : _module_export_name_T;
 identifier_1_i : Identifier_T;
 constructor(_module_export_name_0 : _module_export_name_T,identifier_1 : Identifier_T) { super();this._module_export_name_0_i = _module_export_name_0;this.identifier_1_i = identifier_1; } evaluate(): string { return `${this._module_export_name_0_i.evaluate()}as${this.identifier_1_i.evaluate()}`; } }
export class Import_statement_T extends SyntaticInterior { import_clause_0_i : Import_clause_T | undefined;
 _from_clause_1_i : _from_clause_T | undefined;
 string_2_i : String_T | undefined;
 import_attribute_3_i : Import_attribute_T | undefined;
 _semicolon_4_i : _semicolon_T;
 constructor(import_clause_0 : Import_clause_T | undefined,_from_clause_1 : _from_clause_T | undefined,string_2 : String_T | undefined,import_attribute_3 : Import_attribute_T | undefined,_semicolon_4 : _semicolon_T) { super();this.import_clause_0_i = import_clause_0;this._from_clause_1_i = _from_clause_1;this.string_2_i = string_2;this.import_attribute_3_i = import_attribute_3;this._semicolon_4_i = _semicolon_4; } evaluate(): string { return `import${(this.import_clause_0_i !== undefined ? `${this.import_clause_0_i.evaluate()}${this._from_clause_1_i.evaluate()}` : this.string_2_i !== undefined ? this.string_2_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}${(this.import_attribute_3_i !== undefined ? this.import_attribute_3_i.evaluate() : "")}${this._semicolon_4_i.evaluate()}`; } }
export class Jsx_attribute_T extends SyntaticInterior { _jsx_attribute_name_0_i : _jsx_attribute_name_T;
 _jsx_attribute_value_1_i : _jsx_attribute_value_T | undefined;
 constructor(_jsx_attribute_name_0 : _jsx_attribute_name_T,_jsx_attribute_value_1 : _jsx_attribute_value_T | undefined) { super();this._jsx_attribute_name_0_i = _jsx_attribute_name_0;this._jsx_attribute_value_1_i = _jsx_attribute_value_1; } evaluate(): string { return `${this._jsx_attribute_name_0_i.evaluate()}${(this._jsx_attribute_value_1_i !== undefined ? `=${this._jsx_attribute_value_1_i.evaluate()}` : "")}`; } }
export class Jsx_closing_element_T extends SyntaticInterior { _jsx_element_name_0_i : _jsx_element_name_T | undefined;
 constructor(_jsx_element_name_0 : _jsx_element_name_T | undefined) { super();this._jsx_element_name_0_i = _jsx_element_name_0; } evaluate(): string { return `</${(this._jsx_element_name_0_i !== undefined ? this._jsx_element_name_0_i.evaluate() : "")}>`; } }
export class Jsx_element_T extends SyntaticInterior { jsx_opening_element_0_i : Jsx_opening_element_T;
 _jsx_child_1_i : _jsx_child_T;
 jsx_closing_element_2_i : Jsx_closing_element_T;
 constructor(jsx_opening_element_0 : Jsx_opening_element_T,_jsx_child_1 : _jsx_child_T,jsx_closing_element_2 : Jsx_closing_element_T) { super();this.jsx_opening_element_0_i = jsx_opening_element_0;this._jsx_child_1_i = _jsx_child_1;this.jsx_closing_element_2_i = jsx_closing_element_2; } evaluate(): string { return `${this.jsx_opening_element_0_i.evaluate()}${this._jsx_child_1_i.evaluate()}${this.jsx_closing_element_2_i.evaluate()}`; } }
export class Jsx_expression_T extends SyntaticInterior { expression_0_i : Expression_T | undefined;
 sequence_expression_1_i : Sequence_expression_T | undefined;
 spread_element_2_i : Spread_element_T | undefined;
 constructor(expression_0 : Expression_T | undefined,sequence_expression_1 : Sequence_expression_T | undefined,spread_element_2 : Spread_element_T | undefined) { super();this.expression_0_i = expression_0;this.sequence_expression_1_i = sequence_expression_1;this.spread_element_2_i = spread_element_2; } evaluate(): string { return `{${(this.expression_0_i !== undefined ? this.expression_0_i.evaluate() : "")}}`; } }
export class Jsx_identifier_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Jsx_namespace_name_T extends SyntaticInterior { _jsx_identifier_0_i : _jsx_identifier_T;
 _jsx_identifier_1_i : _jsx_identifier_T;
 constructor(_jsx_identifier_0 : _jsx_identifier_T,_jsx_identifier_1 : _jsx_identifier_T) { super();this._jsx_identifier_0_i = _jsx_identifier_0;this._jsx_identifier_1_i = _jsx_identifier_1; } evaluate(): string { return `${this._jsx_identifier_0_i.evaluate()}:${this._jsx_identifier_1_i.evaluate()}`; } }
export class Jsx_opening_element_T extends SyntaticInterior { _jsx_element_name_0_i : _jsx_element_name_T | undefined;
 _jsx_attribute_1_i : _jsx_attribute_T | undefined;
 constructor(_jsx_element_name_0 : _jsx_element_name_T | undefined,_jsx_attribute_1 : _jsx_attribute_T | undefined) { super();this._jsx_element_name_0_i = _jsx_element_name_0;this._jsx_attribute_1_i = _jsx_attribute_1; } evaluate(): string { return `<${(this._jsx_element_name_0_i !== undefined ? `${this._jsx_element_name_0_i.evaluate()}${this._jsx_attribute_1_i.evaluate()}` : "")}>`; } }
export class Jsx_self_closing_element_T extends SyntaticInterior { _jsx_element_name_0_i : _jsx_element_name_T;
 _jsx_attribute_1_i : _jsx_attribute_T;
 constructor(_jsx_element_name_0 : _jsx_element_name_T,_jsx_attribute_1 : _jsx_attribute_T) { super();this._jsx_element_name_0_i = _jsx_element_name_0;this._jsx_attribute_1_i = _jsx_attribute_1; } evaluate(): string { return `<${this._jsx_element_name_0_i.evaluate()}${this._jsx_attribute_1_i.evaluate()}/>`; } }
export class Labeled_statement_T extends SyntaticInterior { identifier_0_i : Identifier_T | undefined;
 _reserved_identifier_1_i : _reserved_identifier_T | undefined;
 statement_2_i : Statement_T;
 constructor(identifier_0 : Identifier_T | undefined,_reserved_identifier_1 : _reserved_identifier_T | undefined,statement_2 : Statement_T) { super();this.identifier_0_i = identifier_0;this._reserved_identifier_1_i = _reserved_identifier_1;this.statement_2_i = statement_2; } evaluate(): string { return `${(this.identifier_0_i !== undefined ? this.identifier_0_i.evaluate() : this._reserved_identifier_1_i !== undefined ? this._reserved_identifier_1_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}:${this.statement_2_i.evaluate()}`; } }
export class Lexical_declaration_T extends SyntaticInterior { variable_declarator_0_i : Variable_declarator_T;
 variable_declarator_1_i : Variable_declarator_T;
 _semicolon_2_i : _semicolon_T;
 constructor(variable_declarator_0 : Variable_declarator_T,variable_declarator_1 : Variable_declarator_T,_semicolon_2 : _semicolon_T) { super();this.variable_declarator_0_i = variable_declarator_0;this.variable_declarator_1_i = variable_declarator_1;this._semicolon_2_i = _semicolon_2; } evaluate(): string { return `${("let")}${`${this.variable_declarator_0_i.evaluate()}${`,${this.variable_declarator_1_i.evaluate()}`}`}${this._semicolon_2_i.evaluate()}`; } }
export class Member_expression_T extends SyntaticInterior { expression_0_i : Expression_T | undefined;
 primary_expression_1_i : Primary_expression_T | undefined;
 import_2_i : Import_T | undefined;
 optional_chain_3_i : Optional_chain_T | undefined;
 private_property_identifier_4_i : Private_property_identifier_T | undefined;
 identifier_5_i : Identifier_T | undefined;
 constructor(expression_0 : Expression_T | undefined,primary_expression_1 : Primary_expression_T | undefined,import_2 : Import_T | undefined,optional_chain_3 : Optional_chain_T | undefined,private_property_identifier_4 : Private_property_identifier_T | undefined,identifier_5 : Identifier_T | undefined) { super();this.expression_0_i = expression_0;this.primary_expression_1_i = primary_expression_1;this.import_2_i = import_2;this.optional_chain_3_i = optional_chain_3;this.private_property_identifier_4_i = private_property_identifier_4;this.identifier_5_i = identifier_5; } evaluate(): string { return `${(this.expression_0_i !== undefined ? this.expression_0_i.evaluate() : this.primary_expression_1_i !== undefined ? this.primary_expression_1_i.evaluate() : this.import_2_i !== undefined ? this.import_2_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}${(".")}${(this.private_property_identifier_4_i !== undefined ? this.private_property_identifier_4_i.evaluate() : this.identifier_5_i !== undefined ? this.identifier_5_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}`; } }
export class Meta_property_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class Meta_property_0_T extends Meta_property_T {  constructor() { super(); } evaluate(): string { return `new.target`; } }
export class Meta_property_1_T extends Meta_property_T {  constructor() { super(); } evaluate(): string { return `import.meta`; } }
export class Method_definition_T extends SyntaticInterior { decorator_0_i : Decorator_T;
 _property_name_1_i : _property_name_T;
 formal_parameters_2_i : Formal_parameters_T;
 statement_block_3_i : Statement_block_T;
 constructor(decorator_0 : Decorator_T,_property_name_1 : _property_name_T,formal_parameters_2 : Formal_parameters_T,statement_block_3 : Statement_block_T) { super();this.decorator_0_i = decorator_0;this._property_name_1_i = _property_name_1;this.formal_parameters_2_i = formal_parameters_2;this.statement_block_3_i = statement_block_3; } evaluate(): string { return `${this.decorator_0_i.evaluate()}${(("static"))}${("async")}${(("get"))}${this._property_name_1_i.evaluate()}${this.formal_parameters_2_i.evaluate()}${this.statement_block_3_i.evaluate()}`; } }
export class Named_imports_T extends SyntaticInterior { import_specifier_0_i : Import_specifier_T | undefined;
 import_specifier_1_i : Import_specifier_T | undefined;
 constructor(import_specifier_0 : Import_specifier_T | undefined,import_specifier_1 : Import_specifier_T | undefined) { super();this.import_specifier_0_i = import_specifier_0;this.import_specifier_1_i = import_specifier_1; } evaluate(): string { return `{${(this.import_specifier_0_i !== undefined ? `${this.import_specifier_0_i.evaluate()}${`,${this.import_specifier_1_i.evaluate()}`}` : "")}${(",")}}`; } }
export class Namespace_export_T extends SyntaticInterior { _module_export_name_0_i : _module_export_name_T;
 constructor(_module_export_name_0 : _module_export_name_T) { super();this._module_export_name_0_i = _module_export_name_0; } evaluate(): string { return `*as${this._module_export_name_0_i.evaluate()}`; } }
export class Namespace_import_T extends SyntaticInterior { identifier_0_i : Identifier_T;
 constructor(identifier_0 : Identifier_T) { super();this.identifier_0_i = identifier_0; } evaluate(): string { return `*as${this.identifier_0_i.evaluate()}`; } }
export class Nested_identifier_T extends SyntaticInterior { identifier_0_i : Identifier_T | undefined;
 nested_identifier_1_i : Nested_identifier_T | undefined;
 identifier_2_i : Identifier_T;
 constructor(identifier_0 : Identifier_T | undefined,nested_identifier_1 : Nested_identifier_T | undefined,identifier_2 : Identifier_T) { super();this.identifier_0_i = identifier_0;this.nested_identifier_1_i = nested_identifier_1;this.identifier_2_i = identifier_2; } evaluate(): string { return `${(this.identifier_0_i !== undefined ? this.identifier_0_i.evaluate() : this.nested_identifier_1_i !== undefined ? this.nested_identifier_1_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}.${this.identifier_2_i.evaluate()}`; } }
export class New_expression_T extends SyntaticInterior { primary_expression_0_i : Primary_expression_T | undefined;
 new_expression_1_i : New_expression_T | undefined;
 arguments_2_i : Arguments_T | undefined;
 constructor(primary_expression_0 : Primary_expression_T | undefined,new_expression_1 : New_expression_T | undefined,arguments_2 : Arguments_T | undefined) { super();this.primary_expression_0_i = primary_expression_0;this.new_expression_1_i = new_expression_1;this.arguments_2_i = arguments_2; } evaluate(): string { return `new${(this.primary_expression_0_i !== undefined ? this.primary_expression_0_i.evaluate() : this.new_expression_1_i !== undefined ? this.new_expression_1_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}${(this.arguments_2_i !== undefined ? this.arguments_2_i.evaluate() : "")}`; } }
export class Null_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Number_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Object_T extends SyntaticInterior { pair_0_i : Pair_T | undefined;
 spread_element_1_i : Spread_element_T | undefined;
 method_definition_2_i : Method_definition_T | undefined;
 identifier_3_i : Identifier_T | undefined;
 _reserved_identifier_4_i : _reserved_identifier_T | undefined;
 pair_5_i : Pair_T | undefined;
 spread_element_6_i : Spread_element_T | undefined;
 method_definition_7_i : Method_definition_T | undefined;
 identifier_8_i : Identifier_T | undefined;
 _reserved_identifier_9_i : _reserved_identifier_T | undefined;
 constructor(pair_0 : Pair_T | undefined,spread_element_1 : Spread_element_T | undefined,method_definition_2 : Method_definition_T | undefined,identifier_3 : Identifier_T | undefined,_reserved_identifier_4 : _reserved_identifier_T | undefined,pair_5 : Pair_T | undefined,spread_element_6 : Spread_element_T | undefined,method_definition_7 : Method_definition_T | undefined,identifier_8 : Identifier_T | undefined,_reserved_identifier_9 : _reserved_identifier_T | undefined) { super();this.pair_0_i = pair_0;this.spread_element_1_i = spread_element_1;this.method_definition_2_i = method_definition_2;this.identifier_3_i = identifier_3;this._reserved_identifier_4_i = _reserved_identifier_4;this.pair_5_i = pair_5;this.spread_element_6_i = spread_element_6;this.method_definition_7_i = method_definition_7;this.identifier_8_i = identifier_8;this._reserved_identifier_9_i = _reserved_identifier_9; } evaluate(): string { return `{${(this.pair_0_i !== undefined ? `${this.pair_0_i.evaluate()}${`,${(this.pair_5_i !== undefined ? this.pair_5_i.evaluate() : "")}`}` : "")}}`; } }
export class Object_assignment_pattern_T extends SyntaticInterior { _reserved_identifier_0_i : _reserved_identifier_T | undefined;
 identifier_1_i : Identifier_T | undefined;
 _destructuring_pattern_2_i : _destructuring_pattern_T | undefined;
 expression_3_i : Expression_T;
 constructor(_reserved_identifier_0 : _reserved_identifier_T | undefined,identifier_1 : Identifier_T | undefined,_destructuring_pattern_2 : _destructuring_pattern_T | undefined,expression_3 : Expression_T) { super();this._reserved_identifier_0_i = _reserved_identifier_0;this.identifier_1_i = identifier_1;this._destructuring_pattern_2_i = _destructuring_pattern_2;this.expression_3_i = expression_3; } evaluate(): string { return `${(this._reserved_identifier_0_i !== undefined ? this._reserved_identifier_0_i.evaluate() : this._destructuring_pattern_2_i !== undefined ? this._destructuring_pattern_2_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}=${this.expression_3_i.evaluate()}`; } }
export class Object_pattern_T extends SyntaticInterior { pair_pattern_0_i : Pair_pattern_T | undefined;
 rest_pattern_1_i : Rest_pattern_T | undefined;
 object_assignment_pattern_2_i : Object_assignment_pattern_T | undefined;
 identifier_3_i : Identifier_T | undefined;
 _reserved_identifier_4_i : _reserved_identifier_T | undefined;
 pair_pattern_5_i : Pair_pattern_T | undefined;
 rest_pattern_6_i : Rest_pattern_T | undefined;
 object_assignment_pattern_7_i : Object_assignment_pattern_T | undefined;
 identifier_8_i : Identifier_T | undefined;
 _reserved_identifier_9_i : _reserved_identifier_T | undefined;
 constructor(pair_pattern_0 : Pair_pattern_T | undefined,rest_pattern_1 : Rest_pattern_T | undefined,object_assignment_pattern_2 : Object_assignment_pattern_T | undefined,identifier_3 : Identifier_T | undefined,_reserved_identifier_4 : _reserved_identifier_T | undefined,pair_pattern_5 : Pair_pattern_T | undefined,rest_pattern_6 : Rest_pattern_T | undefined,object_assignment_pattern_7 : Object_assignment_pattern_T | undefined,identifier_8 : Identifier_T | undefined,_reserved_identifier_9 : _reserved_identifier_T | undefined) { super();this.pair_pattern_0_i = pair_pattern_0;this.rest_pattern_1_i = rest_pattern_1;this.object_assignment_pattern_2_i = object_assignment_pattern_2;this.identifier_3_i = identifier_3;this._reserved_identifier_4_i = _reserved_identifier_4;this.pair_pattern_5_i = pair_pattern_5;this.rest_pattern_6_i = rest_pattern_6;this.object_assignment_pattern_7_i = object_assignment_pattern_7;this.identifier_8_i = identifier_8;this._reserved_identifier_9_i = _reserved_identifier_9; } evaluate(): string { return `{${(this.pair_pattern_0_i !== undefined ? `${this.pair_pattern_0_i.evaluate()}${`,${(this.pair_pattern_5_i !== undefined ? this.pair_pattern_5_i.evaluate() : "")}`}` : "")}}`; } }
export class Optional_chain_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Pair_T extends SyntaticInterior { _property_name_0_i : _property_name_T;
 expression_1_i : Expression_T;
 constructor(_property_name_0 : _property_name_T,expression_1 : Expression_T) { super();this._property_name_0_i = _property_name_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this._property_name_0_i.evaluate()}:${this.expression_1_i.evaluate()}`; } }
export class Pair_pattern_T extends SyntaticInterior { _property_name_0_i : _property_name_T;
 pattern_1_i : Pattern_T | undefined;
 assignment_pattern_2_i : Assignment_pattern_T | undefined;
 constructor(_property_name_0 : _property_name_T,pattern_1 : Pattern_T | undefined,assignment_pattern_2 : Assignment_pattern_T | undefined) { super();this._property_name_0_i = _property_name_0;this.pattern_1_i = pattern_1;this.assignment_pattern_2_i = assignment_pattern_2; } evaluate(): string { return `${this._property_name_0_i.evaluate()}:${(this.pattern_1_i !== undefined ? this.pattern_1_i.evaluate() : this.assignment_pattern_2_i !== undefined ? this.assignment_pattern_2_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}`; } }
export class Parenthesized_expression_T extends SyntaticInterior { _expressions_0_i : _expressions_T;
 constructor(_expressions_0 : _expressions_T) { super();this._expressions_0_i = _expressions_0; } evaluate(): string { return `(${this._expressions_0_i.evaluate()})`; } }
export class Pattern_T extends SyntaticInterior { _lhs_expression_0_i : _lhs_expression_T | undefined;
 rest_pattern_1_i : Rest_pattern_T | undefined;
 constructor(_lhs_expression_0 : _lhs_expression_T | undefined,rest_pattern_1 : Rest_pattern_T | undefined) { super();this._lhs_expression_0_i = _lhs_expression_0;this.rest_pattern_1_i = rest_pattern_1; } evaluate(): string { if (this._lhs_expression_0_i !== undefined) { return this._lhs_expression_0_i.evaluate(); } if (this.rest_pattern_1_i !== undefined) { return this.rest_pattern_1_i.evaluate(); } throw new Error("No alternative matched in CHOICE node"); } }
export class Primary_expression_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class Primary_expression__identifier_T extends Primary_expression_T { _identifier_0_i : _identifier_T;
 constructor(_identifier_0 : _identifier_T) { super();this._identifier_0_i = _identifier_0; } evaluate(): string { return this._identifier_0_i.evaluate(); } }
export class Primary_expression__reserved_identifier_T extends Primary_expression_T { _reserved_identifier_0_i : _reserved_identifier_T;
 constructor(_reserved_identifier_0 : _reserved_identifier_T) { super();this._reserved_identifier_0_i = _reserved_identifier_0; } evaluate(): string { return this._reserved_identifier_0_i.evaluate(); } }
export class Primary_expression_array_T extends Primary_expression_T { array_0_i : Array_T;
 constructor(array_0 : Array_T) { super();this.array_0_i = array_0; } evaluate(): string { return this.array_0_i.evaluate(); } }
export class Primary_expression_arrow_function_T extends Primary_expression_T { arrow_function_0_i : Arrow_function_T;
 constructor(arrow_function_0 : Arrow_function_T) { super();this.arrow_function_0_i = arrow_function_0; } evaluate(): string { return this.arrow_function_0_i.evaluate(); } }
export class Primary_expression_call_expression_T extends Primary_expression_T { call_expression_0_i : Call_expression_T;
 constructor(call_expression_0 : Call_expression_T) { super();this.call_expression_0_i = call_expression_0; } evaluate(): string { return this.call_expression_0_i.evaluate(); } }
export class Primary_expression_class_T extends Primary_expression_T { class_0_i : Class_T;
 constructor(class_0 : Class_T) { super();this.class_0_i = class_0; } evaluate(): string { return this.class_0_i.evaluate(); } }
export class Primary_expression_false_T extends Primary_expression_T { false_0_i : False_T;
 constructor(false_0 : False_T) { super();this.false_0_i = false_0; } evaluate(): string { return this.false_0_i.evaluate(); } }
export class Primary_expression_function_expression_T extends Primary_expression_T { function_expression_0_i : Function_expression_T;
 constructor(function_expression_0 : Function_expression_T) { super();this.function_expression_0_i = function_expression_0; } evaluate(): string { return this.function_expression_0_i.evaluate(); } }
export class Primary_expression_generator_function_T extends Primary_expression_T { generator_function_0_i : Generator_function_T;
 constructor(generator_function_0 : Generator_function_T) { super();this.generator_function_0_i = generator_function_0; } evaluate(): string { return this.generator_function_0_i.evaluate(); } }
export class Primary_expression_member_expression_T extends Primary_expression_T { member_expression_0_i : Member_expression_T;
 constructor(member_expression_0 : Member_expression_T) { super();this.member_expression_0_i = member_expression_0; } evaluate(): string { return this.member_expression_0_i.evaluate(); } }
export class Primary_expression_meta_property_T extends Primary_expression_T { meta_property_0_i : Meta_property_T;
 constructor(meta_property_0 : Meta_property_T) { super();this.meta_property_0_i = meta_property_0; } evaluate(): string { return this.meta_property_0_i.evaluate(); } }
export class Primary_expression_null_T extends Primary_expression_T { null_0_i : Null_T;
 constructor(null_0 : Null_T) { super();this.null_0_i = null_0; } evaluate(): string { return this.null_0_i.evaluate(); } }
export class Primary_expression_number_T extends Primary_expression_T { number_0_i : Number_T;
 constructor(number_0 : Number_T) { super();this.number_0_i = number_0; } evaluate(): string { return this.number_0_i.evaluate(); } }
export class Primary_expression_object_T extends Primary_expression_T { object_0_i : Object_T;
 constructor(object_0 : Object_T) { super();this.object_0_i = object_0; } evaluate(): string { return this.object_0_i.evaluate(); } }
export class Primary_expression_parenthesized_expression_T extends Primary_expression_T { parenthesized_expression_0_i : Parenthesized_expression_T;
 constructor(parenthesized_expression_0 : Parenthesized_expression_T) { super();this.parenthesized_expression_0_i = parenthesized_expression_0; } evaluate(): string { return this.parenthesized_expression_0_i.evaluate(); } }
export class Primary_expression_regex_T extends Primary_expression_T { regex_0_i : Regex_T;
 constructor(regex_0 : Regex_T) { super();this.regex_0_i = regex_0; } evaluate(): string { return this.regex_0_i.evaluate(); } }
export class Primary_expression_string_T extends Primary_expression_T { string_0_i : String_T;
 constructor(string_0 : String_T) { super();this.string_0_i = string_0; } evaluate(): string { return this.string_0_i.evaluate(); } }
export class Primary_expression_subscript_expression_T extends Primary_expression_T { subscript_expression_0_i : Subscript_expression_T;
 constructor(subscript_expression_0 : Subscript_expression_T) { super();this.subscript_expression_0_i = subscript_expression_0; } evaluate(): string { return this.subscript_expression_0_i.evaluate(); } }
export class Primary_expression_super_T extends Primary_expression_T { super_0_i : Super_T;
 constructor(super_0 : Super_T) { super();this.super_0_i = super_0; } evaluate(): string { return this.super_0_i.evaluate(); } }
export class Primary_expression_template_string_T extends Primary_expression_T { template_string_0_i : Template_string_T;
 constructor(template_string_0 : Template_string_T) { super();this.template_string_0_i = template_string_0; } evaluate(): string { return this.template_string_0_i.evaluate(); } }
export class Primary_expression_this_T extends Primary_expression_T { this_0_i : This_T;
 constructor(this_0 : This_T) { super();this.this_0_i = this_0; } evaluate(): string { return this.this_0_i.evaluate(); } }
export class Primary_expression_true_T extends Primary_expression_T { true_0_i : True_T;
 constructor(true_0 : True_T) { super();this.true_0_i = true_0; } evaluate(): string { return this.true_0_i.evaluate(); } }
export class Private_property_identifier_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Program_T extends SyntaticInterior { hash_bang_line_0_i : Hash_bang_line_T | undefined;
 statement_1_i : Statement_T;
 constructor(hash_bang_line_0 : Hash_bang_line_T | undefined,statement_1 : Statement_T) { super();this.hash_bang_line_0_i = hash_bang_line_0;this.statement_1_i = statement_1; } evaluate(): string { return `${(this.hash_bang_line_0_i !== undefined ? this.hash_bang_line_0_i.evaluate() : "")}${this.statement_1_i.evaluate()}`; } }
export class Regex_T extends SyntaticInterior { regex_flags_0_i : Regex_flags_T | undefined;
 constructor(regex_flags_0 : Regex_flags_T | undefined) { super();this.regex_flags_0_i = regex_flags_0; } evaluate(): string { return `//${(this.regex_flags_0_i !== undefined ? this.regex_flags_0_i.evaluate() : "")}`; } }
export class Regex_flags_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Regex_pattern_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Rest_pattern_T extends SyntaticInterior { _lhs_expression_0_i : _lhs_expression_T;
 constructor(_lhs_expression_0 : _lhs_expression_T) { super();this._lhs_expression_0_i = _lhs_expression_0; } evaluate(): string { return `...${this._lhs_expression_0_i.evaluate()}`; } }
export class Return_statement_T extends SyntaticInterior { _expressions_0_i : _expressions_T | undefined;
 _semicolon_1_i : _semicolon_T;
 constructor(_expressions_0 : _expressions_T | undefined,_semicolon_1 : _semicolon_T) { super();this._expressions_0_i = _expressions_0;this._semicolon_1_i = _semicolon_1; } evaluate(): string { return `return${(this._expressions_0_i !== undefined ? this._expressions_0_i.evaluate() : "")}${this._semicolon_1_i.evaluate()}`; } }
export class Sequence_expression_T extends SyntaticInterior { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `${this.expression_0_i.evaluate()}${`,${this.expression_1_i.evaluate()}`}`; } }
export class Spread_element_T extends SyntaticInterior { expression_0_i : Expression_T;
 constructor(expression_0 : Expression_T) { super();this.expression_0_i = expression_0; } evaluate(): string { return `...${this.expression_0_i.evaluate()}`; } }
export class Statement_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class Statement_block_T extends Statement_T { statement_0_i : Statement_T;
 constructor(statement_0 : Statement_T) { super();this.statement_0_i = statement_0; } evaluate(): string { return `{${this.statement_0_i.evaluate()}}${("")}`; } }
export class Statement_break_statement_T extends Statement_T { break_statement_0_i : Break_statement_T;
 constructor(break_statement_0 : Break_statement_T) { super();this.break_statement_0_i = break_statement_0; } evaluate(): string { return this.break_statement_0_i.evaluate(); } }
export class Statement_continue_statement_T extends Statement_T { continue_statement_0_i : Continue_statement_T;
 constructor(continue_statement_0 : Continue_statement_T) { super();this.continue_statement_0_i = continue_statement_0; } evaluate(): string { return this.continue_statement_0_i.evaluate(); } }
export class Statement_debugger_statement_T extends Statement_T { debugger_statement_0_i : Debugger_statement_T;
 constructor(debugger_statement_0 : Debugger_statement_T) { super();this.debugger_statement_0_i = debugger_statement_0; } evaluate(): string { return this.debugger_statement_0_i.evaluate(); } }
export class Statement_declaration_T extends Statement_T { declaration_0_i : Declaration_T;
 constructor(declaration_0 : Declaration_T) { super();this.declaration_0_i = declaration_0; } evaluate(): string { return this.declaration_0_i.evaluate(); } }
export class Statement_do_statement_T extends Statement_T { do_statement_0_i : Do_statement_T;
 constructor(do_statement_0 : Do_statement_T) { super();this.do_statement_0_i = do_statement_0; } evaluate(): string { return this.do_statement_0_i.evaluate(); } }
export class Statement_empty_statement_T extends Statement_T { empty_statement_0_i : Empty_statement_T;
 constructor(empty_statement_0 : Empty_statement_T) { super();this.empty_statement_0_i = empty_statement_0; } evaluate(): string { return this.empty_statement_0_i.evaluate(); } }
export class Statement_export_statement_T extends Statement_T { export_statement_0_i : Export_statement_T;
 constructor(export_statement_0 : Export_statement_T) { super();this.export_statement_0_i = export_statement_0; } evaluate(): string { return this.export_statement_0_i.evaluate(); } }
export class Statement_expression_statement_T extends Statement_T { expression_statement_0_i : Expression_statement_T;
 constructor(expression_statement_0 : Expression_statement_T) { super();this.expression_statement_0_i = expression_statement_0; } evaluate(): string { return this.expression_statement_0_i.evaluate(); } }
export class Statement_for_in_statement_T extends Statement_T { for_in_statement_0_i : For_in_statement_T;
 constructor(for_in_statement_0 : For_in_statement_T) { super();this.for_in_statement_0_i = for_in_statement_0; } evaluate(): string { return this.for_in_statement_0_i.evaluate(); } }
export class Statement_for_statement_T extends Statement_T { for_statement_0_i : For_statement_T;
 constructor(for_statement_0 : For_statement_T) { super();this.for_statement_0_i = for_statement_0; } evaluate(): string { return this.for_statement_0_i.evaluate(); } }
export class Statement_if_statement_T extends Statement_T { if_statement_0_i : If_statement_T;
 constructor(if_statement_0 : If_statement_T) { super();this.if_statement_0_i = if_statement_0; } evaluate(): string { return this.if_statement_0_i.evaluate(); } }
export class Statement_import_statement_T extends Statement_T { import_statement_0_i : Import_statement_T;
 constructor(import_statement_0 : Import_statement_T) { super();this.import_statement_0_i = import_statement_0; } evaluate(): string { return this.import_statement_0_i.evaluate(); } }
export class Statement_labeled_statement_T extends Statement_T { labeled_statement_0_i : Labeled_statement_T;
 constructor(labeled_statement_0 : Labeled_statement_T) { super();this.labeled_statement_0_i = labeled_statement_0; } evaluate(): string { return this.labeled_statement_0_i.evaluate(); } }
export class Statement_return_statement_T extends Statement_T { return_statement_0_i : Return_statement_T;
 constructor(return_statement_0 : Return_statement_T) { super();this.return_statement_0_i = return_statement_0; } evaluate(): string { return this.return_statement_0_i.evaluate(); } }
export class Statement_statement_block_T extends Statement_T { statement_block_0_i : Statement_block_T;
 constructor(statement_block_0 : Statement_block_T) { super();this.statement_block_0_i = statement_block_0; } evaluate(): string { return this.statement_block_0_i.evaluate(); } }
export class Statement_switch_statement_T extends Statement_T { switch_statement_0_i : Switch_statement_T;
 constructor(switch_statement_0 : Switch_statement_T) { super();this.switch_statement_0_i = switch_statement_0; } evaluate(): string { return this.switch_statement_0_i.evaluate(); } }
export class Statement_throw_statement_T extends Statement_T { throw_statement_0_i : Throw_statement_T;
 constructor(throw_statement_0 : Throw_statement_T) { super();this.throw_statement_0_i = throw_statement_0; } evaluate(): string { return this.throw_statement_0_i.evaluate(); } }
export class Statement_try_statement_T extends Statement_T { try_statement_0_i : Try_statement_T;
 constructor(try_statement_0 : Try_statement_T) { super();this.try_statement_0_i = try_statement_0; } evaluate(): string { return this.try_statement_0_i.evaluate(); } }
export class Statement_while_statement_T extends Statement_T { while_statement_0_i : While_statement_T;
 constructor(while_statement_0 : While_statement_T) { super();this.while_statement_0_i = while_statement_0; } evaluate(): string { return this.while_statement_0_i.evaluate(); } }
export class Statement_with_statement_T extends Statement_T { with_statement_0_i : With_statement_T;
 constructor(with_statement_0 : With_statement_T) { super();this.with_statement_0_i = with_statement_0; } evaluate(): string { return this.with_statement_0_i.evaluate(); } }
export class String_T extends SyntaticInterior {  constructor() { super(); } evaluate(): string { throw new Error("No alternative matched in CHOICE node"); } }
export class String_0_T extends String_T { unescaped_double_string_fragment_0_i : Unescaped_double_string_fragment_T | undefined;
 constructor(unescaped_double_string_fragment_0 : Unescaped_double_string_fragment_T | undefined) { super();this.unescaped_double_string_fragment_0_i = unescaped_double_string_fragment_0; } evaluate(): string { return `\"${(this.unescaped_double_string_fragment_0_i !== undefined ? this.unescaped_double_string_fragment_0_i.evaluate() : "")}\"`; } }
export class String_1_T extends String_T { unescaped_single_string_fragment_0_i : Unescaped_single_string_fragment_T | undefined;
 constructor(unescaped_single_string_fragment_0 : Unescaped_single_string_fragment_T | undefined) { super();this.unescaped_single_string_fragment_0_i = unescaped_single_string_fragment_0; } evaluate(): string { return `'${(this.unescaped_single_string_fragment_0_i !== undefined ? this.unescaped_single_string_fragment_0_i.evaluate() : "")}'`; } }
export class Subscript_expression_T extends SyntaticInterior { expression_0_i : Expression_T | undefined;
 primary_expression_1_i : Primary_expression_T | undefined;
 optional_chain_2_i : Optional_chain_T | undefined;
 _expressions_3_i : _expressions_T;
 constructor(expression_0 : Expression_T | undefined,primary_expression_1 : Primary_expression_T | undefined,optional_chain_2 : Optional_chain_T | undefined,_expressions_3 : _expressions_T) { super();this.expression_0_i = expression_0;this.primary_expression_1_i = primary_expression_1;this.optional_chain_2_i = optional_chain_2;this._expressions_3_i = _expressions_3; } evaluate(): string { return `${(this.expression_0_i !== undefined ? this.expression_0_i.evaluate() : this.primary_expression_1_i !== undefined ? this.primary_expression_1_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}${(this.optional_chain_2_i !== undefined ? this.optional_chain_2_i.evaluate() : "")}[${this._expressions_3_i.evaluate()}]`; } }
export class Super_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Switch_body_T extends SyntaticInterior { switch_case_0_i : Switch_case_T | undefined;
 switch_default_1_i : Switch_default_T | undefined;
 constructor(switch_case_0 : Switch_case_T | undefined,switch_default_1 : Switch_default_T | undefined) { super();this.switch_case_0_i = switch_case_0;this.switch_default_1_i = switch_default_1; } evaluate(): string { return `{${(this.switch_case_0_i !== undefined ? this.switch_case_0_i.evaluate() : this.switch_default_1_i !== undefined ? this.switch_default_1_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}}`; } }
export class Switch_case_T extends SyntaticInterior { _expressions_0_i : _expressions_T;
 statement_1_i : Statement_T;
 constructor(_expressions_0 : _expressions_T,statement_1 : Statement_T) { super();this._expressions_0_i = _expressions_0;this.statement_1_i = statement_1; } evaluate(): string { return `case${this._expressions_0_i.evaluate()}:${this.statement_1_i.evaluate()}`; } }
export class Switch_default_T extends SyntaticInterior { statement_0_i : Statement_T;
 constructor(statement_0 : Statement_T) { super();this.statement_0_i = statement_0; } evaluate(): string { return `default:${this.statement_0_i.evaluate()}`; } }
export class Switch_statement_T extends SyntaticInterior { parenthesized_expression_0_i : Parenthesized_expression_T;
 switch_body_1_i : Switch_body_T;
 constructor(parenthesized_expression_0 : Parenthesized_expression_T,switch_body_1 : Switch_body_T) { super();this.parenthesized_expression_0_i = parenthesized_expression_0;this.switch_body_1_i = switch_body_1; } evaluate(): string { return `switch${this.parenthesized_expression_0_i.evaluate()}${this.switch_body_1_i.evaluate()}`; } }
export class Template_string_T extends SyntaticInterior { template_substitution_0_i : Template_substitution_T | undefined;
 constructor(template_substitution_0 : Template_substitution_T | undefined) { super();this.template_substitution_0_i = template_substitution_0; } evaluate(): string { return "`" + ("") + "`"; } }
export class Template_substitution_T extends SyntaticInterior { _expressions_0_i : _expressions_T;
 constructor(_expressions_0 : _expressions_T) { super();this._expressions_0_i = _expressions_0; } evaluate(): string { return "${" + this._expressions_0_i.evaluate() + "}"; } }
export class Ternary_expression_T extends SyntaticInterior { expression_0_i : Expression_T;
 expression_1_i : Expression_T;
 expression_2_i : Expression_T;
 constructor(expression_0 : Expression_T,expression_1 : Expression_T,expression_2 : Expression_T) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1;this.expression_2_i = expression_2; } evaluate(): string { return `${this.expression_0_i.evaluate()}${this.expression_1_i.evaluate()}:${this.expression_2_i.evaluate()}`; } }
export class This_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Throw_statement_T extends SyntaticInterior { _expressions_0_i : _expressions_T;
 _semicolon_1_i : _semicolon_T;
 constructor(_expressions_0 : _expressions_T,_semicolon_1 : _semicolon_T) { super();this._expressions_0_i = _expressions_0;this._semicolon_1_i = _semicolon_1; } evaluate(): string { return `throw${this._expressions_0_i.evaluate()}${this._semicolon_1_i.evaluate()}`; } }
export class True_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Try_statement_T extends SyntaticInterior { statement_block_0_i : Statement_block_T;
 catch_clause_1_i : Catch_clause_T | undefined;
 finally_clause_2_i : Finally_clause_T | undefined;
 constructor(statement_block_0 : Statement_block_T,catch_clause_1 : Catch_clause_T | undefined,finally_clause_2 : Finally_clause_T | undefined) { super();this.statement_block_0_i = statement_block_0;this.catch_clause_1_i = catch_clause_1;this.finally_clause_2_i = finally_clause_2; } evaluate(): string { return `try${this.statement_block_0_i.evaluate()}${(this.catch_clause_1_i !== undefined ? this.catch_clause_1_i.evaluate() : "")}${(this.finally_clause_2_i !== undefined ? this.finally_clause_2_i.evaluate() : "")}`; } }
export class Unary_expression_T extends SyntaticInterior { expression_0_i : Expression_T;
 constructor(expression_0 : Expression_T) { super();this.expression_0_i = expression_0; } evaluate(): string { return `${("!")}${this.expression_0_i.evaluate()}`; } }
export class Undefined_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Unescaped_double_jsx_string_fragment_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Unescaped_double_string_fragment_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Unescaped_single_jsx_string_fragment_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Unescaped_single_string_fragment_T extends SyntaticLeaf {  constructor(value : string) { super(value); } }
export class Update_expression_T extends SyntaticInterior { expression_0_i : Expression_T | undefined;
 expression_1_i : Expression_T | undefined;
 constructor(expression_0 : Expression_T | undefined,expression_1 : Expression_T | undefined) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { if (this.expression_0_i !== undefined) { return `${this.expression_0_i.evaluate()}${("++")}`; } if (this.expression_1_i !== undefined) { return `${("++")}${this.expression_1_i.evaluate()}`; } throw new Error("No alternative matched in CHOICE node"); } }
export class Using_declaration_T extends SyntaticInterior { variable_declarator_0_i : Variable_declarator_T;
 variable_declarator_1_i : Variable_declarator_T;
 _semicolon_2_i : _semicolon_T;
 constructor(variable_declarator_0 : Variable_declarator_T,variable_declarator_1 : Variable_declarator_T,_semicolon_2 : _semicolon_T) { super();this.variable_declarator_0_i = variable_declarator_0;this.variable_declarator_1_i = variable_declarator_1;this._semicolon_2_i = _semicolon_2; } evaluate(): string { return `${("using")}${`${this.variable_declarator_0_i.evaluate()}${`,${this.variable_declarator_1_i.evaluate()}`}`}${this._semicolon_2_i.evaluate()}`; } }
export class Variable_declaration_T extends SyntaticInterior { variable_declarator_0_i : Variable_declarator_T;
 variable_declarator_1_i : Variable_declarator_T;
 _semicolon_2_i : _semicolon_T;
 constructor(variable_declarator_0 : Variable_declarator_T,variable_declarator_1 : Variable_declarator_T,_semicolon_2 : _semicolon_T) { super();this.variable_declarator_0_i = variable_declarator_0;this.variable_declarator_1_i = variable_declarator_1;this._semicolon_2_i = _semicolon_2; } evaluate(): string { return `var${`${this.variable_declarator_0_i.evaluate()}${`,${this.variable_declarator_1_i.evaluate()}`}`}${this._semicolon_2_i.evaluate()}`; } }
export class Variable_declarator_T extends SyntaticInterior { identifier_0_i : Identifier_T | undefined;
 _destructuring_pattern_1_i : _destructuring_pattern_T | undefined;
 _initializer_2_i : _initializer_T | undefined;
 constructor(identifier_0 : Identifier_T | undefined,_destructuring_pattern_1 : _destructuring_pattern_T | undefined,_initializer_2 : _initializer_T | undefined) { super();this.identifier_0_i = identifier_0;this._destructuring_pattern_1_i = _destructuring_pattern_1;this._initializer_2_i = _initializer_2; } evaluate(): string { return `${(this.identifier_0_i !== undefined ? this.identifier_0_i.evaluate() : "of")}${(this._initializer_2_i !== undefined ? this._initializer_2_i.evaluate() : "")}`; } }
export class While_statement_T extends SyntaticInterior { parenthesized_expression_0_i : Parenthesized_expression_T;
 statement_1_i : Statement_T;
 constructor(parenthesized_expression_0 : Parenthesized_expression_T,statement_1 : Statement_T) { super();this.parenthesized_expression_0_i = parenthesized_expression_0;this.statement_1_i = statement_1; } evaluate(): string { return `while${this.parenthesized_expression_0_i.evaluate()}${this.statement_1_i.evaluate()}`; } }
export class With_statement_T extends SyntaticInterior { parenthesized_expression_0_i : Parenthesized_expression_T;
 statement_1_i : Statement_T;
 constructor(parenthesized_expression_0 : Parenthesized_expression_T,statement_1 : Statement_T) { super();this.parenthesized_expression_0_i = parenthesized_expression_0;this.statement_1_i = statement_1; } evaluate(): string { return `with${this.parenthesized_expression_0_i.evaluate()}${this.statement_1_i.evaluate()}`; } }
export class Yield_expression_T extends SyntaticInterior { expression_0_i : Expression_T | undefined;
 expression_1_i : Expression_T | undefined;
 constructor(expression_0 : Expression_T | undefined,expression_1 : Expression_T | undefined) { super();this.expression_0_i = expression_0;this.expression_1_i = expression_1; } evaluate(): string { return `yield${(this.expression_0_i !== undefined ? `*${this.expression_0_i.evaluate()}` : this.expression_1_i !== undefined ? this.expression_1_i.evaluate() : (() => { throw new Error("No alternative matched in CHOICE node"); })())}`; } }
