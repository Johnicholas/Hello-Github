expression = {}
expression = Or {
  left = And {
    left = Singleton { s = "Assign[" },
    right = And {
      left = Singleton { s = "exp" },
      right = And {
	left = Singleton { s = ",_" },
	right = And {
	  left = Singleton { s = "exp" },
	  right = Singleton { s = "]" }
	}
      }
    }
  },
  right = Singleton { s= "DEBUG" }
}
--[[
expression ::= Assign[ expression ,_ expression ] .
expression ::= Conditional[ expression ,_ expression ,_ expression ] .
operator_name ::= Or .
operator_name ::= And .
operator_name ::= BitwiseOr .
operator_name ::= BitwiseXOr .
operator_name ::= BitwiseAnd .
operator_name ::= NotEquals .
operator_name ::= Equals .
operator_name ::= GreaterThanOrEqual .
operator_name ::= LessThanOrEqual .
operator_name ::= GreaterThan .
operator_name ::= LessThan .
operator_name ::= RightShift .
operator_name ::= LeftShift .
operator_name ::= Minus .
operator_name ::= Plus .
operator_name ::= Mod .
operator_name ::= Divide .
operator_name ::= Times .
list_of_expression ::= expression ,_ list_of_expression .
list_of_expression ::= expression .
expression ::= Operator[ operator_name ,_ { list_of_expression } ] .
expression ::= Cast[ type_name ,_ expression ] .
expression ::= Sizeof[ type_name ] .
expression ::= Sizeof[ expression ] .
expression ::= Dereference[ expression ] .
expression ::= Address[ expression ] .
expression ::= PointerMember[ expression ,_ ID ] .
expression ::= Member[ expression ,_ ID ] .
argument_expression_list ::= expression ,_ argument_expression_list .
argument_expression_list ::= expression .
expression ::= Call[ expression ,_ argument_expression_list ] .
expression ::= Call[ expression ,_ {} ] .
expression ::= Array[ expression ,_ expression ] .
expression ::= Parentheses[ expression ] .
expression ::= Constant[ CONSTANT ] .
expression ::= ID .
statement ::= Return[ expression ] .
statement ::= Return[] .
statement ::= Break[] .
statement ::= Continue[] .
statement ::= Goto[ ID ] .
statement ::= Label[ ID ] .
#comment ::= This is a bit wrong, labels are really prefixes to statements .
statement ::= For[ expression ,_ expression ,_ expression ,_ statement ] .
statement ::= Do[ statement ,_ expression ] .
statement ::= While[ expression ,_ statement ] .
#statement ::= DEFAULT_ COLON_ statement .
#statement ::= CASE_ expression COLON_ statement .
#statement ::= Switch[ expression ,_ statement ] .
#comment ::= Switch syntax is fucked up.
statement ::= If[ expression ,_ statement ,_ statement ] .
statement ::= If[ expression ,_ statement ] .
block_item ::= statement .
block_item ::= declaration .
block_item_list ::= block_item ,_ block_item_list .
block_item_list ::= block_item .
statement ::= Block[ { block_item_list } ] .
statement ::= Block[ { } ] .
type ::= Union[ None ,_ list_of_fields ] .
type ::= Union[ ID ,_ list_of_fields ] .
type ::= Union[ ID ] .
type ::= Struct[ None ,_ list_of_fields ] .
type ::= Struct[ ID ,_ list_of_fields ] .
type ::= Struct[ ID ] .
nonempty_list_of_identifiers ::= ID ,_ nonempty_list_of_identifiers .
nonempty_list_of_identifiers ::= ID .
list_of_identifiers ::= { nonempty_list_of_identifiers } .
list_of_identifiers ::= { } .
type ::= Enum[ list_of_identifiers ] .
type ::= Enum[ ID ,_ list_of_identifiers ] .
type ::= PointerType[ type ] .
type ::= Void .
type ::= Int .
declaration ::= Declare[ type ,_ ID ] .
nonempty_declaration_list ::= declaration_list ,_ declaration .
nonempty_declaration_list ::= declaration .
declaration_list ::= { nonempty_declaration_list } .
declaration_list ::= {} .
external_declaration ::= Function[ type ,_ ID ,_ declaration_list ,_ statement ] .
external_declaration ::= Function[ type ,_ ID ,_ declaration_list ] .
#comment ::= You can forward-declare functions .
#external_declaration ::= declaration .
#comment ::= Globals are grammatically possible but not good style .
external_declaration ::= Typedef[ type ,_ ID ] .
external_declaration ::= Include[ ID ] .
nonempty_list_of_external_declaration ::= external_declaration ,_ nonempty_list_of_external_declaration .
nonempty_list_of_external_declaration ::= external_declaration .
start ::= Program[ { nonempty_list_of_external_declaration } ] .
]]

