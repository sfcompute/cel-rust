
// Generated from CEL.g4 by ANTLR 4.13.2

use antlr4rust::tree::ParseTreeVisitor;
use super::celparser::*;

// A complete Visitor for a parse tree produced by CELParser.

pub trait CELBaseVisitor<'input>:
    ParseTreeVisitor<'input, CELParserContextType> {
	// Visit a parse tree produced by CELParser#start.
	fn visit_start(&mut self, ctx: &StartContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#expr.
	fn visit_expr(&mut self, ctx: &ExprContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#conditionalOr.
	fn visit_conditionalor(&mut self, ctx: &ConditionalOrContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#conditionalAnd.
	fn visit_conditionaland(&mut self, ctx: &ConditionalAndContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#relation.
	fn visit_relation(&mut self, ctx: &RelationContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#calc.
	fn visit_calc(&mut self, ctx: &CalcContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#MemberExpr.
	fn visit_memberexpr(&mut self, ctx: &MemberExprContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#LogicalNot.
	fn visit_logicalnot(&mut self, ctx: &LogicalNotContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#Negate.
	fn visit_negate(&mut self, ctx: &NegateContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#MemberCall.
	fn visit_membercall(&mut self, ctx: &MemberCallContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#Select.
	fn visit_select(&mut self, ctx: &SelectContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#PrimaryExpr.
	fn visit_primaryexpr(&mut self, ctx: &PrimaryExprContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#Index.
	fn visit_index(&mut self, ctx: &IndexContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#Ident.
	fn visit_ident(&mut self, ctx: &IdentContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#GlobalCall.
	fn visit_globalcall(&mut self, ctx: &GlobalCallContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#Nested.
	fn visit_nested(&mut self, ctx: &NestedContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#CreateList.
	fn visit_createlist(&mut self, ctx: &CreateListContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#ListComprehension.
	fn visit_listcomprehension(&mut self, ctx: &ListComprehensionContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#CreateStruct.
	fn visit_createstruct(&mut self, ctx: &CreateStructContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#CreateMessage.
	fn visit_createmessage(&mut self, ctx: &CreateMessageContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#ConstantLiteral.
	fn visit_constantliteral(&mut self, ctx: &ConstantLiteralContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#exprList.
	fn visit_exprlist(&mut self, ctx: &ExprListContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#listInit.
	fn visit_listinit(&mut self, ctx: &ListInitContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#field_initializer_list.
	fn visit_field_initializer_list(&mut self, ctx: &Field_initializer_listContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#optField.
	fn visit_optfield(&mut self, ctx: &OptFieldContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#mapInitializerList.
	fn visit_mapinitializerlist(&mut self, ctx: &MapInitializerListContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#SimpleIdentifier.
	fn visit_simpleidentifier(&mut self, ctx: &SimpleIdentifierContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#EscapedIdentifier.
	fn visit_escapedidentifier(&mut self, ctx: &EscapedIdentifierContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#optExpr.
	fn visit_optexpr(&mut self, ctx: &OptExprContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#Int.
	fn visit_int(&mut self, ctx: &IntContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#Uint.
	fn visit_uint(&mut self, ctx: &UintContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#Double.
	fn visit_double(&mut self, ctx: &DoubleContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#String.
	fn visit_string(&mut self, ctx: &StringContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#Bytes.
	fn visit_bytes(&mut self, ctx: &BytesContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#BoolTrue.
	fn visit_booltrue(&mut self, ctx: &BoolTrueContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#BoolFalse.
	fn visit_boolfalse(&mut self, ctx: &BoolFalseContext<'input>) {
            self.visit_children(ctx)
        }

	// Visit a parse tree produced by CELParser#Null.
	fn visit_null(&mut self, ctx: &NullContext<'input>) {
            self.visit_children(ctx)
        }

}