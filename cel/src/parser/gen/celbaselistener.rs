// Generated from CEL.g4 by ANTLR 4.13.2

use super::celparser::*;
use antlr4rust::tree::ParseTreeListener;

// A complete Visitor for a parse tree produced by CELParser.

pub trait CELBaseListener<'input>:
    ParseTreeListener<'input, CELParserContextType> {

    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_start(&mut self, _ctx: &StartContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_start(&mut self, _ctx: &StartContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_expr(&mut self, _ctx: &ExprContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_expr(&mut self, _ctx: &ExprContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_conditionalor(&mut self, _ctx: &ConditionalOrContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_conditionalor(&mut self, _ctx: &ConditionalOrContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_conditionaland(&mut self, _ctx: &ConditionalAndContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_conditionaland(&mut self, _ctx: &ConditionalAndContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_relation(&mut self, _ctx: &RelationContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_relation(&mut self, _ctx: &RelationContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_calc(&mut self, _ctx: &CalcContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_calc(&mut self, _ctx: &CalcContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_memberexpr(&mut self, _ctx: &MemberExprContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_memberexpr(&mut self, _ctx: &MemberExprContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_logicalnot(&mut self, _ctx: &LogicalNotContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_logicalnot(&mut self, _ctx: &LogicalNotContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_negate(&mut self, _ctx: &NegateContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_negate(&mut self, _ctx: &NegateContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_membercall(&mut self, _ctx: &MemberCallContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_membercall(&mut self, _ctx: &MemberCallContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_select(&mut self, _ctx: &SelectContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_select(&mut self, _ctx: &SelectContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_primaryexpr(&mut self, _ctx: &PrimaryExprContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_primaryexpr(&mut self, _ctx: &PrimaryExprContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_index(&mut self, _ctx: &IndexContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_index(&mut self, _ctx: &IndexContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_ident(&mut self, _ctx: &IdentContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_ident(&mut self, _ctx: &IdentContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_globalcall(&mut self, _ctx: &GlobalCallContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_globalcall(&mut self, _ctx: &GlobalCallContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_nested(&mut self, _ctx: &NestedContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_nested(&mut self, _ctx: &NestedContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_createlist(&mut self, _ctx: &CreateListContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_createlist(&mut self, _ctx: &CreateListContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_listcomprehension(&mut self, _ctx: &ListComprehensionContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_listcomprehension(&mut self, _ctx: &ListComprehensionContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_createstruct(&mut self, _ctx: &CreateStructContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_createstruct(&mut self, _ctx: &CreateStructContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_createmessage(&mut self, _ctx: &CreateMessageContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_createmessage(&mut self, _ctx: &CreateMessageContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_constantliteral(&mut self, _ctx: &ConstantLiteralContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_constantliteral(&mut self, _ctx: &ConstantLiteralContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_exprlist(&mut self, _ctx: &ExprListContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_exprlist(&mut self, _ctx: &ExprListContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_listinit(&mut self, _ctx: &ListInitContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_listinit(&mut self, _ctx: &ListInitContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_field_initializer_list(&mut self, _ctx: &Field_initializer_listContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_field_initializer_list(&mut self, _ctx: &Field_initializer_listContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_optfield(&mut self, _ctx: &OptFieldContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_optfield(&mut self, _ctx: &OptFieldContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_mapinitializerlist(&mut self, _ctx: &MapInitializerListContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_mapinitializerlist(&mut self, _ctx: &MapInitializerListContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_simpleidentifier(&mut self, _ctx: &SimpleIdentifierContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_simpleidentifier(&mut self, _ctx: &SimpleIdentifierContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_escapedidentifier(&mut self, _ctx: &EscapedIdentifierContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_escapedidentifier(&mut self, _ctx: &EscapedIdentifierContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_optexpr(&mut self, _ctx: &OptExprContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_optexpr(&mut self, _ctx: &OptExprContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_int(&mut self, _ctx: &IntContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_int(&mut self, _ctx: &IntContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_uint(&mut self, _ctx: &UintContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_uint(&mut self, _ctx: &UintContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_double(&mut self, _ctx: &DoubleContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_double(&mut self, _ctx: &DoubleContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_string(&mut self, _ctx: &StringContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_string(&mut self, _ctx: &StringContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_bytes(&mut self, _ctx: &BytesContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_bytes(&mut self, _ctx: &BytesContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_booltrue(&mut self, _ctx: &BoolTrueContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_booltrue(&mut self, _ctx: &BoolTrueContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_boolfalse(&mut self, _ctx: &BoolFalseContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_boolfalse(&mut self, _ctx: &BoolFalseContext<'input>) {}


    /**
     * Enter a parse tree produced by \{@link CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn enter_null(&mut self, _ctx: &NullContext<'input>) {}
    /**
     * Exit a parse tree produced by \{@link  CELBaseParser#s}.
     * @param ctx the parse tree
     */
    fn exit_null(&mut self, _ctx: &NullContext<'input>) {}


}