// Generated from C:/Users/Taha/Desktop/compiler_p3/src/antlr\acton.g4 by ANTLR 4.7.2
package antlr;

    import main.ast.node.*;
    import main.ast.node.declaration.*;
    import main.ast.node.declaration.handler.*;
    import main.ast.node.statement.*;
    import main.ast.node.expression.*;
    import main.ast.node.expression.operators.*;
    import main.ast.node.expression.values.*;
    import main.ast.type.primitiveType.*;
    import main.ast.type.arrayType.*;
    import main.ast.type.actorType.*;
    import main.ast.type.*;

import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link actonParser}.
 */
public interface actonListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link actonParser#program}.
	 * @param ctx the parse tree
	 */
	void enterProgram(actonParser.ProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#program}.
	 * @param ctx the parse tree
	 */
	void exitProgram(actonParser.ProgramContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#actorDeclaration}.
	 * @param ctx the parse tree
	 */
	void enterActorDeclaration(actonParser.ActorDeclarationContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#actorDeclaration}.
	 * @param ctx the parse tree
	 */
	void exitActorDeclaration(actonParser.ActorDeclarationContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#mainDeclaration}.
	 * @param ctx the parse tree
	 */
	void enterMainDeclaration(actonParser.MainDeclarationContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#mainDeclaration}.
	 * @param ctx the parse tree
	 */
	void exitMainDeclaration(actonParser.MainDeclarationContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#actorInstantiation}.
	 * @param ctx the parse tree
	 */
	void enterActorInstantiation(actonParser.ActorInstantiationContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#actorInstantiation}.
	 * @param ctx the parse tree
	 */
	void exitActorInstantiation(actonParser.ActorInstantiationContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#initHandlerDeclaration}.
	 * @param ctx the parse tree
	 */
	void enterInitHandlerDeclaration(actonParser.InitHandlerDeclarationContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#initHandlerDeclaration}.
	 * @param ctx the parse tree
	 */
	void exitInitHandlerDeclaration(actonParser.InitHandlerDeclarationContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#msgHandlerDeclaration}.
	 * @param ctx the parse tree
	 */
	void enterMsgHandlerDeclaration(actonParser.MsgHandlerDeclarationContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#msgHandlerDeclaration}.
	 * @param ctx the parse tree
	 */
	void exitMsgHandlerDeclaration(actonParser.MsgHandlerDeclarationContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#argDeclarations}.
	 * @param ctx the parse tree
	 */
	void enterArgDeclarations(actonParser.ArgDeclarationsContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#argDeclarations}.
	 * @param ctx the parse tree
	 */
	void exitArgDeclarations(actonParser.ArgDeclarationsContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#varDeclarations}.
	 * @param ctx the parse tree
	 */
	void enterVarDeclarations(actonParser.VarDeclarationsContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#varDeclarations}.
	 * @param ctx the parse tree
	 */
	void exitVarDeclarations(actonParser.VarDeclarationsContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#varDeclaration}.
	 * @param ctx the parse tree
	 */
	void enterVarDeclaration(actonParser.VarDeclarationContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#varDeclaration}.
	 * @param ctx the parse tree
	 */
	void exitVarDeclaration(actonParser.VarDeclarationContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#statement}.
	 * @param ctx the parse tree
	 */
	void enterStatement(actonParser.StatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#statement}.
	 * @param ctx the parse tree
	 */
	void exitStatement(actonParser.StatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#blockStmt}.
	 * @param ctx the parse tree
	 */
	void enterBlockStmt(actonParser.BlockStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#blockStmt}.
	 * @param ctx the parse tree
	 */
	void exitBlockStmt(actonParser.BlockStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#printStmt}.
	 * @param ctx the parse tree
	 */
	void enterPrintStmt(actonParser.PrintStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#printStmt}.
	 * @param ctx the parse tree
	 */
	void exitPrintStmt(actonParser.PrintStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#assignStmt}.
	 * @param ctx the parse tree
	 */
	void enterAssignStmt(actonParser.AssignStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#assignStmt}.
	 * @param ctx the parse tree
	 */
	void exitAssignStmt(actonParser.AssignStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#assignment}.
	 * @param ctx the parse tree
	 */
	void enterAssignment(actonParser.AssignmentContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#assignment}.
	 * @param ctx the parse tree
	 */
	void exitAssignment(actonParser.AssignmentContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#forStmt}.
	 * @param ctx the parse tree
	 */
	void enterForStmt(actonParser.ForStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#forStmt}.
	 * @param ctx the parse tree
	 */
	void exitForStmt(actonParser.ForStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#ifStmt}.
	 * @param ctx the parse tree
	 */
	void enterIfStmt(actonParser.IfStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#ifStmt}.
	 * @param ctx the parse tree
	 */
	void exitIfStmt(actonParser.IfStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#continueStmt}.
	 * @param ctx the parse tree
	 */
	void enterContinueStmt(actonParser.ContinueStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#continueStmt}.
	 * @param ctx the parse tree
	 */
	void exitContinueStmt(actonParser.ContinueStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#breakStmt}.
	 * @param ctx the parse tree
	 */
	void enterBreakStmt(actonParser.BreakStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#breakStmt}.
	 * @param ctx the parse tree
	 */
	void exitBreakStmt(actonParser.BreakStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#msgHandlerCall}.
	 * @param ctx the parse tree
	 */
	void enterMsgHandlerCall(actonParser.MsgHandlerCallContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#msgHandlerCall}.
	 * @param ctx the parse tree
	 */
	void exitMsgHandlerCall(actonParser.MsgHandlerCallContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterExpression(actonParser.ExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitExpression(actonParser.ExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#orExpression}.
	 * @param ctx the parse tree
	 */
	void enterOrExpression(actonParser.OrExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#orExpression}.
	 * @param ctx the parse tree
	 */
	void exitOrExpression(actonParser.OrExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#andExpression}.
	 * @param ctx the parse tree
	 */
	void enterAndExpression(actonParser.AndExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#andExpression}.
	 * @param ctx the parse tree
	 */
	void exitAndExpression(actonParser.AndExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#equalityExpression}.
	 * @param ctx the parse tree
	 */
	void enterEqualityExpression(actonParser.EqualityExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#equalityExpression}.
	 * @param ctx the parse tree
	 */
	void exitEqualityExpression(actonParser.EqualityExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#relationalExpression}.
	 * @param ctx the parse tree
	 */
	void enterRelationalExpression(actonParser.RelationalExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#relationalExpression}.
	 * @param ctx the parse tree
	 */
	void exitRelationalExpression(actonParser.RelationalExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#additiveExpression}.
	 * @param ctx the parse tree
	 */
	void enterAdditiveExpression(actonParser.AdditiveExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#additiveExpression}.
	 * @param ctx the parse tree
	 */
	void exitAdditiveExpression(actonParser.AdditiveExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#multiplicativeExpression}.
	 * @param ctx the parse tree
	 */
	void enterMultiplicativeExpression(actonParser.MultiplicativeExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#multiplicativeExpression}.
	 * @param ctx the parse tree
	 */
	void exitMultiplicativeExpression(actonParser.MultiplicativeExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#preUnaryExpression}.
	 * @param ctx the parse tree
	 */
	void enterPreUnaryExpression(actonParser.PreUnaryExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#preUnaryExpression}.
	 * @param ctx the parse tree
	 */
	void exitPreUnaryExpression(actonParser.PreUnaryExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#postUnaryExpression}.
	 * @param ctx the parse tree
	 */
	void enterPostUnaryExpression(actonParser.PostUnaryExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#postUnaryExpression}.
	 * @param ctx the parse tree
	 */
	void exitPostUnaryExpression(actonParser.PostUnaryExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#postUnaryOp}.
	 * @param ctx the parse tree
	 */
	void enterPostUnaryOp(actonParser.PostUnaryOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#postUnaryOp}.
	 * @param ctx the parse tree
	 */
	void exitPostUnaryOp(actonParser.PostUnaryOpContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#otherExpression}.
	 * @param ctx the parse tree
	 */
	void enterOtherExpression(actonParser.OtherExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#otherExpression}.
	 * @param ctx the parse tree
	 */
	void exitOtherExpression(actonParser.OtherExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#arrayCall}.
	 * @param ctx the parse tree
	 */
	void enterArrayCall(actonParser.ArrayCallContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#arrayCall}.
	 * @param ctx the parse tree
	 */
	void exitArrayCall(actonParser.ArrayCallContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#actorVarAccess}.
	 * @param ctx the parse tree
	 */
	void enterActorVarAccess(actonParser.ActorVarAccessContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#actorVarAccess}.
	 * @param ctx the parse tree
	 */
	void exitActorVarAccess(actonParser.ActorVarAccessContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#expressionList}.
	 * @param ctx the parse tree
	 */
	void enterExpressionList(actonParser.ExpressionListContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#expressionList}.
	 * @param ctx the parse tree
	 */
	void exitExpressionList(actonParser.ExpressionListContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#identifier}.
	 * @param ctx the parse tree
	 */
	void enterIdentifier(actonParser.IdentifierContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#identifier}.
	 * @param ctx the parse tree
	 */
	void exitIdentifier(actonParser.IdentifierContext ctx);
	/**
	 * Enter a parse tree produced by {@link actonParser#value}.
	 * @param ctx the parse tree
	 */
	void enterValue(actonParser.ValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link actonParser#value}.
	 * @param ctx the parse tree
	 */
	void exitValue(actonParser.ValueContext ctx);
}