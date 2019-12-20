package main.visitor;

import main.ast.node.Main;
import main.ast.node.Program;
import main.ast.node.declaration.ActorDeclaration;
import main.ast.node.declaration.ActorInstantiation;
import main.ast.node.declaration.VarDeclaration;
import main.ast.node.declaration.handler.HandlerDeclaration;
import main.ast.node.declaration.handler.MsgHandlerDeclaration;
import main.ast.node.expression.*;
import main.ast.node.expression.operators.UnaryOperator;
import main.ast.node.expression.values.BooleanValue;
import main.ast.node.expression.values.IntValue;
import main.ast.node.expression.values.StringValue;
import main.ast.node.expression.values.Value;
import main.ast.node.statement.*;
import main.ast.type.Type;
import main.ast.type.actorType.ActorType;
import main.ast.type.arrayType.ArrayType;
import main.ast.type.noType.NoType;
import main.ast.type.primitiveType.BooleanType;
import main.ast.type.primitiveType.IntType;
import main.ast.type.primitiveType.StringType;
import main.symbolTable.SymbolTable;
import main.symbolTable.SymbolTableActorItem;
import main.symbolTable.SymbolTableHandlerItem;
import main.symbolTable.SymbolTableMainItem;
import main.symbolTable.itemException.ItemAlreadyExistsException;
import main.symbolTable.itemException.ItemNotFoundException;
import main.symbolTable.symbolTableVariableItem.SymbolTableActorVariableItem;
import main.symbolTable.symbolTableVariableItem.SymbolTableKnownActorItem;
import main.symbolTable.symbolTableVariableItem.SymbolTableLocalVariableItem;
import main.symbolTable.symbolTableVariableItem.SymbolTableVariableItem;
import org.antlr.v4.gui.SystemFontMetrics;

import java.util.ArrayList;

public class typeCheckerImpl implements Visitor {
    String errorCont;
    SymbolTable currentSymbolTable =  null;
    SymbolTable currentActorTable =  null;
    int inLoop = 0;
    Boolean inMain = false;
    Boolean inInitial = false;
    protected void visitStatement( Statement stat )
    {
        if( stat == null )
            return;
        else if( stat instanceof MsgHandlerCall)
            this.visit( ( MsgHandlerCall ) stat );
        else if( stat instanceof Block)
            this.visit( ( Block ) stat );
        else if( stat instanceof Conditional)
            this.visit( ( Conditional ) stat );
        else if( stat instanceof For)
            this.visit( ( For ) stat );
        else if( stat instanceof Break )
            this.visit( ( Break ) stat );
        else if( stat instanceof Continue )
            this.visit( ( Continue ) stat );
        else if( stat instanceof Print )
            this.visit( ( Print ) stat );
        else if( stat instanceof Assign )
            this.visit( ( Assign ) stat );
    }

    protected void visitExpr( Expression expr )
    {
        if( expr == null )
            return;
        else if( expr instanceof UnaryExpression)
            this.visit( ( UnaryExpression ) expr );
        else if( expr instanceof BinaryExpression)
            this.visit( ( BinaryExpression ) expr );
        else if( expr instanceof ArrayCall)
            this.visit( ( ArrayCall ) expr );
        else if( expr instanceof ActorVarAccess)
            this.visit( ( ActorVarAccess ) expr );
        else if( expr instanceof Identifier )
            this.visit( ( Identifier ) expr );
        else if( expr instanceof Self )
            this.visit( ( Self ) expr );
        else if( expr instanceof Sender )
            this.visit( ( Sender ) expr );
        else if( expr instanceof BooleanValue)
            this.visit( ( BooleanValue ) expr );
        else if( expr instanceof IntValue)
            this.visit( ( IntValue ) expr );
        else if( expr instanceof StringValue)
            this.visit( ( StringValue ) expr );
    }

    @Override
    public void visit(Program program) {
        if (program.getActors() != null) {
            for (ActorDeclaration actorDeclaration : program.getActors()) {
                actorDeclaration.accept(this);
            }
        }

        if (program.getMain() != null)
            program.getMain().accept(this);
    }

    @Override
    public void visit(ActorDeclaration actorDeclaration) {
        SymbolTableActorItem newSymbolActorItem = null;
        try {
            newSymbolActorItem = (SymbolTableActorItem)(SymbolTable.root.get(SymbolTableActorItem.STARTKEY + actorDeclaration.getName().getName()));
        }
        catch (ItemNotFoundException e) { }
        if (newSymbolActorItem != null)
            currentActorTable = newSymbolActorItem.getActorSymbolTable();


        if (actorDeclaration.getParentName() != null) {
            try {
                currentActorTable.get(SymbolTableActorItem.STARTKEY + actorDeclaration.getParentName().getName());
            }
            catch (ItemNotFoundException e) {
                errorCont = "Line:" + actorDeclaration.getLine() + ":" + "actor " + actorDeclaration.getParentName().getName().toString() + " is not declared";
                errors.put(errorCont, actorDeclaration.getLine());
            }
        }

        if (actorDeclaration.getKnownActors() != null) {
            for (VarDeclaration varDeclaration : actorDeclaration.getKnownActors()) {
                try {
                    currentActorTable.get(SymbolTableActorItem.STARTKEY + varDeclaration.getType().toString());
                }
                catch (ItemNotFoundException e) {
                    errorCont = "Line:" + varDeclaration.getLine() + ":" + "actor " + varDeclaration.getType().toString() + " is not declared";
                    errors.put(errorCont, varDeclaration.getLine());
                }

            }
        }

        if (actorDeclaration.getActorVars() != null) {
            for (VarDeclaration varDeclaration : actorDeclaration.getActorVars()) {
                varDeclaration.accept(this);
            }
        }

        if (actorDeclaration.getInitHandler() != null) {
            inInitial = true;
            actorDeclaration.getInitHandler().accept(this);
            inInitial = false;
        }

        if (actorDeclaration.getMsgHandlers() != null) {
            for (MsgHandlerDeclaration msgHandlerDeclaration : actorDeclaration.getMsgHandlers()) {
                msgHandlerDeclaration.accept(this);
            }
        }

    }

    @Override
    public void visit(HandlerDeclaration handlerDeclaration) {
        SymbolTableHandlerItem newSymbolHandleItem = null;
        try {
            newSymbolHandleItem = (SymbolTableHandlerItem)(currentActorTable.get(SymbolTableHandlerItem.STARTKEY + handlerDeclaration.getName().getName()));
        }
        catch (ItemNotFoundException e) { }
        if (newSymbolHandleItem != null)
            currentSymbolTable = newSymbolHandleItem.getHandlerSymbolTable();

        if (handlerDeclaration.getBody() != null) {
            for (Statement statement : handlerDeclaration.getBody()) {
                if (statement != null)
                    statement.accept(this);
            }
        }
    }

    @Override
    public void visit(VarDeclaration varDeclaration) {
    }

    @Override
    public void visit(Main mainActors) {
        SymbolTableMainItem currentSymbolMainItem = null;
        inMain = true;
        try {
            currentSymbolMainItem = (SymbolTableMainItem)(SymbolTable.root.get(SymbolTableMainItem.STARTKEY + "main"));
        }
        catch (ItemNotFoundException e) { }
        if (currentSymbolMainItem != null)
            currentSymbolTable = currentSymbolMainItem.getMainSymbolTable();

        if (mainActors.getMainActors() != null) {
            for (ActorInstantiation actorInstantiation : mainActors.getMainActors()) {
                actorInstantiation.accept(this);
            }
        }
        inMain = false;
    }

    @Override
    public void visit(ActorInstantiation actorInstantiation) {
        SymbolTableActorItem currentActorItem = null;
        ActorDeclaration currentActorDec = null;
        SymbolTableVariableItem currentKnownActor = null;
        ArrayList<VarDeclaration> knownActors;
        if (actorInstantiation.getIdentifier() != null) {

            try {
                currentActorItem = (SymbolTableActorItem)currentSymbolTable.get(SymbolTableActorItem.STARTKEY + actorInstantiation.getType().toString());
            }
            catch (ItemNotFoundException e) {
                errorCont = "Line:" + actorInstantiation.getLine() + ":" + "actor " + actorInstantiation.getType().toString() + " is not declared";
                errors.put(errorCont, actorInstantiation.getLine());
            }
        }

        if (currentActorItem != null)
            currentActorDec = currentActorItem.getActorDeclaration();

        if (currentActorDec != null) {
            knownActors = currentActorDec.getKnownActors();
            if (actorInstantiation.getKnownActors().size() != knownActors.size()) {
                errorCont = "Line:" + actorInstantiation.getLine() + ":" + "knownactors does not match with definition";
                errors.put(errorCont, actorInstantiation.getLine());
            }
            else {
                int knownActorIndex = 0;
                Boolean errorInKnownActor = false;
                if (actorInstantiation.getKnownActors() != null) {
                    for (Identifier identifier : actorInstantiation.getKnownActors()) {
                        try {
                            currentKnownActor = (SymbolTableVariableItem) (currentSymbolTable.get(SymbolTableVariableItem.STARTKEY + identifier.getName()));
                        }
                        catch (ItemNotFoundException e) {
                            errorInKnownActor = true;
                        }

                        if (currentKnownActor != null)
                            if (!currentKnownActor.getVarDeclaration().getType().toString().equals(knownActors.get(knownActorIndex).getType().toString())) {
                                errorInKnownActor = true;
                            }
                        knownActorIndex++;
                    }
                    if (errorInKnownActor) {
                        errorCont = "Line:" + actorInstantiation.getLine() + ":" + "knownactors does not match with definition";
                        errors.put(errorCont, actorInstantiation.getLine());
                    }

                }
            }
        }
        if (actorInstantiation.getKnownActors() != null) {
            for (Identifier identifier : actorInstantiation.getKnownActors()) {
                identifier.accept(this);
            }
        }

        if (currentActorDec != null) {
            if (currentActorDec.getInitHandler() == null) {
                if (actorInstantiation.getInitArgs().size() != 0) {
                    errorCont = "Line:" + actorInstantiation.getLine() + ":" + "arguments does not match with initial";
                    errors.put(errorCont, actorInstantiation.getLine());
                }
            }
            else if (currentActorDec.getInitHandler().getArgs().size() != actorInstantiation.getInitArgs().size()) {
                errorCont = "Line:" + actorInstantiation.getLine() + ":" + "arguments does not match with initial";
                errors.put(errorCont, actorInstantiation.getLine());
            }
            else {
                ArrayList<VarDeclaration> agrsInst = currentActorDec.getInitHandler().getArgs();
                Boolean errorInArgs = false;
                int argIndex = 0;
                if (actorInstantiation.getInitArgs() != null) {
                    for (Expression expression : actorInstantiation.getInitArgs()) {
                        if (expressionType(expression).toString() != ((VarDeclaration) agrsInst.get(argIndex)).getType().toString())
                            errorInArgs = true;
                        argIndex++;
                    }
                    if (errorInArgs) {
                        errorCont = "Line:" + actorInstantiation.getLine() + ":" + "arguments does not match with initial";
                        errors.put(errorCont, actorInstantiation.getLine());
                    }
                }
            }
        }

        if (actorInstantiation.getInitArgs() != null) {
            for (Expression expression : actorInstantiation.getInitArgs()) {
                expression.accept(this);
            }
        }
    }

    @Override
    public void visit(UnaryExpression unaryExpression) {
        if (unaryExpression.getOperand() != null) {
            unaryExpression.getOperand().accept(this);
            String operator = unaryExpression.getUnaryOperator().toString();
            if (operator == "preinc" || operator == "postinc" || operator == "predec" || operator == "postDec"){
                if (!(unaryExpression.getOperand() instanceof Identifier) && !(unaryExpression.getOperand() instanceof ActorVarAccess)) {
                    errorCont = "Line:" + unaryExpression.getLine() + ":" + "lvalue required as increment/decrement operand";
                    errors.put(errorCont, unaryExpression.getLine());
                }
            }
            Type expType = expressionType(unaryExpression);
            unaryExpression.setType(expType);
            if (expType instanceof NoType){
                errorCont = "Line:" + unaryExpression.getLine() + ":" + "unsupported operand type for " + unaryExpression.getUnaryOperator().toString();
                errors.put(errorCont, unaryExpression.getLine());
            }
        }
    }

    @Override
    public void visit(BinaryExpression binaryExpression) {
        if(binaryExpression.getLeft() != null)
            binaryExpression.getLeft().accept(this);

        if(binaryExpression.getRight() != null)
            binaryExpression.getRight().accept(this);

        Type expType = expressionType(binaryExpression);
        binaryExpression.setType(expType);
        if (expType instanceof NoType){
            errorCont = "Line:" + binaryExpression.getLine() + ":" + "unsupported operand type for " + binaryExpression.getBinaryOperator().toString();
            errors.put(errorCont, binaryExpression.getLine());
        }
        else{

        }


    }

    @Override
    public void visit(ArrayCall arrayCall) {
        SymbolTableVariableItem array = null;
        if (arrayCall.getArrayInstance() != null) {
            arrayCall.getArrayInstance().accept(this);
            try {
                if (arrayCall.getArrayInstance() instanceof ActorVarAccess) {
                    array = (SymbolTableVariableItem) currentActorTable.get(SymbolTableVariableItem.STARTKEY + ((ActorVarAccess)arrayCall.getArrayInstance()).getVariable().getName().toString());
                }
                else
                    array = (SymbolTableVariableItem) currentSymbolTable.get(SymbolTableVariableItem.STARTKEY + ((Identifier)arrayCall.getArrayInstance()).getName().toString());
            }
            catch (ItemNotFoundException e) {}
            if (array != null)
                if (!(array.getType() instanceof ArrayType)) {
                    errorCont = "Line:" + arrayCall.getLine() + ":" + "variable being indexed must be array";
                    errors.put(errorCont, arrayCall.getLine());
                }
        }
        if (!(expressionType(arrayCall.getIndex()) instanceof IntType)){
            errorCont = "Line:" + arrayCall.getLine() + ":" + "index must be integer";
            errors.put(errorCont, arrayCall.getLine());
        }
    }

    @Override
    public void visit(ActorVarAccess actorVarAccess) {

        if (actorVarAccess.getSelf() != null) {
            actorVarAccess.getSelf().setLine(actorVarAccess.getLine());
            actorVarAccess.getSelf().accept(this);
        }

        if (actorVarAccess.getVariable() != null) {
            SymbolTableVariableItem varDec = null;
            try {
                varDec = (SymbolTableVariableItem) (currentActorTable.get(SymbolTableVariableItem.STARTKEY + actorVarAccess.getVariable().getName()));
            }
            catch (ItemNotFoundException e) {
                errorCont = "Line:" + actorVarAccess.getLine() + ":" + "variable " + actorVarAccess.getVariable().getName().toString() + " is not declared";
                errors.put(errorCont, actorVarAccess.getLine());
            }
        }
    }

    @Override
    public void visit(Identifier identifier) {
        SymbolTableVariableItem varDec = null;
        try {
            varDec = (SymbolTableVariableItem) (currentSymbolTable.get(SymbolTableVariableItem.STARTKEY + identifier.getName()));
        }
        catch (ItemNotFoundException e){
            errorCont = "Line:" + identifier.getLine() + ":" + "variable " + identifier.getName().toString() + " is not declared";
            errors.put(errorCont, identifier.getLine());
        }

    }

    @Override
    public void visit(Self self) {
        if (inMain) {
            errorCont = "Line:" + self.getLine() + ":" + "self can't be used in main";
            errors.put(errorCont, self.getLine());
        }
    }

    @Override
    public void visit(Sender sender) {
        if (inInitial) {
            errorCont = "Line:" + sender.getLine() + ":" + "no sender in initial msghandler";
            errors.put(errorCont, sender.getLine());
        }
        if (inMain) {
            errorCont = "Line:" + sender.getLine() + ":" + "sender can't be used in main";
            errors.put(errorCont, sender.getLine());
        }
    }

    @Override
    public void visit(BooleanValue value) {
    }

    @Override
    public void visit(IntValue value) {
    }

    @Override
    public void visit(StringValue value) {
    }

    @Override
    public void visit(Block block) {
        if (block.getStatements() != null) {
            for (Statement statement : block.getStatements()) {
                statement.accept(this);
            }
        }
    }

    @Override
    public void visit(Conditional conditional) {
        if (conditional.getExpression() != null) {
            conditional.getExpression().accept(this);
            if (!(expressionType(conditional.getExpression()) instanceof BooleanType) && !(expressionType(conditional.getExpression()) instanceof NoType)) {
                errorCont = "Line:" + conditional.getLine() + ":" + "condition type must be Boolean";
                errors.put(errorCont, conditional.getLine());
            }
        }

        if (conditional.getThenBody() != null)
            conditional.getThenBody().accept(this);

        if (conditional.getElseBody() != null)
            conditional.getElseBody().accept(this);
    }

    @Override
    public void visit(For loop) {
        inLoop += 1;
        if (loop.getInitialize() != null)
            loop.getInitialize().accept(this);

        if (loop.getCondition() != null) {
            loop.getCondition().accept(this);
            if (!(expressionType(loop.getCondition()) instanceof BooleanType) && !(expressionType(loop.getCondition()) instanceof NoType)) {
                errorCont = "Line:" + loop.getLine() + ":" + "condition type must be Boolean";
                errors.put(errorCont, loop.getLine());
            }
        }

        if (loop.getUpdate() != null)
            loop.getUpdate().accept(this);

        if (loop.getBody() != null)
            loop.getBody().accept(this);

        inLoop -= 1;
    }

    @Override
    public void visit(Break breakLoop) {
        if (inLoop < 1) {
            errorCont = "Line:" + breakLoop.getLine() + ":" + "break statement not within loop";
            errors.put(errorCont, breakLoop.getLine());
        }
    }

    @Override
    public void visit(Continue continueLoop) {
        if (inLoop < 1) {
            errorCont = "Line:" + continueLoop.getLine() + ":" + "continue statement not within loop";
            errors.put(errorCont, continueLoop.getLine());
        }
    }

    @Override
    public void visit(MsgHandlerCall msgHandlerCall) {
        SymbolTableKnownActorItem currentKnownActorItem = null;
        SymbolTableActorItem currentActorItem = null;
        SymbolTableHandlerItem currentHandler = null;
        Type currentType = null;
        SymbolTable currentActor = null;
        Boolean actorDefinedInThisScope = false;

        if (msgHandlerCall.getInstance() != null) {
            msgHandlerCall.getInstance().accept(this);
            SymbolTableVariableItem inst = null;
            try {
                if (!(msgHandlerCall.getInstance() instanceof Sender || msgHandlerCall.getInstance() instanceof Self)) {
                    inst = (SymbolTableVariableItem) currentActorTable.get(SymbolTableVariableItem.STARTKEY + ((Identifier) msgHandlerCall.getInstance()).getName());
                }
            }
            catch (ItemNotFoundException e) {
                actorDefinedInThisScope = true;
                try {
                    inst = (SymbolTableVariableItem) currentSymbolTable.get(SymbolTableVariableItem.STARTKEY + ((Identifier) msgHandlerCall.getInstance()).getName());
                }
                catch (ItemNotFoundException e2) {}
                errorCont = "Line:" + msgHandlerCall.getLine() + ":" + "variable " + inst.getName().toString() + " is not callable";
                errors.put(errorCont, msgHandlerCall.getLine());
            }
            if (!actorDefinedInThisScope) {
                if (!(inst instanceof SymbolTableKnownActorItem) && !(msgHandlerCall.getInstance() instanceof Sender) && !(msgHandlerCall.getInstance() instanceof Self)) {
                    errorCont = "Line:" + msgHandlerCall.getLine() + ":" + "variable " + inst.getName().toString() + " is not callable";
                    errors.put(errorCont, msgHandlerCall.getLine());
                } else {
                    if (msgHandlerCall.getMsgHandlerName() != null) {
                        try {
                            if (!(msgHandlerCall.getInstance() instanceof Self) && !(msgHandlerCall.getInstance() instanceof Sender))
                                currentKnownActorItem = (SymbolTableKnownActorItem) (currentActorTable.get(SymbolTableKnownActorItem.STARTKEY + ((Identifier) msgHandlerCall.getInstance()).getName()));
                        } catch (ItemNotFoundException e) {
                        }
                        if (msgHandlerCall.getInstance() instanceof Self) {
                            Identifier id = new Identifier(currentActorTable.getName());
                            Type curType = new ActorType(id);
                            currentType = curType;
                        } else if (currentKnownActorItem != null) {
                            currentType = currentKnownActorItem.getType();
                        }

                        try {
                            currentActorItem = (SymbolTableActorItem) (SymbolTable.root.get(SymbolTableActorItem.STARTKEY + currentType.toString()));
                        } catch (ItemNotFoundException e) {
                        }

                        if (currentActorItem != null)
                            currentActor = currentActorItem.getActorSymbolTable();

                        try {
                            currentHandler = (SymbolTableHandlerItem) (currentActor.get(SymbolTableHandlerItem.STARTKEY + ((Identifier) msgHandlerCall.getMsgHandlerName()).getName()));
                        } catch (ItemNotFoundException e) {
                            errorCont = "Line:" + msgHandlerCall.getLine() + ":" + "there is no msghandler name " + msgHandlerCall.getMsgHandlerName().getName().toString();
                            errorCont = errorCont + " in actor " + currentType.toString();
                            errors.put(errorCont, msgHandlerCall.getLine());
                        }
                    } else {
                        errorCont = "Line:" + msgHandlerCall.getLine() + ":" + "there is no msghandler name " + msgHandlerCall.getMsgHandlerName().getName().toString();
                        errorCont = errorCont + " in actor " + ((Identifier) msgHandlerCall.getInstance()).getName().toString();
                        errors.put(errorCont, msgHandlerCall.getLine());
                    }
                }
            }
        }


        Boolean errorInArgs = false;
        int argsIdx = 0;
        if (currentHandler != null) {
            ArrayList<VarDeclaration> args = currentHandler.getHandlerDeclaration().getArgs();
            if (args.size() != msgHandlerCall.getArgs().size()) {
                errorCont = "Line:" + msgHandlerCall.getLine() + ":" + "arguments do not match with definition";
                errors.put(errorCont, msgHandlerCall.getLine());
            }
            else if (msgHandlerCall.getArgs() != null) {
                for (Expression expression : msgHandlerCall.getArgs()) {
                    if (args.get(argsIdx).getType().toString() != expressionType(expression).toString())
                        errorInArgs = true;
                    argsIdx++;
                }
                if (errorInArgs) {
                    errorCont = "Line:" + msgHandlerCall.getLine() + ":" + "arguments in msghandler " + msgHandlerCall.getMsgHandlerName().getName().toString();
                    errorCont = errorCont + " does not match with definition";
                    errors.put(errorCont, msgHandlerCall.getLine());
                }
            }
        }
        if (msgHandlerCall.getArgs() != null) {
            for (Expression expression : msgHandlerCall.getArgs()) {
                expression.accept(this);
            }
        }
    }

    @Override
    public void visit(Print print) {

        if (print.getArg() != null) {
            print.getArg().accept(this);
            Type argType = expressionType(print.getArg());
            if (!(argType instanceof NoType) && !(argType instanceof IntType) && !(argType instanceof BooleanType) && !(argType instanceof ArrayType) && !(argType instanceof StringType)){
                errorCont = "Line:" + print.getLine() + ":" + "unsupported type for print";
                errors.put(errorCont, print.getLine());
            }
        }


    }

    @Override
    public void visit(Assign assign) {
        Type rType = null;
        Type lType = null;
        SymbolTableVariableItem actorVaraccess = null;

        if (assign.getlValue() != null) {
            if (!(assign.getlValue() instanceof Identifier) && !(assign.getlValue() instanceof ActorVarAccess) && !(assign.getlValue() instanceof ArrayCall)) {
                errorCont = "Line:" + assign.getLine() + ":" + "left side of assignment must be a valid lvalue";
                errors.put(errorCont, assign.getLine());
            }
            assign.getlValue().accept(this);
            lType = expressionType(assign.getlValue());
        }

        if (assign.getrValue() != null) {
            assign.getrValue().accept(this);
            rType = expressionType(assign.getrValue());
        }
        if (lType != null && rType != null)
            if (rType.toString() != lType.toString() && (rType.toString() != "notype" && lType.toString() != "notype")) {
                errorCont = "Line:" + assign.getLine() + ":" + "unsupported operand type for " + assign.toString();
                errors.put(errorCont, assign.getLine());
            }
    }


    public Type expressionType(Expression expression) {
        Type returnType = null;
        SymbolTableVariableItem actorVaraccess = null;


        if (expression instanceof UnaryExpression) {
            Expression operand = ((UnaryExpression) expression).getOperand();
            String operator = ((UnaryExpression) expression).getUnaryOperator().toString();
            if (operator == "minus" || operator == "preinc" || operator == "postinc" || operator == "predec" || operator == "postDec") {
                if (expressionType(operand) instanceof IntType)
                    returnType = new IntType();
                else
                    returnType = new NoType();
            }
            else if (operator == "not") {
                if (expressionType(operand) instanceof BooleanType)
                    returnType = new BooleanType();
                else
                    returnType = new NoType();
            }
        }


        else if (expression instanceof BinaryExpression) {
            Expression lOperand = ((BinaryExpression) expression).getLeft();
            Expression rOperand = ((BinaryExpression) expression).getRight();
            String operator = ((BinaryExpression) expression).getBinaryOperator().toString();
            if (operator == "add" || operator == "sub" || operator == "mult" || operator == "div" || operator == "mod") {
                if (expressionType(lOperand) instanceof IntType && expressionType(rOperand) instanceof IntType)
                    returnType = new IntType();
                else
                    returnType = new NoType();
            }
            else if (operator == "gt" || operator == "lt") {
                if (expressionType(lOperand) instanceof IntType && expressionType(rOperand) instanceof IntType)
                    returnType = new BooleanType();
                else
                    returnType = new NoType();
            }
            else if (operator == "and" || operator == "or") {
                if (expressionType(lOperand) instanceof BooleanType && expressionType(rOperand) instanceof BooleanType)
                    returnType = new BooleanType();
                else
                    returnType = new NoType();
            }
            else if (operator == "assign") {
                if (expressionType(lOperand).toString() == expressionType(rOperand).toString())
                    returnType = expressionType(lOperand);
                else
                    returnType = new NoType();
            }
            else if (operator == "eq" || operator == "neq") {
                if (expressionType(lOperand).toString() == expressionType(rOperand).toString())
                    returnType = new BooleanType();
                else
                    returnType = new NoType();
            }
        }


        else if (expression instanceof Identifier) {
            try {
                SymbolTableVariableItem idItem = (SymbolTableVariableItem)currentSymbolTable.get(SymbolTableVariableItem.STARTKEY + ((Identifier) expression).getName());
                returnType = idItem.getType();
            }
            catch (ItemNotFoundException e) {
                returnType = new NoType();
            }
        }


        else if (expression instanceof Value)
            returnType = expression.getType();


        else if (expression instanceof ActorVarAccess) {
            try {
                actorVaraccess = (SymbolTableVariableItem) currentActorTable.get(SymbolTableVariableItem.STARTKEY + ((ActorVarAccess) expression).getVariable().getName());
            }
            catch (ItemNotFoundException e) {
                returnType = new NoType();
                return returnType;
            }
            returnType = actorVaraccess.getType();
        }
        else if (expression instanceof ArrayCall){
            returnType = new IntType();
        }
        return returnType;
    }
}
